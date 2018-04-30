library(googleAnalyticsR)
library(googleAuthR)
library(zeallot)
library(assertthat)

config <- yaml::read_yaml('config.yaml')
wqwatch_segment <- readRDS('wqwatch_segment.rds')
no_wqwatch_segment <- readRDS('no_wqwatch_segment.rds')

#now get most popular pages for last year
source('R/popular_pages.R')
date_range <- c(start = Sys.Date() - 365, end = Sys.Date())
all_watch_pages <- get_popular_pages(wq_segment = wqwatch_segment, 
                                     no_wq_segment = no_wqwatch_segment,
                                     wwatch_view = config$ga_views$wwatch,
                                     gwwatch_view = config$ga_views$gwwatch,
                                     date_range)
gar_auth_service(config$ga_token)

nwis_web <- google_analytics(viewId = config$ga_views$nwisweb_desktop, 
                             date_range = date_range, metrics = "uniquePageviews",
                             dimensions = c("source", "pagePath", "deviceCategory"), 
                             max = -1)

nwis_web_filtered <- nwis_web %>% filter(grepl(pattern = "site_no", x = pagePath) &
                                           grepl(patter="waterwatch", x= source))
attr(nwis_web_filtered, "dataPullDate") <- Sys.Date()
all_watch_pages$all_raw$nwis <- nwis_web_filtered
assert_that(attr(nwis_web_filtered, "dataPullDate") == attr(all_watch_pages$all_raw$wwatch, "dataPullDate"))
saveRDS(all_watch_pages, file = 'all_pages.rds')

#page group breakdowns
source('R/plot_helper.R')
source('R/gwwatch_urls.R')
gwwatch_urls <- analyze_gww_urls(path_df = all_watch_pages$all_raw$gwwatch) %>% 
  mutate(watch = "GroundwaterWatch")

source('R/water_watch_urls.R')
c(df, plot) %<-% analyze_wwatch_urls(path_df = all_watch_pages$all_raw$wwatch, 
                                     nwis_df = all_watch_pages$all_raw$nwis) %>% 
  mutate(watch = "WaterWatch")
wqwatch_urls <- analyze_wqwatch_urls(all_watch_pages$all_raw$wqwatch) %>% 
  mutate(watch = "WaterQualityWatch")

#network-level plots
gww_networks <- gwwatch_networks(all_watch_pages$all_raw$gwwatch)
ww_networks <- wwatch_networks(all_watch_pages$all_raw$wwatch, ws = "ww_networks", 
                               plot_file = "wwatch_networks.png")
wq_networks <- wwatch_networks(all_watch_pages$all_raw$wqwatch, ws = "wqw_networks",
                               plot_file = "wq_networks.png")
source('R/total_sessions.R')
all_cat_plot <- all_watch_plot(gwwatch_urls, wwatch_urls, wqwatch_urls, 
                               pullDate = attr(all_watch_pages$all_raw$wwatch, "dataPullDate"))
