library(googleAnalyticsR)
library(googleAuthR)

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
all_watch_pages$all_raw$nwis <- nwis_web_filtered

saveRDS(all_watch_pages, file = 'all_pages.rds')

#page group breakdowns
source('R/gwwatch_urls.R')
gwwatch_urls <- analyze_gww_urls(path_df = all_pages$all_raw$gwwatch)

source('R/water_watch_urls.R')
analyze_wwatch_urls(path_df = all_pages$all_raw$wwatch, nwis_df = all_pages$all_raw$nwis)
analyze_wqwatch_urls(path_df = all_pages$all_raw$wqwatch)



source('R/water_watch_urls.R')
wqwatch_urls <- analyze_wqwatch_urls(all_pages$all_raw$wqwatch)

