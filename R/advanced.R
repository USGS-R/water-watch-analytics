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
gwwatch_urls <- analyze_gww_urls(path_df = all_watch_pages$all_raw$gwwatch) %>% 
  mutate(watch = "GroundwaterWatch")

source('R/water_watch_urls.R')
wwatch_urls <- analyze_wwatch_urls(path_df = all_watch_pages$all_raw$wwatch, 
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

#overall category plot
all_watch_categories <- bind_rows(gwwatch_urls, wwatch_urls, wqwatch_urls) %>% 
  #removing some categories that aren't data views 
  filter(category != "REMOVE")  %>% group_by(category, watch) %>% 
  summarize(uniquePageviews = sum(uniquePageviews))

watch_cat_plot <- ggplot(all_watch_categories, aes(x = category, y = uniquePageviews))+
  geom_col(aes(fill = watch)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Page Category", y = "Summed unique page views* of category pages") + 
  scale_y_continuous(labels=scales::comma) + 
  scale_x_discrete(limits = c("Site", "State", "National", "Other"))
ggsave(filename = "all_watches_categories.png")
