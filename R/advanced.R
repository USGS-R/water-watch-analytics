config <- yaml::read_yaml('config.yaml')
wqwatch_segment <- readRDS('wqwatch_segment.rds')
no_wqwatch_segment <- readRDS('no_wqwatch_segment.rds')

#now get most popular pages for last year
source('R/popular_pages.R')
all_pages <- get_popular_pages(wq_segment = wqwatch_segment, 
                               no_wq_segment = no_wqwatch_segment,
                               wwatch_view = config$ga_views$wwatch,
                               gwwatch_view = config$ga_views$gwwatch)
saveRDS(all_pages, file = 'all_pages.rds')

#water watch url breakdown
source('R/gwwatch_urls.R')
gwwatch_urls <- analyze_gww_urls(path_df = all_pages$all_raw$gwwatch)
