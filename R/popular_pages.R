summarize_pages <- function(ga_df, base_url) {
  summarized_df <- ga_df %>% group_by(pagePath) %>% 
    summarize(uniquePageViews = sum(uniquePageViews)) %>% 
    mutate(fullURL = paste0(base_url, pagePath)) %>% 
    arrange(desc(uniquePageViews)) %>% slice(1:25)
  return(summarized_df)
}


get_popular_pages <- function(wq_segment, no_wq_segment, gwwatch_view, wwatch_view, date_range) {
  wq_seg_data <- google_analytics(viewId = wwatch_view, date_range = date_range,
                                dimensions = c("segment", "pagePath", "pageTitle", "date"),
                                metrics = c("uniquePageViews"), max = -1, 
                                segments = wq_segment, anti_sample = TRUE)
  
  nowq_seg_data <- google_analytics(viewId = wwatch_view, date_range = date_range,
                                  dimensions = c("segment", "pagePath", "pageTitle", "date"),
                                  metrics = c("uniquePageViews"), max = -1, 
                                  segments = no_wq_segment, anti_sample = TRUE)
  gw_data <- google_analytics(viewId = gwwatch_view, date_range = date_range,
                              dimensions = c("pagePath", "pageTitle", "date"),
                              metrics = c("uniquePageViews"), max = -1, 
                               anti_sample = TRUE)
  
  waterwatch_pages <- summarize_pages(nowq_seg_data, 
                                      base_url = "waterwatch.usgs.gov")
  wqwatch_pages <- summarize_pages(wq_seg_data, 
                                   base_url = "waterwatch.usgs.gov/wqwatch")
  gwwatch_pages <- summarize_pages(gw_data, 
                                   base_url = "groundwaterwatch.usgs.gov")
  all_top_pages <- list(wwatch = waterwatch_pages, 
                    wqwatch = wqwatch_pages,
                    gwwatch = gwwatch_pages)
  all_raw <- list(wwatch = nowq_seg_data,
                  wqwatch = wq_seg_data,
                  gwwatch = gw_data)
  all <- list(all_top_pages = all_top_pages, all_raw = all_raw)
  return(all)
}