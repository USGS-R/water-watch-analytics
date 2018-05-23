get_popular_pages <- function(wq_segment, no_wq_segment, gwwatch_view, wwatch_view, date_range) {
  metrics <- c("uniquePageviews", "pageViews","exits", "timeOnPage", "entrances")
  dimensions <- c("pagePath")
  
  wq_seg_data <- google_analytics(viewId = wwatch_view, date_range = date_range,
                                dimensions = c("segment", dimensions),
                                metrics = metrics, max = -1, 
                                segments = wq_segment, anti_sample = TRUE)
  
  nowq_seg_data <- google_analytics(viewId = wwatch_view, date_range = date_range,
                                  dimensions = c("segment", dimensions),
                                  metrics = metrics, max = -1, 
                                  segments = no_wq_segment, anti_sample = TRUE)
  gw_data <- google_analytics(viewId = gwwatch_view, date_range = date_range,
                              dimensions = dimensions,
                              metrics = metrics, max = -1, 
                               anti_sample = TRUE)
  
  attr(nowq_seg_data, "dataPullDate") <- Sys.Date()
  attr(wq_seg_data, "dataPullDate") <- Sys.Date()
  attr(gw_data, "dataPullDate") <- Sys.Date()
  
  all_raw <- list(wwatch = nowq_seg_data,
                  wqwatch = wq_seg_data,
                  gwwatch = gw_data)
  all <- list(all_raw = all_raw)
  return(all)
}
