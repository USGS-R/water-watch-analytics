
sites_path <- segment_element(name = "pagePath", operator = "BEGINS_WITH",
                                type = "DIMENSION", expressions = "/awlsites.asp",
                                caseSensitive = FALSE)
date_range <- c(start = Sys.Date() - 90, end = Sys.Date())
sites_define <- segment_define(segment_vector_simple(list(list(sites_path))))
sites_segment <- segment_ga4("sitePages", session_segment = sites_define)
sites_seg_data <- google_analytics(viewId = gwwatch_view, date_range = date_range,
                                dimensions = "pagePath",
                                metrics = c("sessions", "uniquePageViews"), max = -1, 
                                segments = sites_segment, anti_sample = TRUE)
