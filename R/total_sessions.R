library(googleAnalyticsR)
library(googleAuthR)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

total_sessions_users_plot <- function() {
  config <- yaml::read_yaml('config.yaml')
  wwatch_view <- config$ga_views$wwatch
  gwwatch_view <- config$ga_views$gwwatch
  
  gar_auth_service(config$ga_token)
  out_file <- 'total_sessions_users.rds'
  if(file.exists(out_file)) {
    all_watch_data <- readRDS(out_file)
  } else {
    #create custom segment to get total metrics for water quality watch
    #Note that segments are defined at the session level - i.e. it will include
    #all sessions that accessed a page matching the criteria.  Some sessions
    #also accessed pages that don't match this criteria, and that will be reflected
    #in the page-level data
    wqwatch_path <- segment_element(name = "pagePath", operator = "BEGINS_WITH",
                                    type = "DIMENSION", expressions = "/wqwatch",
                                    caseSensitive = TRUE)
    wqwatch_define <- segment_define(segment_vector_simple(list(list(wqwatch_path))))
    wqwatch_segment <- segment_ga4("wqwatch", session_segment = wqwatch_define)
    #first data is May 2017
    wq_seg_data <- google_analytics(viewId = wwatch_view, date_range = c(start = as.Date("2017-04-01"),
                                                                         end = Sys.Date()),
                                    dimensions = c("year","month", "segment"),
                                    metrics = c("sessions", "users"), max = -1, 
                                    segments = wqwatch_segment, anti_sample = TRUE)
    
    #TODO: Is this segment excluding traffic that went to both WQWatch and WaterWatch?  
    #Total WQWatch traffic is insignificant compared to other watches though,
    #so not worrying about it for now
    no_wqwatch_path <- segment_element(name = "pagePath", operator = "BEGINS_WITH",
                                       type = "DIMENSION", expressions = "/wqwatch",
                                       caseSensitive = TRUE, not = TRUE)
    no_wqwatch_define <- segment_define(segment_vector_simple(list(list(no_wqwatch_path))))
    no_wqwatch_segment <- segment_ga4("no_wqwatch", session_segment = no_wqwatch_define)
    
    no_wq_seg_data <- google_analytics(viewId = wwatch_view, date_range = c(start = as.Date("2010-01-01"),
                                                                            end = Sys.Date()),
                                       dimensions = c("year","month", "segment"),
                                       metrics = c("sessions", "users"), max = -1, 
                                       segments = no_wqwatch_segment, anti_sample = TRUE)
    
    gw_watch_data <- google_analytics(viewId = gwwatch_view, date_range = c(start = as.Date("2010-01-01"),
                                                                            end = Sys.Date()),
                                      dimensions = c("year","month"),
                                      metrics = c("sessions", "users"), max = -1, 
                                      anti_sample = TRUE)
    gw_watch_data_segment <- gw_watch_data %>% mutate(segment = "gwwatch") %>% filter(sessions > 0)
    
    all_watch_data <- bind_rows(no_wq_seg_data, wq_seg_data, gw_watch_data_segment) %>% 
      mutate(yearmon = zoo::as.yearmon(paste(year, month, sep = "-")),
             segment = gsub(pattern = "gwwatch", replacement = "GroundWaterWatch", x= segment),
             segment = gsub(pattern = "no_wqwatch", replacement = "WaterWatch (no WQ)", x= segment),
             segment = gsub(pattern = "wqwatch", replacement = "WQ Watch", x= segment)) %>% 
      filter(yearmon < zoo::as.yearmon("2018-04"))
    saveRDS(object = all_watch_data, file = out_file)
    saveRDS(object = wqwatch_segment, file = "wqwatch_segment.rds")
    saveRDS(object = no_wqwatch_segment, file = "no_wqwatch_segment.rds")
  }
  sessions_plot <- ggplot(data=all_watch_data,
                          aes(x=yearmon, y=sessions, colour=segment)) +
    geom_line() + scale_y_continuous(labels = scales::comma,
                                     minor_breaks = seq(from = 0, to = 250000, by = 10000)) + 
    labs(x = "Date", y = "Sessions", color= "Watch") + 
    ggtitle('Monthly Session Counts', subtitle = "Through March 2018")  
  ggsave(filename = "sessions.png", plot = sessions_plot)
  
  users_plot <- ggplot(data=all_watch_data,
                       aes(x=yearmon, y=users, colour=segment)) +
    geom_line() + scale_y_continuous(labels = scales::comma,
                                     minor_breaks = seq(from = 0, to = 200000, by = 5000)) + 
    labs(x = "Date", y = "Users", color= "Watch") + 
    ggtitle('Monthly User Counts', subtitle = "Through March 2018")
  ggsave(filename = 'users.png', plot = users_plot)
  
  
  return(list(sessions_plot, users_plot))
}

all_watch_plot <- function(gwwatch_urls, wwatch_urls, wqwatch_urls,  pullDate) {
  #overall category plot
  all_watch_categories <- bind_rows(gwwatch_urls, wwatch_urls, wqwatch_urls) %>% 
    #removing some categories that aren't data views 
    filter(category != "REMOVE")  %>% group_by(category, watch) %>% 
    summarize(uniquePageviews = sum(uniquePageviews))
  
  watch_cat_plot <- ggplot(all_watch_categories, aes(x = category, y = uniquePageviews))+
    geom_col(aes(fill = watch)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Page Category", y = "Summed unique page views of category pages") + 
    scale_y_continuous(labels=format_si()) + 
    scale_x_discrete(limits = c("Site", "State", "National", "Other")) +
    ggtitle("Page categories across all watches",
            subtitle = paste("Previous twelve months from", pullDate))
  ggsave(filename = "all_watches_categories.png")
  return(watch_cat_plot)
}