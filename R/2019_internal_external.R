library(tidyverse)
library(googleAnalyticsR)
library(googleAuthR)
config <- yaml::read_yaml('config.yaml')
gar_auth_service('~/.vizlab/VIZLAB-a48f4107248c.json')
date_range <- c("2018-05-15", "2019-05-15")
#note that there is some double-counting if 
#we sum users with a networkDomain dimension
#A user  (i.e. device) could have had sessions over multiple networks
gww_data <- google_analytics(viewId = config$ga_views$gwwatch,
                            date_range = date_range,
                            metrics = c("sessions", "users"),
                            dimensions = "networkDomain",
                            max = -1, anti_sample = TRUE) %>% 
  mutate(internal = if_else(networkDomain == "usgs.gov", 
                            true = 'internal', false = 'external')) %>% 
  group_by(internal) %>% summarize(sum_sessions = sum(sessions),
                                   sum_users = sum(users)) %>% 
  mutate(sessions_per_user = sum_sessions/sum_users,
         watch = "Groundwater Watch")
  
gww_no_dim <- google_analytics(viewId = config$ga_views$gwwatch,
                            date_range = date_range,
                            metrics = c("sessions", "users"),
                            max = -1, anti_sample = TRUE)

#create custom segment to get total metrics for water quality watch
#Note that segments are defined at the session level - i.e. it will include
#all sessions that accessed a page matching the criteria.  Some sessions
#also accessed pages that don't match this criteria, and that will be reflected
#in the page-level data
#Segments at user level are limited to past 90 days, so no use here
wqwatch_path <- segment_element(name = "pagePath", operator = "BEGINS_WITH",
                                type = "DIMENSION", expressions = "/wqwatch",
                                caseSensitive = TRUE)
wqwatch_define <- segment_define(segment_vector_simple(list(list(wqwatch_path))))
wqwatch_segment <- segment_ga4("wqwatch", session_segment = wqwatch_define)

wqwatch_data <- google_analytics(viewId = config$ga_views$wwatch,
                                 date_range = date_range,
                                 metrics = c("sessions", "users"),
                                 dimensions = "networkDomain",
                                 max = -1, anti_sample = TRUE,
                                 segments = wqwatch_segment) %>% 
  mutate(internal = if_else(networkDomain == "usgs.gov", 
                            true = 'internal', false = 'external')) %>% 
  group_by(internal) %>% summarize(sum_sessions = sum(sessions),
                                   sum_users = sum(users)) %>% 
  mutate(sessions_per_user = sum_sessions/sum_users,
         watch = "Water Quality Watch")

wqwatch_data_no_dim <- google_analytics(viewId = config$ga_views$wwatch,
                                 date_range = date_range,
                                 metrics = c("sessions", "users"),
                                 max = -1, anti_sample = TRUE,
                                 segments = wqwatch_segment)


not_wqwatch_path <- segment_element(name = "pagePath", operator = "BEGINS_WITH",
                                   type = "DIMENSION", expressions = "/wqwatch",
                                   caseSensitive = TRUE, not = TRUE)
not_wqwatch_define <- segment_define(segment_vector_simple(list(list(not_wqwatch_path))))
not_wqwatch_segment <- segment_ga4("no_wqwatch", session_segment = not_wqwatch_define)

wwatch_data <- google_analytics(viewId = config$ga_views$wwatch,
                               segments = not_wqwatch_segment,
                 date_range = date_range,
                 metrics = c("sessions", "users"),
                 dimensions = "networkDomain",
                 max = -1, anti_sample = TRUE) %>% 
  mutate(internal = if_else(networkDomain == "usgs.gov", 
                            true = 'internal', false = 'external')) %>% 
  group_by(internal) %>% summarize(sum_sessions = sum(sessions),
                                   sum_users = sum(users)) %>% 
  mutate(sessions_per_user = sum_sessions/sum_users,
         watch = "Water Watch")

ww_no_dim <- google_analytics(viewId = config$ga_views$wwatch,
                                   date_range = date_range,
                                   metrics = c("sessions", "users"),
                                   max = -1, anti_sample = TRUE,
                                segments = not_wqwatch_segment)

all_data <- bind_rows(wwatch_data, wqwatch_data, gww_data) %>% 
  pivot_wider(names_from = internal, 
              values_from = c("sum_sessions", "sum_users", "sessions_per_user"))  
all_data_formatted <- all_data %>%   
  mutate_at(2:5, comma, digits = 0) %>% 
  mutate_at(6:7, comma, digits = 2) %>% 
  rename(Watch = watch, `External Sessions` = sum_sessions_external,
         `Internal sessions` = sum_sessions_internal,
         `External users` = sum_users_external,
         `Internal users` = sum_users_internal, 
         `Avg. sessions per user (external)` = sessions_per_user_external,
         `Avg. sessions per user (internal)` = sessions_per_user_internal)

library("htmltools")
library("webshot")    
library(formattable)
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}
table <- formattable(all_data_formatted)
export_formattable(table, "table_watches_internal_external.png")
write_csv(x = all_data, "watches_internal_external.csv")
