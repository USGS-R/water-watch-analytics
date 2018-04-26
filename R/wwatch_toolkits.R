library(googlesheets)
library(ggplot2)
library(dplyr)

ww_toolkit_plot <- function() {
  config <- yaml::read_yaml('config.yaml')
  toolkit_query_strings <- gs_read_csv(ss = gs_title(config$url_sheet_name),
                                       ws = "ww toolkits") %>% 
    mutate(query_string_id = paste0("/?id=", query_string_id))
  all_pages <- readRDS('all_pages.rds')
  #TODO: internal traffic, unique users
  
  #remove index.php, exact match?
  wwatch_pages <- all_pages$all_raw$wwatch %>% 
    mutate(pagePath = gsub("index.php", replacement = "", x= pagePath))
  
  toolkit_pages <- left_join(wwatch_pages, toolkit_query_strings, 
                             by = c(pagePath = "query_string_id")) %>% 
    group_by(toolkit_name) %>% summarize(uniquePageViews = sum(uniquePageViews)) %>% 
    filter(!is.na(toolkit_name)) %>% arrange(desc(uniquePageViews))
  toolkit_plot <- ggplot(toolkit_pages, aes(x = reorder(toolkit_name, -uniquePageViews), y = uniquePageViews))+
    geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Toolkit name", y = "Summed unique page views\nof toolkit home page") + scale_y_continuous(labels=scales::comma) +
    ggtitle("Unique page views of toolkit main pages", 
            subtitle = paste("Previous twelve months from", attr(all_pages$all_raw$wwatch, "dataPullDate")))
  
  ggsave(filename = "toolkits.png")
  return(toolkit_plot)
}