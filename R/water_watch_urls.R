library(dplyr)
library(googlesheets)
analyze_wqwatch_urls <- function(path_df){
#only main map and state views for WQWatch
path_df_no_string <- path_df %>% filter(grepl(pattern = "wqwatch", x = pagePath, 
                                              ignore.case = TRUE)) %>% 
  mutate(path_no_query = tolower(gsub(x = pagePath, pattern = "\\?.*", 
                                      replacement = ""))) %>% 
  group_by(path_no_query) %>% summarize(uniquePageViews = sum(uniquePageViews)) %>% 
  filter(uniquePageViews > 200) %>% arrange(desc(uniquePageViews))

  #join on human names
  plot_human_names <- gs_read_csv(ss = gs_title("Watches url mapping"), ws = "WQW")
  #join on human readable names
  path_df_human_names <- left_join(path_df_no_string, plot_human_names, 
                                     by = c(path_no_query = "pagePath minus query string"))
  wqwatch_plot <- ggplot(path_df_human_names, aes(x = reorder(contents, -uniquePageViews), y = uniquePageViews))+
    geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Page group", y = "Summed unique Page Views")
  ggsave(filename = "wqwatch_paths.png")
  return(path_df_human_names)
}

#assign human names based on rules from Google sheet
path_df_human <- path_df %>% filter(!grepl(pattern = "wqwatch", x = pagePath, 
                                      ignore.case = TRUE)) %>% 
  mutate(pagePath = gsub(pattern = "index.php", replacement = "", x = pagePath),
                   contents = ifelse(pagePath == "/", yes = "Root home page",
                                     no = "Everything else")) %>% 
  mutate(contents = ifelse(test = (pagePath == "/?id=ww_current" | pagePath == "/?id=real"),
                           yes = "National realtime historical percentile map", no = contents)) %>% 
  mutate(contents = ifelse(test = grepl("id=ww_flood", x = pagePath) & !grepl("r=..", x= pagePath),
         yes = "National flood and high flow map", no = contents)) %>% 
  mutate(contents = ifelse(test = grepl("id=ww_flood", x = pagePath) & grepl("r=..", x= pagePath),
         yes = "State/region flood and high flow maps", no = contents)) %>%
  mutate(contents = ifelse(test = grepl("m=real", x = pagePath) & grepl("r=..", x= pagePath),
         yes = "State/region real time \nhistorical percentile maps", no = contents)) %>% 
  group_by(contents) %>% summarize(uniquePageViews = sum(uniquePageViews))
wwatch_plot <- ggplot(path_df_human, aes(x = reorder(contents, -uniquePageViews), y = uniquePageViews))+
  geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Page group", y = "Summed unique page views") + scale_y_continuous(labels=scales::comma)
ggsave(filename = "wwatch_paths.png")

