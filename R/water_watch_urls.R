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

analyze_wwatch_urls <- function(path_df) {
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
  
}

mutate_grep_query_param <- function(df, name, query_param) {
  df_net <- df %>% mutate(network = ifelse(test = grepl(pattern = query_param,
                                                        x = pagePath),
                                           yes = name, no = network))
  return(df_net)
}

#same for wqwatch too
wwatch_networks <- function(path_df, ws, plot_file) {
  #now network views
  wwatch_networks <- gs_read_csv(gs_title('Watches url mapping'), ws = ws)
  if(ws == "ww_networks"){
    path_df <- path_df %>% filter(!grepl(pattern = "wqwatch", x = pagePath)) %>% mutate(network=NA)
  } else if(ws == "wqw_networks") {
    path_df <- path_df %>% filter(grepl(pattern = "wqwatch", x = pagePath)) %>% 
      mutate(network=ifelse(pagePath == "/wqwatch/", yes = "Temperature", no = NA))
  }
  
  for(i in seq_along(wwatch_networks$name)) {
    path_df <- mutate_grep_query_param(df = path_df, name = wwatch_networks$name[i],
                                       query_param = wwatch_networks$query_param[i]) 
  }
  
  networks_df <- path_df %>% group_by(network) %>% summarize(uniquePageViews = sum(uniquePageViews), 
                                                             n = n()) %>% filter(!is.na(network))
  wwatch_nets_plot <- ggplot(networks_df, aes(x = reorder(network, -uniquePageViews), y = uniquePageViews))+
    geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Network", y = "Summed unique page views") + scale_y_continuous(labels=scales::comma)
  ggsave(filename = plot_file)
}
