library(dplyr)
library(googlesheets)
library(ggplot2)
analyze_wqwatch_urls <- function(path_df){
  config <- yaml::read_yaml('config.yaml')
  #only main map and state views for WQWatch
  path_df_no_string <- path_df %>% filter(grepl(pattern = "wqwatch", x = pagePath, 
                                                ignore.case = TRUE)) %>% 
    mutate(path_no_query = tolower(gsub(x = pagePath, pattern = "\\?.*", 
                                        replacement = ""))) %>% 
    group_by(path_no_query) %>% summarize(uniquePageviews = sum(uniquePageviews)) %>% 
    arrange(desc(uniquePageviews))
  
  #join on human names
  plot_human_names <- gs_read_csv(ss = gs_title("Watches url mapping"), ws = "WQW")
  #join on human readable names
  path_df_human_names <- left_join(path_df_no_string, plot_human_names, 
                                   by = c(path_no_query = "pagePath minus query string")) %>% 
    mutate(contents = ifelse(is.na(contents), yes = "Everything else", no = contents),
           cateogry = ifelse(is.na(contents), yes = "Other", no = category))
  pull_date <- attr(path_df, "dataPullDate")
  wqwatch_plot <- ggplot(path_df_human_names, aes(x = reorder(contents, -uniquePageviews), y = uniquePageviews))+
    geom_col(fill = config$palette$wqw) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Page group", y = "Summed unique Page Views") + 
    scale_y_continuous(labels = format_si()) +
    ggtitle('WaterQualityWatch page groups', 
            subtitle = paste(pull_date - 365, "through", pull_date))
  ggsave(filename = "wqwatch_paths.png")
  invisible(list(path_df_human_names, wqwatch_plot))
}

analyze_wwatch_urls <- function(path_df, nwis_df) {
  config <- yaml::read_yaml('config.yaml')
  #assign human names based on rules from Google sheet
  path_df_human <- path_df %>% filter(!grepl(pattern = "wqwatch", x = pagePath, 
                                             ignore.case = TRUE)) %>% 
    mutate(pagePath = gsub(pattern = "index.php", replacement = "", x = pagePath),
           contents = ifelse(pagePath == "/", yes = "Root home page",
                             no = "Everything else"),
           category = ifelse(pagePath == "/", yes = "Other", no = "Other")) %>% 
    
    mutate(contents = ifelse(test = (pagePath == "/?id=ww_current" | pagePath == "/?id=real"),
                             yes = "National realtime historical percentile map", no = contents),
           category = ifelse(pagePath == "/?id=ww_current", yes = "National", no = category)) %>% 
    
    mutate(contents = ifelse(test = grepl("id=ww_flood", x = pagePath) & !grepl("r=..", x= pagePath),
                             yes = "National flood and high flow map", no = contents),
           category = ifelse(test = grepl("id=ww_flood", x = pagePath) & !grepl("r=..", x= pagePath),
                             yes = "National", no = category)) %>% 
    
    mutate(contents = ifelse(test = grepl("id=ww_flood", x = pagePath) & grepl("r=..", x= pagePath),
                             yes = "State/region flood and high flow maps", no = contents),
           category = ifelse(test = grepl("id=ww_flood", x = pagePath) & grepl("r=..", x= pagePath),
                             yes = "State", no = category)) %>%
    
    mutate(contents = ifelse(test = grepl("[m|id]=real|id=ww_current", x = pagePath) & grepl("r=..", x= pagePath),
                             yes = "State/region real time \nhistorical percentile maps", no = contents),
           category = ifelse(test = grepl("[m|id]=real|id=ww_current", x = pagePath) & grepl("r=..", x= pagePath),
                             yes = "State", no = category)) %>% 
    
    group_by(contents, category) %>% summarize_at(vars(uniquePageviews, pageViews, exits, entrances, timeOnPage),
                                                  funs(sum=sum))
  
  #add in nwis site page traffic from waterwatch
  nwis_df <- nwis_df %>% filter(source == "waterwatch.usgs.gov") 
  nwis_row <- tibble(contents = "NWIS web site page \nreferred from WaterWatch*", 
                     uniquePageviews_sum = sum(nwis_df$uniquePageviews),
                     pageViews_sum = sum(nwis_df$pageViews),
                     exits_sum = sum(nwis_df$exits),
                     entrances_sum = sum(nwis_df$entrances),
                     timeOnPage_sum = sum(nwis_df$timeOnPage),
                     category = "Site")
  path_df_human <- bind_rows(path_df_human, nwis_row) %>% 
    mutate(exitRate = exits_sum/pageViews_sum,
           entranceRate = entrances_sum/pageViews_sum,
           avgTimeOnPage = timeOnPage_sum/(pageViews_sum - exits_sum)) %>% 
    arrange(desc(uniquePageviews_sum))
  pull_date <- attr(path_df, "dataPullDate")
  wwatch_plot_views <- ggplotGrob(ggplot(path_df_human, aes(x = contents, y = uniquePageviews_sum))+
    geom_col(fill = config$palette$ww) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Page group", y = "Summed unique\npage views") + scale_y_continuous(labels=format_si()) +
    labs(caption = "* May be an underestimate")) 
  
  wwatch_plot_exit <- ggplotGrob(ggplot(path_df_human, aes(x = contents, y = exitRate))+
    geom_col(fill = config$palette$ww) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = "Exit rate") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()))
  
  wwatch_plot_entrance <- ggplotGrob(ggplot(path_df_human, aes(x = contents, y = entranceRate))+
    geom_col(fill = config$palette$ww) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = "Entrance rate") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()))
  
  wwatch_plot_time <-  ggplotGrob(ggplot(path_df_human, aes(x = contents, y = avgTimeOnPage))+
    geom_col(fill = config$palette$ww) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = "Mean time\non page (s)") +
    ggtitle('WaterWatch page groups', 
            subtitle = paste(pull_date - 365, "through", pull_date))  + 
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()))
    
  all_plot_grob <- rbind(wwatch_plot_time, wwatch_plot_entrance, wwatch_plot_exit, wwatch_plot_views, size = 'first')
  
  all_plot_grob$widths <- grid::unit.pmax(wwatch_plot_time$widths, wwatch_plot_entrance$widths, 
                                          wwatch_plot_exit$widths, wwatch_plot_views$widths)
  ggsave(filename = "wwatch_paths.png", plot = all_plot_grob, height = 8)
  
  invisible(list(path_df_human, all_plot_grob))
}

mutate_grep_query_param <- function(df, name, query_param) {
  df_net <- df %>% mutate(network = ifelse(test = grepl(pattern = query_param,
                                                        x = pagePath, ignore.case = TRUE),
                                           yes = name, no = network))
  return(df_net)
}

#same for wqwatch too
wwatch_networks <- function(path_df, ws, plot_file) {
  #now network views
  wwatch_networks <- gs_read_csv(gs_title('Watches url mapping'), ws = ws)
  pull_date <- attr(path_df, "dataPullDate")
  config <- yaml::read_yaml('config.yaml')
  if(ws == "ww_networks"){
    title <- "Water Watch network views"
    bar_fill <- config$palette$ww
    path_df <- path_df %>% filter(!grepl(pattern = "wqwatch", x = pagePath)) %>% mutate(network=NA)
  } else if(ws == "wqw_networks") {
    title <- "Water Quality Watch network views"
    bar_fill <- config$palette$wqw
    path_df <- path_df %>% filter(grepl(pattern = "wqwatch", x = pagePath)) %>% 
      mutate(network=ifelse(pagePath == "/wqwatch/", yes = "Temperature", no = NA))
  }
  
  for(i in seq_along(wwatch_networks$name)) {
    path_df <- mutate_grep_query_param(df = path_df, name = wwatch_networks$name[i],
                                       query_param = wwatch_networks$query_param[i]) 
  }
  
  networks_df <- path_df %>% group_by(network) %>% summarize(uniquePageviews = sum(uniquePageviews), 
                                                             n = n()) %>% mutate(network = ifelse(is.na(network),
                                                                                                  yes = "Other",
                                                                                                  no = network))
  wwatch_nets_plot <- ggplot(networks_df, aes(x = reorder(network, -uniquePageviews), y = uniquePageviews))+
    geom_col(fill = bar_fill) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Network", y = "Summed unique page views") + scale_y_continuous(labels=format_si()) +
    ggtitle(title, subtitle = paste(pull_date - 365, "through", pull_date))
  ggsave(filename = plot_file)
  invisible(list(networks_df, wwatch_nets_plot))
}

