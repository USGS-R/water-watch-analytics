library(googlesheets)
library(dplyr)
library(cowplot)
analyze_gww_urls <- function(path_df){
  config <- yaml::read_yaml('config.yaml')
  #get the rest of the path before query string
  #don't be case sensitive for this analysis
  path_df_asp <- path_df %>% mutate(path_no_query = tolower(gsub(x = path_df$pagePath, 
                                                                 pattern = "\\?.*", 
                                                                 replacement = "")))
  #pull from google sheet
  plot_human_names <- gs_read_csv(ss = gs_title("Watches url mapping"), ws = "GWW")
  #join on human readable names
  path_df_asp_human_names <- left_join(path_df_asp, plot_human_names, 
                                      by = c("path_no_query" = "pagePath minus query string")) %>% 
    mutate(contents = ifelse(is.na(contents), yes = "Everything else", no = contents),
           category = ifelse(is.na(category), yes = "Other", no = category)) %>% 
    filter( contents != "REMOVE") %>% group_by(contents, category) %>% summarize_at(vars(uniquePageviews, pageViews, exits, entrances,
                                                                                         timeOnPage), funs(sum=sum)) %>% 
    arrange(desc(uniquePageviews_sum)) %>% 
    mutate(exitRate = exits_sum/pageViews_sum,
           entranceRate = entrances_sum/pageViews_sum,
           avgTimeOnPage = timeOnPage_sum/(pageViews_sum - exits_sum))
  
  #do we want summed unique pageviews for a category?  Or sessions that went to a category (would be lower)?
  #maybe content groups will provide this?
  asp_plot <- panel_ga_plot(path_df_asp_human_names, title = "Groundwater Watch page groups",
                pull_date = attr(path_df, "dataPullDate"), 
                filename = "gwwatch_paths.png", bar_col = config$palette$gww)
  invisible(list(path_df_asp_human_names, asp_plot))
}

gwwatch_networks <- function(path_df) {
  config <- yaml::read_yaml('config.yaml')
  gww_networks <- gs_read_csv(gs_title('Watches url mapping'), ws = "gww_networks")
  path_df_ncd <- path_df %>% mutate(ncd = stringr::str_extract(pagePath, "ncd=[:alnum:]{0,6}"),
                                    network = NA) %>% filter(!grepl(pattern = "awlsite", x = pagePath,
                                                                    ignore.case = TRUE))
  for(i in seq_along(gww_networks$name)) {
    path_df_ncd <- mutate_grep_query_param(df = path_df_ncd, name = gww_networks$name[i],
                                           query_param = gww_networks$query_param[i]) 
  }
  #assuming everything left with a non-empty ncd is a state/aquifer network
  path_df_ncd_st_aq <- path_df_ncd %>% mutate(network = ifelse(is.na(network) & !is.na(ncd) & ncd != "ncd=", 
                                                               yes = "State/local/aquifer",
                                                               no = network)) %>% 
    mutate(network = ifelse(is.na(network), yes = "Other", no = network))
  pull_date <- attr(path_df, "dataPullDate")
  gwwatch_nets_plot <- ggplot(path_df_ncd_st_aq, aes(x = reorder(network, -uniquePageviews), y = uniquePageviews))+
    geom_col(fill = config$palette$gww) + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                                plot.subtitle = element_text(hjust = 0.5)) +
    labs(x = "Network", y = "Summed unique page views") + scale_y_continuous(labels=format_si()) +
    ggtitle('GroundwaterWatch network views',
            subtitle = paste(pull_date - 365, "through", pull_date))
  ggsave(filename = "gwwatch_networks.png")
  invisible(list(path_df_ncd_st_aq, gwwatch_nets_plot))
}
