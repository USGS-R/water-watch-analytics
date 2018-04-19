library(googlesheets)
library(dplyr)
analyze_gww_urls <- function(path_df){
  #get the rest of the path before query string
  #don't be case sensitive for this analysis
  path_df_asp <- path_df %>% mutate(path_no_query = tolower(gsub(x = path_df$pagePath, 
                                                                 pattern = "\\?.*", 
                                                                 replacement = "")))
  path_df_asp_summary <- group_by(path_df_asp, by = path_no_query) %>% 
    summarise(uniquePageviews = sum(uniquePageViews)) %>% arrange(desc(uniquePageviews))
  path_df_asp_toplot <- slice(path_df_asp_summary, 1:15)
  
  #pull from google sheet
  plot_human_names <- gs_read_csv(ss = gs_title("Watches url mapping"), ws = "GWW")
  #join on human readable names
  path_df_asp_human_names <- left_join(path_df_asp_toplot, plot_human_names, 
                                      by = c(by = "pagePath minus query string")) %>% 
    filter(contents != "REMOVE")
  
  #do we want summed unique pageviews for a category?  Or sessions that went to a category (would be lower)?
  #maybe content groups will provide this?
  
  asp_plot <- ggplot(path_df_asp_human_names, aes(x = reorder(contents, -uniquePageviews), y = uniquePageviews))+
    geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Page group", y = "Summed unique page views*")
  ggsave(filename = "gwwatch_path_asp.png")
  invisible(path_df_asp_human_names)
}

gwwatch_networks <- function(path_df) {
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
    filter(!is.na(network
    ))
  gwwatch_nets_plot <- ggplot(path_df_ncd_st_aq, aes(x = reorder(network, -uniquePageViews), y = uniquePageViews))+
    geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Network", y = "Summed unique page views*") + scale_y_continuous(labels=scales::comma)
  ggsave(filename = "gwwatch_networks.png")
  invisible(path_df_ncd_st_aq)
}
