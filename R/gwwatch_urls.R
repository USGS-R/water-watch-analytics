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
  
  #pull from google sheet
  plot_human_names <- gs_read_csv(ss = gs_title("Watches url mapping"), ws = "GWW")
  #join on human readable names
  path_df_asp_human_names <- left_join(path_df_asp_summary, plot_human_names, 
                                      by = c(by = "pagePath minus query string")) %>% 
    mutate(contents = ifelse(is.na(contents), yes = "Everything else", no = contents),
           category = ifelse(is.na(contents), yes = "Other", no = category)) %>% 
    filter( contents != "REMOVE")
  
  #do we want summed unique pageviews for a category?  Or sessions that went to a category (would be lower)?
  #maybe content groups will provide this?
  
  asp_plot <- ggplot(path_df_asp_human_names, aes(x = reorder(contents, -uniquePageviews), y = uniquePageviews))+
    geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Page group", y = "Summed unique page views") + 
    scale_y_continuous(labels = format_si()) +
    ggtitle('GroundwaterWatch page groups', 
            subtitle = paste("Previous twelve months from", attr(path_df, "dataPullDate")))         
  ggsave(filename = "gwwatch_path_asp.png", width = 6)
  invisible(list(path_df_asp_human_names, asp_plot))
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
    mutate(network = ifelse(is.na(network), yes = "Other", no = network))
  gwwatch_nets_plot <- ggplot(path_df_ncd_st_aq, aes(x = reorder(network, -uniquePageViews), y = uniquePageViews))+
    geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Network", y = "Summed unique page views") + scale_y_continuous(labels=scales::comma) +
    ggtitle('GroundwaterWatch network views',
            subtitle = paste("Previous twelve months from", attr(path_df, "dataPullDate")))
  ggsave(filename = "gwwatch_networks.png")
  invisible(list(path_df_ncd_st_aq, gwwatch_nets_plot))
}
