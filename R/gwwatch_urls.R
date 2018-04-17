library(googlesheets)
library(dplyr)
analyze_gww_urls <- function(path_df){
  #get the rest of the path before query string
  #don't be case sensitive for this analysis
  path_df_asp <- path_df %>% mutate(path_no_query = tolower(gsub(x = path_df$pagePath, 
                                                                 pattern = "\\?.*", 
                                                                 replacement = "")))
  path_df_asp_summary <- group_by(path_df_asp, by = path_no_query) %>% 
    summarise(uniquePageViews = sum(uniquePageViews)) %>% arrange(desc(uniquePageViews))
  path_df_asp_toplot <- slice(path_df_asp_summary, 1:15)
  
  #pull from google sheet
  plot_human_names <- gs_read_csv(ss = gs_title("Watches url mapping"), ws = "GWW")
  #join on human readable names
  path_df_asp_human_names <- left_join(path_df_asp_toplot, plot_human_names, 
                                      by = c(by = "pagePath minus query string")) %>% 
    filter(contents != "REMOVE")
  
  #do we want summed unique pageviews for a category?  Or sessions that went to a category (would be lower)?
  #maybe content groups will provide this?
  
  asp_plot <- ggplot(path_df_asp_human_names, aes(x = reorder(contents, -uniquePageViews), y = uniquePageViews))+
    geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Page group", y = "Unique Page Views")
  ggsave(filename = "gwwatch_path_asp.png")
  return(path_df_asp_summary)
}