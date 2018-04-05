analyze_urls <- function(path_df){
  #get the rest of the path before query string
  #don't be case sensitive for this analysis
  path_df_asp <- path_df %>% mutate(path_no_query = tolower(gsub(x = path_df$pagePath, 
                                                                 pattern = "\\?.*", 
                                                                 replacement = "")))
  path_df_asp_summary <- group_by(path_df_asp, by = path_no_query) %>% 
    summarise(uniquePageViews = sum(uniquePageViews)) %>% arrange(desc(uniquePageViews))
  path_df_asp_toplot <- slice(path_df_asp_summary, 1:15)
  asp_plot <- ggplot(path_df_asp_toplot, aes(x = reorder(by, -uniquePageViews), y = uniquePageViews))+
    geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Page path minus query string", y = "Unique Page Views")
  ggsave(filename = "gwwatch_path_asp.png")
  return(path_df_asp_summary)
}