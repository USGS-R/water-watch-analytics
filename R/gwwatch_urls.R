analyze_urls <- function(path_df){
  #get the rest of the path before query string
  #don't be case sensitive for this analysis
  path_df_asp <- path_df %>% mutate(path_no_query = gsub(x = path_df$pagePath, pattern = "\\?.*", replacement = ""))
  
}