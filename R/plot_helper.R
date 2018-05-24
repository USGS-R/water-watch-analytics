#found here: http://www.moeding.net/archives/32-Metric-prefixes-for-ggplot2-scales.html
format_si <- function(...) {
  # Format a vector of numeric values according
  # to the International System of Units.
  # http://en.wikipedia.org/wiki/SI_prefix
  #
  # Based on code by Ben Tupper
  # https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
  # Args:
  #   ...: Args passed to format()
  #
  # Returns:
  #   A function to format a vector of strings using
  #   SI prefix notation
  #
  
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "µ",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")
    
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)
    
    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}

#4-panel plot of pageviews, entrance rate, exit rate, time on page for diff
#groups
panel_ga_plot <- function(path_df_human, title, pull_date, 
                          filename, bar_col, nwis_caption = FALSE) {
  plot_views <- ggplot(path_df_human, aes(x = contents, y = uniquePageviews_sum))+
    geom_col(fill = bar_col) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Page group", y = "Summed unique\npage views") + scale_y_continuous(labels=format_si()) 
  if(nwis_caption) {
    wwatch_plot_views <- wwatch_plot_views + labs(caption = "* May be an underestimate")
  }  
    
  plot_exit <- ggplot(path_df_human, aes(x = contents, y = exitRate))+
    geom_col(fill = bar_col)  + labs(y = "Exit rate") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  
  plot_entrance <- ggplot(path_df_human, aes(x = contents, y = entranceRate))+
    geom_col(fill = bar_col) + labs(y = "Entrance rate") +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  
  plot_time <-  ggplot(path_df_human, aes(x = contents, y = avgTimeOnPage))+
    geom_col(fill = bar_col) + labs(y = "Mean time\non page (s)") +
    ggtitle(title, subtitle = paste(pull_date - 365, "through", pull_date))  + 
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  
  all_plots <- plot_grid(plot_time, plot_entrance, plot_exit, 
                         plot_views, rel_heights = c(1,1,1,3),
                         ncol = 1, align = "v")
  png(filename = filename, height = 9, units = "in", res = 200, width = 6)
  print(all_plots)
  dev.off()
  return(all_plots)
}
