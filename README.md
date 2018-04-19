## Water Watches Analytics

#### config.yaml
Contains the relevant Google Analytics view ids

#### R/total_sessions.R
Defines segments to split WaterQualityWatch from WaterWatch; pulls long-term overal monthly session/user counts for all three watches and makes plots

#### R/advanced.R
Main script that runs functions in other files. Generates plots for popular page groups and networks for each watch, plus overall stacked bar chart of different page categories for all watches.

#### R/gwwatch_urls.R and R/water_watch_urls.R
Functions for creating popular page groups and networks plots for each watch.  `water_watch_urls.R` also handles WaterQualityWatch.

#### R/wwatch_toolkits.R
Stand-alone script (other than loading `all_pages.rds`) that generates plot of WaterWatch toolbox unique pageviews.