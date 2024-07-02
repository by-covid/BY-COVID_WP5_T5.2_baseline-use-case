packages <- c("dplyr","arrow","validate","DataExplorer","DT","purrr","dlookr","survminer",
       "quarto","ggplot2","plotly","scales","formattable","naniar","duckdb","DBI","here",
       "grDevices","visdat","mice","tidyr","shiny","consort","MatchIt",
       "survival","table1","tab","forestmodel","gtsummary","survRM2","log4R","finalfit","dbplyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages],repos ="http://cran.us.r-project.org")
}
