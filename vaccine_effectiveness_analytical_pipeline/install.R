packages <- c("dplyr","arrow","validate","DataExplorer","DT","purrr","dlookr","survminer",
       "quarto","ggplot2","plotly","scales","formattable","naniar","duckdb","DBI","here",
       "grDevices","visdat","mice","tidyr","shiny","consort","parallel","MatchIt",
       "survival","table1","tab","forestmodel","gtsummary","survRM2")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

