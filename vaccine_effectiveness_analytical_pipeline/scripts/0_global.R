################
### Metadata ###
################

# DATE LAST MODIFIED:
# 19/09/2023

# METADATA: 
if(FALSE) {
  title      <- 'BY-COVID WP5.2 Baseline Use Case: SARS-CoV-2 vaccine effectiveness - analytical pipeline - general settings and loading of data'
  author     <- list('Marjan Meurisse','Javier González-Galindo','Francisco Estupiñán-Romero','Santiago Royo-Sierra','Nina Van Goethem','Enrique Bernal-Delgado')
  version    <- '1.0.2'
  maintainer <- 'Marjan Meurisse'
  email      <- 'Marjan.Meurisse@sciensano.be'
  input      <- list('csv upload')
  output     <- list('BY-COVID-WP5-BaselineUseCase-VE.duckdb (database)','cohort_data (database table in BY-COVID-WP5-BaselineUseCase-VE.duckdb)')
}

########################
### General settings ###
########################

### Description: load required packages
x <- c("dplyr","arrow","validate","DataExplorer","DT","purrr","dlookr","survminer",
       "quarto","ggplot2","plotly","scales","formattable","naniar","duckdb","DBI","here",
       "grDevices","visdat","mice","tidyr","shiny","consort","parallel","MatchIt","logger",
       "survival","table1","tab","forestmodel","gtsummary","survRM2","knitr","log4r","xlsx","finalfit","dbplyr") 
lapply(x, require, character.only = TRUE)

### Description: directories
input_data_path <- here('input')
output_data_path <- here('output')
auxiliary_database_name <- 'BY-COVID-WP5-BaselineUseCase-VE.duckdb'
auxiliary_database_path <- file.path(input_data_path, auxiliary_database_name)
log_path <- here('logs')
log_file_name <- 'logfile.txt'
log_file_path <- file.path(log_path, log_file_name)
logger <- logger("DEBUG", appender = file_appender(log_file_path, append=TRUE, layout=default_log_layout()))
logger_simple <- logger("DEBUG", appender = file_appender(log_file_path, append=TRUE, layout=simple_log_layout()))

### Description: function f_load_data
f_load_data <- function(create_db_tables=FALSE, load_data=FALSE) {
  
  if(create_db_tables) {
    
    ### Create header log file ###
    
    basePkgs <- paste(sessionInfo()$basePkgs, collapse = " ")
    otherPkgs <- c()
    for (i in 1:length(sessionInfo()$otherPkgs)) {otherPkgs <- c(otherPkgs,paste0(sessionInfo()$otherPkgs[[i]][["Package"]],"_",sessionInfo()$otherPkgs[[i]][["Version"]]))}
    otherPkgs <- paste(otherPkgs, collapse = " ")
    loadedOnly <- c()
    for (i in 1:length(sessionInfo()$loadedOnly)) {loadedOnly <- c(loadedOnly,paste0(sessionInfo()$loadedOnly[[i]][["Package"]],"_",sessionInfo()$loadedOnly[[i]][["Version"]]))}
    loadedOnly <- paste(loadedOnly, collapse = " ")
    info(logger, 
     paste0("
========================================================================================
",
"Log Path: ", log_file_path, "
", 
"Working Directory: ", getwd(), "
",
"R version: ", R.version$version.string, "
",
"Machine: ", Sys.info()[["nodename"]], " ", Sys.info()[["machine"]],"
",
"Operating System: ", sessionInfo()$running, "
",
"Base Packages Attached: ", basePkgs, "
",
"Other Packages Attached: ", otherPkgs, "
",
"Packages loaded via a namespace (and not attached): ", otherPkgs, "
",
"========================================================================================
     "))

    ### Log of 0_global.R ###
    
    info(logger, 
    paste0("
========================================================================================
",
"0_global.R","
",
"========================================================================================
    "))
    
    ### Create necessary auxiliary tables ###
    
    info(logger_simple, "CREATING NECESSARY AUXILIARY TABLES...")
    
    createDB <- function() {
      
      tryCatch(
        {
          ## get database connection
          con = dbConnect(duckdb::duckdb(), dbdir=auxiliary_database_path, read_only=FALSE)
          
          ## drop tables if the script has been executed previously
          dbExecute(con, "DROP TABLE IF EXISTS cohort_data;")
          
          ## create 'cohort_data' table
          dbExecute(con, "CREATE OR REPLACE TABLE cohort_data (
                  	person_id VARCHAR,
                  	age_nm TINYINT,
                  	sex_cd VARCHAR,
                  	socecon_lvl_cd VARCHAR,
                  	residence_area_cd VARCHAR,
                  	country_cd VARCHAR,
                  	foreign_bl BOOLEAN,
                  	exitus_dt DATE,
                  	exitus_bl BOOLEAN,
                  	essential_worker_bl BOOLEAN,
                  	institutionalized_bl BOOLEAN,
                  	dose_1_brand_cd VARCHAR,
                  	dose_1_dt DATE,
                  	dose_2_brand_cd VARCHAR,
                  	dose_2_dt DATE,
                  	dose_3_brand_cd VARCHAR,
                  	dose_3_dt DATE,
                  	doses_nm TINYINT,
                  	fully_vaccinated_dt DATE,
                  	fully_vaccinated_bl BOOLEAN,
                  	vaccination_schedule_cd VARCHAR,
                  	confirmed_case_dt DATE,
                  	confirmed_case_bl BOOLEAN,
                  	previous_infection_dt DATE,
                  	previous_infection_bl BOOLEAN,
                  	test_type_cd VARCHAR,
                  	variant_cd VARCHAR,
                  	diabetes_bl BOOLEAN,
                  	obesity_bl BOOLEAN,
                  	heart_failure_bl BOOLEAN,
                  	copd_bl BOOLEAN,
                  	solid_tumor_without_metastasis_bl BOOLEAN,
                  	chronic_kidney_disease_bl BOOLEAN,
                  	sickle_cell_disease_bl BOOLEAN,
                  	hypertension_bl BOOLEAN,
                  	chronic_liver_disease_bl BOOLEAN,
                  	blood_cancer_bl BOOLEAN,
                  	transplanted_bl BOOLEAN,
                  	hiv_infection_bl BOOLEAN,
                  	primary_immunodeficiency_bl BOOLEAN,
                  	immunosuppression_bl BOOLEAN,
                  	pregnancy_bl BOOLEAN,
                  	flag_violating_val BOOLEAN DEFAULT FALSE,
                  	flag_listwise_del BOOLEAN DEFAULT FALSE
          )")
          
          ## Log info
          info(logger_simple,paste0("Database path: ",auxiliary_database_path))
          tables <- dbGetQuery(con, "SHOW tables")
          info(logger_simple, paste0("Database tables: ", paste(tables[,'name'], collapse = " ")))
          if("cohort_data" %in% tables[,'name']) {
            info(logger_simple, "Table cohort_data created")
          }
          
        },
        error=function(cond) {
          ## Log info
          warn(logger, paste0("MY ERROR: 
                              ", cond))
          return(stop(cond))
        },
        finally={
          ## disconnect from database
          dbDisconnect(con, shutdown=TRUE)
        }
      )
    }
    
    createDB()
    
  }
  
  if(load_data) {
    
    ### Insert data into 'cohort_data' table ###
    
    info(logger_simple, "INSERT DATA INTO 'COHORT_DATA' TABLE...")
    
    read_large_dataset <- function() {
      
      tryCatch(
        {
          ## get database connection
          con = dbConnect(duckdb::duckdb(), dbdir=auxiliary_database_path, read_only=FALSE)
          
          ## Log info
          info(logger_simple,paste0("Number of rows in table cohort_data before: ", dbGetQuery(con,"SELECT COUNT() FROM cohort_data ;")[[1]]))
          
          ## open/read dataset 
          
          # See variable format common data model
          schema <- arrow::schema(
            field("person_id",string(),nullable=TRUE),
            field("age_nm",int64(),nullable=TRUE),
            field("sex_cd",string(),nullable=TRUE),
            field("socecon_lvl_cd",string(),nullable=TRUE),
            field("residence_area_cd",string(),nullable=TRUE),
            field("country_cd",string(),nullable=TRUE),
            field("foreign_bl",bool(),nullable=TRUE),
            field("exitus_dt",date32(),nullable=TRUE), 
            field("exitus_bl",bool(),nullable=TRUE),
            field("essential_worker_bl",bool(),nullable=TRUE), 
            field("institutionalized_bl",bool(),nullable=TRUE), 
            field("dose_1_brand_cd",string(),nullable=TRUE),
            field("dose_1_dt",date32(),nullable=TRUE),
            field("dose_2_brand_cd",string(),nullable=TRUE),
            field("dose_2_dt",date32(),nullable=TRUE),
            field("dose_3_brand_cd",string(),nullable=TRUE),
            field("dose_3_dt",date32(),nullable=TRUE),
            field("doses_nm",double(),nullable=TRUE),
            field("fully_vaccinated_dt",date32(),nullable=TRUE),
            field("fully_vaccinated_bl",bool(),nullable=TRUE),
            field("vaccination_schedule_cd",string(),nullable=TRUE),
            field("confirmed_case_dt",date32(),nullable=TRUE),
            field("confirmed_case_bl",bool(),nullable=TRUE),
            field("previous_infection_dt",date32(),nullable=TRUE),
            field("previous_infection_bl",bool(),nullable=TRUE), 
            field("test_type_cd",string(),nullable=TRUE),
            field("variant_cd",string(),nullable=TRUE),
            field("diabetes_bl",bool(),nullable=TRUE), 
            field("obesity_bl",bool(),nullable=TRUE), 
            field("heart_failure_bl",bool(),nullable=TRUE), 
            field("copd_bl",bool(),nullable=TRUE), 
            field("solid_tumor_without_metastasis_bl",bool(),nullable=TRUE), 
            field("chronic_kidney_disease_bl",bool(),nullable=TRUE),
            field("sickle_cell_disease_bl",bool(),nullable=TRUE),
            field("hypertension_bl",bool(),nullable=TRUE), 
            field("chronic_liver_disease_bl",bool(),nullable=TRUE), 
            field("blood_cancer_bl",bool(),nullable=TRUE), 
            field("transplanted_bl",bool(),nullable=TRUE), 
            field("hiv_infection_bl",bool(),nullable=TRUE), 
            field("primary_immunodeficiency_bl",bool(),nullable=TRUE),
            field("immunosuppression_bl",bool(),nullable=TRUE),
            field("pregnancy_bl",bool(),nullable=TRUE)
          )
          
          data_file <- list.files(paste0(input_data_path,"/"),pattern=".csv")
          file <- file.path(input_data_path,data_file)
          parse_options <- CsvParseOptions$create(delimiter = "|")
          convert_options <- CsvConvertOptions$create(true_values = c("True","true","TRUE"),
                                                      false_values = c("False","false","FALSE"),
                                                      null_values=c("","NA","None"),
                                                      strings_can_be_null=TRUE) 
          ddf <- arrow::read_csv_arrow(file, schema = schema, skip = 1L, parse_options = parse_options, convert_options = convert_options, as_data_frame = FALSE)
          ddf <- ddf %>%
            mutate(flag_violating_val=FALSE,
                   flag_listwise_del=FALSE)
          
          ## register arrow dataset
          duckdb_register_arrow(conn = con, name = "cohort_view", ddf)

          ## Log info
          info(logger_simple,paste0("Number of rows in table cohort_view after: ", dbGetQuery(con,"SELECT COUNT() FROM cohort_view ;")[[1]],"
                                    "))

          ## insert data into 'cohort_data' table
          dbExecute(con, "INSERT INTO cohort_data SELECT * FROM cohort_view")

          ## Log info
          info(logger_simple,paste0("Number of rows in table cohort_data after: ", dbGetQuery(con,"SELECT COUNT() FROM cohort_data ;")[[1]],"
                                    "))
          
        },
        error=function(cond) {
          ## Log info
          warn(logger, paste0("MY ERROR: 
                              ", cond))
          return(stop(cond))
        },
        finally={
          ## disconnect from database
          dbDisconnect(con, shutdown=TRUE)
        }
      )
    }
    
    read_large_dataset()
  }
  
}
