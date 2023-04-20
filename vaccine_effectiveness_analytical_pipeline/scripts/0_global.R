################
### Metadata ###
################

# DATE LAST MODIFIED:
# 20/04/2023

# METADATA: 
if(FALSE) {
  title      <- 'BY-COVID WP5.2 Baseline Use Case: SARS-CoV-2 vaccine effectiveness - analytical pipeline - general settings and loading of data'
  author     <- list('Marjan Meurisse','Javier González-Galindo','Francisco Estupiñán-Romero','Santiago Royo-Sierra','Nina Van Goethem','Enrique Bernal-Delgado')
  version    <- '1.0.0'
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
       "grDevices","visdat","mice","tidyr","shiny","consort","parallel","MatchIt",
       "survival","table1","tab","forestmodel","gtsummary","survRM2") 
lapply(x, require, character.only = TRUE)

### Description: directories
input_data_path <- here('input')
output_data_path <- here('output')
auxilary_database_name <- 'BY-COVID-WP5-BaselineUseCase-VE.duckdb'
auxilary_database_path <- file.path(input_data_path, auxilary_database_name)

### Description: function f_load_data
f_load_data <- function(create_db_tables=FALSE, load_data=FALSE) {
  
  if(create_db_tables) {
    
    ### Create necessary auxiliary tables ###
    
    createDB <- function() {
      
      tryCatch(
        {
          ## get database connection
          con = dbConnect(duckdb::duckdb(), dbdir=auxilary_database_path, read_only=FALSE)
          
          ## drop tables if the script has been executed previously
          dbExecute(con, "DROP TABLE IF EXISTS cohort_data;")
          
          ## create 'cohort_data' table
          dbExecute(con, "CREATE OR REPLACE TABLE cohort_data (
                  	person_id VARCHAR,
                  	age_nm TINYINT,
                  	sex_cd TINYINT,
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
    
    read_large_dataset <- function() {
      
      tryCatch(
        {
          ## get database connection
          con = dbConnect(duckdb::duckdb(), dbdir=auxilary_database_path, read_only=FALSE)
          
          ## open/read dataset 
          
          # See variable format common data model
          schema <- arrow::schema(
            field("person_id",string(),nullable=TRUE),
            field("age_nm",int64(),nullable=TRUE),
            field("sex_cd",int64(),nullable=TRUE),
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
          file <- file.path(paste0(input_data_path,"/"),data_file)
          parse_options <- CsvParseOptions$create(delimiter = "|")
          convert_options <- CsvConvertOptions$create(true_values = c("True","true","TRUE"),
                                                      false_values = c("False","false","FALSE"),
                                                      null_values=c("","NA","None"),
                                                      strings_can_be_null=TRUE) 
          ddf <- arrow::read_csv_arrow(file,schema = schema,skip = 1L ,parse_options = parse_options, convert_options = convert_options , as_data_frame = FALSE )
          ddf <- ddf %>%
            mutate(flag_violating_val=FALSE,
                   flag_listwise_del=FALSE)
          
          ## register arrow dataset
          duckdb_register_arrow(conn = con, name = "cohort_view", ddf)

          ## insert data into 'cohort_data' table
          # If error -> print error end exit knit
          tryCatch({
            dbExecute(con, "INSERT INTO cohort_data SELECT * FROM cohort_view")
          }, error = function(err) {
            print(paste("MY ERROR:  ",err))
            print("Please check your input data")
            knit_exit()
          })
          
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
