################
### Metadata ###
################

# DATE LAST MODIFIED:
# 20/04/2023

# METADATA: 
if(FALSE) {
  title      <- 'BY-COVID WP5.2 Baseline Use Case: SARS-CoV-2 vaccine effectiveness - analytical pipeline - matching'
  author     <- list('Javier González-Galindo','Francisco Estupiñán-Romero','Marjan Meurisse','Santiago Royo-Sierra','Nina Van Goethem','Enrique Bernal-Delgado')
  version    <- '1.0.0'
  maintainer <- 'Marjan Meurisse'
  email      <- 'Marjan.Meurisse@sciensano.be'
  input      <- list('cohort_data and cohort_data_imputed (database tables in BY-COVID-WP5-BaselineUseCase-VE.duckdb)')
  output     <- list('group_similarity, result_matching_alg, matched_data (database tables in BY-COVID-WP5-BaselineUseCase-VE.duckdb)')
}

##################################
### Adjust 'cohort_data' table ###
##################################

f_computation_new_variables <- function() {
  
  tryCatch(
    {
      ## Description: get database connection
      con = dbConnect(duckdb::duckdb(), dbdir=auxilary_database_path, read_only=FALSE)
      
      ## Description: add columns in 'cohort_data' table
      dbExecute(con, "ALTER TABLE cohort_data ADD COLUMN comorbidities_bl BOOLEAN;
                  ALTER TABLE cohort_data ADD COLUMN immunestatus_bl BOOLEAN;
                  ALTER TABLE cohort_data ADD COLUMN age_cd INTEGER;
                  ALTER TABLE cohort_data_imputed ADD COLUMN age_cd INTEGER;
                  ALTER TABLE cohort_data ADD COLUMN boost_bl BOOLEAN;
                  ALTER TABLE cohort_data ADD COLUMN group_id INTEGER;
                  ALTER TABLE cohort_data ADD COLUMN flag_inclusion_record BOOLEAN;")
      ## Description: compute the variables 'comorbidities_bl', 'immunestatus_bl', 'age_cd' and 'group_id' in cohort_data
      dbExecute(con, 
                "UPDATE cohort_data set
                      	comorbidities_bl = CASE  
                      	WHEN diabetes_bl OR obesity_bl OR heart_failure_bl OR 
                      	copd_bl OR solid_tumor_without_metastasis_bl OR 
                      	chronic_liver_disease_bl OR
                      	chronic_kidney_disease_bl OR sickle_cell_disease_bl OR 
                      	hypertension_bl THEN TRUE
                      	ELSE FALSE
                      	END;
                UPDATE cohort_data set
                        immunestatus_bl = CASE  
                        WHEN blood_cancer_bl OR transplanted_bl OR hiv_infection_bl OR 
                        primary_immunodeficiency_bl OR immunosuppression_bl THEN TRUE
                        ELSE FALSE
                        END;
                UPDATE cohort_data set 
                        age_cd = CASE
                        WHEN age_nm >= 0 and age_nm <=4 THEN 1
                        WHEN age_nm >= 5 and age_nm <=9 THEN 2
                        WHEN age_nm >= 10 and  age_nm <=14 THEN 3
                        WHEN age_nm >= 15 and  age_nm <=19 THEN 4
                        WHEN age_nm >= 20 and  age_nm <=24 THEN 5
                        WHEN age_nm >= 25 and  age_nm <=29 THEN 6
                        WHEN age_nm >= 30 and age_nm <=34 THEN 7
                        WHEN age_nm >= 35 and age_nm <=39 THEN 8
                        WHEN age_nm >= 40 and age_nm <=44 THEN 9
                        WHEN age_nm >= 45 and age_nm <=49 THEN 10
                        WHEN age_nm >= 50 and age_nm <=54 THEN 11
                        WHEN age_nm >= 55 and age_nm <=59 THEN 12
                        WHEN age_nm >= 60 and age_nm <=64 THEN 13
                        WHEN age_nm >= 65 and age_nm <=69 THEN 14
                        WHEN age_nm >= 70 and  age_nm <=74 THEN 15
                        WHEN age_nm >= 75 and age_nm <=79 THEN 16
                        WHEN age_nm >= 80 and age_nm <=84 THEN 17
                        WHEN age_nm >= 85 THEN 18
                        ELSE NULL 
                        END;
                UPDATE cohort_data set
                        residence_area_cd = (select residence_area_cd from cohort_data
                                             group by residence_area_cd
                                             order by  COUNT(*) DESC limit 1)
                        where residence_area_cd is NULL;
                UPDATE cohort_data set
                        boost_bl = CASE  
                        WHEN vaccination_schedule_cd == \'JJ\'  AND dose_2_dt IS NOT NULL THEN TRUE
                                              WHEN vaccination_schedule_cd != \'JJ\' and vaccination_schedule_cd is not NULL  and dose_3_dt is not null THEN TRUE
                                              ELSE FALSE
                                              END;
                UPDATE cohort_data set 
                        group_id=b.group_id from(
                        select person_id ,
                        dense_rank() over (order by sex_cd,	age_cd, residence_area_cd, pregnancy_bl, essential_worker_bl, institutionalized_bl, foreign_bl,
                                             comorbidities_bl, immunestatus_bl) as group_id from (  
                         select COALESCE(cohort_data.person_id,cohort_data_imputed.person_id) as person_id,
                  							COALESCE(cohort_data.sex_cd,cohort_data_imputed.sex_cd) as sex_cd,
                  							COALESCE(cohort_data.age_cd,cohort_data_imputed.age_cd) as age_cd,
                  							COALESCE(cohort_data.residence_area_cd,cohort_data_imputed.residence_area_cd) as residence_area_cd,
                  							COALESCE(cohort_data.pregnancy_bl,cohort_data_imputed.pregnancy_bl) as pregnancy_bl,
                  							COALESCE(cohort_data.essential_worker_bl,cohort_data_imputed.essential_worker_bl) as essential_worker_bl,
                  							COALESCE(cohort_data.institutionalized_bl,cohort_data_imputed.institutionalized_bl) as institutionalized_bl,
                  							COALESCE(cohort_data.foreign_bl,cohort_data_imputed.foreign_bl) as foreign_bl,
                  							cohort_data.comorbidities_bl,
                  							cohort_data.immunestatus_bl
                                              from main.cohort_data
                                              left join main.cohort_data_imputed
                  						    on cohort_data.person_id = cohort_data_imputed.person_id) table_with_imputed  ) b
                        where cohort_data.person_id =b.person_id")
      
      ## Description: compute the variable 'age_cd' in cohort_data imputed
      dbExecute(con,"
              UPDATE cohort_data_imputed set
                        age_cd = CASE
                        WHEN age_nm >= 0 and age_nm <=4 THEN 1
                        WHEN age_nm >= 5 and age_nm <=9 THEN 2
                        WHEN age_nm >= 10 and  age_nm <=14 THEN 3
                        WHEN age_nm >= 15 and  age_nm <=19 THEN 4
                        WHEN age_nm >= 20 and  age_nm <=24 THEN 5
                        WHEN age_nm >= 25 and  age_nm <=29 THEN 6
                        WHEN age_nm >= 30 and age_nm <=34 THEN 7
                        WHEN age_nm >= 35 and age_nm <=39 THEN 8
                        WHEN age_nm >= 40 and age_nm <=44 THEN 9
                        WHEN age_nm >= 45 and age_nm <=49 THEN 10
                        WHEN age_nm >= 50 and age_nm <=54 THEN 11
                        WHEN age_nm >= 55 and age_nm <=59 THEN 12
                        WHEN age_nm >= 60 and age_nm <=64 THEN 13
                        WHEN age_nm >= 65 and age_nm <=69 THEN 14
                        WHEN age_nm >= 70 and  age_nm <=74 THEN 15
                        WHEN age_nm >= 75 and age_nm <=79 THEN 16
                        WHEN age_nm >= 80 and age_nm <=84 THEN 17
                        WHEN age_nm >= 85 THEN 18
                        ELSE NULL 
                        END")
      
      ## Description: compute the variable flag_inclusion_record
      dbExecute(con,"UPDATE cohort_data SET 
                flag_inclusion_record = CASE
                WHEN previous_infection_bl==TRUE OR flag_violating_val==TRUE OR flag_listwise_del==TRUE THEN FALSE
                ELSE TRUE
                END")
    },
    finally={
      ## Description: disconnect from database
      dbDisconnect(con, shutdown=TRUE)
    })
}

f_computation_new_variables()

tryCatch(
  {
    ## Description: get database connection
    con = dbConnect(duckdb::duckdb(), dbdir=auxilary_database_path, read_only=FALSE)
    ## Description: create view where only records with flag_inclusion_record==TRUE are selected
    dbExecute(con, "CREATE OR REPLACE VIEW cohort_view AS SELECT * FROM cohort_data WHERE flag_inclusion_record==TRUE")
  },
  finally={
    ## Description: disconnect from database
    dbDisconnect(con, shutdown=TRUE)
  })

################
### Analysis ###
################

### Create vector with dates ###
################################

getDates <- function() {
  tryCatch(
    {
      ## get database connection
      con = dbConnect(duckdb::duckdb(), dbdir=auxilary_database_path, read_only=TRUE)
      
      ## query 'cohort_data' table to obtain distinct dates
      result <- dbGetQuery(con, "select DISTINCT fully_vaccinated_dt from cohort_view 
                     where fully_vaccinated_dt < '2021-09-01' and fully_vaccinated_dt >= '2021-01-01'  ORDER BY random()")
    },
    finally={
      ## disconnect from database
      dbDisconnect(con, shutdown=TRUE)
    })
  return(result)
}

dates_v <- as.Date(as.vector(getDates()$fully_vaccinated_dt),
                   format = "%Y-%m-%d",
                   origin = "1970-01-01")

### Calculate similarity ###
############################

calculate_similarity <- function() {
  tryCatch(
    {
      ## get database connection
      con = dbConnect(duckdb::duckdb(), dbdir=auxilary_database_path, read_only=FALSE)
      
      dbGetQuery(con,"SELECT * FROM cohort_data_imputed")
      
      df_original <- dbGetQuery(con,"SELECT DISTINCT ON (a.group_id)
                            a.person_id,
                            COALESCE(a.sex_cd, b.sex_cd) AS sex_cd,
                            COALESCE(a.age_cd,b.age_cd) AS age_cd,
                            COALESCE(a.residence_area_cd,b.residence_area_cd) AS residence_area_cd,
                            COALESCE(a.pregnancy_bl,b.pregnancy_bl,FALSE) AS pregnancy_bl,
                            COALESCE(a.essential_worker_bl,b.essential_worker_bl) AS essential_worker_bl,
                            COALESCE(a.institutionalized_bl,b.institutionalized_bl) AS institutionalized_bl,
                            COALESCE(a.foreign_bl,b.foreign_bl) AS foreign_bl, 
                            a.comorbidities_bl,
                            a.immunestatus_bl,
                            a.group_id
                    FROM cohort_view a    
                    LEFT JOIN cohort_data_imputed b on a.person_id = b.person_id
                    ORDER BY a.group_id")
      df_original$residence_area_cd <- as.numeric(df_original$residence_area_cd)
      
      ## group numbers
      n_groups <- unique(df_original$group_id)
      
      cl <- makeCluster(4)
      clusterEvalQ(cl, c(library(duckdb),library(DBI),library(MatchIt),library(dplyr)))
      
      ## function
      loop_group <- function(i) {
        
        group_nr <- n_groups[i]
        df_original$fully_vaccinated_bl <- 0
        df_original_2 <- df_original %>% filter(group_id==group_nr)
        df_original_2$fully_vaccinated_bl <- 1
        df_original <- df_original %>% filter(!(group_id==group_nr))
        df_original <- rbind(df_original,df_original_2)
        
        ## matching
        mod_match <- matchit(fully_vaccinated_bl ~ age_cd + sex_cd + residence_area_cd + pregnancy_bl + essential_worker_bl + 
                               institutionalized_bl + foreign_bl + comorbidities_bl + immunestatus_bl,
                             method = "nearest", distance = "glm", ratio = 10, data = df_original) 
        mod_data <- match.data(mod_match) 
        mod_data <- mod_data %>% mutate(matched_group=group_id, group_id=mod_data[which(mod_data$fully_vaccinated_bl==1),"group_id"]) %>% select(c(group_id,matched_group,distance)) 
        return(mod_data)
      }
      ## apply function
      clusterExport(cl, c("n_groups","df_original"), envir = environment())
      output <- parLapply(cl, 1:length(n_groups),loop_group) %>% bind_rows() %>% arrange(group_id,desc(distance))
      
      ## create table 'group_similarity
      dbExecute(con, "CREATE OR REPLACE TABLE group_similarity (
                  	group_id VARCHAR,
                  	matched_group VARCHAR,
                  	distance TINYINT)")
      
      # insert 'output' into 'group_similarity' table
      dbWriteTable(con, "group_similarity",as.data.frame(output),overwrite = TRUE)
    },
    finally={
      ## disconnect from database
      dbDisconnect(con, shutdown=TRUE)
    })
}

calculate_similarity()

### Functions used for matching  ###
####################################

getGroupsByDate <- function(date_, cursor) {
  ## returns a dataset of the groups involved on this date  
  data_ <- dbGetQuery(cursor, paste0("select * from (
                    select group_id as full_vaccine_group, COUNT(*) as full_vaccine_n_group 
                    from cohort_view
                    where fully_vaccinated_dt == '", date_, "' group by group_id) a
                left join 
                (select group_id as control_group_id, COUNT(*) as control_n_group from cohort_view
                                where
                                    (fully_vaccinated_dt > '", date_, "'
                                        or fully_vaccinated_bl == FALSE)
                                    and 
                                    (previous_infection_dt >'", date_, "'
                                        or previous_infection_dt is NULL)
                                    and
                                      (confirmed_case_dt > '", date_, "'
                                        or confirmed_case_dt is NULL)
                                    and
                                      (exitus_dt > '", date_, "'
                                        or exitus_dt is NULL)
                                     group by group_id) b
                on a.full_vaccine_group = b.control_group_id"))
  return(data_)
}

getSampleNBigger <- function(date_, group, n_sample, cursor) { 
  query <- paste0("select * from (
            select
              a.person_id,
              FALSE as fully_vaccinated_bl,
              COALESCE(a.sex_cd,b.sex_cd) AS sex_cd,
              COALESCE(a.age_cd,b.age_cd) AS age_cd,
              COALESCE(a.residence_area_cd,b.residence_area_cd) AS residence_area_cd,
              COALESCE(a.pregnancy_bl,b.pregnancy_bl) AS pregnancy_bl,
              COALESCE(a.essential_worker_bl,b.essential_worker_bl) AS essential_worker_bl,
              COALESCE(a.institutionalized_bl,b.institutionalized_bl) AS institutionalized_bl,
              COALESCE(a.foreign_bl,b.foreign_bl) AS foreign_bl,
              a.comorbidities_bl,
              a.immunestatus_bl 
            from
              cohort_view a 
              LEFT JOIN cohort_data_imputed b on a.person_id = b.person_id
            where
              (fully_vaccinated_dt > '", date_, "'
                        or fully_vaccinated_bl == FALSE)
                    and (previous_infection_dt > '", date_,  "'
                        or previous_infection_dt is NULL)
                    and (confirmed_case_dt > '", date_,  "'
                        or confirmed_case_dt is NULL)
                    and (exitus_dt > '", date_, "'
                        or exitus_dt is NULL) 
                    and group_id = ", as.character(group),  ") USING SAMPLE ",  as.character(n_sample), " 
            union all         
              select 
                c.person_id,
                c.fully_vaccinated_bl,
                COALESCE(c.sex_cd,d.sex_cd) AS sex_cd,
                COALESCE(c.age_cd,d.age_cd) AS age_cd,
                COALESCE(c.residence_area_cd,d.residence_area_cd) AS residence_area_cd,
                COALESCE(c.pregnancy_bl,d.pregnancy_bl) AS pregnancy_bl,
                COALESCE(c.essential_worker_bl,d.essential_worker_bl) AS essential_worker_bl,
                COALESCE(c.institutionalized_bl,d.institutionalized_bl) AS institutionalized_bl,
                COALESCE(c.foreign_bl,d.foreign_bl) AS foreign_bl,
                c.comorbidities_bl,
                c.immunestatus_bl  
              from 
                cohort_view c 
                LEFT JOIN cohort_data_imputed d on c.person_id = d.person_id
              where 
                fully_vaccinated_dt = '" , date_,  "' and group_id =  " ,  as.character(group)
  )  
  data_ <- dbGetQuery(cursor, query)
  return(data_)
}

getSampleForMatch <- function(date_, group, n_sample, cursor){
  query <- paste0("select 
              a.person_id,
              a.fully_vaccinated_bl,
              COALESCE(a.sex_cd, c.sex_cd) AS sex_cd,
              COALESCE(a.age_cd,c.age_cd) AS age_cd,
              COALESCE(a.residence_area_cd,c.residence_area_cd) AS residence_area_cd,
              COALESCE(a.pregnancy_bl,c.pregnancy_bl) AS pregnancy_bl,
              COALESCE(a.essential_worker_bl,c.essential_worker_bl) AS essential_worker_bl,
              COALESCE(a.institutionalized_bl,c.institutionalized_bl) AS institutionalized_bl,
              COALESCE(a.foreign_bl,c.foreign_bl) AS foreign_bl, 
              a.comorbidities_bl,
              a.immunestatus_bl from
              (select
                person_id,
                fully_vaccinated_bl,
                sex_cd,
                age_cd,
                residence_area_cd,
                pregnancy_bl,
                essential_worker_bl,
                institutionalized_bl,
                foreign_bl,
                comorbidities_bl,
                immunestatus_bl 
              from
                cohort_view
              where
                (fully_vaccinated_dt == '", date_, "') and group_id = ", as.character(group), "
              union all
                select 
                  person_id,
                  FALSE as fully_vaccinated_bl,
                  sex_cd,
                  age_cd,
                  residence_area_cd,
                  pregnancy_bl,
                  essential_worker_bl,
                  institutionalized_bl,
                  foreign_bl,
                  comorbidities_bl,
                  immunestatus_bl from (
                select
                  person_id,
                  fully_vaccinated_bl,
                  sex_cd,
                  age_cd,
                  residence_area_cd,
                  pregnancy_bl,
                  essential_worker_bl,
                  institutionalized_bl,
                  foreign_bl,
                  comorbidities_bl,
                  immunestatus_bl,
                  row_number() OVER (PARTITION BY 
                    sex_cd,
                    age_cd,
                    residence_area_cd,
                    pregnancy_bl,
                    essential_worker_bl,
                    institutionalized_bl,
                    foreign_bl,
                    comorbidities_bl,
                    immunestatus_bl
                ORDER BY random()) AS person_num
                    from (select * from (select * from cohort_view v
                      where
                          (fully_vaccinated_dt > '", date_, "'
                          or fully_vaccinated_bl == FALSE)
                        and 
                          (previous_infection_dt > '", date_, "'
                          or previous_infection_dt is NULL)
                        and
                          (confirmed_case_dt > '", date_, "'
                          or confirmed_case_dt is NULL)
                        and
                          (exitus_dt > '", date_, "'
                          or exitus_dt is NULL)
                          ) v
                      inner join (select matched_group from group_similarity where group_id = ", as.character(group), "
                          and matched_group != ", as.character(group), ") gs
                          on v.group_id = gs.matched_group) a       
                          ) b where person_num <= ", as.character(n_sample), ") a
                  LEFT join cohort_data_imputed c
                          ON a.person_id = c.person_id"
  )
  data_ <- dbGetQuery(cursor, query)
  return(data_)
}

getSamplesRandom <-function(date_, group, n_sample, cursor){
     query <- paste0("select a.person_id,
        a.fully_vaccinated_bl,
        COALESCE(a.sex_cd, c.sex_cd) AS sex_cd,
        COALESCE(a.age_cd,c.age_cd) AS age_cd,
        COALESCE(a.residence_area_cd,c.residence_area_cd) AS residence_area_cd,
        COALESCE(a.pregnancy_bl,c.pregnancy_bl) AS pregnancy_bl,
        COALESCE(a.essential_worker_bl,c.essential_worker_bl) AS essential_worker_bl,
        COALESCE(a.institutionalized_bl,c.institutionalized_bl) AS institutionalized_bl,
        COALESCE(a.foreign_bl,c.foreign_bl) AS foreign_bl, 
        a.comorbidities_bl,
        a.immunestatus_bl from
        (select
          person_id,
          fully_vaccinated_bl,
          sex_cd,
          age_cd,
          residence_area_cd,
          pregnancy_bl,
          essential_worker_bl,
          institutionalized_bl,
          foreign_bl,
          comorbidities_bl,
          immunestatus_bl 
        from
          cohort_view
        where
          (fully_vaccinated_dt ==  '", date_, "') and group_id = ", as.character(group), "
        union all
          select 
            person_id,
            FALSE as fully_vaccinated_bl,
            sex_cd,
            age_cd,
            residence_area_cd,
            pregnancy_bl,
            essential_worker_bl,
            institutionalized_bl,
            foreign_bl,
            comorbidities_bl,
            immunestatus_bl from (
          select
            person_id,
            fully_vaccinated_bl,
            sex_cd,
            age_cd,
            residence_area_cd,
            pregnancy_bl,
            essential_worker_bl,
            institutionalized_bl,
            foreign_bl,
            comorbidities_bl,
            immunestatus_bl,
            row_number() OVER (
          ORDER BY random()) AS person_num
              from (select * from (select * from cohort_view v
                where
                    (fully_vaccinated_dt >  '", date_, "'
                    or fully_vaccinated_bl == FALSE)
                  and 
                    (previous_infection_dt >  '", date_, "'
                    or previous_infection_dt is NULL)
                  and
                    (confirmed_case_dt >  '", date_, "'
                    or confirmed_case_dt is NULL)
                  and
                    (exitus_dt >  '", date_, "'
                    or exitus_dt is NULL)
                   ) v
                ) a       
                    ) b where person_num <= ", as.character(n_sample), ") a
            LEFT join cohort_data_imputed c
                    ON a.person_id = c.person_id")

    data_ <- dbGetQuery(cursor, query)
    return(data_)
}

### Execute matching  ###
#########################

doMatch <- function(dates) {
  tryCatch(
    {
      con = dbConnect(duckdb::duckdb(), dbdir=auxilary_database_path, read_only=FALSE)
      n_sample <- 150
      
      ### Loop by date ###
      for(i in 1:length(dates)) {
        k <- as.character(dates[i])
        groups_by_date <- getGroupsByDate(k,con)
        # check condition: in group less (or the same number of) people in intervention than in the control group?
        groups_by_date$condition_less_or_eq <- (groups_by_date$full_vaccine_n_group <= groups_by_date$control_n_group)
        # check condition: is the number of people in the control group null?
        groups_by_date$condition_null_c <- (is.na(groups_by_date$control_n_group))
        
        # n_group <- 1
        last_subclass <- 0
        
        ### Loop by group ###
        loop_group <- function(n) {
          tmp <- groups_by_date[n,]
          ifelse(tmp$condition_null_c,
                 ### If control group id null (no exact match found) ###
                 {
                   df_matching <- getSampleForMatch(k,tmp$full_vaccine_group, n_sample, con);
                   sim_groups <- nrow(df_matching) > tmp[,"full_vaccine_n_group"]
                   ifelse(sim_groups,
                    {
                     ## If similar group found ##
                     cols <- colnames(df_matching)[colnames(df_matching) != 'person_id'];
                     df_matching <- df_matching %>% mutate_at(cols, as.numeric);
                     # check if pregnancy_bl all na -> if yes remove
                     cond <- sum(is.na(df_matching$pregnancy_bl))==nrow(df_matching);
                     ifelse(
                       !cond,
                       {
                       ## Pregnancy not all na
                       psm <- matchit(fully_vaccinated_bl ~ age_cd + sex_cd + residence_area_cd + pregnancy_bl + essential_worker_bl + 
                                          institutionalized_bl + foreign_bl + comorbidities_bl + immunestatus_bl,
                                        method = "nearest", distance = "glm", data = df_matching);
                       },
                       {
                       ## Pregnancy all na
                       df_matching <- df_matching %>% select(-pregnancy_bl)
                       psm <- matchit(fully_vaccinated_bl ~ age_cd + sex_cd + residence_area_cd + essential_worker_bl + 
                                          institutionalized_bl + foreign_bl + comorbidities_bl + immunestatus_bl,
                                        method = "nearest", distance = "glm", data = df_matching)
                       });
                     psm <- match.data(psm);
                     psm$subclass <- as.numeric(as.character(psm[,"subclass"]))+last_subclass;
                     df <- data.frame(matrix(ncol = 3, nrow = length(unique(psm$subclass))));
                     # separate full vaccine and non-vaccine
                     tmp_vacc <- psm %>% filter(fully_vaccinated_bl==1) %>% arrange(subclass)
                     tmp_nonvacc <- psm %>% filter(fully_vaccinated_bl==0) %>% arrange(subclass)
                     df <- data.frame(person_id = list(tmp_vacc$person_id),  matched_ID = list(tmp_nonvacc$person_id), subclass = list(tmp_vacc$subclass))
                     colnames(df) <- c("person_id", "matched_ID", "subclass");
                     last_subclass <<- max(df$subclass)
                     df$subclass <- paste0(k,'_',df$subclass);
                   },
                   {
                     ## If no similar group found ##
                     df_matching <- getSamplesRandom(k,tmp$full_vaccine_group, n_sample, con);
                     cols <- colnames(df_matching)[colnames(df_matching) != 'person_id'];
                     df_matching <- df_matching %>% mutate_at(cols, as.numeric);
                     cond <- sum(is.na(df_matching$pregnancy_bl))==nrow(df_matching)
                     ifelse(
                       !cond,
                       {
                         ## Pregnancy not all na
                         psm <- matchit(fully_vaccinated_bl ~ age_cd + sex_cd + residence_area_cd + pregnancy_bl + essential_worker_bl + 
                                          institutionalized_bl + foreign_bl + comorbidities_bl + immunestatus_bl,
                                        method = "nearest", distance = "glm", data = df_matching);
                       },
                       {
                         ## Pregnancy all na
                         df_matching <- df_matching %>% select(-pregnancy_bl)
                         psm <- matchit(fully_vaccinated_bl ~ age_cd + sex_cd + residence_area_cd + essential_worker_bl + 
                                          institutionalized_bl + foreign_bl + comorbidities_bl + immunestatus_bl,
                                        method = "nearest", distance = "glm", data = df_matching)
                       })
                     psm <- match.data(psm);
                     psm$subclass <- as.numeric(as.character(psm[,"subclass"]))+last_subclass
                     df <- data.frame(matrix(ncol = 3, nrow = length(unique(psm$subclass))));
                     # separate full vaccine and non-vaccine
                     tmp_vacc <- psm %>% filter(fully_vaccinated_bl==1) %>% arrange(subclass)
                     tmp_nonvacc <- psm %>% filter(fully_vaccinated_bl==0) %>% arrange(subclass)
                     df <- data.frame(person_id = list(tmp_vacc$person_id),  matched_ID = list(tmp_nonvacc$person_id), subclass = list(tmp_vacc$subclass))
                     colnames(df) <- c("person_id", "matched_ID", "subclass");
                     last_subclass <<- max(df$subclass)
                     df$subclass <- paste0(k,'_',df$subclass);
                   })
                 }
                 ,
                 ### If control group id not null (exact match(es) found) ###
                 {
                   df_sample <- getSampleNBigger(k,tmp$full_vaccine_group,tmp$full_vaccine_n_group,con);
                   person_id_df <- df_sample %>% filter(fully_vaccinated_bl==TRUE) %>% select(person_id);
                   matched_id_df <- df_sample %>% filter(fully_vaccinated_bl==FALSE) %>% select(person_id) %>% rename(matched_ID=person_id);
                   ifelse(tmp$condition_less_or_eq,
                          {
                            ## If less less (or equal) cases than controls ##
                            df <- cbind(person_id_df,matched_id_df);
                            df$subclass <- as.numeric(rownames(df));
                            df$subclass <- df[,3]+last_subclass
                            last_subclass <<- max(df$subclass)
                            df$subclass <- paste0(k,'_',df$subclass)
                          }
                          ,
                          {
                            ## If more cases than controls ##
                            diff <- tmp$full_vaccine_n_group - tmp$control_n_group;
                            matched_id_df <- rbind(matched_id_df, sample_n(matched_id_df,diff,replace=TRUE));
                            df <- cbind(person_id_df,matched_id_df);
                            df$subclass <- as.numeric(rownames(df));
                            df$subclass <- df[,3]+last_subclass
                            last_subclass <<- max(df$subclass)
                            df$subclass <- paste0(k,'_',df$subclass)
                          })
                 })
          return(df)
        }
        df <-lapply(1:nrow(groups_by_date),loop_group) %>% bind_rows()
        dbWriteTable(con, "result_matching_alg",df,overwrite = FALSE, append=TRUE)
      }
    },
    finally={
      ## disconnect from database
      dbDisconnect(con, shutdown=TRUE)
    })
}

system.time(doMatch(dates_v))

### Calculate status and follow-up time for survival analysis  ###
##################################################################

getStatusMatch <- function() {
  tryCatch(
    {
      con = dbConnect(duckdb::duckdb(), dbdir=auxilary_database_path, read_only=FALSE)
      end_follow_up <- "'2022-09-01'"
      # Calculate censorship date for pairs.      
      dbExecute(con, paste0("
      CREATE OR REPLACE TABLE result_matching_alg AS		
      select
      	censoring_table.person_id,
      	censoring_table.matched_ID,
      	censoring_table.subclass,
      	COALESCE( 
      least(boost_dt_intervention,
      	least(exitus_dt_intervention,
      	least(fully_vaccinated_dt_control,
      	least(boost_dt_control,
      	exitus_dt_control))))
      ,
      	",end_follow_up,"::DATE) as censoring_dt
      from
      	(
      	select
      		person_id ,
      		matched_ID,
      		subclass,
      		date_onset,
      		CASE
      			WHEN boost_dt_intervention >= date_onset and boost_dt_intervention  <= ",end_follow_up,"::DATE  THEN boost_dt_intervention
      			ELSE NULL
      		END as boost_dt_intervention,
      		CASE
      			WHEN exitus_dt_intervention >= date_onset and exitus_dt_intervention  <= ",end_follow_up,"::DATE THEN exitus_dt_intervention
      			ELSE NULL
      		END as exitus_dt_intervention,
      		CASE
      			WHEN fully_vaccinated_dt_control >= date_onset and fully_vaccinated_dt_control  <= ",end_follow_up,"::DATE  THEN fully_vaccinated_dt_control
      			ELSE NULL
      		END as fully_vaccinated_dt_control,
      		CASE
      			WHEN boost_dt_control >= date_onset and boost_dt_control  <= ",end_follow_up,"::DATE THEN boost_dt_control
      			ELSE NULL
      		END as boost_dt_control,
      		CASE
      			WHEN exitus_dt_control >= date_onset and exitus_dt_control  <= ",end_follow_up,"::DATE THEN exitus_dt_control
      			ELSE NULL
      		END as exitus_dt_control
      	from
      		(
      		select
      			result_matching_alg.*,
      			boost_dt_intervention,
      			exitus_dt_intervention,
      			fully_vaccinated_dt_control,
      			boost_dt_control,
      			exitus_dt_control,
      			SUBSTRING(subclass, 1, 10)::DATE as date_onset
      		from
      			main.result_matching_alg
      		left join (
      			select
      				person_id,
      				CASE
          		  WHEN vaccination_schedule_cd == 'JJ' THEN dose_2_dt
          		  WHEN vaccination_schedule_cd != 'JJ'
          		    and  vaccination_schedule_cd is not null THEN dose_3_dt
          		  ELSE NULL
          	  END as boost_dt_intervention,
      				exitus_dt as exitus_dt_intervention
      			from
      				main.cohort_view ) intervention_dates
      			on
      			result_matching_alg.person_id = intervention_dates.person_id
      		left join (
      			select
      				person_id,
      				fully_vaccinated_dt as fully_vaccinated_dt_control ,
      				CASE
          		  WHEN vaccination_schedule_cd == 'JJ' THEN dose_2_dt
          		  WHEN vaccination_schedule_cd != 'JJ'
          		  and  vaccination_schedule_cd is not null THEN dose_3_dt
          		ELSE NULL
          	  END as boost_dt_control,
      				exitus_dt as exitus_dt_control
      			from
      				main.cohort_view) control_dates
      			on
      			result_matching_alg.matched_ID = control_dates.person_id )c) censoring_table"))
      
      dbExecute(con, "
                CREATE OR REPLACE TABLE matched_data as
                  	select
                  	b.person_id ,
                  	b.fully_vaccinated_dt,
                  	false as fully_vaccinated_bl ,
                  	b.confirmed_case_dt ,
                  	b.exitus_dt,
                  	CASE
                  		WHEN b.vaccination_schedule_cd == 'JJ' THEN b.dose_2_dt
                  		WHEN b.vaccination_schedule_cd != 'JJ'
                  		and b.vaccination_schedule_cd is not null THEN b.dose_3_dt
                  		ELSE NULL
                  	END as boost_dt,
                  	a.date_onset,
                  	subclass,
                  	a.censoring_dt
                  from
                  	(
                  	SELECT
                  		matched_ID,
                  		SUBSTRING(subclass, 1, 10)::DATE as date_onset,
                  		subclass,
                  		censoring_dt
                  	FROM
                  		result_matching_alg) a
                  inner join cohort_view b
                                      on
                  	a.matched_ID = b.person_id
                  union all
                                      select
                  	b.person_id ,
                  	b.fully_vaccinated_dt,
                  	b.fully_vaccinated_bl ,
                  	b.confirmed_case_dt ,
                  	b.exitus_dt,
                  	CASE
                  		WHEN b.vaccination_schedule_cd == 'JJ' THEN b.dose_2_dt
                  		WHEN b.vaccination_schedule_cd != 'JJ'
                  		and b.vaccination_schedule_cd is not null THEN b.dose_3_dt
                  		ELSE NULL
                  	END as boost_dt,
                  	a.date_onset,
                  	subclass,
                  	censoring_dt
                  from
                  	(SELECT	person_id, NULL as date_onset,subclass,censoring_dt
                  	FROM result_matching_alg) a
                  inner join cohort_view b
                      on a.person_id = b.person_id ")
      
      dbExecute(con, "ALTER TABLE matched_data ADD COLUMN status VARCHAR;
                      ALTER TABLE matched_data ADD COLUMN futime INTEGER;")
      
      dbExecute(con, paste0("update matched_data set
    	status = CASE
    	   WHEN confirmed_case_dt IS NULL AND fully_vaccinated_bl == FALSE AND fully_vaccinated_dt IS NULL THEN '0-1'
           WHEN confirmed_case_dt IS NULL AND fully_vaccinated_bl == FALSE AND fully_vaccinated_dt IS NOT NULL THEN '0-2'
           WHEN confirmed_case_dt IS NULL AND fully_vaccinated_bl == TRUE AND fully_vaccinated_dt IS NOT NULL AND boost_dt is NULL THEN '0-3'
           WHEN confirmed_case_dt IS NULL AND fully_vaccinated_bl == TRUE AND fully_vaccinated_dt IS NOT NULL AND boost_dt is NOT NULL THEN '0-4'
           WHEN confirmed_case_dt IS NOT NULL AND fully_vaccinated_bl == FALSE AND fully_vaccinated_dt IS NULL THEN '1-1'
           WHEN fully_vaccinated_bl == FALSE AND fully_vaccinated_dt <= confirmed_case_dt THEN '0-2'
           WHEN fully_vaccinated_bl == FALSE AND confirmed_case_dt < fully_vaccinated_dt THEN '1-1'
           WHEN fully_vaccinated_bl == TRUE AND confirmed_case_dt >= fully_vaccinated_dt  AND boost_dt is NULL  THEN '1-2'
           WHEN confirmed_case_dt < fully_vaccinated_dt and fully_vaccinated_bl == TRUE and boost_dt is NULL THEN '0-3'
           WHEN fully_vaccinated_bl == TRUE AND confirmed_case_dt >= boost_dt THEN '0-4'
           WHEN fully_vaccinated_bl == TRUE AND confirmed_case_dt >= fully_vaccinated_dt AND confirmed_case_dt < boost_dt THEN '1-2'
           WHEN confirmed_case_dt < fully_vaccinated_dt and fully_vaccinated_bl == TRUE and boost_dt is NOT NULL THEN '0-4'
           ELSE 'exc' 
    	END
    	where exitus_dt is NULL;
      update matched_data set 
    status = CASE
    	   WHEN confirmed_case_dt IS NULL AND fully_vaccinated_bl == FALSE AND fully_vaccinated_dt IS NULL THEN '0-6'
           WHEN confirmed_case_dt IS NULL AND fully_vaccinated_bl == FALSE AND fully_vaccinated_dt IS NOT NULL THEN '0-2'
           WHEN confirmed_case_dt IS NULL AND fully_vaccinated_bl == TRUE AND fully_vaccinated_dt IS NOT NULL AND boost_dt is NULL THEN '0-7'
           WHEN confirmed_case_dt IS NULL AND fully_vaccinated_bl == TRUE AND fully_vaccinated_dt IS NOT NULL AND boost_dt is NOT NULL THEN '0-4'
           WHEN confirmed_case_dt IS NOT NULL AND fully_vaccinated_bl == FALSE AND fully_vaccinated_dt IS NULL THEN '1-1'
           WHEN fully_vaccinated_bl == FALSE AND fully_vaccinated_dt <= confirmed_case_dt THEN '0-2'
           WHEN fully_vaccinated_bl == FALSE AND confirmed_case_dt < fully_vaccinated_dt THEN '1-1'
           WHEN fully_vaccinated_bl == TRUE AND confirmed_case_dt >= fully_vaccinated_dt  AND boost_dt is NULL  THEN '1-2'
           WHEN confirmed_case_dt < fully_vaccinated_dt and fully_vaccinated_bl == TRUE and boost_dt is NULL THEN '0-7'
           WHEN fully_vaccinated_bl == TRUE AND confirmed_case_dt >= boost_dt THEN '0-4'
           WHEN fully_vaccinated_bl == TRUE AND confirmed_case_dt >= fully_vaccinated_dt AND confirmed_case_dt < boost_dt THEN '1-2'
           WHEN confirmed_case_dt < fully_vaccinated_dt and fully_vaccinated_bl == TRUE and boost_dt is NOT NULL THEN '0-4'
           ELSE 'exc' 
    	END
    	where exitus_dt is NOT NULL;
    	update matched_data set 	
    	futime = case
    	      WHEN status == '0-1' THEN datediff('day',date_onset,least(censoring_dt,",end_follow_up,"::DATE))
    	      WHEN status == '0-2' THEN datediff('day',date_onset,least(censoring_dt,fully_vaccinated_dt))
    	      WHEN status == '0-3' THEN datediff('day',fully_vaccinated_dt,least(censoring_dt,",end_follow_up,"::DATE))
			      WHEN status == '0-4' THEN datediff('day',fully_vaccinated_dt,least(censoring_dt,boost_dt))		     
    	      WHEN status == '0-6' THEN datediff('day',date_onset,least(censoring_dt,exitus_dt))
    	      WHEN status == '0-7' THEN datediff('day',fully_vaccinated_dt,least(censoring_dt,exitus_dt)) 
    	      WHEN status == '1-1' THEN datediff('day',date_onset,least(censoring_dt,confirmed_case_dt))
    	      WHEN status == '1-2' THEN datediff('day',fully_vaccinated_dt,least(censoring_dt,confirmed_case_dt))
    	END;    
    	update matched_data set 	
    	status = case
    	      WHEN SUBSTRING(status,1,1) == '0' THEN '0'
    	      WHEN SUBSTRING(status,1,1) == '1' THEN '1'
    	      ELSE NULL
    	      END;
                
      "))
      # Change the status if the confirmed date is after the censorship date.
      dbExecute(con, "
      update matched_data set 	
    	status = case
    	      WHEN status == '1' and confirmed_case_dt > censoring_dt THEN '0'
    	      ELSE status
    	      END;")
      
    },
    finally={
      ## disconnect from database
      dbDisconnect(con, shutdown=TRUE)
    })
}

getStatusMatch()

