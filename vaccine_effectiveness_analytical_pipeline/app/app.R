library(shiny)
library(shinydashboard)
library(shinyjs)
library(quarto)
library(tools)
# library(zip)

ui <- dashboardPage(

  ### Dashboard header ###
  dashboardHeader(
    title= 'BY-COVID - WP5 - Baseline Use Case: SARS-CoV-2 vaccine effectiveness',
    titleWidth = 700
  ),
    
  ### Dashboard sidebar ###
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Introduction",tabName = "intro",icon = icon("circle-info")),
      menuItem("Instructions",tabName = "instructions",icon = icon("spinner")),
      menuItem("Import data",tabName = "import", icon = icon('upload')),
      menuItem("Initiate analysis",tabName = "initiate",icon = icon("play"))
    )
  ),
    
  ### Dashboard body ###
  dashboardBody(
    
    tabItems(
      
      ### Tab 1: Introduction ###
      tabItem(tabName = "intro",
          h2("BY-COVID - WP5 - Baseline Use Case: SARS-CoV-2 vaccine effectiveness assessment"),
          box(title = "Description",
            h5("Task 5.2 aims to demonstrate how mobilisation of ", tags$b("real-world"), " population, health and care data across national borders 
                 can provide answers to policy-relevant research questions. Eventually, it aims to prototype a", tags$i("workflow that is standard 
                 for population health research."), " Here, the research question is approached by estimating a", tags$b("causal effect"), " that allows to evaluate a public health intervention. 
                 As such, a methodology for ", tags$b("approaching causal inference when conducting federated research"), " is proposed and demonstrated, 
                 guaranteeing different layers of ",tags$b("interoperability"), " (i.e., legal, organisational, and semantic interoperability)."),
            h5("The methodological framework comprises the following ", tags$b("steps:"),
               tags$li("Defining the research question and the exposure-outcome relationship"),
               tags$li("Establishing a causal model using Directed Acyclic Graphs (DAGs)"),
               tags$li("Translating the causal model into data requirements using a Common Data Model (CDM)"),
               tags$li("Generating synthetic data and developing an interoperable analytical pipeline"),
               tags$li("Mobilising individual-level data within each of the nodes and transforming the data to comply with the CDM"),
               tags$li("Deploying the interoperable analytical pipeline within the secure processing environment (SPE) of each of the nodes (Data Hubs)"),
               tags$li("Meta-analysis of the aggregated results")),
              width = 16, solidHeader = TRUE, collapsible = TRUE, status = "primary"),
          box(title = "Use Case",
              h5("The current use case aims to answer the following research question: ", 
                 tags$i("'How effective have the SARS-CoV-2 vaccination programmes been in preventing SARS-CoV-2 infections?'")),
              h5("For more information, please consult the ", 
                 tags$a(href = "https://doi.org/10.5281/zenodo.7551181", "study protocol"), "."),
              width = 16, solidHeader = TRUE, collapsible = TRUE, status = "primary"),
          box(title = "Analytical pipeline",
              h5("The analytical pipeline consists of sequential Quarto documents (.qmd files), using the R programming language, reflecting and reporting different modules:",
                 tags$li("DQA of the original input data"),
                 tags$li("Validation (i.e., applying logical validation rules) of the orginal input data to check compliance with the CDM"),
                 tags$li("Imputation of missing data where required"),
                 tags$li("Iterative matching of the exposed to unexposed individuals and a balance assessment of the matched population"),
                 tags$li("A descriptive analysis of the matched and unmatched population"),
                 tags$li("A survival analysis in the matched study population"),
                 tags$br(),
                 "The individual scripts of the analytical pipeline are technically linked to each other. Each module of the analysis produces an interactive report. More information on the methodology can be found in the ", 
                 tags$a(href = "https://github.com/MarjanMeurisse/BY-COVID_WP5_T5.2_baseline-use-case/blob/main/vaccine_effectiveness_analytical_pipeline/documentation/BY-COVID-WP5-BaselineUseCase-VE-documentation-analytical-pipeline.pdf", "documentation"), ". For illustrative purposes, the interactive reports as output of the analytical pipeline when applied to the synthetic dataset are provided."),
              img(src="analytical-pipeline.png",align = "center",
                  width = "70%"),
              width = 16, solidHeader = TRUE, collapsible = TRUE, status = "primary")
      ),
      
      ### Tab 2: Instructions ###
      tabItem(tabName = "instructions",
              box(title="Instructions",
                  h5("To execute the analysis with your own input data and to create aggregated non-sensitive results, please go through the following steps:",
                     tags$br(),
                     tags$br(),
                     tags$li("1. Create input data complying with the data requirements specified in the ",tags$a(href = "https://doi.org/10.5281/zenodo.6913045", "CDM")," (csv format). 
                             For example, this could entail the linkage of different individual-level data sources and subsequent transformation to capture the syntactic and semantic requirements, as exemplified in the synthetic data."),
                     tags$br(),
                     tags$li("2. Go to the 'Import data' tab"),
                     tags$br(),
                     tags$li("3. Browse to the input data compliant with the", tags$a(href = "https://doi.org/10.5281/zenodo.6913045", "CDM"), " specification (csv format) and upload them"),
                     tags$br(),
                     tags$li("4. Go to the 'Initiate analysis' tab"),
                     tags$br(),
                     tags$li("5. Press the 'Initiate analysis' button to start the execution of the analysis scripts using your uploaded data as input"),
                     tags$br(),
                     tags$li("6. Wait until the analysis is complete (this can take some time)"),
                     tags$br(),
                     tags$li("7. Once the analysis in complete, download buttons will appear, allowing you to download the produced interactive reports and aggregated results for meta-analysis.")
                     ),
                  width = 16, solidHeader = TRUE, collapsible = FALSE, status = "primary")
      ),
      
      ### Tab 3: Import data ###
      tabItem(tabName = "import",
          box(title="Import data",
          htmlOutput("ifeedback"),
          tags$br(),
          fileInput("upload", "Upload CSV file"), # , multiple = FALSE, accept = ".csv"
          solidHeader = TRUE, collapsible = FALSE, status = "primary"
          )
      ),
      
      ### Tab 4: Initiate the analysis ###
      tabItem(tabName = "initiate",

          # Button to initiate the analysis
          fluidRow(
            box(title = "Initiate the analysis",
                htmlOutput("choose"),
                tags$br(),
                actionButton("initiateButton","Initiate analysis"),
                tags$br(),
                tags$br(),
                solidHeader = TRUE, collapsible = FALSE, status = "primary")
          ),

          # Add info running
          # fluidRow(
          #   box(
          #     verbatimTextOutput("console")
          #     )
          # ),
          
          # Download output
          fluidRow(
            box(title = "Download output", status = "primary",
                uiOutput("downloadButton1"),
                uiOutput("downloadButton2"),
                uiOutput("downloadButton3"),
                uiOutput("downloadButton4"),
                uiOutput("downloadButton5"),
                uiOutput("downloadButton6"),
                uiOutput("downloadButton7"),
                tags$br(),
                # uiOutput("downloadButton10"),
                # downloadButton("downloadZip", label = "Download"),
                solidHeader = TRUE, collapsible = TRUE),
            box(title = "Download log info", status = "warning",
                uiOutput("downloadButton8"),
                uiOutput("downloadButton9"),
                solidHeader = TRUE, collapsible = TRUE)
          )
      )
    )
  )
)

server <- function(input, output) {
  
  ### Upload data ###
  output$ifeedback <- renderText({
    x <- reactive({input$upload})
    p <- reactive({input$upload$datapath})
    ext <- tolower(tools::file_ext(p()))
    infile <- input$upload
    colnames <- c("person_id","age_nm","sex_cd","socecon_lvl_cd","residence_area_cd","country_cd","foreign_bl","exitus_dt","exitus_bl",
                  "essential_worker_bl","institutionalized_bl","dose_1_brand_cd","dose_1_dt","dose_2_brand_cd","dose_2_dt","dose_3_brand_cd","dose_3_dt","doses_nm",
                  "fully_vaccinated_dt","fully_vaccinated_bl","vaccination_schedule_cd","confirmed_case_dt","confirmed_case_bl",
                  "previous_infection_dt","previous_infection_bl","test_type_cd","variant_cd","diabetes_bl","obesity_bl","heart_failure_bl",
                  "copd_bl","solid_tumor_without_metastasis_bl","chronic_kidney_disease_bl","sickle_cell_disease_bl","hypertension_bl","chronic_liver_disease_bl",
                  "blood_cancer_bl","transplanted_bl","hiv_infection_bl","primary_immunodeficiency_bl","immunosuppression_bl","pregnancy_bl")
    
    if(is.null(x())) {
      return(paste("<span style=\"color:darkred\"><strong>Please upload your input data complying with the data requirements specified in the CDM (csv format).</strong></span>"))
    } 
    else {
      if (ext!="csv") {
        return(paste("<span style=\"color:red\"><strong>The input data should be a *.csv file!</strong></span>"))
      }
      else {
        if(sum(colnames(read.csv(infile$datapath, nrows = 1, sep = "|"))==colnames)<42) {
          return(paste("<span style=\"color:red\"><strong>Incorrect file header!</strong></span>"))
        } else {
          file.copy(input$upload$datapath,
                    "../scripts/input/cohort.csv", overwrite = TRUE)
          return(paste("<span style=\"color:green\"><strong>The input data were uploaded.</strong></span>"))
          as.character("The input data were uploaded.")
        }
      }
    }
  })
  output$choose <- renderText({
    x <- reactive({input$upload})
    p <- reactive({input$upload$datapath})
    ext <- tolower(tools::file_ext(p()))
    infile <- input$upload
    colnames <- c("person_id","age_nm","sex_cd","socecon_lvl_cd","residence_area_cd","country_cd","foreign_bl","exitus_dt","exitus_bl",
                  "essential_worker_bl","institutionalized_bl","dose_1_brand_cd","dose_1_dt","dose_2_brand_cd","dose_2_dt","dose_3_brand_cd","dose_3_dt","doses_nm",
                  "fully_vaccinated_dt","fully_vaccinated_bl","vaccination_schedule_cd","confirmed_case_dt","confirmed_case_bl",
                  "previous_infection_dt","previous_infection_bl","test_type_cd","variant_cd","diabetes_bl","obesity_bl","heart_failure_bl",
                  "copd_bl","solid_tumor_without_metastasis_bl","chronic_kidney_disease_bl","sickle_cell_disease_bl","hypertension_bl","chronic_liver_disease_bl",
                  "blood_cancer_bl","transplanted_bl","hiv_infection_bl","primary_immunodeficiency_bl","immunosuppression_bl","pregnancy_bl")
    
    if(is.null(x())) {
      return(paste("<span style=\"color:darkred\"><strong>Input data are not yet uploaded. Please first go the 'Import data' tab to upload your input data file.</strong></span>"))
    } 
    else {
      if (ext!="csv") {
        return(paste("<span style=\"color:red\"><strong>The input data have the wrong format. The input data should be a *.csv file. Please go the 'Import data' tab to upload a new input data file.</strong></span>"))
      }
      else {
        if(sum(colnames(read.csv(infile$datapath, nrows = 1, sep = "|"))==colnames)<42) {
          return(paste("<span style=\"color:red\"><strong>The input data have an incorrect file header. Please go the 'Import data' tab to upload a new input data file.</strong></span>"))
        } else {
          return(paste("<span style=\"color:green\"><strong>Input data are uploaded. The analysis can now be initiated.</strong></span>"))
        }
      }
    }
  })
  
  ### Initiate analysis ###
  observeEvent(input$initiateButton, {
    tryCatch({
      withProgress(message = 'Rendering, please wait!',
          quarto::quarto_render("../scripts/analytical-pipeline.QMD")
      )
    },
    warning = function(warn){
        showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')
      showModal(modalDialog( title=paste0("Analytical pipeline not completed"),
                             br(),
                             div(tags$b(paste0("The analytical pipeline scripts have not ran completely and the required output has not been created. Downloadbuttons will not function properly."), style = "color: red;"))
      ))
    })
  })

  ### Download output  ###
  
  # 1_DQA.html
  output$downloadReport1 <- downloadHandler(
    filename = '1_DQA.html',
    content <- function(file) {
      file.copy("../output/1_DQA.html", file)
    }
  )
  observeEvent(input$initiateButton, {
    toggle(
      output$downloadButton1 <- renderUI({
        downloadButton("downloadReport1", label = "Download 1_DQA.html")
      })
    )
  })
  
  # 2_validation.html
  output$downloadReport2 <- downloadHandler(
    filename = '2_validation.html',
    content <- function(file) {
      file.copy("../output/2_validation.html", file)
    }
  )
  observeEvent(input$initiateButton, {
    toggle(
      output$downloadButton2 <- renderUI({
        downloadButton("downloadReport2", label = "Download 2_validation.html")
      })
    )
  })
  
  # 3_imputation.html
  output$downloadReport3 <- downloadHandler(
    filename = '3_imputation.html',
    content <- function(file) {
      file.copy("../output/3_imputation.html", file)
    }
  )
  observeEvent(input$initiateButton, {
    toggle(
      output$downloadButton3 <- renderUI({
        downloadButton("downloadReport3", label = "Download 3_imputation.html")
      })
    )
  })
  
  # 4_matching.html
  output$downloadReport4 <- downloadHandler(
    filename = '4_matching.html',
    content <- function(file) {
      file.copy("../output/4_matching.html", file)
    }
  )
  observeEvent(input$initiateButton, {
    toggle(
      output$downloadButton4 <- renderUI({
        downloadButton("downloadReport4", label = "Download 4_matching.html")
      })
    )
  })
  
  # 5_descriptive.html
  output$downloadReport5 <- downloadHandler(
    filename = '5_descriptive.html',
    content <- function(file) {
      file.copy("../output/5_descriptive.html", file)
    }
  )
  observeEvent(input$initiateButton, {
    toggle(
      output$downloadButton5 <- renderUI({
        downloadButton("downloadReport5", label = "Download 5_descriptive.html")
      })
    )
  })
  
  # 6_survival-analysis.html
  output$downloadReport6 <- downloadHandler(
    filename = '6_survival-analysis.html',
    content <- function(file) {
      file.copy("../output/6_survival-analysis.html", file)
    }
  )
  observeEvent(input$initiateButton, {
    toggle(
      output$downloadButton6 <- renderUI({
        downloadButton("downloadReport6", label = "Download 6_survival-analysis.html")
      })
    )
  })
  
  # results-survival-analysis-<country>.xlsx
  output$downloadReport7 <- downloadHandler(
    filename = "results-survival-analysis.xlsx",
    content <- function(file) {
      file.copy(paste0("../output/",list.files("../output/", pattern="^results-survival-analysis", full.names = TRUE)), file)
    }
  )
  observeEvent(input$initiateButton, {
    toggle(
      output$downloadButton7 <- renderUI({
        downloadButton("downloadReport7", label = "Download results-survival-analysis-<country>.xlsx")
      })
    )
  })
  
  # logfile.txt
  output$downloadReport8 <- downloadHandler(
    filename = "logfile.txt",
    content <- function(file) {
      file.copy("../scripts/logs/logfile.txt", file)
    }
  )
  observeEvent(input$initiateButton, {
    toggle(
      output$downloadButton8 <- renderUI({
        downloadButton("downloadReport8", label = "Download logfile.txt")
      })
    )
  })
  
  # analytical-pipeline.html
  output$downloadReport9 <- downloadHandler(
    filename = "analytical-pipeline.html",
    content <- function(file) {
      file.copy("../scripts/analytical-pipeline.html", file)
    }
  )
  observeEvent(input$initiateButton, {
    toggle(
      output$downloadButton9 <- renderUI({
        downloadButton("downloadReport9", label = "Download analytical-pipeline.html")
      })
    )
  })
  
  output$downloadZip <- downloadHandler(
    filename = function() {
      paste("output", "zip", sep=".")
    },
    content = function(fname) {
      fs <- c()
      list_files <- c("1_DQA.html","2_validation.html","3_imputation.html","4_matching.html","5_descriptive.html","6_survival-analysis.html")
      tmpdir <- tempdir()
      setwd(tempdir())
      for (i in list_files) {
        path <- paste0("../output/","sample_", i)
        fs <- c(fs, path)
        write(i*2, path)
      }
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
}

shinyApp(
  ui, 
  server,
  onStart = function() {
    unlink("../input/*")
    unlink("../scripts/input/*")
    unlink("../logs/*")
    unlink("../scripts/logs/*")
    onStop(function() {
      unlink("../output/*")
      unlink("../input/*")
      unlink("../scripts/input/*")
      unlink("../scripts/logs/*")
      unlink("../logs/*")
    })
  }
)

