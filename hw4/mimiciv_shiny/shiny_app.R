library(shiny)
library(bigrquery)
library(dplyr)
library(ggplot2)
library(shiny)
library(tidyr)
library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)

mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds")

transfers_tble <- tbl(con_bq, "transfers")
lab_parquet <- tbl(con_bq, "labevents")
procedures_tble <- tbl(con_bq, "procedures_icd")
diagnoses_tble <- tbl(con_bq, "diagnoses_icd")

d_icd_procedures_tble <- tbl(con_bq, "d_icd_procedures")
d_icd_diagnoses_tble <- tbl(con_bq, "d_icd_diagnoses")


# Define UI
ui <- fluidPage(
  navbarPage("icu cohort", 
             #tabPanel 1
             tabPanel("Patient Characteristic", fluid = TRUE,
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        #Panel 1 sidebar
                        sidebarPanel(
                          selectInput(inputId = "variable",
                                      label = "Variable of interest:",
                                      choices = c("First care unit" = "first_careunit",
                                                  "Last care unit" = "last_careunit", 
                                                  "Admission Type" = "admission_type",
                                                  "Insurance" = "insurance",
                                                  "Language" = "language",
                                                  "Marital status" ="marital_status",
                                                  "Race" = "race", 
                                                  "Gender" = "gender",
                                                  "Age at intime" = "age_at_intime",
                                                  "Lab Events" = "Lab_Measurement", 
                                                  "Vitals" = "Vital_Measurements")),
                          # whether outlier
                          checkboxInput("outliers", 
                                        "Remove outliers in IQR method for measurments", 
                                        FALSE)),
                        
                        # Panel 1 main
                        mainPanel(
                          plotOutput("needplot")
                        )) ),
             
             #tabPanel 2
             tabPanel("Patient's ADT and ICU stay information", fluid = TRUE,
                      # Sidebar layout with a input and output definitions
                      sidebarLayout(
                        
                        #Panel 2 sidebar
                        sidebarPanel(
                          numericInput("patient_id", "Patient ID", 10013310)
                        ),
                        
                        #Panel 2 mainPanel
                        mainPanel(
                          plotOutput("needplot2") )))))

server <- function(input, output){
  
  #get input
  selected_variable <- reactive({
    input$variable })
  selected_patient <- reactive({
    input$patient_id })
  
  # tabPanel 1
  observe({
    req(selected_variable())
    
    # Lab_Measurement
    if (identical(selected_variable(), "Lab_Measurement")){
      selected_vars <- c("Bicarbonate", "Chloride", "Creatinine", "Glucose", "Potassium", "Sodium", 
                         "Hematocrit", "White Blood Cells")
      selected_data <- mimic_icu_cohort[, selected_vars]
      selected_data_long <- selected_data %>%
        pivot_longer(cols = everything(),
                     names_to="Lab_Measurement",
                     values_to = "Value") %>%
        mutate(Lab_Measurement = as.factor(Lab_Measurement),
               Value = as.numeric(Value))
      
      #IQR
      processed_data <- selected_data_long
      if (input$outliers) {processed_data <- na.omit(selected_data_long)
      Q1 <- quantile(processed_data$Value, 0.25)
      Q3 <- quantile(processed_data$Value, 0.75)
      IQR <- Q3 - Q1
      outliers <- subset(processed_data, Value < (Q1 - 1.5 * IQR) | Value > (Q3 + 1.5 * IQR))
      processed_data <- processed_data[!(processed_data$Value %in% outliers$Value),]
      }
      
      output$needplot <- renderPlot({
        ggplot(processed_data, aes(x = Lab_Measurement, y = Value)) +
          geom_boxplot(outlier.shape = NA) + # This hides outliers
          labs(title = "Boxplot Example", x = "Lab Measurement", y = "Value") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          coord_flip() # Flips the coordinates to match your second image
      })
    }
    
    # Vital_Measurements
    else if (identical(selected_variable(), "Vital_Measurements")){
      selected_vars_icu <- c("Heart Rate", 
                             "Non Invasive Blood Pressure systolic", 
                             "Non Invasive Blood Pressure diastolic", 
                             "Respiratory Rate",
                             "Temperature Fahrenheit")
      selected_data_icu <- mimic_icu_cohort[, selected_vars_icu]
      selected_data_iculong <- selected_data_icu %>%
        pivot_longer(cols = everything(),
                     names_to="Vital_Measurements",
                     values_to = "Value")
      
      #IQR
      processed_data <- selected_data_iculong
      if (input$outliers) {
        processed_data <- na.omit(selected_data_iculong)
        Q1 <- quantile(processed_data$Value, 0.25)
        Q3 <- quantile(processed_data$Value, 0.75)
        IQR <- Q3 - Q1
        outliers <- subset(processed_data, Value < (Q1 - 1.5 * IQR) | Value > (Q3 + 1.5 * IQR))
        processed_data <- processed_data[!(processed_data$Value %in% outliers$Value),]
      }
      
      output$needplot <- renderPlot({
        ggplot(processed_data, aes(x = Vital_Measurements, y = Value)) +
          geom_boxplot(outlier.shape = NA) +
          labs(title = "Boxplot Example", x = "Vital Measurements", y = "Value") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          coord_flip() # Flips the coordinates to match your desired orientation
      })
      
    }
    
    # other
    else {
      output$needplot <- renderPlot({
        ggplot(mimic_icu_cohort, aes(y = !!sym(selected_variable()))) +
          geom_bar(alpha = 0.7) +
          theme_minimal() +
          labs(title = "Patient count by stay or patient variable group") }) } })
  
  # tabPanel 2
  observe({
    selected_patient_id <- as.numeric(selected_patient())
    
    mimic_icu_cohort_1 <- mimic_icu_cohort %>%
      filter(subject_id == selected_patient_id)
    
    # get needed data
    
    patient_id <- selected_patient_id
    
    transfers_sub <- transfers_tble %>%
      filter(subject_id == patient_id) %>%
      select(subject_id, careunit, intime, outtime) %>%
      collect()
    
    labevents_sub <- lab_parquet %>%
      filter(subject_id == patient_id) %>%
      select(subject_id, charttime, storetime) %>%
      collect()
    
    procedures_sub <- procedures_tble %>%
      filter(subject_id == patient_id) %>%
      select(subject_id, chartdate, icd_code, icd_version) %>%
      left_join(d_icd_procedures_tble, by = c("icd_code", "icd_version")) %>%
      select(subject_id, chartdate, long_title) %>%
      collect()
    
    diagnoses_sub <- diagnoses_tble %>%
      filter(subject_id == patient_id) %>%
      select(subject_id,icd_code, icd_version) %>%
      left_join(d_icd_diagnoses_tble, by = c("icd_code", "icd_version")) %>%
      select(subject_id, long_title) %>%
      collect()
    
    # title
    main_title <- paste("Patient", selected_patient_id,",", mimic_icu_cohort_1$gender[1], ",", mimic_icu_cohort_1$anchor_age[1], "years old,", mimic_icu_cohort_1$race[1])
    sub_title <- paste0(diagnoses_sub$long_title[1],"\n",diagnoses_sub$long_title[2],"\n",diagnoses_sub$long_title[3])
    
    output$needplot2 <- renderPlot({
      ggplot() +
        geom_point(data = procedures_sub, aes(x = as.POSIXct(chartdate), y = "Procedure",
                                              shape = factor(long_title)), na.rm = TRUE) +
        geom_point(data = labevents_sub, aes(x = charttime, y = "Lab"),shape = 3, na.rm = TRUE) +
        geom_segment(data = transfers_sub, aes(x = intime, xend = outtime,
                                               y = "ADT", yend = "ADT",
                                               color = careunit,
                                               linewidth = str_detect(careunit, "(ICU|CCU)")), na.rm = TRUE) +
        guides(linewidth = "none", shape = guide_legend(nrow = 3)) +
        scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) +
        theme_bw(base_size = 10) +
        theme(legend.position = "bottom", legend.box = "vertical",
              legend.title = element_text(size=9)) +
        labs(x = "Calender Time", y="", title = main_title,
             subtitle = sub_title,
             color = "Care Unit", shape = "procedure") }) }) }

shinyApp(ui, server)


