# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(shinycssloaders)

source("read_data.R")

dataset <- mriqc

# Define UI for application

shinyUI(fluidPage(
    theme = shinytheme("yeti"),

    # Header
    headerPanel(
        title = "MRIQC",
        windowTitle = "TIGRLab MRIQC Results"
    ),

    # Sidebar layout with a input and output definitions
    sidebarLayout(

        # Inputs
        sidebarPanel(width = 3,

            wellPanel(

                h3("Subsetting"),

                # Select variables to filter dataset
                selectizeInput(inputId = "study",
                               label = "Select study/studies:",
                               choices = unique(dataset$study),
                               multiple = TRUE),

                selectizeInput(inputId = "site",
                               label = "Select site(s):",
                               choices = NULL,
                               multiple = TRUE),

                selectizeInput(inputId = "diagnosis",
                               label = "Select participant group:",
                               choices = NULL,
                               multiple = TRUE),

                selectInput(inputId = "modality",
                            label = "Select imaging modality:",
                            choices = c("Anatomical" = "anat",
                                        "Functional" = "bold")),

                selectizeInput(inputId = "scan_type",
                              label = "Select scan type(s):",
                              choices = NULL,
                              multiple = TRUE)
                ),

            wellPanel(

                h3("Plotting"),

                # Select variable for y-axis
                selectInput(inputId = "y",
                            label = "Y-axis:",
                            choices = NULL),

                # Select variable for x-axis
                selectInput(inputId = "x",
                            label = "X-axis",
                            choices = c("Scan Type" = "scan_type",
                                        "Date" = "date"))
            )

        ),

        # Outputs
        mainPanel(width = 9,

            #tags$style(type="text/css",
            #           ".shiny-output-error { visibility: hidden; }",
            #           ".shiny-output-error:before { visibility: hidden; }"
            #           ),

            tabsetPanel(type = "tabs",
                        tabPanel("Plot",

                                 #conditionalPanel(
                                 #    condition = "input.y == 'fd_num' || input.y == 'fd_perc'",
                                 #    withSpinner(plotlyOutput("plot_fd"), color = "#778899")
                                 #),                                 
                                                                  
                                 conditionalPanel(
                                     condition = "input.x == 'scan_type'",
                                     withSpinner(plotlyOutput("plot_metric"), color = "#778899")
                                     ),

                                 conditionalPanel(
                                     condition = "input.x == 'date'",
                                     withSpinner(plotlyOutput("plot_date"), color = "#778899")
                                     )
                                 ),

                        tabPanel("Summary",
                                 tableOutput(outputId = "summary_table")
                                 ),

                        tabPanel("Data",

                                 conditionalPanel(
                                     condition = "input.modality == 'anat'",
                                     dataTableOutput(outputId = "anat_data_table"),
                                     downloadButton("download_anat", "Download")
                                 ),

                                 conditionalPanel(
                                     condition = "input.modality == 'bold'",
                                     dataTableOutput(outputId = "bold_data_table"),
                                     downloadButton("download_bold", "Download")
                                 )
                                 )
                                 )
            )

        )
    )
    )
