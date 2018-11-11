# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library(shiny)
library(dplyr)
library(plotly)
library(wesanderson)

source('read_data.R')

shinyServer(function(session, input, output) {
    
    observe({
        req(c(input$study))
        
        update_site <- mriqc %>%
            filter(study %in% input$study) %>%
            select(site) %>%
            unlist()
        
        updateSelectizeInput(session,
                            inputId = "site",
                            label = "Select site(s):",
                            choices = unique(update_site))
    })

    observe({
        req(c(input$study, input$site))
        
        update_group <- mriqc %>%
            filter(study %in% input$study,
                   site %in% input$site) %>%
            select(diagnosis) %>%
            unlist()
        
        updateSelectizeInput(session,
                             inputId = "diagnosis",
                             label = "Select participant group:",
                             choices = unique(update_group))
    })

    observe({
        req(c(input$study, input$site, input$diagnosis, input$modality))
        
        update_scan <- mriqc %>%
            filter(study %in% input$study,
                   site %in% input$site,
                   diagnosis %in% input$diagnosis,
                   modality %in% input$modality) %>%
            select(scan_type) %>%
            unlist()
        
        updateSelectizeInput(session,
                             inputId = "scan_type",
                             label = "Select scan type(s):",
                             choices = unique(update_scan))
    })    
    
    observe({
        update_metric <- mriqc %>%
            filter(modality == input$modality) %>%
            select(metric) %>%
            unlist()
        
        updateSelectInput(session,
                          inputId = "y",
                          label = "Y-axis:",
                          choices = unique(update_metric))
    })
    
    dataset <- 
        reactive({
            req(c(input$study, input$site, input$diagnosis, input$modality, input$scan_type))
            
            mriqc %>% 
                filter(study %in% input$study,
                       site %in% input$site,
                       diagnosis %in% input$diagnosis,
                       modality %in% input$modality,
                       scan_type %in% input$scan_type,
                       metric == input$y) 
            })

    demographics <-
        reactive({
            req(c(input$study, input$site, input$diagnosis, input$modality, input$scan_type))
            
            mriqc %>% 
                filter(study %in% input$study,
                       site %in% input$site,
                       diagnosis %in% input$diagnosis,
                       modality %in% input$modality,
                       scan_type %in% input$scan_type) %>%
                select(study, site, subject_id, session_id, diagnosis) %>%
                unique() %>%
                group_by(study, site, diagnosis, session_id) %>%
                count() %>%
                rename(Study = study,
                       Site = site,
                       Group = diagnosis,
                       Visit = session_id,
                       `Total Number Scanned` = n) %>%
                arrange(Study, desc(`Total Number Scanned`))             
        })
    
    anat_dataset <- 
        reactive({
            req(c(input$study, input$site, input$diagnosis, input$scan_type))
            
            anat_report %>% 
                filter(study %in% input$study,
                       site %in% input$site,
                       diagnosis %in% input$diagnosis,
                       scan_type %in% input$scan_type)
        })

    bold_dataset <- 
        reactive({
            req(c(input$study, input$site, input$diagnosis, input$scan_type))
            
            bold_report %>% 
                filter(study %in% input$study,
                       site %in% input$site,
                       diagnosis %in% input$diagnosis,
                       scan_type %in% input$scan_type) 
        })       

    output$plot_metric <- renderPlotly({
        
        validate(need(input$x=="scan_type", message=FALSE))
        
        plot_ly(dataset(), x = ~site, y = ~measurement,
                type = 'box', boxpoints = 'all', jitter = 0.3, pointpos = 0,
                hoverinfo = 'text',
                text = ~paste(' SubjectID: ', subject_id,
                              '<br> Diagnosis: ', diagnosis,
                              '<br>', metric, ': ', measurement),
                color = ~study, colors = "Spectral",
                mode = 'markers',
                marker = list(color = "#4c4c4c")) %>%
            layout(boxmode = 'group',
                   font = t,
                   xaxis = list(title = 'Site'),
                   yaxis = list(title = ~metric)) %>%
            config(displayModeBar = FALSE)
        })    

    output$plot_date <- renderPlotly({
        
        validate(need(input$x=="date", message=FALSE))
        
        plot_ly(dataset(), x = ~date, y = ~measurement,
                type = 'scatter',
                hoverinfo = 'text',
                text = ~paste(' SubjectID: ', subject_id,
                              '<br> Diagnosis: ', diagnosis,
                              '<br>', metric, ': ', measurement),
                color = ~study, colors = "Spectral") %>%
            layout(font = t,
                   xaxis = list(title = 'Date'),
                   yaxis = list(title = ~metric)) %>%
            config(displayModeBar = FALSE)
    })
    
        
    
    output$plot_anat_metric <- renderPlotly({
        
        validate(need(input$modality=="anat", message=FALSE))
        validate(need(input$x=="scan_type", message=FALSE))
        
        subplot(plot_ly(anat_dataset(), 
                        x = ~site, y = ~input$y, 
                        type = 'box', boxpoints = 'all', jitter = 0.3, pointpos = 0,
                        hoverinfo = 'text',
                        text = ~paste(' SubjectID: ', new_id,
                                      '<br> Diagnosis: ', diagnosis,
                                      '<br> Measure: ', input$y),   
                        #mode = 'markers',
                        #marker = list(color = "#4c4c4c"),
                        #symbol = ~diagnos is, symbols = c('circle', 'square'),
                        color = ~study, colors = "Spectral",
                        legendgroup = ~study) %>%
                    layout(boxmode = 'group',
                           list(text = 'T1w'),
                           xaxis = list(title = 'Site'),
                           yaxis = list(title = input$y)) %>%
                    config(displayModeBar = FALSE),
                plot_ly(anat_dataset(), 
                        x = ~site, y = ~cjv, 
                        type = 'box', boxpoints = 'all', jitter = 0.3, pointpos = 0,
                        hoverinfo = 'text',
                        text = ~paste(' SubjectID: ', subject_id,
                                      '<br> Diagnosis: ', diagnosis,
                                      '<br> Measure: ', input$y),  
                        color = ~study, colors = "Spectral",
                        legendgroup = ~study,
                        showlegend = F) %>%
                    layout(boxmode = 'group',
                           list(text = 'T2w'),
                           xaxis = list(title = 'Site'),
                           yaxis = list(title = input$y)) %>%
                    config(displayModeBar = FALSE),
                nrows = 1, shareX = TRUE)
    })

    output$summary_table <- renderTable({
        demographics()
        }, striped = FALSE, hover = TRUE, spacing = "l", align = "lcccr", digits = 4, width = "90%")

})
