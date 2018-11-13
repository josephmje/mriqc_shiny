# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(wesanderson)

source('read_data.R')

shinyServer(function(session, input, output) {

    observe({
      req(input$study)
      
      update_site <- mriqc %>%
        filter(study %in% input$study) %>%
        select(site) %>%
        unlist() %>%
        unique()
      
      updateSelectizeInput(session,
                           inputId = "site",
                           label = "Select site(s):",
                           choices = update_site)
      })

    observe({
      req(input$study, input$site)
      
      update_group <- mriqc %>%
        filter(study %in% input$study,
               site %in% input$site) %>%
        select(diagnosis) %>%
        unlist() %>%
        unique()
      
      updateSelectizeInput(session,
                           inputId = "diagnosis",
                           label = "Select participant group:",
                           choices = update_group)
      })

    observe({
      req(input$study, input$site, input$diagnosis, input$modality)
      
      update_scan <- mriqc %>%
        filter(study %in% input$study,
               site %in% input$site,
               diagnosis %in% input$diagnosis,
               modality %in% input$modality) %>%
        select(scan_type) %>%
        unlist() %>%
        unique()
      
      updateSelectInput(session,
                        inputId = "scan_type",
                        label = "Select scan type(s):",
                        choices = update_scan)
      })

    observe({
      req(input$modality)
      
      update_metric <- mriqc %>%
        filter(modality == input$modality) %>%
        select(metric) %>%
        unlist() %>%
        unique()
      
      updateSelectInput(session,
                        inputId = "y",
                        label = "Y-axis:",
                        choices = update_metric)
      })

    dataset <-
      reactive({
        req(input$study, input$site, input$diagnosis, input$modality, input$scan_type)
        
        mriqc %>%
          filter(study %in% input$study,
                 site %in% input$site,
                 diagnosis %in% input$diagnosis,
                 modality %in% input$modality,
                 scan_type %in% input$scan_type,
                 metric == input$y,
                 fd_threshold %in% c(NA, "0.2 mm"))
        })
    
    demographics <-
      reactive({
        req(input$study, input$site, input$diagnosis, input$modality, input$scan_type)
        
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
          arrange(Study, Group, Site, Visit)
        })

    anat_dataset <-
        reactive({
          req(input$study, input$site, input$diagnosis, input$scan_type)
          
          anat_report %>%
            filter(study %in% input$study,
                   site %in% input$site,
                   diagnosis %in% input$diagnosis,
                   scan_type %in% input$scan_type) %>%
            select(study, site, diagnosis, subject_id, session_id, date, modality, scan_type, run_id, everything(), -new_id)
          })

    bold_dataset <-
        reactive({
          req(input$study, input$site, input$diagnosis, input$scan_type)
          
          bold_report %>%
            filter(study %in% input$study,
                   site %in% input$site,
                   diagnosis %in% input$diagnosis,
                   scan_type %in% input$scan_type) %>%
            select(study, site, diagnosis, subject_id, session_id, date, modality, scan_type, run_id, fd_threshold, everything(), -new_id)
          })

    output$plot_metric <- renderPlotly({

        validate(need(input$x=="scan_type", message=FALSE))

        plot_ly(dataset(),
                x = ~site, y = ~measurement,
                type = 'box', boxpoints = 'all', jitter = 0.3, pointpos = 0,
                hoverinfo = 'text',
                text = ~paste(' Subject: ', new_id,
                              '<br> Diagnosis: ', diagnosis,
                              '<br>', metric, ': ', measurement),
                mode = 'markers',
                marker = list(color = "#000",
                              opacity = 0.5),
                symbol = ~scan_type,
                color = ~study, colors = "Spectral") %>%
        layout(boxmode = 'group',
               xaxis = list(title = 'Site'),
               yaxis = list(title = input$y)) %>%
        plotly::config(displayModeBar = FALSE)
        })

    output$plot_date <- renderPlotly({

        validate(need(input$x=="date", message=FALSE))

        plot_ly(dataset(),
                x = ~date, y = ~measurement,
                type = 'scatter',
                hoverinfo = 'text',
                text = ~paste(' SubjectID: ', new_id,
                              '<br> Diagnosis: ', diagnosis,
                              '<br>', metric, ': ', measurement),
                color = ~study, colors = "Spectral") %>%
        layout(xaxis = list(title = 'Date'),
               yaxis = list(title = ~metric)) %>%
        plotly::config(displayModeBar = FALSE)

    })

    output$plot_fd <- renderPlotly({

        validate(need(input$modality=="bold", message=FALSE))
        validate(need(input$y %in% c("fd_num", "fd_perc"), message=FALSE))

        subplot(plot_ly(bold_dataset(),
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
                           list(text = '0.02 mm'),
                           xaxis = list(title = 'FD Threshold'),
                           yaxis = list(title = input$y)) %>%
                    config(displayModeBar = FALSE),
                plot_ly(bold_dataset(),
                        x = ~site, y = ~input$y,
                        type = 'box', boxpoints = 'all', jitter = 0.3, pointpos = 0,
                        hoverinfo = 'text',
                        text = ~paste(' SubjectID: ', new_id,
                                      '<br> Diagnosis: ', diagnosis,
                                      '<br> Measure: ', input$y),
                        color = ~study, colors = "Spectral",
                        legendgroup = ~study,
                        showlegend = F) %>%
                    layout(boxmode = 'group',
                           list(text = '0.05 mm'),
                           xaxis = list(title = 'FD Threshold'),
                           yaxis = list(title = input$y)) %>%
                    config(displayModeBar = FALSE),
                nrows = 1, shareX = TRUE)
    })

    output$summary_table <- renderTable({
        demographics()
        }, striped = FALSE, hover = TRUE, spacing = "l", align = "lcccr", digits = 4, width = "90%"
        )

    output$anat_data_table <- renderDataTable({
        anat_dataset()
    })

    output$bold_data_table <- renderDataTable({
        bold_dataset()
    })

    output$download_anat <- downloadHandler(
        filename = 'anat.csv',
        content = function(file) {
          write_csv(anat_dataset(), file)
        }
                      )

      output$download_bold <- downloadHandler(
          filename = 'bold.csv',
          content = function(file) {
            write_csv(bold_dataset(), file)
          }
                        )

})
