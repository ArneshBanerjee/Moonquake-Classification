# Load required libraries
library(shiny)
library(tidyverse)
library(readxl)
library(plotly)  # For interactive plots
library(DT)      # For interactive tables
library(signal)  # For spectrogram
library(viridis) # For color scales

# UI Definition
ui <- fluidPage(
  # Custom CSS for better styling
  tags$head(
    tags$style(HTML("
      .sidebar { background-color: #f8f9fa; padding: 15px; }
      .main-panel { padding: 15px; }
      .feature-select { margin-bottom: 15px; }
      .plot-container { height: 600px; }
    "))
  ),
  
  # Title with custom styling
  titlePanel(div("Moonquake Analysis Dashboard", 
                 style = "color: #2c3e50; font-weight: bold;")),
  
  # Main layout with sidebar and main panel
  fluidRow(
    # Sidebar panel
    column(3,
      class = "sidebar",
      # Plot selection
      selectInput("plot_type", "Select Plot Type:",
                  choices = c("Time Series Plot" = "timeseries",
                            "Feature Analysis" = "feature_analysis",
                            "Classification Results" = "classification",
                            "Feature Importance" = "importance",
                            "Data Distribution" = "distribution",
                            "Seismogram" = "seismogram",
                            "Spectrogram" = "spectrogram",
                            "Data Table" = "table"),
                  selected = "timeseries"),
      
      # Feature selection for time series
      conditionalPanel(
        condition = "input.plot_type == 'timeseries'",
        selectInput("feature", "Select Feature:",
                   choices = c("Mean Velocity" = "Mean Velocity",
                             "Standard Deviation" = "Standard Deviation",
                             "Max Velocity" = "Max Velocity",
                             "Min Velocity" = "Min Velocity",
                             "Range Velocity" = "Range Velocity",
                             "Median Velocity" = "Median Velocity",
                             "RMS Velocity" = "RMS Velocity",
                             "Energy" = "Energy",
                             "Impulse Factor" = "Impulse Factor",
                             "Velocity Derivative Mean" = "Velocity Derivative Mean",
                             "Velocity Derivative Std" = "Velocity Derivative Std",
                             "Mean Velocity Around Arrival" = "Mean Velocity Around Arrival",
                             "Max Velocity Around Arrival" = "Max Velocity Around Arrival",
                             "Max STA/LTA" = "Max STA/LTA"),
                   selected = "Mean Velocity")
      ),
      
      # Feature analysis controls
      conditionalPanel(
        condition = "input.plot_type == 'feature_analysis'",
        selectInput("x_feature", "X-axis Feature:",
                   choices = c("Mean Velocity" = "Mean Velocity",
                             "Standard Deviation" = "Standard Deviation",
                             "Max Velocity" = "Max Velocity",
                             "Min Velocity" = "Min Velocity",
                             "Range Velocity" = "Range Velocity",
                             "Median Velocity" = "Median Velocity",
                             "RMS Velocity" = "RMS Velocity",
                             "Energy" = "Energy",
                             "Impulse Factor" = "Impulse Factor",
                             "Velocity Derivative Mean" = "Velocity Derivative Mean",
                             "Velocity Derivative Std" = "Velocity Derivative Std",
                             "Mean Velocity Around Arrival" = "Mean Velocity Around Arrival",
                             "Max Velocity Around Arrival" = "Max Velocity Around Arrival",
                             "Max STA/LTA" = "Max STA/LTA"),
                   selected = "Mean Velocity"),
        selectInput("y_feature", "Y-axis Feature:",
                   choices = c("Mean Velocity" = "Mean Velocity",
                             "Standard Deviation" = "Standard Deviation",
                             "Max Velocity" = "Max Velocity",
                             "Min Velocity" = "Min Velocity",
                             "Range Velocity" = "Range Velocity",
                             "Median Velocity" = "Median Velocity",
                             "RMS Velocity" = "RMS Velocity",
                             "Energy" = "Energy",
                             "Impulse Factor" = "Impulse Factor",
                             "Velocity Derivative Mean" = "Velocity Derivative Mean",
                             "Velocity Derivative Std" = "Velocity Derivative Std",
                             "Mean Velocity Around Arrival" = "Mean Velocity Around Arrival",
                             "Max Velocity Around Arrival" = "Max Velocity Around Arrival",
                             "Max STA/LTA" = "Max STA/LTA"),
                   selected = "Standard Deviation"),
        checkboxInput("color_by_type", "Color by Moonquake Type", value = TRUE)
      ),
      
      # Seismogram and Spectrogram controls
      conditionalPanel(
        condition = "input.plot_type == 'seismogram' || input.plot_type == 'spectrogram'",
        selectInput("selected_file", "Select Moonquake File:",
                   choices = NULL),

      ),
      
      # Data distribution controls
      conditionalPanel(
        condition = "input.plot_type == 'distribution'",
      ),
      
      # Help text
      # helpText("Select different visualizations using the dropdown above.",
      #          "Use the interactive features to zoom and explore the data.")
    ),
    
    # Main panel
    column(9,
      class = "main-panel",
      # Error messages
      uiOutput("error_message"),
      
      # Conditional rendering based on plot type
      conditionalPanel(
        condition = "input.plot_type != 'table' && input.plot_type != 'classification' && input.plot_type != 'distribution'",
        plotlyOutput("selected_plot", height = "600px")
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'table'",
        DTOutput("data_table")
      ),
      
      # Classification results
      conditionalPanel(
        condition = "input.plot_type == 'classification'",
        fluidRow(
          column(4,
                 h3("CatBoost Results"),
                 tableOutput("catboost_metrics"),
                 plotlyOutput("catboost_confusion_matrix")
          ),
          column(4,
                 h3("Random Forest Results"),
                 tableOutput("rf_metrics"),
                 plotlyOutput("rf_confusion_matrix")
          ),
          column(4,
                 h3("ANN (H2O) Results"),
                 tableOutput("ann_metrics"),
                 plotlyOutput("ann_confusion_matrix")
          )
        )
      ),
      
      # Data distribution
      conditionalPanel(
        condition = "input.plot_type == 'distribution'",
        fluidRow(
          column(6,
                 h3("Histogram"),
                 plotlyOutput("histogram_plot", height = "500px")
          ),
          column(6,
                 h3("Pie Chart"),
                 plotlyOutput("pie_chart_plot", height = "500px")
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Read data with error handling
  data <- reactive({
    tryCatch({
      df <- read_excel("final.xlsx")
      return(df)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Update file selection dropdown when data is loaded
  observe({
    req(data())
    updateSelectInput(session, "selected_file",
                     choices = data()$`File Name`)
  })
  
  # Function to read raw data
  read_raw_data <- function(file_name) {
    # Construct the CSV file path in the correct directory
    file_path <- file.path("/Users/arnesh/Developer/R-Moonquake/space_apps_2024_seismic_detection/data/lunar/training/data/S12_GradeA", 
                          paste0(file_name, ".csv"))
    if (!file.exists(file_path)) {
      warning(paste("File not found:", file_path))
      return(NULL)
    }
    
    # Read the CSV file
    raw_data <- read.csv(file_path)
    return(raw_data)
  }
  
  # Error message handling
  output$error_message <- renderUI({
    if (is.null(data())) {
      div(
        style = "color: red; padding: 10px; background-color: #ffe6e6; border-radius: 5px;",
        "Error: Could not load data. Please check if the data file exists and is in the correct format."
      )
    }
  })
  
  # Interactive plot output
  output$selected_plot <- renderPlotly({
    req(data())
    req(input$plot_type)
    
    df <- data()
    
    p <- switch(input$plot_type,
           "timeseries" = {
             # Time series plot with selected feature
             feature_name <- input$feature
             
             p <- plot_ly(df, x = ~seq_len(nrow(df))) %>%
               add_lines(y = as.formula(paste0("~`", feature_name, "`")), 
                        name = feature_name,
                        line = list(color = '#1f77b4'),
                        text = ~paste("File:", `File Name`, "<br>",
                                    feature_name, ":", as.formula(paste0("~`", feature_name, "`"))),
                        hoverinfo = 'text')
             
             p %>% layout(
               title = paste("Time Series of", feature_name),
               xaxis = list(title = "Time"),
               yaxis = list(title = feature_name),
               hovermode = "x unified"
             )
           },
           "feature_analysis" = {
             # Feature analysis scatter plot
             x_feature <- input$x_feature
             y_feature <- input$y_feature
             
             if (input$color_by_type) {
               p <- plot_ly(df, 
                          x = as.formula(paste0("~`", x_feature, "`")),
                          y = as.formula(paste0("~`", y_feature, "`")),
                          color = ~mq_type,
                          type = 'scatter',
                          mode = 'markers',
                          text = ~`File Name`,
                          hoverinfo = 'text+x+y')
             } else {
               p <- plot_ly(df, 
                          x = as.formula(paste0("~`", x_feature, "`")),
                          y = as.formula(paste0("~`", y_feature, "`")),
                          type = 'scatter',
                          mode = 'markers',
                          text = ~`File Name`,
                          hoverinfo = 'text+x+y')
             }
             
             p %>% layout(
               title = paste(y_feature, "vs", x_feature),
               xaxis = list(title = x_feature),
               yaxis = list(title = y_feature)
             )
           },
           "seismogram" = {
             req(input$selected_file)
             raw_data <- read_raw_data(input$selected_file)
             if (is.null(raw_data)) {
               return(plot_ly() %>% 
                      add_annotations(text = "Raw data file not found",
                                    showarrow = FALSE))
             }
             
             # Create the seismogram plot
             plot_ly(x = raw_data$time_rel.sec., 
                    y = raw_data$velocity.m.s.,
                    type = 'scatter',
                    mode = 'lines',
                    line = list(color = 'blue', width = 0.5)) %>%
               layout(title = paste("Seismogram:", input$selected_file),
                      xaxis = list(title = "Time (seconds)"),
                      yaxis = list(title = "Amplitude"),
                      showlegend = FALSE) %>%
               add_segments(x = 20, xend = 20, y = min(raw_data$velocity.m.s.), 
                          yend = max(raw_data$velocity.m.s.),
                          line = list(color = 'red', dash = 'dash'))
           },
           "spectrogram" = {
             req(input$selected_file)
             raw_data <- read_raw_data(input$selected_file)
             if (is.null(raw_data)) {
               return(plot_ly() %>% 
                      add_annotations(text = "Raw data file not found",
                                    showarrow = FALSE))
             }
             
             # Create spectrogram using signal package
             fs <- 1/0.01  # Sampling frequency (assuming 0.01s sampling interval)
             nfft <- 512   # Number of FFT points
             window <- 256 # Window size
             overlap <- 128 # Overlap between windows
             
             # Compute spectrogram
             spec <- specgram(raw_data$velocity.m.s., 
                            n = nfft,
                            Fs = fs,
                            window = window,
                            overlap = overlap)
             
             # Convert to decibels for better visualization
             S <- 20 * log10(abs(spec$S))
             
             plot_ly(x = rep(spec$t, each = length(spec$f)),
                    y = rep(spec$f, times = length(spec$t)),
                    z = as.vector(S),
                    type = "heatmap",
                    colors = viridis_pal(option = "plasma")(100)) %>%
               layout(title = paste("Spectrogram:", input$selected_file),
                      xaxis = list(title = "Time (s)"),
                      yaxis = list(title = "Frequency (Hz)"))
           },
           "classification" = {
             # Classification results visualization
             plot_ly() %>%
               add_annotations(
                 text = "Classification plot will be implemented based on your specific classification results",
                 showarrow = FALSE
               )
           },
           "importance" = {
             # Feature importance visualization
             feature_importance <- data.frame(
               Feature = c("Mean Velocity", "Velocity Derivative Mean", "Median Velocity",
                         "Max Velocity Around Arrival", "Max Velocity", "Impulse Factor",
                         "Standard Deviation", "Max STA/LTA", "Mean Velocity Around Arrival",
                         "RMS Velocity", "Energy", "Velocity Derivative Std",
                         "Min Velocity", "Range Velocity"),
               Importance = c(14.56, 13.79, 11.53, 9.20, 8.13, 6.86, 5.73, 5.63,
                            5.30, 4.65, 3.89, 3.79, 3.62, 3.30)
             )
             
             # Create a bar plot of feature importances
             plot_ly(feature_importance, 
                    x = ~Importance, 
                    y = ~reorder(Feature, Importance),
                    type = 'bar',
                    orientation = 'h',
                    marker = list(color = viridis_pal(option = "plasma")(nrow(feature_importance)))) %>%
               layout(title = "Feature Importance for Moonquake Classification",
                      xaxis = list(title = "Importance Score"),
                      yaxis = list(title = "Feature"),
                      margin = list(l = 200))  # Increase left margin for feature names
           }
    )
    
    return(p)
  })
  
  # Interactive data table
  output$data_table <- renderDT({
    req(data())
    datatable(data(),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ))
  })
  
  # Classification results
  output$catboost_metrics <- renderTable({
    data.frame(
      Metric = c("Accuracy", "Precision (impact_mq)", "Recall (impact_mq)", 
                "F1-score (impact_mq)", "Precision (deep_mq)", "Recall (deep_mq)", 
                "F1-score (deep_mq)"),
      Value = c("0.7333", "0.85", "0.85", "0.85", "0.00", "0.00", "0.00")
    )
  })
  
  output$catboost_confusion_matrix <- renderPlotly({
    confusion_data <- data.frame(
      Predicted = c("impact_mq", "impact_mq", "deep_mq", "deep_mq"),
      Actual = c("impact_mq", "deep_mq", "impact_mq", "deep_mq"),
      Count = c(11, 2, 2, 0)
    )
    
    plot_ly(confusion_data,
            x = ~Predicted,
            y = ~Actual,
            z = ~Count,
            type = "heatmap",
            colors = viridis_pal(option = "plasma")(100),
            text = ~paste("Predicted:", Predicted, "<br>",
                         "Actual:", Actual, "<br>",
                         "Count:", Count),
            hoverinfo = "text") %>%
      layout(title = "CatBoost Confusion Matrix",
             xaxis = list(title = "Predicted"),
             yaxis = list(title = "Actual"))
  })
  
  output$rf_metrics <- renderTable({
    data.frame(
      Metric = c("Accuracy", "Precision (impact_mq)", "Recall (impact_mq)", 
                "F1-score (impact_mq)", "Precision (deep_mq)", "Recall (deep_mq)", 
                "F1-score (deep_mq)"),
      Value = c("0.9333", "0.9333", "1.00", "0.9655", "0.00", "0.00", "0.00")
    )
  })
  
  output$rf_confusion_matrix <- renderPlotly({
    confusion_data <- data.frame(
      Predicted = c("impact_mq", "impact_mq", "deep_mq", "deep_mq"),
      Actual = c("impact_mq", "deep_mq", "impact_mq", "deep_mq"),
      Count = c(14, 1, 0, 0)
    )
    
    plot_ly(confusion_data,
            x = ~Predicted,
            y = ~Actual,
            z = ~Count,
            type = "heatmap",
            colors = viridis_pal(option = "plasma")(100),
            text = ~paste("Predicted:", Predicted, "<br>",
                         "Actual:", Actual, "<br>",
                         "Count:", Count),
            hoverinfo = "text") %>%
      layout(title = "Random Forest Confusion Matrix",
             xaxis = list(title = "Predicted"),
             yaxis = list(title = "Actual"))
  })
  
  output$ann_metrics <- renderTable({
    data.frame(
      Metric = c("Accuracy", "Precision (impact_mq)", "Recall (impact_mq)", 
                "F1-score (impact_mq)", "Precision (deep_mq)", "Recall (deep_mq)", 
                "F1-score (deep_mq)"),
      Value = c("14.29%", "85.71%", "100%", "92.31%", "0.00%", "0.00%", "0.00%")
    )
  })
  
  output$ann_confusion_matrix <- renderPlotly({
    confusion_data <- data.frame(
      Predicted = c("impact_mq", "impact_mq", "deep_mq", "deep_mq"),
      Actual = c("impact_mq", "deep_mq", "impact_mq", "deep_mq"),
      Count = c(6, 1, 0, 0)
    )
    
    plot_ly(confusion_data,
            x = ~Predicted,
            y = ~Actual,
            z = ~Count,
            type = "heatmap",
            colors = viridis_pal(option = "plasma")(100),
            text = ~paste("Predicted:", Predicted, "<br>",
                         "Actual:", Actual, "<br>",
                         "Count:", Count),
            hoverinfo = "text") %>%
      layout(title = "ANN (H2O) Confusion Matrix",
             xaxis = list(title = "Predicted"),
             yaxis = list(title = "Actual"))
  })
  
  # Data distribution
  output$histogram_plot <- renderPlotly({
    req(data())
    df <- data()
    
    # Get the moonquake type distribution
    dist_data <- df %>%
      count(mq_type) %>%
      rename(Type = mq_type, Count = n)
    
    plot_ly(dist_data,
            x = ~Type,
            y = ~Count,
            type = "bar",
            marker = list(color = viridis_pal(option = "plasma")(nrow(dist_data)))) %>%
      layout(title = "Moonquake Type Distribution",
             xaxis = list(title = "Moonquake Type"),
             yaxis = list(title = "Count"))
  })
  
  output$pie_chart_plot <- renderPlotly({
    req(data())
    df <- data()
    
    # Get the moonquake type distribution
    dist_data <- df %>%
      count(mq_type) %>%
      rename(Type = mq_type, Count = n) %>%
      mutate(Percentage = round(Count/sum(Count)*100, 1))
    
    plot_ly(dist_data,
            labels = ~Type,
            values = ~Count,
            type = "pie",
            text = ~paste(Percentage, "%"),
            textposition = "inside",
            marker = list(colors = viridis_pal(option = "plasma")(nrow(dist_data)))) %>%
      layout(title = "Moonquake Type Distribution",
             showlegend = TRUE)
  })
}

# Run the app
shinyApp(ui = ui, server = server) 