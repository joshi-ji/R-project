library(shiny)
library(DT)
library(ggplot2)
library(forecast)
library(plotly)

ui <- navbarPage(
  title = "Railway Data Analysis",
  
  # Welcome Tab
  tabPanel("Welcome",
           fluidPage(
             tags$head(
               tags$style(HTML("
                 body {
                   background-color: #2c3e50;
                   color: #ecf0f1;
                   font-family: 'Helvetica Neue', Arial, sans-serif;
                 }
                 .welcome-container {
                   display: flex;
                   flex-direction: column;
                   align-items: center;
                   justify-content: center;
                   height: 100vh;
                   text-align: center;
                   padding: 20px;
                 }
                 .welcome-header {
                   font-size: 36px;
                   font-weight: bold;
                   margin-bottom: 20px;
                 }
                 .welcome-description {
                   font-size: 18px;
                   line-height: 1.6;
                   max-width: 800px;
                   margin-bottom: 30px;
                 }
                 .welcome-features {
                   display: flex;
                   flex-wrap: wrap;
                   justify-content: center;
                   gap: 20px;
                   margin-bottom: 30px;
                 }
                 .feature-box {
                   background-color: #34495e;
                   padding: 20px;
                   border-radius: 8px;
                   width: 250px;
                   text-align: center;
                   box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
                   transition: transform 0.3s ease, box-shadow 0.3s ease;
                 }
                 .feature-box:hover {
                   transform: scale(1.05);
                   box-shadow: 0 6px 12px rgba(0, 0, 0, 0.2);
                 }
                 .feature-title {
                   font-size: 20px;
                   font-weight: bold;
                   margin-bottom: 10px;
                 }
                 .feature-description {
                   font-size: 14px;
                   color: #bdc3c7;
                 }
                 .welcome-footer {
                   font-size: 14px;
                   color: #bdc3c7;
                   margin-top: auto;
                 }
               "))
             ),
             div(class = "welcome-container",
                 div(class = "welcome-header", "Welcome to the Railway Data Analysis App"),
                 div(class = "welcome-description",
                     "This application is designed to help you analyze historical and current trends in railway data. 
                      Using powerful visualization tools and forecasting models, you can gain valuable insights into 
                      the railway domain. Whether you're a data analyst, a researcher, or a railway enthusiast, this 
                      app has something for everyone."
                 ),
                 div(class = "welcome-features",
                     div(class = "feature-box",
                         div(class = "feature-title", "Upload Your Data"),
                         div(class = "feature-description", "Easily upload your railway datasets in CSV format.")
                     ),
                     div(class = "feature-box",
                         div(class = "feature-title", "Interactive Visualizations"),
                         div(class = "feature-description", "Generate dynamic charts and graphs for better insights.")
                     ),
                     div(class = "feature-box",
                         div(class = "feature-title", "Forecast Trends"),
                         div(class = "feature-description", "Predict future data trends using advanced forecasting models.")
                     ),
                     div(class = "feature-box",
                         div(class = "feature-title", "Export Results"),
                         div(class = "feature-description", "Download your analysis and forecasts for offline use.")
                     )
                 ),
                 div(class = "welcome-footer",
                     "© 2024 Railway Dataset Analysis. All Rights Reserved."
                 )
             )
           )
  ),
  
  # Analysis Tab
  tabPanel("Analysis",
           fluidPage(
             tags$head(
               tags$style(HTML("
                 #title {
                   color: #ecf0f1;
                   font-size: 28px;
                   font-weight: bold;
                   text-align: center;
                   padding: 20px 0;
                 }
                 .sidebar {
                   background-color: #34495e;
                   padding: 20px;
                   border-radius: 8px;
                   color: #ecf0f1;
                 }
                 .sidebar h3, .sidebar label {
                   color: #ecf0f1;
                   font-size: 18px;
                 }
                 .btn-primary {
                   background-color: #1abc9c;
                   border: none;
                   color: white;
                   padding: 10px 20px;
                   width: 100%;
                 }
                 .btn-primary:hover {
                   background-color: #16a085;
                 }
                 .tab-content {
                   background-color: #34495e;
                   padding: 20px;
                   border-radius: 8px;
                   box-shadow: 0px 4px 12px rgba(0, 0, 0, 0.1);
                 }
                 .data-table, .forecast-table { 
                   margin-top: 10px;
                   color: #ecf0f1;
                 }
                 .dataTables_wrapper {
                   background-color: #34495e;
                   border-radius: 8px;
                   color: #ecf0f1;
                 }
                 .footer {
                   text-align: center;
                   padding: 10px 0;
                   background-color: #34495e;
                   color: #ecf0f1;
                   position: fixed;
                   bottom: 0;
                   width: 100%;
                 }
               "))
             ),
             div(id = "title", "Railway Dataset Analysis with Forecasting"),
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar",
                 fileInput("file", "Upload CSV File", accept = ".csv"),
                 selectInput("chart_type", "Select Chart Type:", 
                             choices = c("Line Chart", "Histogram", "Pie Chart", "Scatter Plot", "Box Plot", "3-D Scatter Plot")),
                 uiOutput("column_selector_x"),
                 uiOutput("column_selector_y"),
                 actionButton("plot_btn", "Generate Plot", class = "btn-primary"),
                 br(),
                 selectInput("forecast_column", "Select Column for Forecasting:", choices = NULL),
                 actionButton("forecast_btn", "Forecast Next 20 Years", class = "btn-primary")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("View Data", DT::dataTableOutput("data_table")),
                   tabPanel("Visualizations", plotlyOutput("data_plot")),  
                   tabPanel("Forecast", plotOutput("forecast_plot")),
                   tabPanel("Forecast Table", DT::dataTableOutput("forecast_table"))
                 )
               )
             ),
             div(class = "footer",
                 "© 2024 Railway Dataset Analysis. All Rights Reserved."
             )
           )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    data <- read.csv(input$file$datapath)
    updateSelectInput(session, "forecast_column", choices = names(data)[-1])
    return(data)
  })
  
  output$data_table <- DT::renderDataTable({
    dataset()
  })
  
  output$column_selector_x <- renderUI({
    req(input$chart_type)
    selectInput("x_col", "X-axis Column:", choices = names(dataset()))
  })
  
  output$column_selector_y <- renderUI({
    req(input$chart_type %in% c("Line Chart", "Scatter Plot", "Box Plot", "3-D Scatter Plot"))
    selectInput("y_col", "Y-axis Column:", choices = names(dataset()))
  })
  
  output$data_plot <- renderPlotly({
    req(input$plot_btn)
    data <- dataset()
    p <- NULL
    
    if (input$chart_type == "Line Chart") {
      p <- ggplot(data, aes_string(x = input$x_col, y = input$y_col)) + 
        geom_line(color = "#3498db", size = 1.2) + 
        labs(title = "Line Chart", x = input$x_col, y = input$y_col) + 
        theme_minimal()
      
    } else if (input$chart_type == "Histogram") {
      p <- ggplot(data, aes_string(x = input$x_col)) + 
        geom_histogram(binwidth = 5, fill = "#e67e22", color = "black") + 
        labs(title = "Histogram", x = input$x_col, y = "Frequency") + 
        theme_classic()
      
    } else if (input$chart_type == "Pie Chart") {
      p <- ggplot(data, aes(x = "", y = !!sym(input$x_col), fill = factor(!!sym(input$x_col)))) + 
        geom_bar(stat = "identity", width = 1) + 
        coord_polar(theta = "y") + 
        labs(title = "Pie Chart") + 
        theme_void()
      
    } else if (input$chart_type == "Scatter Plot") {
      p <- ggplot(data, aes_string(x = input$x_col, y = input$y_col)) + 
        geom_point(color = "#9b59b6", size = 3) + 
        labs(title = "Scatter Plot", x = input$x_col, y = input$y_col) + 
        theme_light()
      
    } else if (input$chart_type == "Box Plot") {
      p <- ggplot(data, aes_string(x = input$x_col, y = input$y_col)) + 
        geom_boxplot(fill = "#f39c12", color = "black") + 
        labs(title = "Box Plot", x = input$x_col, y = input$y_col) + 
        theme_minimal()
      
    } else if (input$chart_type == "3-D Scatter Plot") {
      p <- plot_ly(data, x = ~get(input$x_col), y = ~get(input$y_col), z = ~get(input$y_col),
                   type = "scatter3d", mode = "markers", marker = list(color = "#1abc9c", size = 5)) %>%
        layout(title = "3-D Scatter Plot", scene = list(xaxis = list(title = input$x_col),
                                                        yaxis = list(title = input$y_col),
                                                        zaxis = list(title = input$y_col)))
    }
    
    p
  })
  
  forecast_data <- reactive({
    req(input$forecast_column)
    data <- dataset()
    ts_data <- ts(data[[input$forecast_column]], start = min(data$Year), frequency = 1)
    forecast(ts_data, h = 20)
  })
  
  output$forecast_plot <- renderPlot({
    req(input$forecast_btn)
    forecast_result <- forecast_data()
    autoplot(forecast_result) + 
      ggtitle("Forecast for Next 20 Years") + 
      theme_minimal()
  })
  
  output$forecast_table <- DT::renderDataTable({
    req(input$forecast_btn)
    forecast_result <- forecast_data()
    
    years <- (max(dataset()$Year) + 1):(max(dataset()$Year) + 20)
    forecast_df <- data.frame(
      Year = years,
      Forecast = forecast_result$mean,
      Lo80 = forecast_result$lower[,1],
      Hi80 = forecast_result$upper[,1],
      Lo95 = forecast_result$lower[,2],
      Hi95 = forecast_result$upper[,2]
    )
    forecast_df
  })
}

shinyApp(ui = ui, server = server)
