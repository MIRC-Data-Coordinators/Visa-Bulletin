library(shiny)
library(plotly)

# Define the UI
ui <- fluidPage(
  titlePanel("VISAbulliten"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Select a variable to plot:", choices = names(df_split[["F1"]][["2018"]])[5:14]), #5-14 column names in dropdown
      selectizeInput("groups", "Select groups:", choices = names(df_split), selected = names(df_split), multiple = TRUE),
      selectInput("x_var", "Select a variable for the x-axis:", choices = c("Month", "Year", "Month-Year")),
      dateRangeInput("date_range", "Select date range:", start = "2018-01-01", end = Sys.Date())
    ),
    
    mainPanel(
      uiOutput("plots")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  var_name <- reactive({ input$var })
  x_var_name <- reactive({ input$x_var })
  date_range <- reactive({ input$date_range })
  
  output$plots <- renderUI({
    selected_groups <- input$groups
    
    plot_output_list <- lapply(selected_groups, function(group_name) {
      plotlyOutput(paste0("plot_", group_name))
    })
    
    plot_output_list <- lapply(plot_output_list, function(x) {
      div(style = "margin-bottom: 20px;", x)
    })
    
    do.call(tagList, plot_output_list)
  })
  
  lapply(names(df_split), function(group_name) {
    output[[paste0("plot_", group_name)]] <- renderPlotly({
      if (group_name %in% input$groups) {
        df_combined <- do.call(rbind, df_split[[group_name]])
        
        # Filter data based on date range
        df_combined <- df_combined %>%
          filter(as.Date(paste(Year, Month, "01", sep = "-")) >= date_range()[1] &
                   as.Date(paste(Year, Month, "01", sep = "-")) <= date_range()[2])
        
        if (x_var_name() == "Month-Year") {
          df_combined$Month_Year <- paste(df_combined$Year, df_combined$Month, sep = "-")
          fig <- df_combined %>% plot_ly(type = 'scatter', mode = 'lines+markers', color = ~Year) %>%
            add_trace(x = ~Month_Year, y = as.formula(paste0("~`", var_name(), "`"))) %>%
            layout(
              title = paste("Plot for", group_name),
              showlegend = TRUE,
              xaxis = list(title = "Month-Year"),
              yaxis = list(title = var_name())
            )
        } else if (x_var_name() == "Month") {
          fig <- df_combined %>% plot_ly(type = 'scatter', mode = 'lines+markers', color = ~Year) %>%
            add_trace(x = as.formula(paste0("~`", x_var_name(), "`")), y = as.formula(paste0("~`", var_name(), "`"))) %>%
            layout(
              title = paste("Plot for", group_name),
              showlegend = TRUE,
              xaxis = list(title = x_var_name(), dtick = 1, tickvals = 1:12),
              yaxis = list(title = var_name())
            )
        } else {
          fig <- df_combined %>% group_by(Year) %>% summarise(y = mean(get(var_name()))) %>%
            plot_ly(type = 'scatter', mode = 'lines+markers') %>%
            add_trace(x = ~Year, y = ~y) %>%
            layout(
              title = paste("Yearly Plot for", group_name),
              showlegend = FALSE,
              xaxis = list(title = "Year"),
              yaxis = list(title = var_name())
            )
        }
        
        fig
      }
    })
  })
}


# Run the app
shinyApp(ui = ui, server = server)



