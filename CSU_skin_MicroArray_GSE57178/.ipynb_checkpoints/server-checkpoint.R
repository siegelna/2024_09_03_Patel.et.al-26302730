# Function to check if a package is installed, and install it if not
install_if_missing <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
  }
}

# List of packages to check and install if missing
packages_to_check <- c("shiny", "dplyr", "ggplot2", "ggpmisc", "shinyjs", "plotly", "DT", "openxlsx", "RColorBrewer")

# Apply the install_if_missing function to each package in the list
# invisible(sapply(packages_to_check, install_if_missing))
sapply(packages_to_check, install_if_missing)

library(shiny)  
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(shinyjs)
library(plotly)
library(DT)
library(dplyr)
library(openxlsx)
library(RColorBrewer)

expression.mat <- readRDS("expression.rds")
meta.df <- readRDS("metadata.rds")

server <- function(input, output, session) {
  
  output$gene <- renderUI({
    selectInput("gene", "Select Gene to Display", choices = rownames(expression.mat))
  })
  
  output$group <- renderUI({
    selectInput("group", "Grouping Variable", choices = "Stim")
  })
  
#   output$group2 <- renderUI({
#     selectInput("group2", "Select Time Point", choices = unique(meta.df$Time_Point))
#   })
  
#   output$additional_group_select <- renderUI({
#     selectizeInput("additional_group", "Subset Grouping Variable", choices = NULL, multiple = TRUE)
#   })
  
#   observeEvent(input$facet, {
#     if (!is.null(input$facet)) {
#       group_values <- unique(meta.df[[input$facet]])
#       updateSelectizeInput(session, "additional_group", choices = group_values, server = TRUE)
#     }
#   })

#   output$facet <- renderUI({
#     if(input$plotType == "Distribution Plot") {
#       choices <- "Stim"
#       selectInput("facet", "Additional Grouping Variable", choices = choices, selected = "Stim")
#     }
#   })
  
  output$y_axis <- renderUI({
    selectInput("y_axis", "Select Y-Axis Column", choices = c("Stim",  setdiff(colnames(meta.df), "Sample")))
  })

  observe({
    if (!is.null(input$group)) {
      updateSelectInput(session, "y_axis", selected = "Stim")
    }
  })
  
  scatter.plot <- reactive({
    req(input$gene, input$y_axis)
    scatter.plot <- NULL
    if (!is.null(input$gene)) {
      gene.idx <- which(rownames(expression.mat) == input$gene)
      plot.df <- suppressWarnings(
        meta.df %>%
          left_join(data.frame(Sample = colnames(expression.mat), value = expression.mat[gene.idx, ]), by = c("Sample" = "Sample"))
      )
      scatter.plot <- plot_ly(data = plot.df,
                              type = 'scatter',
                              mode = "markers",
                              color = ~value,
                              x = ~value,
                              y = plot.df[[input$y_axis]], 
                              showlegend = FALSE,
                              colors = colorRamp(c("lightgray", "darkred")),
                              marker = list(size = 3))
    }
    return(scatter.plot)
  })

  distribution.plot <- reactive({
    req(input$gene, input$group)  # Require these inputs
    
    gene.idx <- which(rownames(expression.mat) == input$gene)
    plot.df <- meta.df %>%
      left_join(data.frame(Sample = colnames(expression.mat), value = expression.mat[gene.idx, ]), by = c("Sample" = "Sample"))
    
    # plot.df <- plot.df %>%
    #   mutate_at(vars(input$facet, input$group), factor) %>%
    #   arrange(.data[[input$facet]])
    
    # Filter for selected additional grouping variables
    # if (!is.null(input$additional_group)) {
    #   plot.df <- plot.df %>% filter(.data[[input$facet]] %in% input$additional_group)
    # }
    
    # Filter for the desired Time_Point value
    # desired_time_point <- input$group2
    # plot.df_filtered <- plot.df %>%
    #   filter(Time_Point == desired_time_point)
    
    # Create the distribution plot for the filtered data
    palette <- rev(brewer.pal(n = length(unique(plot.df$Stim)), name = "Set1"))
    distribution.plot_filtered <- ggplot(plot.df, aes(x = Stim, y = value, fill = Stim)) +
      geom_boxplot(color = "black") +  
      facet_grid(. ~ Stim, scales = "free_x") +  
      labs(y = "Expression (RMA)", title = paste(input$gene)) +
      theme_minimal() +
      theme(legend.position = "top", axis.text.x = element_text(angle = 35, hjust = 1)) +
      theme(axis.title.x = element_blank()) +
      theme(axis.text.x = element_text(size = 10),  axis.text.y = element_text(size = 10)) +
      scale_fill_manual(values = palette)
    
    return(distribution.plot_filtered)
  })

  output$out.plot_plotly <- renderPlotly({
    if(input$plotType == "Scatter Plot"){
      scatter.plot()
    } else {
    #   req(input$gene, input$facet)
      distribution.plot()
    }
  })
  
  output$out_plot_table <- renderDT({
    req(input$plotType)
    if(input$plotType == "Distribution Plot"){
    #   req(input$gene, input$facet)
      distribution.plot()$data %>%
        arrange(Stim) %>%
        mutate(value = round(value, 3)) %>%
        select(-c(Sample, Time_Point))
    } else {
      NULL
    }
  }, options = list(scrollX = TRUE, scrollY = TRUE, paging = FALSE))
  
output$download_data <- downloadHandler(
  filename = function() {
    paste("data", ".xlsx", sep = "")
  },
  content = function(file) {
    if(input$plotType == "Distribution Plot") {
      write.xlsx(distribution.plot()$data, file, rowNames = FALSE, sheetName = input$group2)
    } else {
      write.xlsx(scatter.plot()$data, file, rowNames = FALSE)
    }
  }
)
  
  observeEvent(c(input$group, input$plotType), {
    req(input$group)
    if (input$plotType == "Distribution Plot") {
      hide("out.plot_plotly")
      show("out_plot_table")
    } else {
      hide("out_plot_table")
      show("out.plot_plotly")
    }
  })
}
