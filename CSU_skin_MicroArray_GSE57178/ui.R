# ui.R

ui <- fluidPage(
  titlePanel("CIU Skin RNA-seq"),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(
          HTML(
            ".multicol {
                  -webkit-column-count: 3; /* Chrome, Safari, Opera */
                  -moz-column-count: 3; /* Firefox */
                  column-count: 3;
            }"
          )
        ),
        tags$style(
          type = "text/css",
          "#loadmessage {
                  position: fixed;
                  top: 0px;
                  left: 0px;
                  width: 100%;
                  padding: 5px 0px 5px 0px;
                  text-align: center;
                  font-weight: bold;
                  font-size: 100%;
                  color: #000000;
                  background-color: #CCFF66;
                  z-index: 105;
          }"
        ),
        tags$style(
          type = "text/css",
          ".shiny-output-error { 
              visibility: hidden; 
          }",
          ".shiny-output-error:before { 
              visibility: hidden; 
          }"
        ),
        tags$script("
          $(document).ready(function(){
            $('select[multiple]').on('click', 'option', function (e) {
              $(this).prop('selected', !$(this).prop('selected'));
              e.stopPropagation();
            });
          });
        ")
      ),
      conditionalPanel(
        condition = "$('html').hasClass('shiny-busy')",
        tags$div("In Progress...", id = "loadmessage")
      ),
      selectInput("plotType", "Plot Type", choices = c("Distribution Plot", "Scatter Plot")),
      uiOutput("gene"),
      uiOutput("group"),
    #   uiOutput("group2"),  # New UI element for Grouping Variable 2
      uiOutput("facet"),
      uiOutput("fill"),
      conditionalPanel(
        condition = "input.plotType == 'Scatter Plot'",
        uiOutput("y_axis")
      )
    ),
    mainPanel(
      fluidRow(
        column(width = 8, plotly::plotlyOutput("out.plot_plotly", height = "400px")),
        column(width = 4, dataTableOutput("out_plot_table"))  # Display the table
      ),
      downloadButton("download_data", "Download Data")
    )
  )
)
