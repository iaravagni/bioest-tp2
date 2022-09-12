#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(ggpubr)
source("functions.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Analisis exploratorio", tabName = "visualization", icon = icon("search")),
    #menuItem("Preprocesamiento", tabName = "preprocessing", icon = icon("table")),
    menuItem("Normalidad", tabName = "normal", icon = icon("poll"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "visualization",
              fluidRow(
                sidebarPanel(
                fileInput("file1", "Escoger Archivo CSV",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                tags$hr(),
                
                # Input: Checkbox if file has header ----
                checkboxInput("header", "Header", TRUE),
                
                # Input: Select separator ----
                radioButtons("sep", "Separador",
                             choices = c(Coma = ",",
                                         PuntoYcoma = ";",
                                         Tab = "\t"),
                             selected = ";"),
                tags$hr(),
                
                # Input: Select number of rows to display ----
                radioButtons("disp", "Display",
                             choices = c(Head = "head",
                                         Todo = "all"),
                             selected = "head")),
                mainPanel(width = 12,
                  
                  # Output: Data file ----
                  tabsetPanel(id='tables',
                  tabPanel("Datos",DT::dataTableOutput("contents")),
                  tabPanel("Tipos de datos",DT::dataTableOutput("dataTypes")),
                  tabPanel("Datos Faltantes",DT::dataTableOutput("missing")))
                  # uiOutput("out")))
                  
                )
                #uiOutput("out"),
                
                
              )),
      tabItem(
        tabName = "preprocessing",
        sidebarPanel(
          selectInput("select_col1","Seleccionar una columna",choices = ""),
          actionButton("do", "Convertir a Factor"))
        
      ),
      tabItem(
        tabName = "normal",
              fluidRow(
                
                  sidebarPanel(
                    selectInput("select_col2","Seleccionar una columna",choices = ""),
                    sliderInput("bins",
                                "Number of bins:",
                                min = 1,
                                max = 50,
                                value = 30)
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    plotOutput("histograma"),
                    plotOutput("boxplot"),
                    plotOutput("qq_plot"),
                    verbatimTextOutput("shapiro_wilk"),
                    verbatimTextOutput("kolgomorov")
                  )
              )
        )
  )
)
)

server <- function(input, output,session) {
  #x = reactiveVal()
  x <- reactive({
    req(input$file1)
    tryCatch(
      {
        x = read_file(input$file1$datapath,input$header,input$sep)
        updateSelectInput(session,"select_col1",choices=names(x))
        updateSelectInput(session,"select_col2",choices=names(x))
        x
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

       #return(df)
    # }

  })
  

  output$contents <- DT::renderDataTable({
    req(input$file1)
    
    df <- x()
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }})
  
    
   
  
  output$missing <- DT::renderDataTable({
    req(input$file1)
    tryCatch(
          {
            na_count = na_counts(x())
            
            return(na_count)

          },
          error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
            }
          )
    
   
  })
  
output$dataTypes = DT::renderDataTable({
  req(input$file1)
  tryCatch(
    {
      data_types <- data_type(x())
      return(data_types)
      
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  
})

observeEvent(input$do,
             {
               req(input$select_col1)
               df = convert_to_factor(x(),input$select_col1)
               x(reactive({df}))
               session$sendCustomMessage(type = 'testmessage',
                                         message = 'Thank you for clicking')
             })
output$histograma = renderPlot({
  req(input$select_col2)
  histogram(x(),input$select_col2,input$bins)
})

output$boxplot = renderPlot({
   req(input$select_col2)
   my_boxplot(x(), input$select_col2)
  
 })
output$qq_plot = renderPlot({
  req(input$select_col2)
  my_qqplot(x(),input$select_col2)
})
}


# Run the application 
shinyApp(ui = ui, server = server)
