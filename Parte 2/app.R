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
    menuItem("Preprocesamiento", tabName = "preprocessing", icon = icon("table")),
    menuItem("Normalidad", tabName = "normal", icon = icon("poll")),
    menuItem("Homocedasticidad",tabName="homoced",icon=icon('bar-chart')),
    menuItem("Tests",tabName="tests",icon=icon('poll'))
    
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
        tabName = "tests",
        sidebarPanel(
          selectInput("select_col_test","Seleccionar la columna de variable continua",choices = ""),
          selectInput("group_col_test","Seleccionar columna de variable dicotomica",choices=""),
          
          ),
        mainPanel(verbatimTextOutput("test_out"))
        
      ),
      tabItem(
        tabName = "homoced",
        sidebarPanel(
          selectInput("select_col_homoced","Seleccionar la columna de variable continua",choices = ""),
          selectInput("group_col_homoced","Seleccionar columna de variable dicotomica",choices=""),
        ),
        mainPanel(verbatimTextOutput("homoced_out"))
        
      ),
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
                    selectInput("select_col2","Variable para analizar normalidad",choices = ""),
                    selectInput("group_col","Variable para agrupar",choices = ""),
                    sliderInput("bins",
                                "Cantindad de bines",
                                min = 1,
                                max = 50,
                                value = 30)
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    plotOutput("histograma"),
                    plotOutput("boxplot"),
                    plotOutput("qq_plot"),
                    verbatimTextOutput(outputId="shapiro_wilk"),
                    verbatimTextOutput("kolgomorov")
                  )
              )
        )
  )
)
)

server <- function(input, output,session) {
  #x = reactiveVal()
  values <- reactiveValues(df_data = NULL)
  
  observeEvent(input$file1, {
    values$df = read_file(input$file1$datapath,input$header,input$sep)
    
    cols = names(values$df)[sapply(values$df, is.numeric)]
    updateSelectInput(session,"select_col1",choices=cols)
    updateSelectInput(session,"select_col2",choices=cols)
    updateSelectInput(session,"select_col_test",choices=cols)
    updateSelectInput(session,"select_col_homoced",choices=cols)
    updateSelectInput(session,"group_col",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
    updateSelectInput(session,"group_col_test",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
    updateSelectInput(session,"group_col_homoced",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    
    
  })
  observeEvent(input$sep, {
    req(input$file1)
    values$df = read_file(input$file1$datapath,input$header,input$sep)
    cols = names(values$df)[sapply(values$df, is.numeric)]
    updateSelectInput(session,"select_col1",choices=cols)
    updateSelectInput(session,"select_col2",choices=cols)
    updateSelectInput(session,"select_col_test",choices=cols)
    updateSelectInput(session,"group_col",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
    updateSelectInput(session,"group_col_test",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
    updateSelectInput(session,"group_col_homoced",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    
  })
  observeEvent(input$header, {
    req(input$file1)
    values$df = read_file(input$file1$datapath,input$header,input$sep)
    cols = names(values$df)[sapply(values$df, is.numeric)]
    updateSelectInput(session,"select_col1",choices=cols)
    updateSelectInput(session,"select_col2",choices=cols)
    updateSelectInput(session,"select_col_test",choices=cols)
    updateSelectInput(session,"group_col",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
    updateSelectInput(session,"group_col_test",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
    updateSelectInput(session,"group_col_homoced",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
    
    
  })
  # x <- reactive({
  #   (input$file1)
  #   tryCatch(
  #     {
  #       x = read_file(input$file1$datapath,input$header,input$sep)
  #       updateSelectInput(session,"select_col1",choices=names(x))
  #       updateSelectInput(session,"select_col2",choices=names(x))
  #       x
  #       
  #     },
  #     error = function(e) {
  #       # return a safeError if a parsing error occurs
  #       stop(safeError(e))
  #     }
  #   )

       #return(df)
    # }

  # })
  # rv  = reactiveVal(x)
  
  # RA_s <- reactiveValues()
  # observe(RA_s$ra <- x())
  
  output$contents <- DT::renderDataTable({
    req(input$file1)
    
    df <- values$df
    
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
            na_count = na_counts(values$df)
            
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
      data_types <- data_type(values$df)
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
              values$df <-  convert_to_factor( values$df,input$select_col1)
              cols = names(values$df)[sapply(values$df, is.numeric)]
              updateSelectInput(session,"select_col1",choices=cols)
              updateSelectInput(session,"select_col2",choices=cols)
              updateSelectInput(session,"select_col_test",choices=cols)
              updateSelectInput(session,"select_col_homoced",choices=cols)
              updateSelectInput(session,"group_col",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
              updateSelectInput(session,"group_col_test",choices=paste(c(names(values$df)[sapply(values$df, is.factor)],"No agrupar")))
              updateSelectInput(session,"group_col_homoced",choices=paste(names(values$df)[sapply(values$df, is.factor)]))
              
              
               
             })
output$histograma = renderPlot({
  req(input$select_col2)
  req(input$group_col)
  histogram(values$df,input$group_col,input$select_col2,input$bins)
})

output$boxplot = renderPlot({
   req(input$select_col2)
   req(input$group_col)
   my_boxplot(values$df, input$select_col2,input$group_col)
  
 })
output$qq_plot = renderPlot({
  req(input$select_col2)
  req(input$group_col)
  my_qqplot(values$df,input$select_col2,input$group_col)
})


values$col = reactive({
  req(input$select_col2)
  shapiro_w(values$df,input$select_col2)
})

output$shapiro_wilk = renderText({
  req(input$select_col2)
  req(input$group_col)
  paste0(capture.output(shapiro_w(values$df,input$select_col2,input$group_col)),collapse = '\n')
 })

output$kolgomorov <- renderText({ 
  req(input$select_col2)
  req(input$group_col)
  paste0(capture.output(lilliefors(values$df,input$select_col2,input$group_col)),collapse = '\n')  
})

output$test_out = renderText({
  req(input$select_col2)
  req(input$group_col)
  paste0(capture.output(my_test(values$df,input$select_col2,input$group_col)),collapse = '\n')
  
})
output$homoced_out = renderText({
  req(input$select_col2)
  req(input$group_col_homoced)
  paste0(capture.output(levene(values$df,input$select_col2,input$group_col)),collapse = '\n')
})
}

# Run the application 
shinyApp(ui = ui, server = server)
