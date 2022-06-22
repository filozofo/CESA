library(shiny)
library(readr)
options(encoding="UTF-8")
# Define UI for application that draws a histogram
source('www/source.R')
shinyUI(fluidPage(
    tags$style("@import url(https://use.fontawesome.com/releases/v6.0.0/css/all.css);"),
    tags$style(
        ".p1,.p2,.p3 {
      color: #3b6089;
    }"
    ),
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            
            p(class = "p1","Please paste your data below, which contains one sheet of three colums, ",tags$b('days,'),tags$b('group1,'),tags$b('group2.')),
            tags$br(),
            p(class = "p2","You can use customed header, and the second column name will be the file name of download."),
            p(class = "p3","You may run the sample file as shown in text box."),
            actionButton(inputId = "Run",label = "Run",width="90%",icon=icon("person-running",verify_fa = FALSE)),
            tags$br(),
            tags$br(),
            downloadButton("downspss",label='Download SPSS',width="30%"),
            downloadButton("downtable",label='Download Table',width="30%"),
            downloadButton("downplot",label='Download Plot',width="30%"),
            tags$br(),
            textAreaInput("file5", "", value=Text,width="90%",height = "400px",
                          cols=3,rows = 100,resize = "both",placeholder=Text),
            tags$div(
                tags$h3('Author'),
                tags$h5('Chongyang Wang'),
                tags$h5("Email: complete@skiff.com"),
                tags$a(href="https://github.com/filozofo/CESA", "Github Repo"))
        ),
        mainPanel(
            tags$h1(tags$em("Caenorhabditis elegans"),"survival analysis"),
            tags$hr(),
            tableOutput(outputId='table1'),
            tags$p(tags$b('meanse:'),"mean ± SEM, ", tags$b('pv:'),"p-value, ", tags$b('PLC:'),"percent life span change, ", tags$b('N:'),"number of each group"),
            plotOutput("plot1",
                       width = "50%",
                       height = "500px"),
            hr(),
            div(
              class = "footer",
              includeHTML("www/icp.html")
            )
            
            
))))