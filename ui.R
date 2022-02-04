library(shiny)
options(encoding="UTF-8")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            p("Please upload single excel (.xlsx) file, whcih the contains one sheet of three colums, ",tags$b('days'),', ' ,tags$b('group1'),', ' ,tags$b('group2'),"."),
            tags$br(),
            p("You can use customed header, and the second column name will be the file name of download."),
            p("You may run and download the sample file."),
            actionButton(inputId = "Runsample",label = "Run sample"),
            downloadButton("Downsample",label='Download sample'),
            tags$br(),tags$br(),
            fileInput("file1", 'Please upload XLSX format file', buttonLabel = "Upload...", accept=c(".xlsx"),multiple = F),

            downloadButton("downtable",label='Download Table'),
            downloadButton("downplot",label='Download Plot'),
            tags$br(),tags$br(),
            tags$div(
                tags$h3('Author'),
                tags$h5('Chongyang Wang'),
                tags$h5("Email: aubot@hotmail.com"),
                tags$a(href="https://github.com/sajesi/CESA", "github shiny repo"))
        ),
        mainPanel(
            tags$h1(tags$em("Caenorhabditis elegans"),"survival analysis"),
            tags$hr(),
            tableOutput(outputId='table1'),
            tags$p(tags$b('meanse'),": mean ± SEM, ", tags$b('pv'),": p-value, ", tags$b('PLC'),": percent life span change, ", tags$b('N'),": number of each group"),
            plotOutput("plot1",
                       width = "30%",
                       height = "300px")
            )
    )
))
