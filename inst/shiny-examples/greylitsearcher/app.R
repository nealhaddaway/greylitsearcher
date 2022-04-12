library(shiny)
library(magrittr)
library(zip)
library(RCurl)
library(shinybusy)
library(cfhttr)
library(purrr)
library(dplyr)
library(shinyjs)

source('buildLinks_google.R')
source('save_htmls.R')
source('scrape_google.R')

# Define UI for application that draws a histogram
ui <- navbarPage("greylitsearcher", id = "tabs",

                 tabPanel("Home",
                          fluidRow(
                              column(10,
                                     h2('greylitsearcher'),
                                     br(),
                                     'Welcome to greylitsearcher, a web-based tool for performing systematic and transparent searches of organisational websites.',
                                     #shinybusy::add_busy_spinner(spin = "fading-circle", color = "#bababa", margins = c(70, 20))
                                     br(),
                                     br(),
                                     'You can use this tool to perform structured and transparent searches of websites using Google\'s sitesearch functionality.',
                                     br(),
                                     br(),
                                     'To get started, enter your websites and search terms in the "Search" tab. Next, download the pages of search results in the "Save HTMLs" tab. Finally, scrape the downloaded files in the "Scrape data" tab. You can then download a CSV file containing your search results (source, title, URL and description).',
                                     br(),
                                     br(),
                                     'Be aware that repetitive searching may result in a temporary block from Google (your table of search results will be blank). Please use this tool responsibly.',
                                     br(),
                                     br(),
                                     'At present, terms can only be combined additively (i.e. term1 AND term2). Capabilities will be expanded shortly. Thanks for your interest!',
                                     br(),
                                     br(),
                                     hr(),
                                     br(),
                                     'greylitsearcher was produced by Neal Haddaway.', tags$a(href="https://github.com/nealhaddaway/greylitsearcher/", 'The code is available on Github'), '.',
                                     br(),
                                     br(),
                                     'Please cite as: Haddaway, NR (2022) greylitsearcher: An R package and Shiny app for systematic and transparent searching for grey literature. Zenodo. ', tags$a(href="10.5281/zenodo.6451616", "10.5281/zenodo.6451616")
                                     ),
                              column(2,
                                     br(),tags$img(height = 150, src = "https://raw.githubusercontent.com/nealhaddaway/greylitsearcher/main/inst/shiny-examples/greylitsearcher/www/hex.png"))
                              )
                          ),

                 tabPanel("Search",
                          fluidRow(
                              column(10,
                                     h2('Build and check your searches'),
                                     br(),
                                     'Use this tab to build and check your searches.',
                                     br(),
                                     hr(),
                                     'Enter the websites you wish to search:',
                                     br(),br(),
                                     textAreaInput('websites', 'Websites to search - starting with "www" (each on a new line)', placeholder = 'www.wwf.org'),
                                     textAreaInput('search', 'Words to search (separated by a space)', placeholder = 'climate sea level'),
                                     textInput('pages', 'Number of pages of results', placeholder = 10),
                                     hr(),
                                     'Select your search engine:',
                                     br(),br()
                                     ),
                              column(5,
                                     splitLayout(
                                                 actionButton("google", "Google", class = "btn-primary")
                                                 )
                                     ),
                              column(12,
                                     br(),
                                     uiOutput('preview_table')
                                     )
                          )
                 ),

                 tabPanel("Save HTMLs",
                          fluidRow(
                              column(10,
                                     h2('Download your search results as HTML files'),
                                     br(),
                                     'Here, You can save each page of search results for scraping in the next step.',
                                     hr(),
                                     conditionalPanel(
                                         condition='input.google!=null && input.google!=""',
                                         actionButton("download_HTMLs", "Save search results")
                                         ),
                                     br(),
                                     br(),
                                     shinyjs::useShinyjs(),
                                     textOutput("text"),
                                     br(),
                                     br(),
                                     textOutput('save_report')
                                     )
                          )
                 ),

                 tabPanel("Scrape data",
                          fluidRow(
                              column(10,
                                     h2('Scrape data from the downloaded search results'),
                                     br(),
                                     'Now, we can scrape search results based on patterns in the HTML code.',
                                     hr(),
                                     conditionalPanel(
                                         condition='input.download_HTMLs!=null && input.download_HTMLs!=""',
                                         actionButton("scrape_HTMLs", "Scrape the results HTMLs"),
                                         br(),
                                         conditionalPanel(
                                             condition='input.scrape_HTMLs!=null && input.scrape_HTMLs!=""',
                                             downloadButton('downloadData', 'Download results as CSV', icon = icon("file-download")))
                                         ),
                                     br(),
                                     br(),
                                     textOutput('scrape_report'),
                                     br(),
                                     dataTableOutput('data'),
                                     br()
                              )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    rv <- reactiveValues()

    #on pressing google, generate preview
    observeEvent(input$google, {

        if (any(identical(input$websites, ''), identical(input$search, ''), identical(input$pages, ''))==TRUE){
        } else {

            rv$sites <- input$websites
            rv$search <- input$search
            rv$pages <- as.numeric(input$pages)

            rv$links <- buildLinks_google(rv$search,
                                          rv$sites,
                                          rv$pages)

            rv$links <- cbind(rv$links, link_num=paste0('link', seq(1, nrow(rv$links), 1)))

            #show preview of links
            output$preview <- renderDataTable({
                rv$links
            }, escape = FALSE)

            #render preview UI
            output$preview_table <- renderUI({
                tagList(
                    "If you're happy with these links, proceed to the 'Results' tab to see your search results",
                    br(),
                    dataTableOutput('preview')
                )
            })
        }

    })

    #prepare HTML files for scraping
    observeEvent(input$download_HTMLs, {

        if (any(identical(input$websites, ''), identical(input$search, ''), identical(input$pages, ''))==TRUE){
        } else {
            withCallingHandlers({
                shinyjs::html("text", "")

                htmls <- list()
                for(i in 1:length(rv$links[,4])){
                    html <- save_html((rv$links[,4])[i], pause = 0.5, backoff = FALSE)
                    htmls <- c(htmls, html)
                }
                rv$htmls <- htmls
                print(paste0(length(rv$htmls), ' files downloaded.'))

            },
            message = function(m) {
                shinyjs::html(id = "text", html = m$message, add = TRUE)})

            output$save_report <- renderText({
                paste0(length(rv$htmls),' pages of results successfully downloaded. Proceed to the "Scrape data" tab to extract search results and download the final dataset.')
            })
        }
    })

    #scrape HTML files
    observeEvent(input$scrape_HTMLs, {

        if (any(identical(input$websites, ''), identical(input$search, ''), identical(input$pages, ''))==TRUE){
        } else {

            df <- data.frame()
            for(i in 1:length(rv$htmls)){
                data <- get_info(unlist(rv$htmls[i]))
                print(data)
                df <- dplyr::bind_rows(df, data)
            }
            df <- df[!duplicated(df), ]
            rv$data <- df

            output$data <- renderDataTable({
                rv$data
            })

            output$save_report <- renderText({
                paste0('A total of ', nrow(rv$data),' search results have been exported and are shown in the table below.')
            })

            output$downloadData <- downloadHandler(
                filename = function() {
                    paste("results.csv", sep = "")
                },
                content = function(file) {
                    write.csv(rv$data, file, row.names = FALSE)}
                )

        }
    })

}

# Run the application
shinyApp(ui = ui, server = server)
