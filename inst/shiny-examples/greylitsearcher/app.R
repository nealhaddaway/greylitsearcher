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
source('buildGoogleLinksAdv.R')
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
                                     shinybusy::add_busy_spinner(spin = "fading-circle", color = "#bababa", margins = c(70, 20)),
                                     br(),
                                     br(),
                                     'You can use this tool to perform structured and transparent searches of websites using Google\'s sitesearch functionality.',
                                     br(),
                                     br(),
                                     'To get started, enter your websites and search terms in the "Search" tab. Next, download the pages of search results in the "Save HTMLs" tab. Finally, scrape the downloaded files in the "Scrape data" tab. You can then download a CSV file containing your search results (source, title, URL and description).',
                                     br(),
                                     br(),
                                     tags$img(src="buildGooglelinks.png", width = 700),
                                     br(),
                                     br(),
                                     'Please note that websites much be indexed by Google.com for the site: operator to work in greylitsearcher. ', tags$a(href="https://developers.google.com/search/docs/advanced/debug/search-operators/all-search-site", "Read more here."),
                                     br(),
                                     br(),
                                     'Be aware that repetitive searching may result in a temporary block from Google (your table of search results will be blank). Please use this tool responsibly.',
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
                                     shinybusy::add_busy_spinner(spin = "fading-circle", color = "#bababa", margins = c(70, 20)),
                                     br(),
                                     'Use this tab to build and check your searches.',
                                     br(),
                                     hr(),
                                     'Enter the websites you wish to search:',
                                     br(),br(),
                                     textAreaInput('websites', 'Website URLs to search (each on a new line)', placeholder = 'www.sei.org')),
                              column(7,
                                     div(
                                         style = "background-color:#f5f5f5;",
                                         p(style="color: #666; font-family: Arial,sans-serif; text-overflow: ellipsis; flex: 1 1 auto; font-size: 18px; text-align: center; padding-top: 10px; padding-bottom: 10px;",
                                           'Advanced search')),
                                     p(style = 'text-align: left;',
                                       strong('Find articles'),br(),
                                       tags$table(width = "100%",
                                                  tags$tr(width = "100%",
                                                          tags$td(width = "40%", 'with ', strong('all'), ' of the words'),
                                                          tags$td(width = "60%", textInput('and_terms', NULL, width =  "100%"))
                                                  ),
                                                  tags$tr(width = "100%",
                                                          tags$td(width = "40%", 'with the ', strong('exact phrase')),
                                                          tags$td(width = "60%", textInput('exact_phrase', NULL, width =  "100%"))
                                                  ),
                                                  tags$tr(width = "100%",
                                                          tags$td(width = "40%", 'with ', strong('at least one'), 'of the words'),
                                                          tags$td(width = "60%", textInput('or_terms', NULL, width =  "100%"))
                                                  ),
                                                  tags$tr(width = "100%",
                                                          tags$td(width = "40%", strong('without'), ' the words'),
                                                          tags$td(width = "60%", textInput('not_terms', NULL, width =  "100%"))
                                                  ),
                                                  tags$tr(width = "100%",
                                                          tags$td(width = "40%", 'return records ', strong('dated'), ' between'),
                                                          tags$td(width = "60%",
                                                                  splitLayout(textInput('date_from', NULL, width =  "100%", placeholder = "YYYY-MM-DD"), p(style = 'text-align: center;',' - '),
                                                                              textInput('date_to', NULL, width =  "100%", placeholder = "YYYY-MM-DD"),
                                                                              cellWidths = c('45%', '5%', '45%')))
                                                  ),
                                                  tags$tr(width = "100%",
                                                          tags$td(width = "40%", 'pages of results'),
                                                          tags$td(width = "60%", numericInput('pages', NULL, width =  "100%", value = 1, max = 100, step = 1))
                                                  )
                                                  ),
                                       br(),
                                       actionButton("google", "Build search links", icon("search"), class = 'btn-primary')
                                     ),
                                     ),
                              column(12,
                                     hr(),
                                     uiOutput('preview_table')
                                     )
                          )
                 ),

                 tabPanel("Save HTMLs",
                          fluidRow(
                              column(10,
                                     h2('Download your search results as HTML files'),
                                     shinybusy::add_busy_spinner(spin = "fading-circle", color = "#bababa", margins = c(70, 20)),
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
                                     shinybusy::add_busy_spinner(spin = "fading-circle", color = "#bababa", margins = c(70, 20)),
                                     br(),
                                     'Now, we can scrape search results based on patterns in the HTML code.',
                                     hr(),
                                     conditionalPanel(
                                         condition='input.download_HTMLs!=null && input.download_HTMLs!=""',
                                         actionButton("scrape_HTMLs", "Scrape the results HTMLs"),
                                         br(),
                                         br(),
                                         conditionalPanel(
                                             condition='input.scrape_HTMLs!=null && input.scrape_HTMLs!=""',
                                             downloadButton('downloadData', 'Download results as CSV', icon = icon("file-download")),
                                             br(),
                                             br(),
                                             downloadButton('downloadReport', 'Download search report', icon = icon('file-download'))
                                             )
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

        if (identical(input$websites, '')==TRUE){
        } else {

            rv$pages <- as.numeric(input$pages)

            rv$link <- buildGoogleLinksAdv(
                     site = input$websites,
                     and_terms = input$and_terms,
                     exact_phrase = input$exact_phrase,
                     or_terms = input$or_terms,
                     not_terms = input$not_terms,
                     date_from = input$date_from,
                     date_to = input$date_to,
                     pages = input$pages)

            rv$links <- rv$link$link
            rv$report <- rv$link$report

            #show preview of links
            output$preview <- renderDataTable({
                table <- rv$links
                table$link <- paste0("<a href='",table$link,"'>",table$link,"</a>")
                table
            }, escape = FALSE)

            #render preview UI
            output$preview_table <- renderUI({
                tagList(
                    "If you're happy with these links, proceed to the 'Save HTMLs' tab to save your search results pages",
                    br(),
                    br(),
                    dataTableOutput('preview')
                )
            })
        }

    })

    #prepare HTML files for scraping
    observeEvent(input$download_HTMLs, {

        withCallingHandlers({
            shinyjs::html("text", "")

            htmls <- list()
            for(i in 1:length(rv$links$link)){
                html <- save_html(rv$links$link[i], pause = 0.5, backoff = FALSE)
                htmls <- c(htmls, html)
            }
            rv$htmls <- htmls

        },
        message = function(m) {
            shinyjs::html(id = "text", html = m$message, add = TRUE)})

        output$save_report <- renderText({
            paste0(length(rv$htmls),' pages of results successfully downloaded. Proceed to the "Scrape data" tab to extract search results and download the final dataset.')
        })

    })

    #scrape HTML files
    observeEvent(input$scrape_HTMLs, {

        #loop through downloaded htmls and extract data
        df <- data.frame()
        for(i in 1:length(rv$htmls)){
            data <- get_info(unlist(rv$htmls[i]))
            df <- dplyr::bind_rows(df, data)
        }
        df <- df[!duplicated(df), ]
        rv$data <- df

        #render data table
        output$data <- renderDataTable({
            table <- rv$data
            table$link <- paste0("<a href='",table$link,"'>",table$link,"</a>")
            table$source <- paste0("<a href='",table$source,"'>",table$source,"</a>")
            table
        }, escape = FALSE)

        #visual report text
        output$scrape_report <- renderText({
            paste0('A total of ', nrow(rv$data),' search results have been exported and are shown in the table below (you may see than 10 results per page if Google adverts took their place).')
        })

        #build download handler for CSV data
        output$downloadData <- downloadHandler(
            filename = function() {
                paste("results.csv", sep = "")
            },
            content = function(file) {
                write.csv(rv$data, file, row.names = FALSE)}
        )

        #build download hanlder for report text file
        output$downloadReport <- downloadHandler(
            filename = function() {
                paste("search_record.txt", sep = "")
            },
            content = function(file) {
                writeLines(rv$report, file)}
        )

    })

}

# Run the application
shinyApp(ui = ui, server = server)
