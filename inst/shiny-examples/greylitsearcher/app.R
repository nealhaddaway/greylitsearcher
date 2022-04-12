library(shiny)
library(magrittr)
library(zip)
library(RCurl)
library(shinybusy)
library(cfhttr)
library(purrr)
library(dplyr)

source('buildLinks_google.R')
source('save_htmls.R')
source('scrape_google.R')

# Define UI for application that draws a histogram
ui <- navbarPage("greylitsearcher", id = "tabs",

                 tabPanel("Home",
                          fluidRow(
                              column(10,
                                     h2('GreyLitSearcher'),
                                     br(),
                                     'Welcome to GreyLitSearcher, a web-based tool for performing systematic and transparent searches of organisational websites.',
                                     shinybusy::add_busy_spinner(spin = "fading-circle", color = "#bababa", margins = c(70, 20))
                                     )
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
                                     textAreaInput('websites', 'Websites to search - starting with "www" (each on a new line)'),
                                     textAreaInput('search', 'Words to search (separated by a space)'),
                                     textInput('pages', 'Number of pages of results'),
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
                                     actionButton("download_HTMLs", "Save search results"),
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
                                     actionButton("scrape_HTMLs", "Scrape the results HTMLs"),
                                     br(),
                                     br(),
                                     textOutput('scrape_report'),
                                     br(),
                                     dataTableOutput('data'),
                                     br(),
                                     downloadButton('downloadData', 'Download results as CSV', icon = icon("file-download"))
                              )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    rv <- reactiveValues()

    #on pressing google, generate preview
    observeEvent(input$google, {
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

    })

    #prepare HTML files for scraping
    observeEvent(input$download_HTMLs, {
        htmls <- list()
        for(i in 1:length(rv$links[,4])){
            html <- save_html((rv$links[,4])[i], pause = 0.5, backoff = FALSE)
            htmls <- c(htmls, html)
        }
        rv$htmls <- htmls
        print(paste0(length(rv$htmls), ' files downloaded.'))

        output$save_report <- renderText({
            paste0(length(rv$htmls),' pages of results successfully downloaded. Proceed to the "Scrape data" tab to extract search results and download the final dataset.')
        })
    })

    #scrape HTML files
    observeEvent(input$scrape_HTMLs, {
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
                write.csv(rv$data, file, row.names = FALSE)
            })

    })

}

# Run the application
shinyApp(ui = ui, server = server)
