#' wrapper for multiple sites
#' query <- 'climate change'
#' sites <- c('www.sei.org\nwww.wwf.org')
#' links <- buildLinks_ecosia(query, sites, pages=3)
buildLinks_ecosia <- function(query,
                              sites,
                              pages = 1){

  sites <- trimws(unlist(strsplit(sites, "\n")))

  df <- data.frame()
  for(i in 1:length(sites)){
    results <- buildLink_ecosia(query, sites[i], pages)
    df <- rbind(df, results)
  }

  return(df)

}


#' Create links to search results on Ecosia
#'
#' @description Create functioning URLs for pages of search results in Ecosia.com.
#' @param query A string query using Boolean operators and field codes formatted
#' for OADT (see examples below).
#' @param pages A colon separated character string with the start page and end
#' page. Single pages not starting at 1 will return a single page. If left blank,
#' the default is to return the first page.
#' @return A single URL if pages = '1' or a dataframe for each page of results
#' that shows the page number and record starting number.
#' @examples
#' query <- 'climate change'
#' site <- 'www.sei.org'
#' links <- buildLink_ecosia(query, site, pages = 10)
#' links
#' urls <- links$URL
#' @export
buildLink_ecosia <- function(query,
                              site,
                              pages = 10){

  # if only one page sought
  if (identical(pages, '1') == TRUE) {

    #prepare query for url
    query <- unlist(strsplit(query, " "))
    query <- paste0(trimws(query), collapse = '%20')

    # create URL
    output <- paste0('https://www.ecosia.org/search?q=site%3A', site, '%20', query, '%22&p=0')

    return(output)

  # if multiple pages sought
  } else {
    # extract pages from 'start:end' pages input
    start_p <- 0
    end_p <- pages - 1

    # create link function
    link <- function(query, site, start_page){
      x <- unlist(strsplit(query, " "))
      x <- paste0(trimws(query), collapse = '%20')

      x <- paste0('https://www.ecosia.org/search?q=site%3A', site, '%20', query, '%22&p=', start_page)
    }

    # create output dataframe
    all_links <- mapply(link, query, site, seq(start_p, end_p, 1))
    output <- data.frame(site = site,
                         query = query,
                         page = seq(start_p, end_p, 1),
                         url = all_links,
                         row.names = NULL)

    return(output)
  }

}
