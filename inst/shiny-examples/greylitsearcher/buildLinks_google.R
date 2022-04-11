#' wrapper for multiple sites
#' query <- 'climate change'
#' sites <- c('www.sei.org\nwww.wwf.org')
#' links <- buildLinks_google(query, sites, pages=3)
buildLinks_google <- function(query,
                              sites,
                              pages = 1){

  sites <- trimws(unlist(strsplit(sites, "\n")))

  df <- data.frame()
  for(i in 1:length(sites)){
    results <- buildLink_google(query, sites[i], pages)
    df <- rbind(df, results)
  }

  return(df)

}


#' Create links to search results on Google
#'
#' @description Create functioning URLs for pages of search results in google.com.
#' @param query A set of space separated terms.
#' @param pages A number indicating the number of pages to return.
#' @return A single URL if pages = '1' or a dataframe for each page of results
#' that shows the page number and record starting number.
#' @examples
#' query <- 'climate change'
#' site <- 'www.sei.org'
#' links <- buildLink_google(query, site, pages = 10)
#' links
#' urls <- links$URL
#' @export
buildLink_google <- function(query,
                             site,
                             pages = 10){

  # if only one page sought
  if (identical(pages, '1') == TRUE) {

    #prepare query for url
    query <- unlist(strsplit(query, " "))
    query <- paste0(trimws(query), collapse = '+')

    # create URL
    output <- paste0('https://www.google.com/search?q=site:', site, '+', query, '&start=0')

    return(output)

  # if multiple pages sought
  } else {
    # extract pages from 'start:end' pages input
    start_p <- 0
    end_p <- pages - 1

    # create link function
    link <- function(query, site, start_page){
      x <- unlist(strsplit(query, " "))
      x <- paste0(trimws(x), collapse = '+')

      output <- paste0('https://www.google.com/search?q=site:', site, '+', x, '&start=', start_page)
      return(output)
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
