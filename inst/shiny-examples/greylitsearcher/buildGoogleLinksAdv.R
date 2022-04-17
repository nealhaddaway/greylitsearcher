#' Build G links
#'
#' Function takes as input a partial Boolean search string and produces a functioning (set of)
#' URLs; one for each page of search results on Google.
#' @description Constructs series of Google search page URLs
#' @param site URL of site to search.
#' @param and_terms Vector of alphanumeric terms searched using the AND Boolean operator,
#' specified by Google Scholar as 'with all of the words'.
#' @param exact_phrase Vector of alphanumeric terms enclosed in inverted commas and searched
#' as phrases (e.g. "large cat"), specified by Google as 'with the exact phrase'.
#' @param or_terms Vector of alphanumeric terms searched using the OR Boolean operator,
#' specified by Google Scholar as 'with at least one of the words'.
#' @param not_terms Vector of alphanumeric terms searched using the NOT Boolean operator,
#' specified by Google Scholar as 'without the words'.
#' @param date_from Date from which searching will be performed, in the format YYY-MM-DD. If
#' no value provided, all years are searched.
#' @param date_to Date to which searching will be performed, in the format YYY-MM-DD. If no
#' value provided, all years are searched.
#' @param start_page Integer specifying which page(s) of search results should be displayed.
#' If multiple pages are selected, multiple URLs are returned, one for each page of ten
#' search results. The default is set to generate a list of 100 URLs (maximum set of Google
#' Scholar results visible).
#' @param pages Integer for the number of pages of search results to be returned (one link per
#' page). A maximum of 100 pages can be displayed in Google Scholar. The default value is 1.
#' @examples
#' \dontrun{
#' and_terms <- c('river, aquatic')
#' exact_phrase <- c('water chemistry')
#' or_terms <- c('crayfish, fish')
#' not_terms <- c('lobster, coral')
#' date_from <- '1900-01-01'
#' date_to <- '2020-01-01'
#' link <- buildGoogleLinkAdv(
#'     site = 'www.sei.org',
#'     and_terms = and_terms,
#'     exact_phrase = exact_phrase,
#'     or_terms = or_terms,
#'     not_terms = not_terms,
#'     date_from = date_from,
#'     date_to = date_to,
#'     pages = 10)
#' link$link
#' }
#' @return A list containing: 1) ($link) links to the specified search results; and, 2) ($report)
#' a text string containing a report of the links generated and the input variables used.
#' @export
buildGoogleLinkAdv <- function(site = '',
                               and_terms = '',
                               exact_phrase = '',
                               or_terms = '',
                               not_terms = '',
                               date_from = '',
                               date_to = '',
                               pages = 1) {
  #report
  report <- paste(
          'File generated: ',
          paste('Search date, time, timezone: ',
                Sys.time(),
                ' (',
                Sys.timezone(),
                ')',
                sep = ''),
          '\n',
          'Search parameters:',
          paste('Site searched: ',
                site,
                sep = ''),
          paste('All these words: ',
                paste(and_terms,
                      collapse = '; '),
                sep = ''),
          paste('None of these words: ',
                paste(not_terms,
                      collapse = '; '),
                sep = ''),
          paste('This exact word or phrase: ',
                paste('"',
                      exact_phrase,
                      '"',
                      sep = ''),
                sep = ''),
          paste('Any these words: ',
                paste(or_terms,
                      collapse = '; '),
                sep = ''),
          paste('Between these dates:',
                date_from,
                'and',
                date_to,
                sep = ' '),
          if(pages == ''){
            'Number of pages exported: 1'
          } else {
            paste('Number of pages exported: ',
                  pages,
                  sep = '')
          },
          sep = '\n')

  site_orig <- site
  site <- if(identical(site, '') == TRUE){
    site <- ''
  } else {
    site <- paste0('site%3A',
                   site)
  }


    and_terms <- if(any(and_terms == '') == TRUE) { #if and_terms is blank then leave it blank, otherwise combine terms with '+'
      and_terms <- ''
    } else {
      and_terms <- gsub(',', '', paste('+',
                         gsub(' ',
                              '+',
                              paste(and_terms,
                                    collapse = '+')),
                         sep = ''))
    }

    exact_phrase <- if(any(exact_phrase == '') == TRUE){ #if exact_phrase is blank then leave it blank, otherwise combine terms with '+' and top/tail with '+%22'/'%22'
      exact_phrase <- ''
    } else {
      exact_phrase <- paste('+"',
                            gsub(' ',
                                 '+',
                                 paste(exact_phrase,
                                       collapse = '+')),
                            '"',
                            sep = '')
    }

    or_terms <- if(any(or_terms == '') == TRUE){ #if or_terms is blank then leave it blank, otherwise combine terms with '+OR+'
      or_terms <- ''
    } else {
      or_terms <- gsub(',', '', paste('+OR+',
                        gsub(' ',
                             '+OR+',
                             paste(or_terms,
                                   collapse = '+OR+')),
                        sep = ''))
    }

    not_terms <- if(any(not_terms == '') == TRUE){ #if not_terms is blank then leave it blank, otherwise combine terms with '+OR+'
      not_terms <- ''
    } else {
      not_terms <- gsub(',', '', paste('+-',
                         gsub(' ',
                              '+-',
                              paste(not_terms,
                                    collapse = '+-')),
                         sep = ''))
    }

    if((date_from == '') == TRUE){ #specify the start year
      Fdate <- ''
    } else {
      Fyear <- format(as.Date(date_from, format="%Y-%m-%d"), "%Y")
      Fmonth <- format(as.Date(date_from, format="%Y-%m-%d"), "%m")
      Fday <- format(as.Date(date_from, format="%Y-%m-%d"), "%d")
      Fdate <- paste0('%2Ccd_min%3A', Fmonth, '%2F', Fday, '%2F', Fyear)
    }
    if((date_to == '') == TRUE){ #specify the stop year
      Tdate <- ''
    } else {
      Tyear <- format(as.Date(date_to, format="%Y-%m-%d"), "%Y")
      Tmonth <- format(as.Date(date_to, format="%Y-%m-%d"), "%m")
      Tday <- format(as.Date(date_to, format="%Y-%m-%d"), "%d")
      Tdate <- paste0('%2Ccd_max%3A', Tmonth, '%2F', Tday, '%2F', Tyear)
    }

    if(Fdate == '' && Tdate == ''){
      Drange <- ''
    } else {
      Drange <- paste0(
        '&tbs=cdr%3A1',
        Fdate,
        Tdate
      )
    }


    #build URL
    if (pages == 1){ #if pages = 1 then only a single link is generated
      link <- paste0('https://www.google.com/search?q=',
                    site,
                    and_terms,
                    or_terms,
                    exact_phrase,
                    not_terms,
                    Drange,
                    '&start=0')
    } else { #otherwise, one link is generated for each page
      link <- list()
      init <- seq(0, (pages*10-10), 10)
      for (i in init){
        x <- paste0('https://www.google.com/search?q=',
                   '&q=',
                   site,
                   and_terms,
                   or_terms,
                   exact_phrase,
                   not_terms,
                   Drange,
                   '&start=',
                   i)
        link <- append(link, x)
        link <- unlist(link)
      }

    }

    report <- paste(report,
                    '\n',
                    'Google links generated:',
                    paste(link,
                          collapse = '\n'),
                    '\n',
                    sep = '\n')

    linkdf <- data.frame(site = site_orig,
                         page = ((as.numeric(sub('.*&start=', '', link)))/10) + 1,
                         link = link,
                         row.names = NULL)

    output <- list(link = linkdf, report = report)

    return(output)
}


#' wrapper for multiple sites
#' and_terms <- c('river', 'aquatic')
#' exact_phrase <- c('water chemistry')
#' or_terms <- c('crayfish', 'fish')
#' not_terms <- c('lobster', 'coral')
#' date_from <- '1900-01-01'
#' date_to <- '2020-01-01'
#' sites <- 'www.sei.org\nwww.wwf.org'
#' links <- buildGoogleLinkAdv(
#'     site = sites,
#'     and_terms = and_terms,
#'     exact_phrase = exact_phrase,
#'     or_terms = or_terms,
#'     not_terms = not_terms,
#'     date_from = date_from,
#'     date_to = date_to,
#'     pages = 10)
#' links$link
buildGoogleLinksAdv <- function(sites = '',
                                and_terms = '',
                                exact_phrase = '',
                                or_terms = '',
                                not_terms = '',
                                date_from = '',
                                date_to = '',
                                pages = 1) {

  #report
  report <- paste(
    'File generated: ',
    paste('Search date, time, timezone: ',
          Sys.time(),
          ' (',
          Sys.timezone(),
          ')',
          sep = ''),
    '\n',
    'Search parameters:',
    paste('Sites searched: ',
          paste0(sites, collapse = '; '),
          sep = ''),
    paste('All these words: ',
          paste(and_terms,
                collapse = '; '),
          sep = ''),
    paste('None of these words: ',
          paste(not_terms,
                collapse = '; '),
          sep = ''),
    paste('This exact word or phrase: ',
          paste('"',
                exact_phrase,
                '"',
                sep = ''),
          sep = ''),
    paste('Any these words: ',
          paste(or_terms,
                collapse = '; '),
          sep = ''),
    paste('Between these dates:',
          date_from,
          'and',
          date_to,
          sep = ' '),
    if(pages == ''){
      'Number of pages exported: 1'
    } else {
      paste('Number of pages exported: ',
            pages * length(trimws(unlist(strsplit(sites, "\n")))),
            sep = '')
    },
    sep = '\n')

  #prepare sites
  sites <- trimws(unlist(strsplit(sites, "\n")))

  #loop through sites to generate links
  df <- data.frame()
  for(i in 1:length(sites)){
    results <- buildGoogleLinkAdv(
      site = sites[i],
      and_terms = and_terms,
      exact_phrase = exact_phrase,
      or_terms = or_terms,
      not_terms = not_terms,
      date_from = date_from,
      date_to = date_to,
      pages = pages)
    resultsdf <- results$link
    resultsreport <- results$report
    df <- rbind(df, resultsdf)
  }

  report <- paste(report,
                  '\n',
                  'Google links generated:',
                  paste(df$link,
                        collapse = '\n'),
                  '\n',
                  sep = '\n')

  output <- list(link = df, report = report)

  return(output)

}
