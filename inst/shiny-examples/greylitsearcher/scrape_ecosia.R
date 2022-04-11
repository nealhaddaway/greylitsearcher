#' Scrape all ecosia information
#'
#' @description A wrapper for the scraping functions to produce a dataframe of citations for
#' each page of Ecosia.com search results.
#' @return A dataframe containing all extractible information from all html files in the working
#' directory.
#' @importFrom magrittr "%>%"
#' @examples
#' info <- get_info();
#'@export
get_info <- function(){
  codes <- get_htmls_code()
  code_lines <- mapply(split_by_div, codes)
  titles <- as.data.frame(mapply(get_titles, code_lines)) %>%
    utils::stack()
  names(titles) <- c('title', 'sourcefile')
  descriptions <- as.data.frame(mapply(get_descriptions, code_lines)) %>%
    utils::stack()
  names(descriptions) <- c('descriptions', 'sourcefile')
  links <- as.data.frame(mapply(get_links, code_lines)) %>%
    utils::stack()
  names(links) <- c('links', 'sourcefile')
  df <- data.frame(titles, descriptions, links)
  return(df)
}


#' Scrape in code from local html file
#'
#'@description Scrape in code from an html file saved locally
#'@param filename Name of the file to be scraped. File should be saved in the working
#'directory.
#'@return A string of html code for a webpage
#'@examples
#'code <- get_html_code(file.choose());
#'@export
get_html_code <- function(filename){
  x <- xml2::read_html(filename)
  x <- paste(x, collapse = '')
  return(x)
}


#' Scrape in code from multiple local html files
#'
#'@description Scrape in code from html files saved locally. Files should be saved in the working
#'directory.
#'@return One or more strings of html code for a webpage, stored as a list
#'@examples
#'html_codes <- get_htmls_code()
#'file1 <- unlist(html_codes[1]);
#'@export
get_htmls_code <- function(){
  filenames <- list.files(getwd())
  filenames <- filenames[grep('.html', filenames)] #select only the HTML files in the working directory
  x <- as.list(mapply(get_html_code, filenames))
  return(x)
}


#' Split file by '<div' code into different lines
#'
#' @description Split the imported html code into lines based on the '<div' field code.
#' @param html A string consisting of the html code for a webpage
#' @return A vector of strings, one for each separated line
#' @examples
#' lines <- split_by_div(code)
#' lines;
#' @export
split_by_div <- function(html) {
  x <- unlist(strsplit(html, '\\<div class', useBytes = TRUE))
  return(x)
}


#' Extract titles from Google Scholar results
#'
#' @description Extract the titles of search results from Google Scholar search results, saved as
#' html files.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of result titles (10 per page)
#' @examples
#' titles <- get_titles(lines);
#' @export
get_titles <- function(html){
  y <- grep("result js-result", html) #find location of lines containing 'result-title js-result-title' tag
  titles <- html[y] #extract lines
  titles <- stringr::str_match(titles, "data-title=\"\\s*(.*?)\\s*\" data-url")[,2]
  return(titles)
}


#' Extract article descriptions from Google Scholar results
#'
#' @description Extract article descriptions (summaries of key sentences), from Google
#' Scholar search results.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of descriptions
#' @examples
#' descriptions <- get_descriptions(lines)
#' descriptions;
#' @export
get_descriptions <- function(html){
  y <- grep('card-desktop card-web', html)
  results <- html[y:length(html)]
  d <- grep("class=\"result-snippet\"", results)
  d <- results[d]
  descriptions <- stringr::str_match(d, "<p class=\"result-snippet\">\n                \\s*(.*?)\\s*\n            </p>")[,2]
  descriptions <- cleanFun(descriptions)
  return(descriptions)
}


#' Extract article links from Google Scholar results
#'
#' @description Extract links to websites holding article information from Google
#' Scholar search results.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of URLs
#' @examples
#' links <- get_links(lines)
#' links;
#' @export
get_links <- function(html){
  y <- grep("result js-result", html) #find location of lines containing 'result-title js-result-title' tag
  links <- html[y] #extract lines
  links <- stringr::str_match(links, "data-url=\"\\s*(.*?)\\s*\" data-track-id")[,2]
  return(links)
}

#' Function to clean HTML code from a string
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
