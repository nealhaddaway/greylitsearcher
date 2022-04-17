#' Scrape all google information
#'
#' @description A wrapper for the scraping functions to produce a dataframe of citations for
#' each page of google.com search results.
#' @return A dataframe containing all extractable information from all html files in the working
#' directory.
#' @importFrom magrittr "%>%"
#' @examples
#' info <- get_info(html)
#'@export
get_info <- function(html){
  code_lines <- unlist(strsplit(html, '\\<div', useBytes = TRUE))
  titles <- get_titles(code_lines)
  subpages <- get_subpages(code_lines)
  descriptions <- get_descriptions(code_lines)
  links <- get_links(code_lines)
  df <- data.frame(source = rep(names(html), length(titles)), title = titles, description = descriptions, link = links)
  return(df)
}


#' Extract titles from Google results
#'
#' @description Extract the titles of search results from Google search results, saved as
#' html files.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of result titles (10 per page)
#' @examples
#' titles <- get_titles(lines);
#' @export
get_titles <- function(code_lines){
  y <- grep("ZINbbc luh4tb xpd O9g5cc uUPGi", code_lines) #find location of lines containing 'result-title js-result-title' tag
  titles <- code_lines[y+2] #extract lines
  titles <- gsub('>', '', gsub('<', '', stringr::str_extract(titles, ">\\s*(.*?)\\s*<")))
  return(titles)
}


#' Extract article subpage from Google results
#'
#' @description Extract website subpage location, from Google search results.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of subpages
#' @examples
#' descriptions <- get_subpages(lines)
#' descriptions;
#' @export
get_subpages <- function(code_lines){
  y <- grep("ZINbbc luh4tb xpd O9g5cc uUPGi", code_lines) #find location of lines containing 'result-title js-result-title' tag
  subpages <- code_lines[y+4] #extract lines
  subpages <- gsub('>', '', gsub('<', '', stringr::str_extract(subpages, ">\\s*(.*?)\\s*<")))
  return(subpages)
}


#' Extract article descriptions from Google results
#'
#' @description Extract article descriptions (summaries of key sentences), from Google
#' search results.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of descriptions
#' @examples
#' descriptions <- get_descriptions(lines)
#' descriptions;
#' @export
get_descriptions <- function(code_lines){
  y <- grep("ZINbbc luh4tb xpd O9g5cc uUPGi", code_lines) #find location of lines containing 'result-title js-result-title' tag
  descriptions <- code_lines[y+12] #extract lines
  descriptions <- gsub("<.*?>", "", sub(".*AP7Wnd\">", "", descriptions))
  descriptions <- gsub("</", "", descriptions)
  return(descriptions)
}


#' Extract article links from Google results
#'
#' @description Extract links to websites holding article information from Google
#' search results.
#' @param html A vector of lines consisting of the html code for a webpage
#' @return A vector of URLs
#' @examples
#' links <- get_links(lines)
#' links;
#' @export
get_links <- function(code_lines){
  y <- grep("ZINbbc luh4tb xpd O9g5cc uUPGi", code_lines) #find location of lines containing 'result-title js-result-title' tag
  links <- code_lines[y+1] #extract lines
  #links <- gsub('q=', '', gsub('\"><h3', '', stringr::str_extract(links, "q=\\s*(.*?)\\s*<h3")))
  links <- sub("&amp;sa=.*", "", links)
  links <- sub('.*q=', '', links)
  return(links)
}

#' Function to clean HTML code from a string
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
