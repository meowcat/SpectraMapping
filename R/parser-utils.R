#' @import Ramble
#' @import furrr
#' @import tidyverse
NULL

# https://gist.github.com/mmaechler/9cfc3219c4b89649313bfe6853d87894
##' warnifnot(): a "only-warning" version of stopifnot():
##'   {Yes, learn how to use do.call(substitute, ...) in a powerful manner !!}
warnifnot <- stopifnot
body(warnifnot) <- do.call(substitute, list(body(stopifnot),
                                            list(stop = quote(warning))))
## (now, this was really cute ....)


#' Specify a grammar term including test cases
#'
#' @param query The Ramble parser to define
#' @param testcases The testcases to run through the parser, a list of strings
#' @param leftovers The expected leftovers, or NULL if the parsing should fail
#'
#' @return The parser.
#' 
#' The function exits with an error if a test fails.
#' If you specify any NULL results, `leftovers` needs to be a list, not vector!
#' 
#' @export
#'
#' @examples
specify <- function(query, testcases, success = NULL, leftovers = NULL) {
  results <- testcases %>% map(query)
  if(!is.null(success)) {
    success_results <- results %>% map("result") %>% map2_lgl(success, identical)
    warnifnot(all(success_results))
  }
  if(!is.null(leftovers))
  {
    leftover_results <- results %>% map("leftover") %>% map2_lgl(leftovers, identical)
    warnifnot(all(leftover_results))
  }
  query
}

zap_entry <- function(query) query %using% function(x) c()


regex_char <- function(regex) satisfy(function(x) grepl(regex, x))
yes_char <- function(delimiter) regex_char(paste0("[", delimiter, "]"))
not_char <- function(delimiter) regex_char(paste0("[^", delimiter, "]"))
delimited_string <- function(delimiter)
  (many(not_char(delimiter)) %using%
     function(x) paste0(unlist(c(x)), collapse=""))
delimited_string_delimiter <- function(delimiter)
  delimited_string(delimiter) %then% yes_char(delimiter) %using% function(x) x[[1]]

newline <- String("\n") %using% function(x) c()
some_ <- Ramble::some
ident_ <- Ramble::ident

spacing <- specify(
  query = (some_(String("\t") %alt% String(" ")) %using% function(x) c()),
  testcases = c(" ", "   ", "\t", " \t "),
  leftovers = c("", "", "", "")
)


float <- function() {
  some_(Digit() %alt% String(".")) %using%
    function(x) {
      as.numeric(paste(unlist(c(x)), collapse=""))
    }
}
