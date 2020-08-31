#' @import Ramble
#' @import furrr
#' @import tidyverse
#' @import stringr
NULL


regex <- function(pattern) 
{
  return(function(string) {
    pattern_ <- paste0("^", pattern)
    res <- str_starts(string, pattern_)
    if(isTRUE(res)) {
      pattern_ <- paste0("^", pattern)
      pos <- str_locate(string, pattern_)[1,]
      result <- str_sub(string, pos[1], pos[2])
      leftover <- str_sub(string, pos[2] + 1)
      return(succeed(result)(leftover))
    }
    else
      return(list())
  })
}
    



fixed_string <- function(pattern) 
{
  return(function(string) {
    res <- str_starts(string, fixed(pattern))
    if(isTRUE(res)) {
      pos <- str_locate(string, fixed(pattern))[1,]
      result <- str_sub(string, pos[1], pos[2])
      leftover <- str_sub(string, pos[2] + 1)
      return(succeed(result)(leftover))
    }
    else
      return(list())
  })
}




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


#regex_char <- function(regex) satisfy(function(x) grepl(regex, x))
yes_char <- function(delimiter) regex(paste0("[", delimiter, "]"))
not_char <- function(delimiter) regex(paste0("[^", delimiter, "]"))
delimited_string <- function(delimiter)
  regex(paste0("[^", delimiter, "]+"))
delimited_string_delimiter <- function(delimiter)
  delimited_string(delimiter) %then% yes_char(delimiter) %using% function(x) x[[1]]

newline <- fixed_string("\n") %using% function(x) c()
some_ <- Ramble::some
ident_ <- Ramble::ident

spacing <- specify(
  query = (regex("[\t ]+") %using% function(x) c()),
  testcases = c(" ", "   ", "\t", " \t "),
  leftovers = c("", "", "", "")
)


float <- regex("[0-9.]+")

#' Iterative implementation of many()
#' 
#' Does not recurse deeply and break on long lists.
#'
#' @param p 
#'
#' @return
#' @export
#'
#' @examples
some_iter <- function(p) {
  function(string) {
    res <- list()
    res_ <- p(string)
    while(length(res_) > 0) {
      res <- c(res, list(res_$result))
      string <- res_$leftover
      res_ <- p(string)
    }
    if(length(res) > 0)
      succeed(res)(string)
    else
      return(list())
    }
}

many_iter <- function(p) {
  function(string) {
    res <- list()
    res_ <- p(string)
    while(length(res_) > 0) {
      res <- c(res, list(res_$result))
      string <- res_$leftover
      res_ <- p(string)
    }
    if(length(res) > 0)
      succeed(res)(string)
    else
      succeed(NULL)(string)
  }
}
# 
# 
# # Ion table entries of type 123.1234 999
# ion <- specify(
#   query=(
#     (float %then% spacing %then% float %then% newline) 
#     %using% 
#       function(x) list(mz = x[[1]], int = x[[3]])
#   ),
#   testcases = c("123.1234\t666\n", "121.2323     222\n"),
#   leftovers = c("",""))
# 
# 
# many_ion <- specify(
#   query = many(ion) %using% bind_rows,
#   testcases = "121.2121\t434\n123.1234 543.2\n444    111\n",
#   leftovers = c("")
# )
# 
# many_ion_iter <- specify(
#   query = many_iter(ion) %using% bind_rows,
#   testcases = "121.2121\t434\n123.1234 543.2\n444    111\n",
#   leftovers = c("")
# )
# 
# 
# spectrum <- specify(
#   query = (begin_marker %then% 
#              (many(specVariable) %using% bind_rows)
#            %then% 
#              (many_iter(ion) %using% bind_rows)
#            %then% 
#              end_marker)
#   %using% function(x) compact(x) %>% (function(xx) list(variables = xx[[1]], ions = xx[[2]])),
#   testcases = c("\nBEGIN IONS\nSCANS=NA\nblub=TEST\n121.1212\t4343\n121.3333  3434\nEND IONS\n",
#                 "\nBEGIN IONS\nSCANS=NA\nblub=TEST\n121.1212\t4343\n121.3333  3434\nEND IONS"),
#   leftovers = c("", ""))
# 
# 
