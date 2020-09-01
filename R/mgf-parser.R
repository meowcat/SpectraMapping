#' @import progress
#' @include parser-utils.R
#' 
NULL

.mgf_reader <- function(parallel = FALSE, single_spectrum = FALSE, progress=FALSE) {
  
  # Properties of type XXX=value\n
  ## Key
  specVariable_key <- delimited_string("= ")
  ## Value
  specVariable_value <- specify(
    query = delimited_string("\n"),
    testcases = c("elende penner! du horst**\"*", "abcsb\nbc"),
    leftovers = list("", "\nbc"),
    success = c("elende penner! du horst**\"*", "abcsb")
  )
  # Property
  specVariable <- specify(
    query = (specVariable_key %then% fixed_string("=") %then% specVariable_value %then% newline 
             %using% function(x) {
               list(formatKey = x[[1]], value = x[[3]])
             }),
    testcases = list("GUGUS=gaga\n", "MS1PRECURSOR=123123232.1232", "MS1PRECURSOR=123123232.1232\nleft"),
    success = list(list(formatKey="GUGUS", value="gaga"),
                   NULL,
                   list(formatKey="MS1PRECURSOR", value="123123232.1232")),
    leftovers = list("", NULL, "left"))
  
  # Ion table entries of type 123.1234 999
  ion <- specify(
    query=(
      (float %then% spacing %then% float %then% newline) 
      %using% 
        function(x) list(mz = x[[1]], int = x[[3]])
    ),
    testcases = c("123.1234\t666\n", "121.2323     222\n"),
    leftovers = c("",""))
  
  # Spectrum: spectrum start delimiter, variable block, ion table, spectrum end delimiter
  ## Spectrum delimiters
  begin_marker <- zap_entry((many_iter(newline)) %then% fixed_string("BEGIN IONS\n"))
  end_marker <- zap_entry((fixed_string("END IONS") %then% maybe(newline)))
  ## spectrum
  spectrum <- specify(
    query = (begin_marker %then% 
               (many_iter(specVariable) %using% bind_rows)
             %then% 
               (many_iter(ion) %using% bind_rows)
             %then% 
               end_marker)
    %using% function(x) compact(x) %>% (function(xx) list(variables = xx[[1]], ions = xx[[2]])),
    testcases = c("\nBEGIN IONS\nSCANS=NA\nblub=TEST\n121.1212\t4343\n121.3333  3434\nEND IONS\n",
                  "\nBEGIN IONS\nSCANS=NA\nblub=TEST\n121.1212\t4343\n121.3333  3434\nEND IONS"),
    leftovers = c("", ""))
  
  if(single_spectrum)
    return(spectrum)
  
  safe_spectrum <- function(x, pb=NULL) {
    tryCatch({
    if(!is.null(pb))
      pb$tick()
      spectrum(x)
    },
    error=function(e) {
      message("Error: ", x)
      return(list(result=NULL))
    })
  }
  
  document <- function(data) {
    
    
    data_ <- str_split(data, "\n\n+")
    data_ <- data_[[1]]
    
    pb <- NULL
    if(progress)
      pb <- progress::progress_bar$new(total = length(data_))
    
    if(!parallel)
      return(data_ %>% map( ~ safe_spectrum(.x, pb)) %>% map("result") %>% compact())
    else
      return(data_ %>% future_map( ~ safe_spectrum(.x), .progress = progress) %>% map("result") %>% compact())
  }
  return(document)
}

