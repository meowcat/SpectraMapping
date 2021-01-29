#' @import progress
#' @include parser-utils.R
#' 
NULL

.mgf_reader <- function(parallel = FALSE, single_spectrum = FALSE, progress=FALSE) {
  
  # Properties of type XXX=value\n
  message("using the fast parser")
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
               list(type = "specVariable", formatKey = x[[1]], value = x[[3]])
             }),
    testcases = list("GUGUS=gaga\n", "MS1PRECURSOR=123123232.1232", "MS1PRECURSOR=123123232.1232\nleft"),
    success = list(list(type = "specVariable", formatKey="GUGUS", value="gaga"),
                   NULL,
                   list(type = "specVariable", formatKey="MS1PRECURSOR", value="123123232.1232")),
    leftovers = list("", NULL, "left"))
  
  # # Ion table entries of type 123.1234 999
  # ion <- specify(
  #   query=(
  #     (float %then% spacing %then% float %then% newline) 
  #     %using% 
  #       function(x) list(type = "ion", mz = x[[1]], int = x[[3]], annotation = NA)
  #   ),
  #   testcases = c("123.1234\t666\n", "121.2323     222\n"),
  #   leftovers = c("",""))
  
  # Spectrum: spectrum start delimiter, variable block, ion table, spectrum end delimiter
  ## Spectrum delimiters
  begin_marker <- zap_entry((many_iter(newline)) %then% fixed_string("BEGIN IONS\n"))
  end_marker <- zap_entry((fixed_string("END IONS") %then% maybe(newline)))
  ## spectrum
  spectrum_line <- function(line) {
    (ion %alt% specVariable %alt% end_marker)
  }
  spectrum <- function(data) {
    spectrum_ <- data %>% begin_marker() %>% extract2("leftover")
    spectrum_ <- spectrum_ %>% str_split("\n") %>% extract2(1) %>% map(~ paste0(.x, "\n"))
    #stopifnot(spectrum_[[1]])
    spectrum_lines_var <- spectrum_ %>% str_detect(".*?=.*")
    spectrum_lines_end <- spectrum_ %>% str_detect("^END IONS")
    vars <- spectrum_[spectrum_lines_var] %>% 
      map(specVariable) %>% map(`$`, "result") %>% 
      compact() %>%
      bind_rows() %>%
      select(-type)
    ions <- spectrum_[!spectrum_lines_var & !spectrum_lines_end] %>%
      paste0(collapse = "\n") %>%
      read_table2(col_names = c("mz", "int"),
                  col_types = 'dd')
    # data <- spectrum_ %>% map(spectrum_line()) %>% map(`$`, "result") %>% compact()
    # vars <- data %>% keep(~ .x$type == "specVariable") %>% bind_rows() %>% select(-type)
    # ions <- data %>% keep(~ .x$type == "ion") %>% bind_rows() %>% select(-type)
    list(variables = vars, ions = ions)
  }
  
  
  
  # spectrum <- specify(
  #   query = (begin_marker %then% 
  #              (many_iter(specVariable) %using% bind_rows)
  #            %then% 
  #              (many_iter(ion) %using% bind_rows)
  #            %then% 
  #              end_marker)
  #   %using% function(x) compact(x) %>% (function(xx) list(variables = xx[[1]], ions = xx[[2]])),
  #   testcases = c("\nBEGIN IONS\nSCANS=NA\nblub=TEST\n121.1212\t4343\n121.3333  3434\nEND IONS\n",
  #                 "\nBEGIN IONS\nSCANS=NA\nblub=TEST\n121.1212\t4343\n121.3333  3434\nEND IONS"),
  #   leftovers = c("", ""))
  
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
      return(data_ %>% map( ~ safe_spectrum(.x, pb)) %>% compact())
    else
      return(data_ %>% future_map( ~ safe_spectrum(.x), .progress = progress) %>% compact())
  }
  return(document)
}

