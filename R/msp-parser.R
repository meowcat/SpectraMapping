
.msp_reader <- function(parallel = FALSE, single_spectrum = FALSE) {
  
  # Properties of type XXX=value\n
  ## Key
  specVariable_key <- delimited_string(":")
  ## Value
  specVariable_value <- specify(
    query = delimited_string("\n"),
    testcases = c("elende penner! du horst**\"*", "abcsb\nbc"),
    leftovers = list("", "\nbc"),
    success = c("elende penner! du horst**\"*", "abcsb")
  )
  # Property
  specVariable <- specify(
    query = (specVariable_key %then% String(": ") %then% specVariable_value %then% newline 
             %using% function(x) {
               list(key = x[[1]], value = x[[3]])
             }),
    testcases = list("GUGUS: gaga\n", "MS1PRECURSOR: 123123232.1232", "MS1PRECURSOR: 123123232.1232\nleft"),
    success = list(list(key="GUGUS", value="gaga"),
                   NULL,
                   list(key="MS1PRECURSOR", value="123123232.1232")),
    leftovers = list("", NULL, "left"))
  
  # Ion table entries of type 123.1234 999
  ion_plain <- specify(
    query=(
      (float() %then% spacing %then% float() %then% newline) 
      %using% 
        function(x) list(mz = x[[1]], int = x[[3]], annotation = NA)
    ),
    testcases = c("123.1234\t666\n", "121.2323     222\n"),
    leftovers = c("",""))
  ion_annotated <- specify(
    query=(
      (float() %then% spacing %then% float() 
       %then% spacing %then% 
         String("\"") %then% delimited_string("\"") %then% String("\"") %then% newline) 
      %using% 
        function(x) list(mz = x[[1]], int = x[[3]], annotation = x[[6]])
    ),
    testcases = c('123.1234\t666\t"asdfa sdfa 2312"\n', '121.2323     222 "myannotation"\n'),
    leftovers = c("","")
  )
  ion  <- (ion_plain %alt% ion_annotated)
  
  
  # Spectrum: spectrum start delimiter, variable block, ion table, spectrum end delimiter
  ## Spectrum delimiters
  end_marker <- zap_entry(many(newline))
  ## spectrum
  spectrum <- specify(
    query = ((
               (many(specVariable) %using% bind_rows)
             %then% 
               (many(ion) %using% bind_rows)
             %then% 
                end_marker)
    %using% function(x) compact(x) %>% (function(xx) list(variables = xx[[1]], ions = xx[[2]]))),
    testcases = c("Name: Spectra\nSynon: $32 gugus\n121.1212\t4343\n121.3333  3434 \"fasdfasdf\"\n\n"),
    leftovers = c(""))
  
  if(single_spectrum)
    return(spectrum)
  
  document <- function(data) {
    data_ <- str_split(data, fixed("\n\n"))
    data_ <- data_[[1]]
    if(!parallel)
      return(data_ %>% map(spectrum) %>% map("result"))
    else
      return(data_ %>% future_map(spectrum) %>% map("result"))
  }
  return(document)
}




