
.msp_reader <- function(parallel = FALSE, single_spectrum = FALSE, progress=FALSE) {
  
  # data_ <- str_split(data, "\n\n+")
  # data_ <- data_[[1]]
  # 
  # 
  # spectrum_d <- data_[[22]]
  # 
  # spectrum_ <- spectrum_d %>% 
  #   str_split("\n") %>% 
  #   magrittr::extract2(1) %>%
  #   map_chr(~ paste0(.x, "\n"))
  
  # Properties of type XXX=value\n
  ## Key
  # specVariable_key <- ((regex_replace("Synon: (\\$[0-9][0-9]) ", "\\1") %then% succeed(""))
  #                      %alt% 
  #                        (delimited_string(":") %then% (fixed_string(": "))))
  # ## Value
  # specVariable_value <- specify(
  #   query = delimited_string("\n"),
  #   testcases = c("elende penner! du horst**\"*", "abcsb\nbc"),
  #   leftovers = list("", "\nbc"),
  #   success = c("elende penner! du horst**\"*", "abcsb")
  # )
  # # Property
  # specVariable <- specify(
  #   query = (specVariable_key %then% specVariable_value %then% newline 
  #            %using% function(x) {
  #              list(type = "specVariable", formatKey = x[[1]], value = x[[3]])
  #            }),
  #   testcases = list("GUGUS: gaga\n", "MS1PRECURSOR: 123123232.1232",
  #                    "MS1PRECURSOR: 123123232.1232\nleft",
  #                    "Synon: $66 synoncomment\nasdf"),
  #   success = list(list(type = "specVariable", formatKey="GUGUS", value="gaga"),
  #                  NULL,
  #                  list(type = "specVariable", formatKey="MS1PRECURSOR", value="123123232.1232"),
  #                  list(type = "specVariable", formatKey="$66", value="synoncomment")),
  #   leftovers = list("", NULL, "left","asdf"))
  # 
  # # Ion table entries of type 123.1234 999
  # ion_plain <- specify(
  #   query=(
  #     (float %then% spacing %then% float %then% newline) 
  #     %using% 
  #       function(x) list(type = "ion", mz = x[[1]], int = x[[3]], annotation = NA)
  #   ),
  #   testcases = c("123.1234\t666\n", "121.2323     222\n"),
  #   leftovers = c("",""))
  # ion_annotated <- specify(
  #   query=(
  #     (float %then% spacing %then% float 
  #      %then% spacing %then% 
  #        fixed_string("\"") %then% delimited_string("\"") %then% fixed_string("\"") %then% newline) 
  #     %using% 
  #       function(x) list(type = "ion", mz = x[[1]], int = x[[3]], annotation = x[[6]])
  #   ),
  #   testcases = c('123.1234\t666\t"asdfa sdfa 2312"\n', '121.2323     222 "myannotation"\n'),
  #   leftovers = c("","")
  # )
  # ion  <- (ion_plain %alt% ion_annotated)
  
  # 
  # # Spectrum: spectrum start delimiter, variable block, ion table, spectrum end delimiter
  # ## Spectrum delimiters
  # end_marker <- zap_entry(many_iter(newline))
  # ## spectrum
  # spectrum <- specify(
  #   query = ((
  #     (many_iter(specVariable) %using% bind_rows)
  #     %then% 
  #       (many_iter(ion) %using% bind_rows)
  #     %then% 
  #       end_marker)
  #     %using% function(x) compact(x) %>% (function(xx) list(variables = xx[[1]], ions = xx[[2]]))),
  #   testcases = c("Name: Spectra\nSynon: $32 gugus\n121.1212\t4343\n121.3333  3434 \"fasdfasdf\"\n\n"),
  #   leftovers = c(""))
  # 
  
  # spectrum_line <- function(line) {
  #   (ion %alt% specVariable)
  # }
  spectrum <- function(data) {
    spectrum_ <- data %>% str_split("\n") %>% extract2(1) %>% map(~ paste0(.x, "\n"))
    #stopifnot(spectrum_[[1]])
    spectrum_lines_split <- spectrum_ %>% str_detect("^Num [pP]eaks:.*") %>% cumsum() %>% lag(default = 0)
    vars <- spectrum_[spectrum_lines_split == 0] %>%
      str_replace("^Synon:\\s?(\\$[0-9][0-9]) ", "\\1: ") %>% # Replace e.g. "Synon: $99 bla" with "$99: bla"
      str_match("^(.*?):\\s?(.*)$") %>%
      `[`(,c(2:3)) %>%
      set_colnames(c("formatKey", "value")) %>%
      as_tibble(.name_repair = "unique")
    
    # Read ions
    lines_ions <- spectrum_[spectrum_lines_split == 1]
    # find low-res ion definitions of type 123 234; 234 234; 1234 234 etc
    lines_ions_lr <- lines_ions  %>%
      str_detect("([0-9.]+)[\\s\\t]+([0-9.]+);")
    if(sum(lines_ions_lr) > 0) {
      # read as LR spectrum
      spec_lr <- lines_ions[lines_ions_lr] %>%
        map(~str_split(fixed(";"))) %>%
        flatten_chr()
        stop("LR spectra not yet supported")
    } 
    else
    {
      # read as HR spectrum
      ions <- spectrum_[spectrum_lines_split == 1] %>%
        str_match("([0-9.]+)[\\s\\t]+([0-9.]+)(.*)$") %>%
        `[`(,c(2:4), drop=FALSE) %>%
        set_colnames(c("mz", "int", "annotation")) %>%
        as_tibble(.name_repair = "unique") %>%
        mutate(mz = as.numeric(mz), int = as.numeric(int))
    }
    

      
    #read_table2(col_names = c("mz", "int", "annotation"), col_types = 'ddc')
# 
#     ions <- spectrum_[!spectrum_lines_var & !spectrum_lines_end] %>%
#       paste0(collapse = "\n") %>%
#       read_table2(col_names = c("mz", "int"),
#                   col_types = 'dd')
#     # data <- spectrum_ %>% map(spectrum_line()) %>% map(`$`, "result") %>% compact()
#     # vars <- data %>% keep(~ .x$type == "specVariable") %>% bind_rows() %>% select(-type)
#     # ions <- data %>% keep(~ .x$type == "ion") %>% bind_rows() %>% select(-type)
    list(variables = vars, ions = ions)
  }
  
  
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




