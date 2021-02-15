
.massbank_reader <- function(parallel = FALSE, single_spectrum = FALSE, progress=FALSE) {
  
  # }
  spectrum <- function(data) {
    spectrum_ <- data %>% 
      str_split("\n") %>% 
      extract2(1) %>% 
      map(~ paste0(.x, "\n")) %>%
      discard(~ str_detect(.x, "^\n$"))
    
    # Find lines that are not continuations of a multiline block
    spectrum_lines_split <- spectrum_ %>% str_detect("^\\s\\s", negate=TRUE) %>% cumsum()
    spectrum_blocks <- spectrum_ %>% split(spectrum_lines_split)
    spectrum_keys <- spectrum_blocks %>% 
      map_chr(extract2, 1) %>% 
      str_match("^(.*?):") %>%
      `[`(,2)
    spectrum_data <- spectrum_blocks %>%
      map(function(block) {
        
        if(length(block) > 1) {
          r1 <- block[1] %>% str_match("^.*?: (.*)$") %>% `[`(,2)
          rn <- block[-1] %>% str_match("\\s\\s(.*)") %>% `[`(,2)
          block_data <- c(paste("index", r1),
                          paste(seq_along(rn), rn))
        }
        else
          block_data <- block %>% str_match("^.*?: (.*)$") %>% `[`(,2)
        return(block_data)
      })
    names(spectrum_data) <- spectrum_keys
    
    ions <- spectrum_data[["PK$PEAK"]] %>%
      read_table2() %>%
      set_colnames(c("HEADER", "mz", "int", "relint")) %>%
      select(-HEADER)
    spectrum_data[["PK$PEAK"]] <- NULL
    
    #spectrum_data_reform <- spectrum_data %>% split(names(spectrum_data)) %>% map(as.character)
    vars <- tibble(formatKey = names(spectrum_data), 
                   value = spectrum_data %>% set_names(NULL)) %>%
      unnest(value)
    
  
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
    
    data_ <- str_split(data, "\n//\n*")
    data_ <- data_[[1]]
    data_ <- data_ %>% discard(~ .x == "")
    
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




