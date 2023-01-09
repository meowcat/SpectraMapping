
.sirius_writer <- function() {
  return(NA)
}

.sirius_reader <- function(parallel = FALSE, single_spectrum = FALSE, progress=FALSE) {

  spectrum <- function(data) {
    # Remove empty lines, add newline at every EOL

    spectrum_ <- data %>%
      str_split("\n") %>%
      extract2(1) %>%
      keep(~ .x != "") %>%
      map(~ paste0(.x, "\n"))


    #stopifnot(spectrum_[[1]])
    #spectrum_lines_split <- spectrum_ %>% str_detect("^Num [pP]eaks:.*") %>% cumsum() %>% lag(default = 0)
    
    spectrum_metadata_lines <- spectrum_ %>% str_detect("^>") 
    vars <- spectrum_[spectrum_metadata_lines] %>%
      str_match("^>(.*?)[\\s$](.*)") %>%
      `[`(,c(2:3),drop=FALSE) %>%
      set_colnames(c("formatKey", "value")) %>%
      as_tibble(.name_repair = "unique")

    # read ions as HR spectrum
    spectrum_metadata_ions <- spectrum_ %>% str_detect("^[0-9]") 
    ions <- spectrum_[spectrum_metadata_ions] %>%
      str_match("([0-9.]+)[\\s\\t]+([0-9.]+)(.*)$") %>%
      `[`(,c(2:4), drop=FALSE) %>%
      set_colnames(c("mz", "int", "annotation")) %>%
      as_tibble(.name_repair = "unique") %>%
      mutate(mz = as.numeric(mz), int = as.numeric(int))



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

    # parse block by block (a block is separated by \n\n)
    if(!parallel)
      blocks <- (data_ %>% map( ~ safe_spectrum(.x, pb)) %>% compact())
    else
      blocks <- (data_ %>% future_map( ~ safe_spectrum(.x), .progress = progress) %>% compact())

    # There are blocks that have compound info and blocks that don't.
    # Further, blocks may have a spectrum or not.
    
    # A compound block is initiated by >compound, this info is valid until the
    # next >compound statement.,
    # A spectrum contains >ms1peaks, >ms2peaks or >collision [0-9]+
    # Propagate compound info to subsequent spectra,
    # and finally remove records that are not spectra.
    spectrum_markers <- c("ms1peaks", "ms2peaks", "collision")
    is_compound <- map_lgl(blocks, ~ "compound" %in% .x$variables$formatKey)
    compound_ids <- cumsum(is_compound)
    is_spectrum <- map_lgl(blocks, ~
          length(intersect(spectrum_markers, .x$variables$formatKey)) > 0)

    blocks <- map2(blocks, compound_ids, function(block, cpd_id) {
      
      if(!("compound" %in% block$variables$formatKey)) {
        
      }
      
      # Get compound info from compound info block,
      # but never overwrite existing info and don't copy the marker line
      block$variables <- bind_rows(list(
          blocks[[cpd_id]]$variables %>% 
            filter(!(formatKey %in% block$variables$formatKey)) %>%
            filter(!(formatKey %in% spectrum_markers)),
          block$variables
          ))
      block
    })
    
    blocks <- blocks[is_spectrum]
    
    # find and drop empty spectra
    has_ions <- map_lgl(blocks, ~ nrow(.x$ions) > 0)
    message(glue("dropping {sum(!has_ions)} spectra without ions"))
    blocks <- blocks[has_ions]
    
    return(blocks)

  }
  return(document)

}




