
#' Metadata action: split
#' 
#' Split (in read direction) a string field into a character vector according to a separator.
#' 
#' @examples
# backend <- get_proto_backend(Comment = list("Hans=Gammel, Fritz=Foerster,Max=gurke", "", "RT=5.2", "nomatch", c()))
# action <- get_proto_action(
#    "split",
#    source = "Comment",
#    target = "Comments",
#    read = ',\\s?',
#    write = ', ',
#    trim = FALSE
#    )
# 
# fw <- action$execute_read(backend)
# fw@variables <- fw@variables %>% select(-Comment)
# bw <- action$execute_write(fw)
#'
#'
#' @param data `MsBackendMapping` to execute the metadata mapping step on
#' @param params List of parameters for a single action (one `params` entry fully merged.)
MetadataActionSplit <- R6::R6Class(
  "MetadataActionSplit",
  inherit = MetadataActionBase,
  
  public = list(
    base_settings = list(
      source = c(),
      target = c(),
      read = c(),
      write = c(),
      trim = TRUE,
      n = Inf
    ),
    
    #' @description read implementation
    process_read = function(data, params) {
      
      source <- .v(params$source)
      if(length(params$target) == 0)
        target_ <- source
      else
        target_ <- params$target
      target <- .v(target_)
      set_spectra_var <- target[.flag(target_)]
      
      if(!all(source %in% colnames(data@variables)))
        return(data)
      
      
      pattern <- params$read
      data@variables[[target]] <- data@variables[[source]] %>% 
        modify_if(~ length(.x) == 0, ~ NA_character_) %>%
        str_split(pattern)
      if(params$trim)
        data@variables[[target]] <- data@variables[[target]] %>% map(str_trim)
      data@spectraVariables <- union(data@spectraVariables, set_spectra_var)
      return(data)
    },
    
    #' @description write implementation
    process_write = function(data, params) {
      
      source <- .v(params$source)
      if(length(params$target) == 0)
        target_ <- source
      else
        target_ <- params$target
      target <- .v(target_)
      set_source_var <- source[.flag(params$source)] 
      
      if(!all(target %in% colnames(data@variables)))
        return(data)
      
      # 
      # if(length(params$sep) > 0)
      #    pattern <- fixed(params$sep)
      # else
      pattern <- params$write
      data@variables[[source]] <- data@variables[[target]] %>% 
        modify_if(~ length(.x) > 0, ~ str_flatten(.x, pattern))
      data@sourceVariables <- union(data@sourceVariables, set_source_var)
      return(data)
    }
    
    
  )
)
