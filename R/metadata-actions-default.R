#' Metadata action: default
#' 
#' Sets a default value, on read and/or write, if no value is present for a particular column
#' Creates a column if no column exists.
#' Can be used on read (target), write (source) or both.
#' 
#' @examples 
#' backend <- get_proto_backend(
#'    readCol = list("POSITIVE", c(NA_character_), "n", "gugus", NA_character_),
#'    WRITE_COL = list("value", NA_character_, "value", "otherValue", c())
#' )
#' action <- get_proto_action(
#'    "default",
#'    params = list(
#'       c(target = 'default_new_read_col', read = 'new_read'),
#'       c(target = 'readCol', read = 'pos_default'),
#'       c(source = 'WRITE_COL', write = 'default'),
#'       c(source = 'WRITE_NEW_COL', write = 'one_value')
#' 
#'    )
#' )
#' fw <- action$execute_read(backend)
#' bw <- action$execute_write(fw)
MetadataActionDefault <- R6::R6Class(
  "MetadataActionDefault",
  inherit = MetadataActionBase,
  public = list(
    
    base_settings = list(
      source = c(),
      target = c(),
      read = '',
      write = '',
      type = "list"
    ),
    
    #' @description read implementation
    process_read = function(data, params) {
      
      
      target <- .v(params$target)
      set_spectra_var <- target[.flag(params$target)] # may be multiple vars
      
      if(length(target) > 0) {
        
        if(target %in% colnames(data@variables)) {
          curr_type <- typeof(data@variables[[target]])
          data@variables[[target]] <- data@variables[[target]] %>%
            modify_if(~ length(.x) == 0, ~ NA_character_) %>%
            modify_if(~ all(is.na(.x)), ~ params$read) %>%
            as(curr_type)
        }
        else
          data@variables[[target]] <- rep(params$read, nrow(data@variables)) %>% as(params$type)
        
        data@spectraVariables <- union(data@spectraVariables, set_spectra_var)
      }
      
      return(data)
    },
    
    #' @description write implementation
    process_write = function(data, params) {
      
      source <- .v(params$source)
      set_source_var <- source[.flag(params$source)]
      
      if(length(source) > 0)
      {
        
        if(source %in% colnames(data@variables)) {
          curr_type <- typeof(data@variables[[source]])
          data@variables[[source]] <- data@variables[[source]] %>%
            modify_if(~ length(.x) == 0, ~ NA_character_) %>%
            modify_if(~ all(is.na(.x)), ~ params$write) %>%
            as(curr_type)
        }
        else
          data@variables[[source]] <- rep(params$write, nrow(data@variables)) %>% as(params$type)
        
        data@sourceVariables <- union(data@sourceVariables, set_source_var)
      }
      return(data)
    }
  ))