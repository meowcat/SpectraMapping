#' Metadata action: order
#' 
#' Order data in export
#' 
#' 
MetadataActionOrder <- R6::R6Class(
  "MetadataActionOrder",
  inherit = MetadataActionBase,
  public = list(
    
    base_settings = list(
      source = c(),
      target = c(),
      read = '',
      write = '',
      required = c(),
      trim = FALSE,
      convert = TRUE
    ),
    
    #' @description read implementation
    process_read = function(data, params) {
      # Do nothing during read
      return(data)
    },
    
    #' @description write implementation
    process_write = function(data, params) {
      
      varOrder <- params$order
      vars <- data@sourceVariables %>% factor(levels = varOrder) %>% sort() %>% as.character()
      data@sourceVariables <- vars
      
      return(data)
    }
  ))