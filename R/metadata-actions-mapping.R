
#' Metadata action: mapping
#' 
#' Maps a field in the input data to a field in the output data.
#' By default, registers the output field as a spectraVariable.
#' 
#' @details 
#' 
#' 
#' @examples
#' 
#' mapping <- get_proto_action("mapping", source='FIELD', target='field')
#' backend <-  get_proto_backend(FIELD=c('1','2','3'))
#' mapped <- mapping$execute_read(backend)
#' 
#' @param data `MsBackendMapping` to execute the metadata mapping step on
#' @param params List of parameters for a single action (one `params` entry fully merged.)
MetadataActionMapping <- R6::R6Class(
  "MetadataActionMapping",
  inherit = MetadataActionBase,
  public = list(
    
    #' @field base_settings
    #' 
    #' `source`: field of origin
    #' `target`: target field
    #' `explicit`: If `TRUE`, a star marker is required to register the output field as a `spectraVariable`.
    #'   This is `FALSE` by default, since the main purpose of this action is mapping input to `spectraVariable`s.
    base_settings = list(
      source = '',
      target = '',
      explicit = FALSE
    ),
    
    #' @description read implementation
    process_read = function(data, params) {
      source <- .v(params$source)
      target <- .v(params$target)
      source_sym <- sym(source)
      set_spectra_var <- target[.flag(params$target, default = !params$explicit)]
      if(source %in% colnames(data@variables)) {
        data@variables <- data@variables %>% mutate(!!target := !!source_sym)
        
        data@spectraVariables <- union(data@spectraVariables, set_spectra_var)
      }
      return(data)
    },
    
    #' @description write implementation
    process_write = function(data, params) {
      source <- .v(params$source)
      target <- .v(params$target)
      target_sym <- sym(target)
      set_source_var <- .flag(params$source)
      if(target %in% colnames(data@variables)) {
        data@variables <- data@variables %>% mutate(!!source := !!target_sym)
        if(set_source_var)
          data@sourceVariables <- union(data@sourceVariables, source)
      }
      return(data)
    }
  ))
