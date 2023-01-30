#' Metadata action: extract
#' 
#' Converts one metadata column into one or multiple columns based on a separator or on a regex pattern. 
#' Note that the column may be single- or multilined but is typically single-lined. 
#' This is really used e.g. to read a numeric value out of a formatted entry. The demo application is retention times.
#' Contrast with:
#' * `split`: Converts one single-line metadata column into one multiline metadata column based on a separator or regex pattern
#' * `nesting`: Converts one multiline metadata column into multiple columns
#' * `tabular`: Converts one usually multiline metadata column into a tabular column
#' 
#' @examples
#' backend <- get_proto_backend(RT = c("4 min", "30 sec"))
#' action <- get_proto_action(
#'    "extract",
#'    source='RT',
#'    target=c('rt', 'rt_unit'),
#'    read = '([0-9]+)\\s?(.*)',
#'    write = '{rt} {rt_unit}',
#'    convert = TRUE)
#' fw <- action$execute_read(backend)
#' fw@@variables <- fw@@variables %>% select(-RT)
#' bw <- action$execute_write(fw)
#' 
#' @param data `MsBackendMapping` to execute the metadata mapping step on
#' @param params List of parameters for a single action (one `params` entry fully merged.)
MetadataActionExtract <- R6::R6Class(
  "MetadataActionExtract",
  inherit = MetadataActionBase,
  public = list(
    
    base_settings = list(
      # read
      # write
      source = '',
      target = c(),
      trim = FALSE,
      convert = FALSE
    ),
    
    #' @description read implementation
    process_read = function(data, params) {
      
      source <- .v(params$source)
      source <- source[source %in% colnames(data@variables)]
      if(length(source) == 0)
        return(data)
      
      target <- .v(params$target)
      set_spectra_var <- target[.flag(params$target)] # may be multiple vars
      
      if("read" %in% names(params)) {
        data@variables <- data@variables %>%
          extract(!!source, into = target, regex = params$read, 
                  remove = FALSE, convert = params$convert)
        if(params$trim) {
          for(t in target) {
            t_sym <- sym(t)
            data@variables <- data@variables %>%
              mutate(!!t := str_trim(!!t_sym))
          }   
        }
        data@spectraVariables <- union(data@spectraVariables, set_spectra_var)
      }
      
      
      
      return(data)
    },
    
    #' @description write implementation
    process_write = function(data, params) {
      
      source <- .v(params$source)
      target <- .v(params$target)
      if(!all(target %in% colnames(data@variables)))
        return(data)
      
      set_source_var <- source[.flag(params$source)]
      
      if("write" %in% names(params)) {
        data@variables <- data@variables %>%
          mutate(!!source := glue(params$write) %>% as.list())
        
        data@sourceVariables <- union(data@sourceVariables, set_source_var)
      }
      return(data)
    }
  ))