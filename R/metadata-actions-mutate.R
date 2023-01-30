
#' Metadata action: mutate
#' 
#' Generate a new field from an old field and optional helper fields.
#' This uses `glue` expressions. 
#' TODO: By default, only a limited set of functions should be
#' available, since this otherwise allows arbitrary code execution
#' from a metadata transformation schema.
#' See https://stackoverflow.com/questions/66174947/r-glue-with-limited-set-of-functions-allowed-in-pattern/66177581#66177581
#' Continue on `glue_restricted` above.
#' 
#' @examples 
#' backend <- get_proto_backend(temp_rt = c(4,5,6,40,50,60),
#'                              temp_rt_factor = c(1,1,1,60,60,60))
#' action <- get_proto_action(
#'    "mutate",
#'    source = "temp_rt",
#'    target = "*rtime",
#'    required = "temp_rt_factor"
#'    read = "{temp_rt / temp_rt_factor}",
#'    write = "{rtime * temp_rt_factor}"
#' )
#' fw <- action$execute_read(backend)
#' fw@@variables <- fw@@variables %>% select(-temp_rt)
#' bw <- action$execute_write(fw)
#' 
#' @param data `MsBackendMapping` to execute the metadata mapping step on
#' @param params List of parameters for a single action (one `params` entry fully merged.)
MetadataActionMutate <- R6::R6Class(
  "MetadataActionMutate",
  inherit = MetadataActionBase,
  public = list(
    
    base_settings = list(
      source = c(),
      target = c(),
      #read = '',
      #write = '',
      required = c(),
      trim = FALSE,
      convert = TRUE
    ),
    
    #' @description read implementation
    process_read = function(data, params) {
      
      source <- .v(params$source)
      target <- .v(params$target)
      set_spectra_var <- target[.flag(params$target)]
      
      if(!all(source %in% colnames(data@variables)))
        return(data)
      if(!all(params$required %in% colnames(data@variables)))
        return(data)
      
      
      data@variables <- data@variables %>%
        mutate(!!target := glue(params$read))
      
      t_sym <- sym(target)
      if(params$trim)
        data@variables <- data@variables %>%
        mutate(!!target := str_trim(!!t_sym))
      
      if(params$convert)
        data@variables <- data@variables %>%
        mutate(!!target := type.convert(!!t_sym, as.is = TRUE))
      
      data@spectraVariables <- union(data@spectraVariables, set_spectra_var)
      
      return(data)
    },
    
    #' @description write implementation
    process_write = function(data, params) {
      
      source <- .v(params$source)
      target <- .v(params$target)
      set_source_var <- source[.flag(params$source)]
      
      if(!all(target %in% colnames(data@variables)))
        return(data)
      if(!all(params$required %in% colnames(data@variables)))
        return(data)
      
      if("write" %in% names(params)) {
        if(length(source) == 1)
          data@variables <- data@variables %>%
            mutate(!!source := glue(params$write))
        else
          data@variables <- reduce(source, 
                                   ~ .x %>% mutate(!!.y := glue(params$write[[.y]])),
                                   .init = data@variables)
      }
      
      if(params$convert) {
        walk(source, function(src) {
          s_sym <- sym(src)
          data@variables <- data@variables %>%
            mutate(!!src := type.convert(!!s_sym))   
        })
      }
      
      data@sourceVariables <- union(data@sourceVariables, set_source_var)
      
      return(data)
    }
  ))
