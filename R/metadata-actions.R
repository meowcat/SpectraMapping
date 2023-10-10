#' @import R6
#' @import glue
#' @import logger
NULL


#' 
#' Get proto-Backend object
#' 
#' Get a minimally filled `MsBackendMapping` with some `variables` filled to run unittests etc.
#' 
#' @param n the number of spectra to fake
#' @param peaks the number of peaks per spectrum
#' @param variables a tibble with variables set
#'
#' @return
#' @export
#'
#' @examples
get_proto_backend <- function(variables = NULL, peaks = 4, ...) {
   if(is.null(variables))
      variables <- tibble(...)
   n <- nrow(variables)
   be <- new("MsBackendMapping")
   mz_ <- rerun(n, runif(peaks, min = 60, max = 600))
   intensity_ <- rerun(n, rexp(peaks) * 1e4)
   be@peaks <- map2(mz_, intensity_,  ~ tibble(mz=.x, int=.y)) %>% bind_rows(.id='spectrum_id')
   be@variables <- variables %>% rowid_to_column("spectrum_id")
   return(be)
}

#' Get a proto-action
#' 
#' Get an action ad-hoc from a parameter list for use in unit tests etc.
#'
#' @param action The action name
#' @param ... Params
#'
#' @return
#' @export
#'
#' @examples
get_proto_action <- function(action, ...) {
   params <- (list(
      action = action,
      ...
   ))
   get_actions(list(params))[[1]]
}


get_actions <- function(workflow) {
   map(workflow, function(settings) {
      #message("Action: ", settings$action)
      action_module <- settings[["action"]]
      action <- .actions_registry[[action_module]]$new(
         #name = name,
         settings = settings
      )
      action
   })
}

# Tolerant helpers
maybe_set_names <- function(x, ...) {
   if(length(x) > 0)
      return(set_names(x, ...))
   else
      return(x)
}

# 
# glue_restricted <- function(glue_str) {
#    #env <- enquo(env)
#    env <- parent.frame(1)$.data
#    safe_fun <- lst(`*`, `/`, round)
#    safe_env <- list2env(c(env, safe_fun), parent = emptyenv())
#    glue(glue_str, .envir = safe_env)
# }
# 
# df <- tibble(a = c(70,80,90,4,5), conversionunit = c(60,60,60,1,1))
# pattern <- "{a/conversionunit} minutes" # loaded from user's config file
# df <- df %>% mutate(output = glue_restricted("{a/conversionunit} minutes"))
# 

# Functions to extract the variable name and flag status from transformation yaml entries.
.v <- function(x) {
   str_remove(x, "^\\*")
}
.flag <- function(x, default = FALSE) {
   str_detect(x, '^\\*[^*]') | default
}

#' Base class for metadata actions
#' 
#' This class is the base for metadata actions.
#' Any action will be executed by calling the method `execute_read` in the read direction, and
#' `execute_write` in the write direction. Actions are parametrized with the `settings` field;
#' the `settings` are set by merging the class-specific `base_settings` with the partial or full
#'  `settings` passed on construction (i.e. base settings are overridden by instance settings).
#'  
#' Instance settings are composed from base settings (which override the base settings
#' and optional `params` "sub-settings". The action is executed once for every
#' `params` sub-settings, which are applied on top of the instance base settings.
#' 
#' This sounds complicated, but is quite simple. Examples:
#' 
#' ```
#' action: mapping
#' explicit: TRUE
#' params:
#' - {source: FIELD1, target: field1}
#' - {source: FIELD2, target: field2}
#' ```
#' 
#' Here, `FIELD1` is mapped to `field1`, `FIELD2` is mapped to `field2`, and
#' `explicit = TRUE` is valid for both entries. Specific entries may override instance settings:
#'  
#' ```
#' action: mapping
#' explicit: TRUE
#' params:
#' - {source: FIELD1, target: field1}
#' - {source: FIELD2, target: field2, explicit: FALSE}
#' - {source: FIELD3, target: field3}
#' ```
#' Here, `explicit = TRUE` is valid for the first and third entries.
#' 
#' If no `params` are given, a single action is executed using instance settings:
#' ```
#' action: mapping
#' explicit: TRUE
#' source: field1
#' target: FIELD1
#' ```
#' 
#' Note that `explicit=TRUE` overrides the `base_settings` value of `explicit=FALSE`. Not setting
#' `explicit` at all would result in `explicit=FALSE` behaviour.
#'  
#'  
#' Implementing classes should 
#' - specify appropriate `base_settings`
#' - implement `process_read` and `process_write`, which process a *single* action
#' (i.e. one `params` entry). The action obtains the `data` input (an `MsBackendMapping`)
#' and a `params`, which is the merged (sic!) version of parameters. I.e., it contains 
#' the `base_settings` overridden by the `settings` overridden by the `params` entry. 
#' Both return the modified `data` (i.e. an `MsBackendMapping` object) again.
#' 
#' 
#' @param data `MsBackendMapping` to execute the metadata mapping step on
#' @param params List of parameters for a single action (one `params` entry fully merged.)
#' 
#' TODO: introduce reverse functionality
MetadataActionBase <- R6::R6Class(
   "MetadataActionBase", 
   public = list(
      
      #' @field name Action name
      name = "",
      
      #' @field base_settings Class base settings
      base_settings = list(),
      
      #' @field settings Instance settings
      settings = list(),
      
      #' @description Log a message with specified level
      #' @param level Log level, as specified in package ???
      #' @param message Message to log
      log_level = function(level, message) {
         message <- glue(message, .envir = parent.frame())
         log_level(level, "{self$name}: {message}")
      },
      
      #' @description Constructor
      #' @param settings List of class-specific settings which override the base settings.
      initialize = function(settings) {
         self$name <- settings$action
         # Take base settings and replace with settings from user where one is provided
         self$settings <- self$base_settings
         self$set_settings(settings)
         #log_info("initialized action: {self$name}, type {self$settings[['action']]}")
      },
      
      #' @description Execution wrapper: read
      #' @param backend `MsBackendMapping` to execute the metadata mapping step on
      #' `execute` is the wrapper around `process`.
      #' If there are `params`, i.e. multiple steps are executed, `execute` loops through them.
      #' (The basic settings are updated with params, then the action is run.)
      #' If there are no params, only a single step is executed (with the "basic" settings.)
      execute_read = function(backend) {
         time_action_start <- Sys.time()
         if(getOption("SpectraMapping")$verbose >= 2)
            self$log_level(INFO, "executing (read)")
         if(!is.null(self$settings$params)) {
            backend <- reduce(self$settings$params, function(data, params) {
               params_ <- self$merge_settings(params)
               if(getOption("SpectraMapping")$verbose >= 3)
                 self$log_level(INFO, glue("params: {str(params_)}"))
               if("debug" %in% names(params_))
                 if("read" %in% params_$debug)
                   browser()
               data <- self$process_read(data, params_)
               return(data)
            }, .init = backend)
         }
         else {
           if(getOption("SpectraMapping")$verbose >= 3)
             self$log_level(INFO, glue("params: {str(self$settings)}"))
           if("debug" %in% names(self$settings))
             if("read" %in% self$settings$debug)
               browser()
            backend <- self$process_read(backend, self$settings)
         }
         time_action_end <- Sys.time()
         time_action <- time_action_end - time_action_start
         time_action <- difftime(time_action_end, time_action_start, units = "secs") %>% as.numeric()
         if(getOption("SpectraMapping")$verbose >= 2)
            log_level(INFO, "elapsed: {round(time_action, 1)} seconds")
         return(backend)
      },
      
      #' @description Action implementation
      #' @param data `MsBackendMapping` to execute the metadata mapping step on
      #' @param params List of parameters for a single action (one `params` entry fully merged.)
      #' `process` executes a transformation method and returns the transformed backend.
      #'
      #'  Here is where the implementation goes.
      process_read = function(data, params) {
         return(data)
      },
      
      
      #' @description Execution wrapper: write
      #' @param backend `MsBackendMapping` to execute the metadata mapping step on
      #' `execute` is the wrapper around `process`.
      #' If there are `params`, i.e. multiple steps are executed, `execute` loops through them.
      #' (The basic settings are updated with params, then the action is run.)
      #' If there are no params, only a single step is executed (with the "basic" settings.)
      execute_write = function(backend) {
         time_action_start <- Sys.time()
         if(getOption("SpectraMapping")$verbose >= 2)
            self$log_level(INFO, "executing (write)")
         
         if(!is.null(self$settings$params)) {
            backend <- reduce(rev(self$settings$params), function(data, params) {
               params_ <- self$merge_settings(params)
               if(getOption("SpectraMapping")$verbose >= 3)
                 self$log_level(INFO, glue("params: {str(params_)}"))
               if("debug" %in% names(params_))
                 if("write" %in% params_$debug)
                  browser()
               data <- self$process_write(data, params_)
               return(data)
            }, .init = backend)
         }
         else {
           if(getOption("SpectraMapping")$verbose >= 3)
             self$log_level(INFO, glue("params: {str(self$settings)}"))
           if("debug" %in% names(self$settings))
             if("write" %in% self$settings$debug)
               browser()
            backend <- self$process_write(backend, self$settings)
         }
         time_action_end <- Sys.time()
         time_action <- difftime(time_action_end, time_action_start, units = "secs") %>% as.numeric()
         if(getOption("SpectraMapping")$verbose >= 2)
            self$log_level(INFO, "elapsed: {round(time_action, 1)} seconds")
         
         
         return(backend)
      },
      
      #' @description Action implementation
      #' @param data `MsBackendMapping` to execute the metadata mapping step on
      #' @param params List of parameters for a single action (one `params` entry fully merged.)
      #' `process` executes a transformation method and returns the transformed backend.
      #'
      #'  Here is where the implementation goes.
      process_write = function(data, params) {
         return(data)
      },
      
   
      #' @description Set settings and verify that they are OK
      #' 
      #' Any action may/should check settings for consistency
      #' 
      #' @param settings List of settings, subclass-specific
      set_settings = function(settings) {
         self$settings <- do.call(list_modify, 
                                  c(list(self$base_settings), 
                                    settings)
         )
         # unknown_settings <- setdiff(names(settings), names(self$base_settings))
         # if(length(unknown_settings) > 0) {
         #    self$log_level(WARN, "some settings are not recognized and will be ignored:")
         #    for(setting in unknown_settings)
         #       log_warn(setting)
         # }
      },
   
      #' @description Update settings with step-specific entry
      #' @param param_settings A `params` entry from `settings`, i.e. a single action step.
      merge_settings = function(param_settings) {
         settings_ <- self$settings
         settings_[["params"]] <- rlang::zap()
         settings <- do.call(list_modify, 
                                  c(list(settings_), 
                                    param_settings)
         )
         # unknown_settings <- setdiff(names(settings), names(self$base_settings))
         # if(length(unknown_settings) > 0) {
         #    self$log_level(WARN, "some settings are not recognized and will be ignored:")
         #    for(setting in unknown_settings)
         #       log_warn(setting)
         # }
         return(settings)
      }
   )
)