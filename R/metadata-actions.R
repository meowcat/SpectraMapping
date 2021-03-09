#' @import R6
#' @import glue
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
      message("Action: ", settings$action)
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
         #self$name <- name
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
         #log_info("executing action: {self$name}")
         if(!is.null(self$settings$params)) {
            backend <- reduce(self$settings$params, function(data, params) {
               params_ <- self$merge_settings(params)
               data <- self$process_read(data, params_)
               return(data)
            }, .init = backend)
         }
         else {
            backend <- self$process_read(backend, self$settings)
         }
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
         #log_info("executing action: {self$name}")
         if(!is.null(self$settings$params)) {
            backend <- reduce(rev(self$settings$params), function(data, params) {
               params_ <- self$merge_settings(params)
               data <- self$process_write(data, params_)
               return(data)
            }, .init = backend)
         }
         else {
            backend <- self$process_write(backend, self$settings)
         }
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
         settings_[["params"]] <- NULL
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

#' Metadata action: crossmap
#' 
#' This action is to join and split multientry data.
#' For example, in NIST MSP, `Name` is a single compound name,
#' and all subsequent `Synon` are synonyms. In MassBank data,
#' these need to go all into `CH$NAME`. There are now two ways to do this:
#' * one: the internal representation is the "joined" one. 
#'    On reading from MSP, we merge `Name` and `Synon` to e.g. `names`.
#'    On writing to MassBank, `names` -> `CH$NAME`.
#' * the other: the internal representation is the "separated" one. 
#'    On reading from MSP, `Name` -> `name` and `Synon` -> `synonyms`
#'    On writing to MassBank, we merge `name` and `synonyms` to `CH$NAME`. 
#'    On reading from MassBank, `CH$NAME[1]` to `name`, all other `CH$NAME` entries to `synonyms`. 
#'    
#' This can also *split* a field on *reading*!
#'    
#' @details 
#' 
#' `source`: fields of origin
#' `target`: target fields
#' `read_split`: After joining the `source` fields together, how to distribute them to `target`? 
#'     E.g. `[1, *]` extracts the first entry into the first `target` field and the rest into the second `target` field.
#' `write_split`: Same question when writing.
#' 
#' @examples 
#'
#' # Split on write, join on read
#'  
#' crossmap <- get_proto_action(
#'    "crossmap",
#'    source=c('Name', 'Synon'),
#'    target='names',
#'    read_split = list(names="*"),
#'    write_split = list('Name' = '1', 'Synon' = '*'))
#' 
#' backend <-  get_proto_backend(Name=list('Anton', 'Franz', c()), Synon=list(c(), c('Francis', 'Franziskus'), c()))
#' fw <- crossmap$execute_read(backend)
#' fw@@variables <- fw@@variables %>% select(names)
#' bw <- crossmap$execute_write(fw)
#' 
#' # Split on read, join on write
#' 
#' backend <-  get_proto_backend(CHNAME=list(c('N1', 'N2', 'N3'), c('Francis', 'Franziskus'), c('Nsingle'), c()))
#' crossmap <- get_proto_action(
#'    "crossmap",
#'    source=c('CHNAME'),
#'    target=c('name', 'synonyms'),
#'    read_split = list(name="1", synonyms="*"),
#'    write_split = list(CHNAME="*")
#'    )
#' fw <- crossmap$execute_read(backend)
#' fw@@variables <- fw@@variables %>% select(-CHNAME)
#' bw <- crossmap$execute_write(fw)
#' 
#' @param data `MsBackendMapping` to execute the metadata mapping step on
#' @param params List of parameters for a single action (one `params` entry fully merged.)
MetadataActionCrossmap <- R6::R6Class(
   "MetadataActionCrossmap",
   inherit = MetadataActionBase,
   public = list(
      
      base_settings = list(
         source = c(),
         target = c(),
         read_split = c(),
         write_split = c(),
         reverse = FALSE
      ),
      
      #' @description read implementation
      process_read = function(data, params) {
         
         source <- .v(params$source)
         source <- source[source %in% colnames(data@variables)]
         target <- .v(params$target)
         set_spectra_var <- target[.flag(params$target)] # may be multiple vars
         
         read_split <- params$read_split
         if(length(read_split) == 0)
            read_split <- rep("*", length(target))
         
         
         
         
         # concatenate source columns into temp column
         # take the "column" offline for simplicity
         temp_column <- data@variables %>% select(all_of(source)) %>% 
            pmap(~c(...)) %>% 
            map(maybe_set_names, NULL)
         # reverse, if desired (so one can pick from the tail)
         if(params$reverse)
            temp_column <- map(temp_column, rev)
         # select elements according to read_split
         consumed <- c()
         for(t in target) {
            indices <- read_split[[t]]
            if((indices == "*") & (length(consumed) == 0))
               data@variables[[t]] <- temp_column
            else {
               if (indices == "*") 
                  indices <- -consumed
               else
                  indices <- as.integer(indices)
               data@variables[[t]] <- map(temp_column, ~.x[indices])
               consumed <- c(consumed, indices)
            } 
         }
         #
         data@spectraVariables <- union(data@spectraVariables, set_spectra_var)
         return(data)
      },
      
      #' @description write implementation
      process_write = function(data, params) {
         
         source <- .v(params$source)
         target <- .v(params$target)
         target <- target[target %in% colnames(data@variables)]
         set_source_var <- source[.flag(params$source)]
         
         write_split <- params$write_split
         if(length(write_split) == 0)
            write_split <- rep("*", length(source))
         
         temp_column <- data@variables %>% 
            select(all_of(target)) %>% 
            pmap(~c(...)) %>% 
            map(maybe_set_names, NULL)
         # reverse, if desired (so one can pick from the tail)
         if(params$reverse)
            temp_column <- map(temp_column, rev)
         # select elements according to write_split
         consumed <- c()
         for(t in source) {
            indices <- write_split[[t]]
            if((indices == "*") & (length(consumed) == 0))
               data@variables[[t]] <- temp_column
            else {
               if (indices == "*") 
                  indices <- -consumed
               else
                  indices <- as.integer(indices)
               data@variables[[t]] <- map(temp_column, ~.x[indices])
               consumed <- c(consumed, indices)
            } 
         }
         
         data@sourceVariables <- union(data@sourceVariables, set_source_var)
         
         return(data)
      }
   ))

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
      source = '',
      target = c(),
      read = '',
      write = '',
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
      
      return(data)
   },
   
   #' @description write implementation
   process_write = function(data, params) {
      
      source <- .v(params$source)
      target <- .v(params$target)
      if(!all(target %in% colnames(data@variables)))
         return(data)
      
      set_source_var <- source[.flag(params$source)]
      
      data@variables <- data@variables %>%
         mutate(!!source := glue(params$write))
      
      data@sourceVariables <- union(data@sourceVariables, set_source_var)
      
      return(data)
   }
))

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
               curr_type <- type(data@variables[[target]])
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
               curr_type <- type(data@variables[[target]])
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


#' Metadata action: tabular
#' 
#' Converts a metadata column into a column of data frames. This can be used
#' either to get a "subtag-style" column into a key-value store, or to actually
#' read a table like the MassBank peak annotations. 
#' Note: in some cases, this is probably best prepended with `split`, e.g. for NIST comments.
#' 
#' @examples
#'  
#' # Key-value store reading
#' backend <- get_proto_backend(COMMENT = list(c("INTERNAL_ID 1234", "MYNAME Michele", "formless_nosplit"),
#'                                             c("formless nosplit"),
#'                                             c(),
#'                                             c("INTERNAL_ID 1232", "MYNAME something")))
#' action <- get_proto_action(
#'    "tabular",
#'    source = "COMMENT",
#'    target = "commentData",
#'    regex = "([A-Z_-]*)\\s?(.*)",
#'    write = "{key} {value}"
#' )
#' fw <- action$execute_read(backend)
#' fw@@variables <- fw@@variables %>% select(-COMMENT)
#' bw <- action$execute_write(fw)
#' 
#' Table reading with colnames
#' backend <- get_proto_backend(
#'    PKANNOT = list(
#'       c("index m/z tentative_formula formula_count mass error(ppm)",
#'         "1 78.9189 Br- 1 78.9189 -0.33",
#'         "2 193.9252 C7HBrNO- 1 193.9247 2.32",
#'         "3 273.8506 C7H2Br2NO- 1 273.8509 -1.06"),
#'       c("index m/z tentative_formula formula_count mass error(ppm)",
#'         "1 78.9188 Br- 1 78.9189 -0.71",
#'         "2 193.9246 C7HBrNO- 1 193.9247 -0.56",
#'         "3 273.8512 C7H2Br2NO- 1 273.8509 1.16"),
#'       c(),
#'       c("index m/z tentative_formula formula_count mass error(ppm)")
#'    ))
#' action <- get_proto_action(
#'    "tabular",
#'    source = "PKANNOT",
#'    target = "annotation",
#'    sep= " ",
#'    header= 1
#' )
#' fw <- action$execute_read(backend)
#' fw@@variables <- fw@@variables %>% select(-PKANNOT)
#' bw <- action$execute_write(fw)
#' 
#' @param data `MsBackendMapping` to execute the metadata mapping step on
#' @param params List of parameters for a single action (one `params` entry fully merged.)
MetadataActionTabular <- R6::R6Class(
   "MetadataActionTabular",
   inherit = MetadataActionBase,
   public = list(
      
      
      base_settings = list(
         source = '',
         target = c(),
         header = NA,
         sep = c(),
         regex = c(),
         write = c(),
         trim = FALSE,
         convert = FALSE, 
         fill = "left", # sic! To deal with the non-subtagged comment case.
         extra = "merge"
      ),
      
      
      #' @description read implementation
      process_read = function(data, params) {
         
         source <- .v(params$source)
         if(!all(source %in% colnames(data@variables)))
            return(data)            
         
         
         if(length(params$target) == 0)
            target_ <- source
         else
            target_ <- params$target
         target <- .v(target_)
         set_spectra_var <- target[.flag(target_)] # may be multiple vars
         
         
         # 
         
         # Decide on header
         # NA: use "key", "value"
         # 1: use first line
         # a list: use the specified list
         if(all(is.na(params$header))) {
            header <- rep_along(table, list(c("key", "value")))
            table <- data@variables[[source]]
         }
         else if(all(params$header == 1)) {
            header_ <- data@variables[[source]] %>% map(~ .x[1])
            table <- data@variables[[source]] %>% 
               map(~ .x[-1]) %>%
               modify_if(is.null, ~ character(0))
            if(length(params$regex) > 0)
               header <- str_match(header_, params$regex) %>% `[`(1,-1)
            else
               header <- str_split(header_, params$sep)
         }
         else {
            header <- rep_along(table, params$header)
            table <- data@variables[[source]]
         }

         
         
            
         temp_col <- table %>% map(~ tibble(col = .x))
         
         if(length(params$regex) > 0) {
            data@variables[[target]] <- map2(
               temp_col, header,
               ~ .x %>% tidyr::extract(col, into = .y, regex = params$regex, convert = params$convert)
            )
         }
         else if(length(params$sep) > 0) {
            #message('Tabular: using sep')
            data@variables[[target]] <- map2(
               temp_col, header,
               ~ .x %>% separate(col, into = .y, sep = params$sep, convert = params$convert)
            )
         } 
         
         if(params$trim) {
            data@variables[[target]] <- data_variables[[target]] %>%
               map(function(t) t %>% mutate(across(.fns = ~ str_trim(.x))))
         }
         
         
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
         
         if(!all(target %in% colnames(data@variables)))
            return(data)
         
         target_sym <- sym(target)
         set_source_var <- source[.flag(params$source)]
         
         if(length(params$write) > 0)
            data@variables <- data@variables %>%
               mutate(!!source := map(!!target_sym, ~.x %>% glue_data(params$write)) %>% as.character())
         else {
            temp_col <- data@variables[[target]] %>% map(~.x %>% unite("col", sep=params$sep)) %>% map(~ pull(.x, "col"))
            if(params$header == 1) {
               headers <- data@variables[[target]] %>% 
                  map(colnames) %>% 
                  modify_if(~ all(.x == "NULL"), ~ character(0)) %>%
                  map(paste, collapse = params$sep) %>%
                  modify_if(~.x == "", ~ character(0))
               temp_col <- map2(temp_col, headers, ~c(.y, .x))
            }
            data@variables[[source]] <- temp_col
         }
            
            
         data@sourceVariables <- union(data@sourceVariables, set_source_var)
         
         return(data)
      }
   ))

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
         read = '',
         write = '',
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
         
         data@variables <- data@variables %>%
            mutate(!!source := glue(params$write))
         
         s_sym <- sym(source)
         
         if(params$convert)
            data@variables <- data@variables %>%
            mutate(!!source := type.convert(!!s_sym))
         
         data@sourceVariables <- union(data@sourceVariables, set_source_var)
         
         return(data)
      }
   ))

#' Metadata action: translate
#' 
#' Performs a dictionary-style translation action for an entry.
#' In the reading direction, multiple entries may be mapped to one output value;
#' in the writing direction, a single value needs to be specified.
#' If `coalesce` is set, untranslated values will be copied verbatim.
#' 
#' @examples
#' 
#' backend <- get_proto_backend(MS_TYPE = c("POSITIVE", "P", "n", "gugus", NA))
#' action <- get_proto_action(
#'    "translate",
#'    source = "MS_TYPE",
#'    target = "*msLevel",
#'    dictionary = list(
#'    list(value = 1, read = c("POSITIVE", "P", "p", "pos"), write = "POSITIVE"),
#'    list(value = 0, read = c("NEGATIVE", "N", "n", "neg"), write = "NEGATIVE")
#'    )
#' )
#' fw <- action$execute_read(backend)
#' fw@@variables <- fw@@variables %>% select(-MS_TYPE)
#' 
#' # Example with `coalesce`: 
#' # Note that `coalesce` only works when source and destination type are equal.
#' 
#' backend <- get_proto_backend(MS_TYPE = c("POSITIVE", "P", "n", "gugus", NA))
#' action <- get_proto_action(
#'    "translate",
#'    source = "MS_TYPE",
#'    target = "*msLevel",
#'    coalesce = TRUE,
#'    dictionary = list(
#'    list(value = "1", read = c("POSITIVE", "P", "p", "pos"), write = "POSITIVE"),
#'    list(value = "0", read = c("NEGATIVE", "N", "n", "neg"), write = "NEGATIVE")
#'    )
#' )
#' 
#' fw <- action$execute_read(backend)
#' fw@@variables <- fw@@variables %>% select(-MS_TYPE)
#' bw <- action$execute_write(fw)
#' 
#' @param data `MsBackendMapping` to execute the metadata mapping step on
#' @param params List of parameters for a single action (one `params` entry fully merged.)
MetadataActionTranslate <- R6::R6Class(
   "MetadataActionTranslate",
   inherit = MetadataActionBase,
   
   public = list(
      base_settings = list(
         source = '',
         target = '',
         dictionary = list(),
         coalesce = FALSE
      ),
      
      #' @description read implementation
      process_read = function(data, params) {
         
         
         source <- .v(params$source)
         target <- .v(params$target)
         source_sym <- sym(source)
         target_sym <- sym(target)
         set_spectra_var <- target[.flag(params$target)]
         
         if(!all(source %in% colnames(data@variables)))
            return(data)
         
         # extract the reads from the dictionary
         dictionary_read <- params$dictionary %>% 
            map(~list_modify(.x, write = NULL)) %>% 
            map_dfr(as_tibble) %>%
            rename(!!source := read,
                   !!target := value)
         
         if(target %in% colnames(data@variables))
            data@variables <- data@variables %>% select(-all_of(target))
         
         # Translate by join
         # Note: The source columns are natively lists, which is good,
         # because they may be one or more elements to translate.
         # Therefore, first unnest the source column, then translate it.
         # Finally, the columns need to be made into list columns again; 
         # chop() does that (nest() doesn't, it makes an internal tibble).
         # In between, we have to check if we set up a coalesce() handling for NA, 
         # since this works on the unnested/unchopped full length column.
         # We mutate the fancy list_of columns back to simple list(),
         # because I don't know yet how to handle list_of columns
         # in the final datatype assignment step.
         
         data@variables <- data@variables %>%
            unnest(!!source) %>%
            left_join(dictionary_read, by = source)
         
         # If coalesce is set, "translate" unmatched entries verbatim
         if(params$coalesce)
            data@variables <- data@variables %>%
               mutate(!!target := coalesce(!!target_sym, !!source_sym))
         
         data@variables <- data@variables %>%
            chop(c(source, target)) %>%
            mutate(!!source := as.list(!!source_sym),
                   !!target := as.list(!!target_sym))
         
         data@spectraVariables <- union(data@spectraVariables, set_spectra_var)
         return(data)
      },
      
      
      #' @description write implementation
      process_write = function(data, params) {
         
         
         source <- .v(params$source)
         target <- .v(params$target)
         source_sym <- sym(source)
         target_sym <- sym(target)
         set_source_var <- source[.flag(params$source)]
         
         if(!all(target %in% colnames(data@variables)))
            return(data)
         
         
         # extract the reads from the dictionary
         dictionary_write <- params$dictionary %>% 
            map(~list_modify(.x, read = NULL)) %>% 
            map_dfr(as_tibble) %>%
            rename(!!source := write,
                   !!target := value)
         
         if(source %in% colnames(data@variables))
            data@variables <- data@variables %>% select(-all_of(source))
         
         # Translate by join. See above for the logic. The only difference is that
         # we always have only one "write" per "value", but that doesn't change
         # anything for the process.
         
         data@variables <- data@variables %>%
            unnest(!!target) %>%
            left_join(dictionary_write, by = target)
         
         # If coalesce is set, "translate" unmatched entries verbatim
         if(params$coalesce)
            data@variables <- data@variables %>%
            mutate(!!source := coalesce(!!source_sym, !!target_sym))
         
         data@variables <- data@variables %>%
            chop(c(source, target)) %>%
            mutate(!!source := as.list(!!source_sym),
                   !!target := as.list(!!target_sym))
         
         data@sourceVariables <- union(data@sourceVariables, set_source_var)
         
         return(data)
      }
      
   )
)

#' Metadata action: type
#' 
#' Convert a list column into a typed column.
#' 
#' @examples 
#' backend <- get_proto_backend(
#'    MS_TYPE = list("POSITIVE", c(NA_character_), "n", "gugus", NA_character_)
#' )
#' action <- get_proto_action(
#'    "type",
#'    field = "MS_TYPE",
#'    type = "character"
#' )
#' fw <- action$execute_read(backend)
#' bw <- action$execute_write(backend)
MetadataActionType <- R6::R6Class(
   "MetadataActionType",
   inherit = MetadataActionBase,
   
   public = list(
      base_settings = list(
         defaults = TRUE,
         field = c(),
         type = c(),
         omit = c()
      ),
      
      #' @description read implementation
      process_read = function(data, params) {

         if(length(params$field) > 0) {
            
            field <- params$field[params$field %in% colnames(data@variables)]
            
            for(s in field) {
               s_sym <- sym(s)
               data@variables <- data@variables %>%
                  mutate(!!s := .transform_function[[params$type]](!!s_sym))
            }
            return(data)   
         }
         
         if(params$defaults) {
            
            fields <- data@fields %>% filter(
               !(spectraKey %in% params$omit))
               
            data@variables <- fields %>%
               rowwise() %>%
               group_split() %>%
               reduce(function(data_, field) {
                  field_ <- as.list(field)
                  col <- sym(field_$spectraKey)
                  fun <- .transform_function[[field_$dataType]]
                  if(field_$spectraKey %in% data@spectraVariables)
                     data_ <- data_ %>%
                        mutate(!!col := fun(!!col))
                  return(data_)
               }, .init = data@variables)
            return(data)     
         }
         
      },
      
      
      #' @description write implementation
      process_write = function(data, params) {
         
         if(length(params$field) > 0) {
            
            field <- params$field[params$field %in% colnames(data@variables)]
            
            for(s in field) {
               s_sym <- sym(s)
               data@variables <- data@variables %>%
                  mutate(!!s := as.list(!!s_sym))
            }
            return(data)   
         }
         
         if(params$defaults) {
            
            
            fields <- data@fields %>% filter(
               !(spectraKey %in% params$omit))
               
            data@variables <- fields %>%
               rowwise() %>%
               group_split() %>%
               reduce(function(data_, field) {
                  field_ <- as.list(field)
                  col <- sym(field_$spectraKey)
                  fun <- .transform_function[[field_$dataType]]
                  if(field_$spectraKey %in% data@spectraVariables)
                     data_ <- data_ %>%
                     mutate(!!col := as.list(!!col))
                  return(data_)
               }, .init = data@variables)
            return(data)     
         }
         
      }
   )
)

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
         

#' Metadata action: nesting
#' 
#' Convert a list of "key-value pair" strings (e.g. MassBank subtags) to prefixed columns.
#'
#' @param data `MsBackendMapping` to execute the metadata mapping step on
#' @param params List of parameters for a single action (one `params` entry fully merged.)
#'
# backend <- get_proto_backend(
#    a=c(1,2,3, 4),
#    ms = list(
#       list("ION 4", "BLUB 5", "GAGA six"),
#       list("ION 7", "BLUB 8", "GAGA nine"),
#       list("ION 10", "BLUB eleven", "NOGAGAHERE 10000"),
#       list("BLUB 12", "GAGA thirteen")
#    ),
#    c = c("a", "b", "c", "d")
# )
# action <- get_proto_action(
#    "nesting",
#    source = "ms",
#    prefix = "MS_",
#    read = "(.*?)\\s(.*)",
#    write = "{key} {value}"
#    )
# fw <- action$execute_read(backend)
# fw@variables <- fw@variables %>% select(-ms)
# bw <- action$execute_write(fw)
MetadataActionNest <- R6::R6Class(
   "MetadataActionNest",
   inherit = MetadataActionBase,
   
   public = list(
   base_settings = list(
      source = '',
      prefix = '',
      read = '',
      write = '',
      temp_col = '_temp',
      temp = FALSE,
      remove_orig = TRUE
   ),
   
   #' @description read implementation
   process_read = function(data, params) {
      
      source <- .v(params$source)
      prefix <- .v(params$prefix)
      source_sym <- sym(source)
      stopifnot(length(params$prefix) == 1)
      set_spectra_var <- .flag(params$prefix)
      
      if(!all(source %in% colnames(data@variables)))
         return(data)
      
      data@variables <- data@variables %>%
         select(-starts_with(prefix)) %>%
         mutate(
         !!params$temp_col := !!source_sym  %>% 
            map( ~ tibble(value = .x)) %>% 
            map( ~ extract(.x, value, regex = params$read, into = c("key", "value"))) %>%
            map(pivot_wider, 
                names_from = "key", 
                names_prefix = prefix, 
                values_from = "value",
                values_fn = list,
                values_fill = NA)
         ) %>% 
         unnest(cols = params$temp_col) %>%
         mutate(across(starts_with(prefix), 
                       ~ modify_if(.x, ~length(.x) == 0, ~ NA_character_)
                       ))
         
      
      if(set_spectra_var) {
         cols_new <- colnames(data@variables) %>% keep(~str_starts(fixed(prefix))) %>% map_chr(~.x)
         data@spectraVariables <- union(data@spectraVariables, cols_new)
      }
      
      
      return(data)
   },
   
   #' @description write implementation
   process_write = function(data, params) {
      
      source <- .v(params$source)
      prefix <- .v(params$prefix)
      source_sym <- sym(source)
      
      stopifnot(length(params$prefix) == 1)
      set_source_var <- .flag(params$source)
      
      # Are there any columns to nest?
      if(sum(str_starts(colnames(data@variables), fixed(prefix))) == 0)
         return(data)
      
      data@variables <- data@variables %>%
         mutate(across(starts_with(prefix), as.character)) %>%
         nest(cols = starts_with(prefix)) %>%
         mutate(cols = cols %>%
                   map(
                      pivot_longer,
                      everything(),
                      names_to = "key",
                      names_prefix = prefix,
                      values_to = "value") %>%
                  map(~ .x %>% filter(!is.na(value)) %>% glue_data(params$write))) %>%
         rename(!!source := cols)
      
      if(set_source_var) {
         data@sourceVariables <- union(data@sourceVariables, source)
      }
      
      
      return(data)
   }
   )
)





.actions_registry <- list(
   mapping = MetadataActionMapping,
   nesting = MetadataActionNest,
   extract = MetadataActionExtract,
   translate = MetadataActionTranslate,
   crossmap = MetadataActionCrossmap,
   type = MetadataActionType,
   mutate = MetadataActionMutate,
   tabular = MetadataActionTabular,
   split = MetadataActionSplit,
   default = MetadataActionDefault
)

