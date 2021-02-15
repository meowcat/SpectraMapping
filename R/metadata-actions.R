#' @import R6
#' @import glue
NULL



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


# Functions to extract the variable name and flag status from transformation yaml entries.
.v <- function(x) {
   str_remove(x, "^\\*")
}
.flag <- function(x, default = FALSE) {
   str_detect(x, '^\\*[^*]') | default
}


MetadataActionBase <- R6::R6Class(
   "MetadataActionBase", 
   public = list(
      
      name = "",
      base_settings = list(),
      settings = list(),
      
      log_level = function(level, message) {
         message <- glue(message, .envir = parent.frame())
         log_level(level, "{self$name}: {message}")
      },
      
      initialize = function(settings) {
         #self$name <- name
         # Take base settings and replace with settings from user where one is provided
         self$settings <- self$base_settings
         self$set_settings(settings)
         #log_info("initialized action: {self$name}, type {self$settings[['action']]}")
      },
      # `execute` is the wrapper around `process`.
      # If there are `params`, i.e. multiple steps are executed, `execute` loops through them.
      # (The basic settings are updated with params, then the action is run.)
      # If there are no params, only a single step is executed (with the "basic" settings.)
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
      # `process` executes a transformation method and returns the transformed backend.
      # Here is where the implementation goes.
      process_read = function(data, params) {
         return(data)
      },
      
      execute_write = function(backend) {
         #log_info("executing action: {self$name}")
         if(!is.null(self$settings$params)) {
            backend <- reduce(rev(self$settings$params), function(data, params) {
               params_ <- self$merge_settings(params)
               data <- self$process_write(data, params_)
               return(data)
            })
         }
         else {
            backend <- self$process_write(data, self$settings)
         }
         return(backend)
      },
      # `process` executes a transformation method and returns the transformed backend.
      # Here is where the implementation goes.
      process_write = function(data, params) {
         return(data)
      },
      
   
      # Set settings and verify that they are OK
      # Any action may/should check settings for consistency
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
   
      # Update settings with step-specific entry
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
#' `source`: field of origin
#' `target`: target field
#' `explicit`: If `TRUE`, a star marker is required to register the output field as a `spectraVariable`.
#'   This is `FALSE` by default, since the main purpose of this action is mapping input to `spectraVariable`s.
#' 
#' @example 
#' 
#' mapping <- get_proto_action("mapping", source='FIELD', target='field')
#' backend <-  get_proto_backend(FIELD=c('1','2','3'))
#' mapped <- mapping$execute_read()
#' 
MetadataActionMapping <- R6::R6Class(
      "MetadataActionMapping",
      inherit = MetadataActionBase,
      public = list(
      base_settings = list(
         source = '',
         target = '',
         explicit = FALSE
      ),
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
      process_write = function(data, params) {
         source <- .v(params$source)
         target <- .v(params$target)
         target_sym <- sym(target)
         set_source_var <- .flag(params$source)
         if(params$target %in% colnames(data@variables)) {
            data@variables <- data@variables %>% mutate(!!source := !!target_sym)
            if(set_source_var)
               data@sourceVariables <- union(data@sourceVariables, source)
         }
      }
))


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
      
      process_read = function(data, params) {
         
         source <- .v(params$source)
         target <- .v(params$target)
         set_spectra_var <- target[.flag(params$target)] # may be multiple vars
         
         read_split <- params$read_split
         if(length(read_split) == 0)
            read_split <- rep("*", length(target))
         
         # concatenate source columns into temp column
         # take the "column" offline for simplicity
         temp_column <- select(all_of(source)) %>% 
            pmap(~c(...)) %>% 
            map(set_names, NULL)
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
               data@variables[[t]] <- map(temp_column, ~.x[indices])
               consumed <- c(consumed, indices)
            } 
         }
         #
         data@spectraVariables <- union(data@spectraVariables, set_spectra_var)
         return(data)
      },
      process_write = function(data, params) {
         
         source <- .v(params$source)
         target <- .v(params$target)
         set_source_var <- source[.flag(params$source)]
         
         write_split <- params$write_split
         if(length(read_split) == 0)
            read_split <- rep("*", length(source))
         
         temp_column <- select(all_of(target)) %>% 
            pmap(~c(...)) %>% 
            map(set_names, NULL)
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
               data@variables[[t]] <- map(temp_column, ~.x[indices])
               consumed <- c(consumed, indices)
            } 
         }
         
         data@sourceVariables <- union(data@sourceVariables, set_source_var)
         
         return(data)
      }
   ))


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
   
   process_read = function(data, params) {
      
      source <- .v(params$source)
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
   process_write = function(data, params) {
      
      source <- .v(params$source)
      target <- .v(params$target)
      set_source_var <- source[.flag(params$source)]
      
      data@variables <- data@variables %>%
         mutate(!!source := glue_data(params$write))
      
      data@sourceVariables <- union(data@sourceVariables, set_source_var)
      
      return(data)
   }
))


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
         trim = FALSE,
         convert = FALSE, 
         fill = "left", # sic! To deal with the non-subtagged comment case.
         extra = "merge"
      ),
      
      process_read = function(data, params) {
         
         source <- .v(params$source)
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
            table <- data@variables[[source]] %>% map(~ .x[-1])
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
      process_write = function(data, params) {
         
         source <- .v(params$source)
         target <- .v(params$target)
         set_source_var <- source[.flag(params$source)]
         
         data@variables <- data@variables %>%
            mutate(!!source := glue_data(params$write))
         
         data@sourceVariables <- union(data@sourceVariables, set_source_var)
         
         return(data)
      }
   ))


MetadataActionMutate <- R6::R6Class(
   "MetadataActionMutate",
   inherit = MetadataActionBase,
   public = list(
      
      base_settings = list(
         source = '',
         target = c(),
         read = '',
         write = '',
         trim = FALSE
      ),
      
      process_read = function(data, params) {
         
         source <- .v(params$source)
         target <- .v(params$target)
         set_spectra_var <- target[.flag(params$target)]
         
         data@variables <- data@variables %>%
            mutate(!!target := glue(params$read))
         
         if(params$trim) {
               t_sym <- sym(target)
               data@variables <- data@variables %>%
                  mutate(!!target := str_trim(!!t_sym))
         }
         
         data@spectraVariables <- union(data@spectraVariables, set_spectra_var)
         
         return(data)
      },
      process_write = function(data, params) {
         
         source <- .v(params$source)
         target <- .v(params$target)
         set_source_var <- source[.flag(params$source)]
         
         data@variables <- data@variables %>%
            mutate(!!source := glue(params$write))
         
         data@sourceVariables <- union(data@sourceVariables, set_source_var)
         
         return(data)
      }
   ))

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
      
      
      process_read = function(data, params) {
         
         
         source <- .v(params$source)
         target <- .v(params$target)
         source_sym <- sym(source)
         target_sym <- sym(target)
         set_spectra_var <- target[.flag(params$target)]
         
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
      
      
      
      process_write = function(data, params) {
         
         
         source <- .v(params$source)
         target <- .v(params$target)
         source_sym <- sym(source)
         target_sym <- sym(target)
         set_source_var <- source[.flag(params$source)]
         
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
            left_join(dictionary_read, by = target) %>%
         
         # If coalesce is set, "translate" unmatched entries verbatim
         if(params$coalesce)
            data@variables <- data@variables %>%
            mutate(!!source := coalesce(!!source_sym, !!target_sym))
         
         data@variables <- data@variables %>%
            chop(c(source, target)) %>%
            mutate(!!source := as.list(source_sym),
                   !!target := as.list(target_sym))
         
         data@sourceVariables <- union(data@sourceVariables, set_source_var)
         
         return(data)
      }
      
   )
)


MetadataActionType<- R6::R6Class(
   "MetadataActionType",
   inherit = MetadataActionBase,
   
   public = list(
      base_settings = list(
         defaults = TRUE,
         source = c(),
         type = c()
      ),
      process_read = function(data, params) {
         
         if(length(params$source) > 0) {
            for(s in params$source) {
               s_sym <- sym(s)
               data@variables <- data@variables %>%
                  mutate(!!s := .transform_function[[params$type]](!!s_sym))
            }
            return(data)   
         }
         
         if(params$defaults) {
            data@variables <- data@fields %>%
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
         
      }
   )
)

#' Metadata action: split
#' 
#' Split (in read direction) a string field into a character vector according to a separator.
#' 
#' @example
#'
MetadataActionSplit <- R6::R6Class(
      "MetadataActionSplit",
      inherit = MetadataActionBase,
      
      public = list(
         base_settings = list(
            source = '',
            sep = c(),
            regex = c(),
            n = Inf
         ),
         
         
         process_read = function(data, params) {
         
            source <- .v(params$source)
            if(length(params$target) == 0)
               target_ <- source
            else
               target_ <- params$target
            target <- .v(target_)
            set_spectra_var <- target[.flag(target_)]
            
            if(length(params$sep) > 0)
               pattern <- fixed(params$sep)
            else
               pattern <- params$sep
            data@variables[[target]] <- data@variables[[source]] %>% str_split(pattern)
            data@spectraVariables <- union(data@spectraVariables, set_spectra_var)
            return(data)
         }
         
         process_write = function(data, params) {
            
            source <- .v(params$source)
            if(length(params$target) == 0)
               target_ <- source
            else
               target_ <- params$target
            target <- .v(target_)
            set_source_var <- source[.flag(params$source)] 
            # 
            # if(length(params$sep) > 0)
            #    pattern <- fixed(params$sep)
            # else
            pattern <- params$sep
            data@variables[[source]] <- data@variables[[target]] %>% map(str_flatten, pattern)
            data@sourceVariables <- union(data@sourceVariables, set_source_var)
            return(data)
         }
         
            
      )
)
         


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
   
   
   process_read = function(data, params) {
      
      source <- .v(params$source)
      prefix <- .v(params$prefix)
      source_sym <- sym(source)
      stopifnot(length(params$prefix) == 1)
      set_spectra_var <- .flag(params$prefix)
      
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
                values_fn = list)
      ) %>% unnest(cols = params$temp_col)
      
      if(set_spectra_var) {
         cols_new <- colnames(data@variables) %>% keep(~str_starts(fixed(prefix))) %>% map_chr(~.x)
         data@spectraVariables <- union(data@spectraVariables, cols_new)
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
   tabular = MetadataActionTabular
)

