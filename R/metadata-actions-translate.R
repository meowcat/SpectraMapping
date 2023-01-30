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
        map(~list_modify(.x, write = rlang::zap())) %>% 
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
        map(~list_modify(.x, read = rlang::zap())) %>% 
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