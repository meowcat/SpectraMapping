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
      
      .col.prefix <- prefix
      
      fix_chr <- function(x) map(x, as.character)
      
      # possibly faster alternative:
      # first copy out columns to new tibble
      # then perform pivot_longer without nesting
      # then (nest? and) join to original table,
      # removing the original columns
      
      
      
      nesting_data <- data@variables %>%
        select(spectrum_id, starts_with(.col.prefix)) %>%
        mutate(across(starts_with(.col.prefix), as.list))
      
      nesting_long <- nesting_data %>%
        pivot_longer(-spectrum_id,
                     names_to = "key",
                     names_prefix = .col.prefix,
                     values_to = "value"#,
                     #values_transform = list("value" = fix_chr)
        ) %>%
        mutate(value = map(value, as.character)) %>%
        unchop(value) %>%
        group_by(spectrum_id)
      if("order" %in% names(params))
        nesting_long <- nesting_long %>% 
        arrange(factor(key, params$order)) #%>%
      # mutate(rel_index = row_number())
      nesting_long <- nesting_long %>%
        filter(!is.na(value)) %>%
        mutate(col = glue(params$write)) %>%
        chop(-spectrum_id) %>%
        mutate(col = as.list(col)) %>%
        select(spectrum_id, !!source := col)
      
      
      data@variables <- 
        data@variables %>%
        select(-starts_with(.col.prefix)) %>%
        full_join(nesting_long, by="spectrum_id")
      
      # TODO: check that both datasets are complete
      # or actually: don't! why would you?, It can just have NAs.
      # Good question: Should NA values be removed before glue_data?
      # Probably yes. If the user wants NA written, they need to set defaults.
      # Fixed accordingly. (see above)
      
      if(set_source_var) {
        data@sourceVariables <- union(data@sourceVariables, source)
      }
      
      
      return(data)
      
    }
  )
)
