
.fix_list <- function(l, target_type) {
  if(target_type == "list")
    return(l)
  l_null <- map_lgl(l, is.null)
  l[l_null] <- .transform_function[[target_type]](NA)
  return(unlist(l))
}

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
          if(is.list(data@variables %>% pull(s)))
            data@variables <- data@variables %>%
              mutate(!!s_sym := .fix_list(data@variables %>% pull(s), params$type))
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
              mutate(!!col := !!col %>% .fix_list(field_$dataType) %>% fun())
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
            mutate(!!s := as.list(as.character(!!s_sym)))
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