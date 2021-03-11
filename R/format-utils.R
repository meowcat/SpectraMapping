#' @import yaml
#' @import tidyverse
NULL



order_fixed <- function(vec, ordering) {
  vec_as_factor <- factor(vec, levels = ordering)
  order(vec_as_factor)
}


setGeneric("mapVariables", function(sp, ...) {
  stop("mapVariables is not implemented for this object")
})

setMethod("mapVariables", "MsBackendMapping", function(sp, mapping) {
  if(!is.list(mapping)) 
    mapping <- read_yaml(mapping)
  actions <- get_actions(mapping)
  sp@spectraVariables <- character(0)
  sp <- reduce(actions, ~ .y$execute_read(.x), .init = sp)
  sp
} )


setMethod("mapVariables", "Spectra", function(sp, mapping) {
  if(is(sp@backend, "MsBackendMapping")) {
    sp@backend <- mapVariables(sp@backend, mapping)
    return(sp)
  }
    
  else
    stop("mapping for generic backends not yet implemented")
} )



setGeneric("writeVariables", function(sp, ...) {
  stop("writeVariables is not implemented for this object")
})


setMethod("writeVariables", "MsBackendMapping", function(sp, mapping) {
  if(!is.list(mapping)) 
    mapping <- read_yaml(mapping)
  actions <- get_actions(mapping)
  sp@sourceVariables <- character(0)
  sp@variables <- sp@variables %>% select(all_of(sp@spectraVariables))
  sp <- reduce(rev(actions), ~ .y$execute_write(.x), .init = sp)
  sp
} )


setMethod("writeVariables", "Spectra", function(sp, mapping) {
  if(is(sp@backend, "MsBackendMapping")) {
    sp@backend <- writeVariables(sp@backend, mapping)
    return(sp)
  }
  
  else
    stop("mapping for generic backends not yet implemented")
} )
