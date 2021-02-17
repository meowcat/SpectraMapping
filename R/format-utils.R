#' @import yaml
#' @import tidyverse
NULL


setGeneric("mapVariables", function(sp, ...) {
  stop("mapVariables is not implemented for this object")
})

setMethod("mapVariables", "MsBackendMapping", function(sp, mapping) {
  if(!is.list(mapping)) 
    mapping <- read_yaml(mapping)
  actions <- get_actions(mapping)
  sp <- reduce(actions, ~ .y$execute_read(.x), .init = sp)
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
  sp <- reduce(rev(actions), ~ .y$execute_write(.x), .init = sp)
} )


setMethod("writeVariables", "Spectra", function(sp, mapping) {
  if(is(sp@backend, "MsBackendMapping")) {
    sp@backend <- writeVariables(sp@backend, mapping)
    return(sp)
  }
  
  else
    stop("mapping for generic backends not yet implemented")
} )
