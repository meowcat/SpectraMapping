#' @import yaml
#' @import tidyverse
NULL



order_fixed <- function(vec, ordering) {
  vec_as_factor <- factor(vec, levels = ordering)
  order(vec_as_factor)
}

#' @export
setGeneric("mapVariables", function(sp, ...) {
  stop("mapVariables is not implemented for this object")
})

#' @export
setMethod("mapVariables", "MsBackendMapping", function(sp, mapping, mode=c("read", "write"), clear=FALSE) {
  if(mode[1] == "read")
    return(.mapVariables_read(sp, mapping, clear))
  else if(mode[1] == "write")
    return(.mapVariables_write(sp, mapping, clear))
} )



.mapVariables_read <- function(sp, mapping, clear) {
  if(!is.list(mapping)) 
    mapping <- read_yaml(mapping)
  actions <- get_actions(mapping)
  if(clear)
    sp@spectraVariables <- character(0)
  time_action_start <- Sys.time()
  if(getOption("SpectraMapping")$verbose >= 1)
    log_level(INFO, "mapping variables")
  sp <- reduce(actions, ~ .y$execute_read(.x), .init = sp)
  time_action_end <- Sys.time()
  time_action <- (time_action_end - time_action_start) %>% as.numeric()
  if(getOption("SpectraMapping")$verbose >= 1)
    log_level(INFO, "mapping done, elapsed: {round(time_action, 1)} seconds")
  sp
}

.mapVariables_write <- function(sp, mapping, clear) {
  if(!is.list(mapping)) 
    mapping <- read_yaml(mapping)
  actions <- get_actions(mapping)
  time_action_start <- Sys.time()
  if(getOption("SpectraMapping")$verbose >= 1)
    log_level(INFO, "mapping variables")
  if(clear)
    sp@sourceVariables <- character(0)
  sp@variables <- sp@variables %>% select(all_of(sp@spectraVariables), spectrum_id)
  sp <- reduce(rev(actions), ~ .y$execute_write(.x), .init = sp)
  time_action_end <- Sys.time()
  time_action <- (time_action_end - time_action_start) %>% as.numeric()
  if(getOption("SpectraMapping")$verbose >= 1)
    log_level(INFO, "mapping done, elapsed: {round(time_action, 1)} seconds")
  sp
}


#' @export
setMethod("mapVariables", "Spectra", function(sp, mapping, ...) {
  if(is(sp@backend, "MsBackendMapping")) {
    sp@backend <- mapVariables(sp@backend, mapping, ...)
    return(sp)
  }
    
  else
    stop("mapping for generic backends not yet implemented")
} )


#' @export 
#' 
#' 
preSplit <- function(input_file, format, block_size, out_dir = NA, out_file_schema = "chunk{i}") {
  if(is.na(out_dir)) {
    out_dir <- tempfile()
    fs::dir_create(out_dir)
  }
  format$splitter(input_file, 
                  glue("[out_dir]/[out_file_schema]", .open = "[", .close = "]"),
                  spectra_per_file = block_size)
}

postJoin <- function(input_files, out_file, format) {
  format$joiner(input_files, out_file)
}

# 
# 
# setGeneric("writeVariables", function(sp, ...) {
#   stop("writeVariables is not implemented for this object")
# })
# 
# 
# setMethod("writeVariables", "MsBackendMapping", function(sp, mapping) {
# 
# } )
# 
# 
# setMethod("writeVariables", "Spectra", function(sp, mapping) {
#   if(is(sp@backend, "MsBackendMapping")) {
#     sp@backend <- writeVariables(sp@backend, mapping)
#     return(sp)
#   }
#   
#   else
#     stop("mapping for generic backends not yet implemented")
# } )
