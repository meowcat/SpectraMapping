
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
      # if(length(read_split) == 0)
      #    read_split <- rep("*", length(target))
      if(length(read_split) == 0)
        return(data)
      
      
      
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
      # if(length(write_split) == 0)
      #    write_split <- rep("*", length(source))
      if(length(write_split) == 0)
        return(data)
      
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