##' @param f `character(1)` with the path to an Mapping file.
##' 
##' @param msLevel `numeric(1)` with the MS level. Default is 2.
##' 
##' @param ... Additional parameters, currently ignored.
##'
##' @importFrom S4Vectors DataFrame
##'
##' @importFrom IRanges NumericList
##' 
##' @author Michael Stravs
##' 
##' @noRd
.read_mapping <- function(o, f, ...) {
    if (length(f) != 1L)
        stop("Please provide a single file.")
    
    data <- read_file(f) %>% str_remove_all("\r")
    data_parsed <- o@format$reader(data)
    return(data_parsed)
    
    res$mz <- IRanges::NumericList(res$mz)
    res$intensity <- IRanges::NumericList(res$intensity)
    res$dataOrigin <- f
    res$msLevel <- as.integer(msLevel)
    res
}



.apply_all_regex <- function(regex_map, vars_) {
    # Applying a single regex: 
    # Transform all the values it matches to
    apply_single_regex <- function(vars_, regex) {
        mutate(vars_,
               value = if_else(
                   formatKey == regex$formatKey,
                   str_replace(value, regex$match, regex$sub),
                   value
               ))
    }
    # Pass the vars_ through all regex lines
    vars_ <- regex_map %>%
        rowwise() %>%
        group_split() %>%
        reduce(
            apply_single_regex,
            .init = vars_
        )
    vars_
}


.fill_variables <- function(o) {
    # load variable and dictionary mappings
    vars_map <- o@format$mapping %>% filter(type=="read")
    dict_map <- o@format$dictionary %>% 
        filter(type == "read")
    regex_map <- o@format$regex %>% 
        filter(type == "read")

    vars_all <- o@variables
    
    # Translate regex    
    vars_all <- .apply_all_regex(regex_map, vars_all)

    # Translate verbatim values
    vars_dict <- vars_all %>%
        rename(format = value) %>%
        inner_join(dict_map, by=c("formatKey", "format")) %>%
        select("spectrum_id", "formatKey", "value")
    vars_nodict <- vars_all %>%
        anti_join(dict_map, by="formatKey")
    vars_all <- bind_rows(list(vars_dict, vars_nodict))
    
    # Translate keys and pivot wide
    vars_table <- vars_all %>% 
        inner_join(vars_map, by = c("formatKey")) %>%
        pivot_wider(id_cols = "spectrum_id",
                    names_from = "spectraKey",
                    values_from = "value") %>%
        arrange(spectrum_id) %>%
        .transform_types(o@fields)
    o@spectraData <- DataFrame(vars_table)
    o
}

.fill_backend <- function(o) {
    vars_map <- o@format$mapping %>% 
        filter(type=="write")
    dict_map <- o@format$dictionary %>% 
        filter(type == "write")
    
    # Translate keys and pivot long
    data <- as_tibble(o@spectraData) %>%
        mutate(across(-spectrum_id, as.character)) %>%
        pivot_longer(-spectrum_id, names_to = "spectraKey", values_to = "value",
                     values_ptypes = list(value = character()))
    vars_format <- data %>%
        inner_join(vars_map, by=c("spectraKey")) %>%
        select(spectrum_id, formatKey, value)
    
    # Translate regex: todo
    
    # Translate verbatim values
    vars_dict <- vars_format %>%
        inner_join(dict_map, by=c("formatKey", "value")) %>%
        select(spectrum_id, formatKey, format) %>%
        rename(value=format)
    vars_nodict <- o@variables %>%
        anti_join(dict_map, by="formatKey")
    vars_all <- bind_rows(list(vars_dict, vars_nodict))
    
    o@variables <- vars_all
    o
}

.fill_peaks <- function(o, peaks) {
    o@peaks <- peaks %>% 
        as_tibble() %>% 
        unnest(c(mz, intensity)) %>% 
        rename(int=intensity) %>%
        mutate(annotation = NA)
    o
}


#' Transform variable types according to field definition
#'
#' @param table 
#' @param fields 
#'
#' @return
#' @export
#'
#' @examples
.transform_types <- function(table, fields) {
    reduce(
        fields %>% 
            filter(spectraKey %in% colnames(table)) %>% 
            rowwise() %>% 
            group_split(),
        function(spec_vars, field)
            spec_vars %>% mutate(across(
                .cols = field$spectraKey, 
                .fns = .transform_function[[field$dataType]] )),
        .init = table
    )
}

.subset_peaks <- function(o) {
    o@peaks %>%
        filter(spectrum_id %in% o@spectraData$spectrum_id)
}


.load_default_fields <- function() {
    fields <- yaml.load_file(system.file("mapping/fields.yaml", package="SpectraMapping"))
    return(bind_rows(fields))
}

.transform_function <- list(
 "integer" = as.integer,
 "numeric" = as.numeric
)