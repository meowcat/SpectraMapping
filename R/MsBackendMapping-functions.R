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

.apply_all_unnest <- function(nestings, vars_) {
    apply_single_unnest <- function(vars_, nesting) {
        # Separate KVS into data which is transformed and data which is kept
        vars_keep <- vars_ %>% filter(formatKey != nesting$formatKey)
        vars_act <-  vars_ %>% 
            filter(formatKey == nesting$formatKey) %>%
            select(-formatKey)
        # Transform: first separate if necessary, then extract key/value, then rename with prefix
        if(!is.null(nesting$separator))
            vars_act <- vars_act %>% separate_rows(nesting$separator)
        vars_act <- vars_act %>% 
            extract("value", regex=nesting$regex, into=c("formatKey", "value"))
        if(!is.null(nesting$prefix))
            vars_act <- vars_act %>% mutate(formatKey = paste0(nesting$prefix, formatKey))
        # Finally, recompose the tibble from kept and newly extracted variables
        vars <- bind_rows(list(vars_keep, vars_act))
        return(vars)
    }
    # pass the vars through all unnesting actions
    vars_ <- nestings %>%
        reduce(apply_single_unnest,
               .init = vars_)
    vars_
}


.fill_variables <- function(o) {
    # load variable and dictionary mappings
    vars_map <- o@format$mapping %>% filter(type=="read")
    dict_map <- o@format$dictionary %>% 
        filter(type == "read")
    regex_map <- o@format$regex %>% 
        filter(type == "read")
    nestings <- o@format$nesting

    vars_all <- o@variables
    
    # Unnest
    vars_all <- .apply_all_unnest(nestings, vars_all)
    
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
                    values_from = "value",
                    values_fn = list) %>%
        arrange(spectrum_id) %>%
        .transform_types(o@fields)
    o@spectraData <- DataFrame(vars_table)
    o
}

.fill_backend <- function(o) {
    vars_map <- o@format$mapping %>% 
        filter(type == "write")
    dict_map <- o@format$dictionary %>% 
        filter(type == "write")
    regex_map <- o@format$regex %>% 
        filter(type == "write")
    
    # Translate keys and pivot long
    data <- as_tibble(o@spectraData) %>%
        mutate(across(-spectrum_id, as.character)) %>%
        pivot_longer(-spectrum_id, names_to = "spectraKey", values_to = "value",
                     values_ptypes = list(value = character()))
    vars_format <- data %>%
        inner_join(vars_map, by=c("spectraKey")) %>%
        select(spectrum_id, formatKey, value)
    
    # Translate verbatim values
    vars_dict <- vars_format %>%
        inner_join(dict_map, by=c("formatKey", "value")) %>%
        select(spectrum_id, formatKey, format) %>%
        rename(value=format)
    vars_nodict <- vars_format %>%
        anti_join(dict_map, by="formatKey")
    vars_all <- bind_rows(list(vars_dict, vars_nodict))
    
    # Translate regex
    vars_all <- .apply_all_regex(regex_map, vars_all)
    
    o@variables <- vars_all
    o
}

.fill_peaks <- function(o, peaks) {
    o@peaks <- peaks %>% 
        as_tibble() %>% 
        tidyr::unchop(c(mz, intensity),
               ptype = data.frame(
                   mz = numeric(0),
                   intensity = numeric(0)
               )
               ) %>% 
        rename(int=intensity) %>%
        mutate(annotation = NA)
    o
}

#' @importMethodsFrom S4Vectors extractROWS
#'
#' @importFrom methods slot<-
#'
#' @noRd
.subset_backend_mapping <- function(x, i) {
    if (missing(i))
        return(x)
    i <- MsCoreUtils::i2index(i, length(x), rownames(x@spectraData))
    
    # Subset spectraData
    slot(x, "spectraData", check = FALSE) <- extractROWS(x@spectraData, i)
    
    # Additionally subset the true data source
    x@peaks <- x@peaks %>% filter(spectrum_id %in% x@spectraData$spectrum_id)
    x@variables <- x@variables %>% filter(spectrum_id %in% x@spectraData$spectrum_id)
    
    x
}

.subset_backend_data_frame <- function(x, i) {
    
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
 "numeric" = as.numeric,
 "character" = as.character
 #"CharacterList" = function(x) as(x, "CharacterList")
)


