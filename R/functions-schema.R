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

.fill_variables <- function(o) {
    vars_ <- o@format$mapping %>% filter(type=="read")
    
    vars_table <- vars_ %>% 
        inner_join(o@variables, by = c("formatKey" = "key")) %>%
        pivot_wider(id_cols = "spectrum_id",
                    names_from = "spectraKey",
                    values_from = "value") %>%
        arrange(spectrum_id) %>%
        .transform_types(o@fields)
    o@spectraData <- DataFrame(vars_table)
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
        filter(spectrum_id %in% o@spectraData$spectrumId)
}


.load_default_fields <- function() {
    fields <- yaml.load_file(system.file("mapping/fields.yaml", package="SpectraMapping"))
    return(bind_rows(fields))
}

.transform_function <- list(
 "integer" = as.integer,
 "numeric" = as.numeric
)