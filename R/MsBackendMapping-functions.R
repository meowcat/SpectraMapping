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
.parse_to_kvs <- function(o, f, ...) {
    if (length(f) != 1L)
        stop("Please provide a single file.")
    
    data <- read_file(f) %>% str_remove_all("\r")
    data_parsed <- o@format$reader(data)
    return(data_parsed)
    
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
    i <- MsCoreUtils::i2index(i, length(x))
    
    # # Subset spectraData
    # slot(x, "spectraData", check = FALSE) <- extractROWS(x@spectraData, i)
    x@variables <- x@variables[i,,drop=FALSE]
    
    #x@variables <- x@variables %>% filter(spectrum_id %in% x@spectraData$spectrum_id)
    
    # Additionally subset the true data source
    x@peaks <- x@peaks %>% filter(spectrum_id %in% x@variables$spectrum_id)
    
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
        filter(spectrum_id %in% o@variables$spectrum_id)
}


.load_default_fields <- function() {
    fields <- yaml.load_file(system.file("mapping/fields.yaml", package="SpectraMapping"))
    return(bind_rows(fields))
}

.transform_function <- list(
 "integer" = as.integer,
 "numeric" = as.numeric,
 "character" = as.character,
 "list" = as.list
 #"CharacterList" = function(x) as(x, "CharacterList")
)


#' Helper function to combine backends that base on [MsBackendMapping()].
#'
#' @param objects `list` of `MsBackend` objects.
#'
#' @return [MsBackend()] object with combined content.
#'
#' @author Michele Stravs with code from Johannes Rainer
#'
#' @importFrom MsCoreUtils vapply1c rbindFill
#'
#' @noRd
.combine_backend_mapping <- function(objects) {
    if (length(objects) == 1)
        return(objects[[1]])
    if (!all(vapply1c(objects, class) == class(objects[[1]])))
        stop("Can only merge backends of the same type: ", class(objects[[1]]))
    res <- objects[[1]]
    res@variables <- bind_rows(map(objects, "variables"), .id = "_source_backend") %>%
        mutate(spectrum_id_temp = paste(`_source_backend`, spectrum_id, sep="_"))
    res@peaks <- bind_rows(map(objects, "peaks"), .id = "_source_backend") %>%
        mutate(spectrum_id_temp = paste(`_source_backend`, spectrum_id, sep="_"))
    spectrum_id_new <- unique(union(res@variables$spectrum_id_temp,
                                    res@peaks$spectrum_id_temp))
    
    res@variables <- res@variables %>%
        mutate(spectrum_id = match(spectrum_id_temp, spectrum_id_new)) %>%
        select(-spectrum_id_temp, -`_source_backend`)
    res@peaks <- res@peaks %>%
        mutate(spectrum_id = match(spectrum_id_temp, spectrum_id_new)) %>%
        select(-spectrum_id_temp, -`_source_backend`)
}


.check_int_colname <- function(tbl) {
    if("int" %in% colnames(tbl))
        return(tbl)
    if("intensity" %in% colnames(tbl))
        return(tbl %>% dplyr::rename(int = intensity))
    stop("no intensity column was provided")
}


.set_peaks_data <- function(sp, peaks) {
    peaks_df <- peaks %>%
        map(as_tibble) %>%
        map(.check_int_colname) %>%
        bind_rows(.id = "spectrum_id") %>%
        mutate(spectrum_id = as.numeric(spectrum_id))
    sp@peaks <- peaks_df
    sp
}