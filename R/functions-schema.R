##' @param f `character(1)` with the path to an schema file.
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
.read_schema <- function(o, f, ...) {
    if (length(f) != 1L)
        stop("Please provide a single schema file.")
    
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
        arrange(spectrum_id)
    o@spectraData <- DataFrame(vars_table)
    o
}

.subset_peaks <- function(o) {
    o@peaks %>%
        filter(spectrum_id %in% o@spectraData$spectrumId)
}

##' @param schema `character()` of lines defining a spectrum in schema
##'     format.
##' 
##' @author Laurent Gatto
##' 
##' @importFrom stats setNames
##'
##' @noRd
.extract_schema_spectrum <- function(schema) {
    ## grep description
    desc.idx <- grep("=", schema)
    desc <- schema[desc.idx]
    spec <- schema[-desc.idx]

    ms <- do.call(rbind, strsplit(spec, "[[:space:]]+"))
    mode(ms) <- "double"

    if (!length(ms))
        ms <- matrix(numeric(), ncol = 2L)

    r <- regexpr("=", desc, fixed = TRUE)
    desc <- setNames(substring(desc, r + 1L, nchar(desc)), substring(desc, 1L, r - 1L))
    title <- unname(desc["TITLE"])

    desc[c("PEPMASSMZ", "PEPMASSINT")] <-
        strsplit(desc["PEPMASS"], "[[:space:]]+")[[1L]][1:2]

    ## select only values of interest and convert to numeric
    desc["CHARGE"] <- sub("[+-]", "", desc["CHARGE"])
    voi <- c("RTINSECONDS", "CHARGE", "SCANS", "PEPMASSMZ", "PEPMASSINT")
    desc <- setNames(as.numeric(desc[voi]), voi)
    desc[is.na(desc[voi])] <- 0L
    list(rtime = unname(desc["RTINSECONDS"]),
         scanIndex = unname(as.integer(desc["SCANS"])),
         precursorMz = unname(desc["PEPMASSMZ"]),
         precursorIntensity = unname(desc["PEPMASSINT"]),
         precursorCharge = unname(as.integer(desc["CHARGE"])),
         mz = ms[, 1L],
         intensity = ms[, 2L],
         title = title)
}
