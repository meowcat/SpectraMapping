

#' Metadata action: tabular
#' 
#' Converts a metadata column into a column of data frames. This can be used
#' either to get a "subtag-style" column into a key-value store, or to actually
#' read a table like the MassBank peak annotations. 
#' Note: in some cases, this is probably best prepended with `split`, e.g. for NIST comments.
#' 
#' @examples
#'  
#' # Key-value store reading
#' backend <- get_proto_backend(COMMENT = list(c("INTERNAL_ID 1234", "MYNAME Michele", "formless_nosplit"),
#'                                             c("formless nosplit"),
#'                                             c(),
#'                                             c("INTERNAL_ID 1232", "MYNAME something")))
#' action <- get_proto_action(
#'    "tabular",
#'    source = "COMMENT",
#'    target = "commentData",
#'    regex = "([A-Z_-]*)\\s?(.*)",
#'    write = "{key} {value}"
#' )
#' fw <- action$execute_read(backend)
#' fw@@variables <- fw@@variables %>% select(-COMMENT)
#' bw <- action$execute_write(fw)
#' 
#' Table reading with colnames
#' backend <- get_proto_backend(
#'    PKANNOT = list(
#'       c("index m/z tentative_formula formula_count mass error(ppm)",
#'         "1 78.9189 Br- 1 78.9189 -0.33",
#'         "2 193.9252 C7HBrNO- 1 193.9247 2.32",
#'         "3 273.8506 C7H2Br2NO- 1 273.8509 -1.06"),
#'       c("index m/z tentative_formula formula_count mass error(ppm)",
#'         "1 78.9188 Br- 1 78.9189 -0.71",
#'         "2 193.9246 C7HBrNO- 1 193.9247 -0.56",
#'         "3 273.8512 C7H2Br2NO- 1 273.8509 1.16"),
#'       c(),
#'       c("index m/z tentative_formula formula_count mass error(ppm)")
#'    ))
#' action <- get_proto_action(
#'    "tabular",
#'    source = "PKANNOT",
#'    target = "annotation",
#'    sep= " ",
#'    header= 1
#' )
#' fw <- action$execute_read(backend)
#' fw@@variables <- fw@@variables %>% select(-PKANNOT)
#' bw <- action$execute_write(fw)
#' 
#' @param data `MsBackendMapping` to execute the metadata mapping step on
#' @param params List of parameters for a single action (one `params` entry fully merged.)
MetadataActionTabular <- R6::R6Class(
  "MetadataActionTabular",
  inherit = MetadataActionBase,
  public = list(
    
    
    base_settings = list(
      source = '',
      target = c(),
      header = NA,
      sep = c(),
      regex = c(),
      write = c(),
      trim = FALSE,
      convert = FALSE, 
      fill = "left", # sic! To deal with the non-subtagged comment case.
      extra = "merge"
    ),
    
    
    #' @description read implementation
    process_read = function(data, params) {
      
      source <- .v(params$source)
      if(!all(source %in% colnames(data@variables)))
        return(data)            
      
      
      if(length(params$target) == 0)
        target_ <- source
      else
        target_ <- params$target
      target <- .v(target_)
      set_spectra_var <- target[.flag(target_)] # may be multiple vars
      
      
      # 
      
      # Decide on header
      # NA: use "key", "value"
      # 1: use first line
      # a list: use the specified list
      if(all(is.na(params$header))) {
        header <- rep_along(table, list(c("key", "value")))
        table <- data@variables[[source]]
      }
      else if(all(params$header == 1)) {
        header_ <- data@variables[[source]] %>% map(~ .x[1])
        table <- data@variables[[source]] %>% 
          map(~ .x[-1]) %>%
          modify_if(is.null, ~ character(0))
        if(length(params$regex) > 0)
          header <- str_match(header_, params$regex) %>% `[`(1,-1)
        else
          header <- str_split(header_, params$sep)
      }
      else {
        header <- rep_along(table, params$header)
        table <- data@variables[[source]]
      }
      
      
      
      
      temp_col <- table %>% map(~ tibble(col = .x))
      
      if(length(params$regex) > 0) {
        data@variables[[target]] <- map2(
          temp_col, header,
          ~ .x %>% tidyr::extract(col, into = .y, regex = params$regex, convert = params$convert)
        )
      }
      else if(length(params$sep) > 0) {
        #message('Tabular: using sep')
        data@variables[[target]] <- map2(
          temp_col, header,
          ~ .x %>% separate(col, into = .y, sep = params$sep, convert = params$convert)
        )
      } 
      
      if(params$trim) {
        data@variables[[target]] <- data_variables[[target]] %>%
          map(function(t) t %>% mutate(across(.fns = ~ str_trim(.x))))
      }
      
      
      data@spectraVariables <- union(data@spectraVariables, set_spectra_var)
      
      return(data)
    },
    
    #' @description write implementation
    process_write = function(data, params) {
      
      source <- .v(params$source)
      if(length(params$target) == 0)
        target_ <- source
      else
        target_ <- params$target
      target <- .v(target_)
      
      if(!all(target %in% colnames(data@variables)))
        return(data)
      
      target_sym <- sym(target)
      set_source_var <- source[.flag(params$source)]
      
      if(length(params$write) > 0)
        data@variables <- data@variables %>%
        mutate(!!source := map(!!target_sym, ~.x %>% glue_data(params$write)))
      else {
        temp_col <- data@variables[[target]] %>% map(~.x %>% unite("col", sep=params$sep)) %>% map(~ pull(.x, "col"))
        if(params$header == 1) {
          headers <- data@variables[[target]] %>% 
            map(colnames) %>% 
            modify_if(~ all(.x == "NULL"), ~ character(0)) %>%
            map(paste, collapse = params$sep) %>%
            modify_if(~.x == "", ~ character(0))
          temp_col <- map2(temp_col, headers, ~c(.y, .x))
          attr(temp_col, "table") <- TRUE
        }
        data@variables[[source]] <- temp_col
      }
      
      
      data@sourceVariables <- union(data@sourceVariables, set_source_var)
      
      return(data)
    }
  ))
