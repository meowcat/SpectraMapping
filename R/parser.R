library(rly)

MSP_TOKENS = c('NUMERIC', 'FEATURENAME', 'ALNUMPLUS',
               "NEWSPECTRUM", "NEWLINE", 'WHITESPACE')

MspLexer <- R6::R6Class(
  
  classname = "MspLexer",
  
  public = list(
    
    # It is simpler to  just use t_SEP and t_LEVEL instead of literals.
    tokens = MSP_TOKENS,
    
    t_NUMERIC = '[0-9.+]',
    
      
    t_FEATURENAME = function(re='([A-Za-z_ ]+) *?:', t) {
      t$value <- str_extract(t$value, re)
      return(t)
    },
    
    t_ALNUMPLUS = '[^\n]+',
    
    t_NEWSPECTRUM = '\n\n',
    
    t_NEWLINE = '\n',
    
    t_WHITESPACE = ' +',
    
    t_error = function(t) {
      cat(sprintf("Illegal character '%s'", t$value[1]))
      t$lexer$skip(1)
      return(t)
    }
    
  )
)


MspParser <- R6::R6Class("MspParser",
                      public = list(
                        tokens = MSP_TOKENS,
                        
                        p_doc = function(doc = 'doc : doc NEWSPECTRUM spectrum 
                                | spectrum', p) {
                          if(p$length() == 2) { # doc : SPECTRUM
                            line <- p$get(2)
                            tree <- list()
                            tree <- insert_line(tree, line)
                            p$set(1, tree)
                          }
                          else if(p$length() == 4) { # doc: doc NEWSPECTRUM spectrum
                            tree <- p$get(2)
                            line <- p$get(4)
                            p$set(1, insert_line(tree, line))
                          }
                          else
                            message("fail p_doc")
                        },
                        p_spectrum = function(doc = 'spectrum : features peaks', p) {
                          data <- list()
                          data["features"] <- p$get(2)
                          data["peaks"] <- p$get(3)
                          p$set(1, data)
                        },
                        p_features = function(doc = 'features : features feature
                                              | feature', p) {
                          message("p_features")
                          if(p$length() == 2) { # features : feature
                            line <- p$get(2)
                            message(str(line))
                            tree <- list()
                            tree <- insert_line(tree, line)
                            p$set(1, tree)
                          }
                          else if(p$length() == 3) { # features: features feature
                            tree <- p$get(2)
                            line <- p$get(3)
                            p$set(1, insert_line(tree, line))
                          }
                          else
                            message("fail p_features")
                        },
                        
                        p_feature = function(doc = 'feature : FEATURENAME ALNUMPLUS NEWLINE', p) {
                          message("p_feature")
                          value <- p$get(3)
                          key <- p$get(2)
                          line <- list(key = key, value = value)
                          p$set(1, line)
                          str(line)
                        },
                          
                        p_peaks = function(doc = 'peaks : peaks peak
                                              | peak', p) {
                          if(p$length() == 2) { # peaks: peak
                            line <- p$get(2)
                            tree <- list()
                            tree <- insert_line(tree, line)
                            p$set(1, tree)
                          }
                          else if(p$length() == 3) { # peaks: peaks peak
                            tree <- p$get(2)
                            line <- p$get(3)
                            p$set(1, insert_line(tree, line))
                          }
                          else
                            message("fail p_peaks")
                        },

                        p_peak = function(doc = 'peak : NUMERIC WHITESPACE NUMERIC NEWLINE
                        | NUMERIC WHITESPACE NUMERIC WHITESPACE ALNUMPLUS NEWLINE', p) {
                          if(p$length == 5) {
                            l <- list(
                              mz = p$get(2),
                              int = p$get(4),
                              annot = ""  
                            )
                          }
                          else if(p$length == 7){
                            l <- list(
                              mz = p$get(2),
                              int = p$get(4),
                              annot = p$get(6)
                            )
                          }
                          else
                              message("fail p_peak")
                          p$set(1, l)
                        },
                        
                        p_error = function(p) {
                          message("Error parsing")
                          print(str(p))
                        }
                        
                      ))

.parse_msp <- function(data) {
  data = paste0(paste(data, collapse = '\n'), "\n")
  lexer <- rly::lex(MspLexer)
  parser <- yacc(MspParser)
  data <- parser$parse(data, lexer,debug = RlyLogger$new(".", "file.out"))
  return(data)
}



# representations of MTD structures in the R object:
# unnamed lists are mzTab arrays
# named lists are mzTab nodes
# An array may not also be a list,
# i.e. 
# MTD mzTab-someitem-subitem 1
# MTD mzTab-someitem[1] 4 
# is not allowed.
# The * part is strange, but necessary one way or another:
#
# https://raw.githubusercontent.com/HUPO-PSI/mzTab/master/examples/2_0-Metabolomics_Release/MouseLiver_negative.mzTab
# MTD	software[1]	[, , LipidDataAnalyzer, 2.6.3_2]
# MTD	software[1]-setting[1]	isotope_pattern_checked
#
# So any node can have both its own value and subnodes.
# We can only solve this by 
# 1) making a reference class
# 2) using node <- list(), attr(node, "value") <- value
#    (which would be clunkier to retrieve)
# 3) making a specific entry for "self-value" which I here chose to be "*".
# We can make a function v(node) <- node[["*"]] (or self(node) or such)
insert_line <- function(tree, line) {
  #return(c(tree, line))
  stopifnot(is.list(tree))
  
  tree[[line$key]] <- line$value
  return(tree)
}



`node` <- function(x) x[[MZTAB_NODE_MARKER]]
`node<-` <- function(x, value) {
  x[[MZTAB_NODE_MARKER]] <- value
  return(x)
}