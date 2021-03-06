
# setClassUnion("character_NULL", c("character", "NULL"))

#' @rdname dictionary-class
#' @export
#' @keywords internal dictionary
#' @slot .Data named list of mode character, where each element name is a
#'   dictionary "key" and each element is one or more dictionary entry "values"
#'   consisting of a pattern match
#' @slot concatenator character object specifying space between multi-word
#'   values
setClass("dictionary2", contains = "list",
         slots = c(concatenator = "character"),
         prototype = prototype(concatenator = " "))

setValidity("dictionary2", function(object) {
    # does every element have a name? simply needs to pass
    validate_dictionary(object)
})

# Internal function to chekc if dictionary eintries are all chracters
validate_dictionary <- function(dict){
    dict <- unclass(dict)
    if (is.null(names(dict))) {
        stop("Dictionary elements must be named: ",
             paste(unlist(dict, recursive = TRUE), collapse = ' '))
    }
    if (any(names(dict) == "")) {
        unnamed <- dict[which(names(dict) == "")]
        stop("Unnamed dictionary entry: ",
             paste(unlist(unnamed, use.names = FALSE), collapse = ' '))
    }
    if (is.null(dict@concatenator) || dict@concatenator == '') {
        stop("Concatenator cannot be null or an empty string")
    }
    check_entries(dict)
}

check_entries <- function (dict) {
    for (i in seq_along(dict)) {
        entry <- dict[[i]]
        is_category <- sapply(entry, is.list)
        if (any(!is_category)) {
            word <- unlist(entry[!is_category], use.names = FALSE)
            if (any(!is.character(word))) {
                word_error <- word[!is.character(word)]
                stop("Non-character entries found: ", word_error)
            }
        }
        if (any(is_category)) {
            category <- entry[is_category]
            check_entries(category)
        }
    }
} 

# Internal function to print dictionary
print_dictionary <- function(entry, level = 1) {
    entry <- unclass(entry)
    if (!length(entry)) return()
    is_category <- sapply(entry, is.list)
    category <- entry[is_category]
    word <- unlist(entry[!is_category], use.names = FALSE)
    if (length(word)) {
        cat(rep('  ', level - 1), "- ", paste(word, collapse = ", "), "\n", sep = "")
    }
    for (i in seq_along(category)) {
        cat(rep('  ', level - 1), "- ", names(category[i]), ':\n', sep = "")
        print_dictionary(category[[i]], level + 1)
    }
}


#' print a dictionary object
#' 
#' Print/show method for dictionary objects.
#' @param object the dictionary to be printed
#' @rdname dictionary-class
#' @export
setMethod("show", "dictionary2", 
          function(object) {
              levs <- if ((depth <- dictionary_depth(object)) > 1L) " primary" else ""
              nkeys <- length(names(object))
              cat("Dictionary object with ", nkeys, levs, " key entr", 
                  if (nkeys == 1L) "y" else "ies", sep = "")
              if (levs != "") cat(" and ", depth, " nested levels", sep = "")
              cat(".\n")
              print_dictionary(object)
          })

#' Extractor for dictionary objects
#' @param object the dictionary to be extracted
#' @param i index for entries
#' @rdname dictionary-class
#' @export
setMethod("[",
          signature = c("dictionary2", i = "index"),
          function(x, i) {
              is_category <- sapply(as.list(x)[i], function(y) !is.null(y))
              dictionary(as.list(x)[i][is_category], concatenator = x@concatenator)
        })

#' Extractor for dictionary objects
#' @param object the dictionary to be extracted
#' @param i index for entries
#' @rdname dictionary-class
#' @export
setMethod("[[",
          signature = c("dictionary2", i = "index"),
          function(x, i) {
              if (!is.list(as.list(x)[[i]])) {
                  as.list(x)[[i]]
              } else {
                  dictionary(as.list(x)[[i]], concatenator = x@concatenator)
              }
          })

#' @rdname dictionary-class
#' @param name the dictionary key
#' @export
`$.dictionary2` <- function(x, name) {
    x[[name]]
}

#' Coerce a dictionary object into a list
#' @param object the dictionary to be coerced
#' @rdname dictionary-class
#' @export
setMethod("as.list",
          signature = c("dictionary2"),
          function(x) {
              simplify_dictionary(x)
          })

#' create a dictionary
#' 
#' Create a \pkg{quanteda} dictionary class object, either from a list or by importing from a 
#' foreign format.  Currently supported input file formats are the Wordstat,
#' LIWC, Lexicoder v2 and v3, and Yoshikoder formats.  The import using the 
#' LIWC format works with 
#' all currently available dictionary files supplied as part of the LIWC 2001, 
#' 2007, and 2015 software (see References).
#' @param ... a named list of character vector dictionary entries, including \link{valuetype} pattern
#'  matches, and including multi-word expressions separated by \code{concatenator}.  The argument 
#'  may be an explicit list or named set of elements that can be turned into a list.  See examples.
#'  This argument may be omitted if the dictionary is read from \code{file}.
#' @param file file identifier for a foreign dictionary
#' @param format character identifier for the format of the foreign dictionary. 
#'   If not supplied, the format is guessed from the dictionary file's
#'   extension.
#'   Available options are: \describe{ \item{\code{"wordstat"}}{format used by 
#'   Provalis Research's Wordstat software} \item{\code{"LIWC"}}{format used by 
#'   the Linguistic Inquiry and Word Count software} \item{\code{"yoshikoder"}}{
#'   format used by Yoshikoder software} \item{\code{"lexicoder"}}{format used
#'   by Lexicoder} \item{\code{"YAML"}}{the standard YAML format}}
#' @param concatenator the character in between multi-word dictionary values. 
#'   This defaults to \code{"_"} except LIWC-formatted files, which defaults to 
#'   a single space \code{" "}.
#' @param encoding additional optional encoding value for reading in imported 
#'   dictionaries. This uses the \link{iconv} labels for encoding.  See the 
#'   "Encoding" section of the help for \link{file}.
#' @param tolower if \code{TRUE}, convert all dictionary values to lowercase
#' @return A dictionary class object, essentially a specially classed named list
#'   of characters.
#' @references Wordstat dictionaries page, from Provalis Research 
#'   \url{http://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/}.
#'   
#'   Pennebaker, J.W., Chung, C.K., Ireland, M., Gonzales, A., & Booth, R.J. 
#'   (2007). The development and psychometric properties of LIWC2007. [Software 
#'   manual]. Austin, TX (\url{www.liwc.net}).
#'   
#'   Yoshikoder page, from Will Lowe 
#'   \url{http://conjugateprior.org/software/yoshikoder/}.
#'   
#'   Lexicoder format, \url{http://www.lexicoder.com}
#'   
#' @seealso \link{dfm}
#' @import stringi
#' @examples
#' mycorpus <- corpus_subset(data_corpus_inaugural, Year>1900)
#' mydict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
#'                           opposition = c("Opposition", "reject", "notincorpus"),
#'                           taxing = "taxing",
#'                           taxation = "taxation",
#'                           taxregex = "tax*",
#'                           country = "america"))
#' head(dfm(mycorpus, dictionary = mydict))
#' 
#' # also works
#' mydict2 <- dictionary(christmas = c("Christmas", "Santa", "holiday"),
#'                       opposition = c("Opposition", "reject", "notincorpus"))
#' dfm(mycorpus, dictionary = mydict2)
#' 
#' \dontrun{
#' # import the Laver-Garry dictionary from http://bit.ly/1FH2nvf
#' lgdict <- dictionary(file = "http://www.kenbenoit.net/courses/essex2014qta/LaverGarry.cat",
#'                      format = "wordstat")
#' head(dfm(data_corpus_inaugural, dictionary = lgdict))
#' 
#' # import a LIWC formatted dictionary from http://www.moralfoundations.org
#' mfdict <- dictionary(file = "http://ow.ly/VMRkL", format = "LIWC")
#' head(dfm(data_corpus_inaugural, dictionary = mfdict))}
#' @export
dictionary <- function(..., file = NULL, format = NULL, 
                       concatenator = " ", 
                       tolower = TRUE, encoding = "auto") {
    
    if (is.null(file)) {
        x <- list(...)
        if (length(x) == 1 && is.list(x[[1]]) && is.null(names(x))) {
            x <- x[[1]]
        } else {
            x <- x[sapply(x, function(x) is.character(x) || is.list(x))]
        }
        if (!is.null(x) && !is.list(x)) {
            stop("Dictionaries must be named lists or lists of named lists.")
        }
        if (is.dictionary(x)) {
            return(x)
        } else {
            # a dictionary is a list of lists in the new implementation
            x <- list2dictionary(x)
        }
    } else { 
        
        formats <- c(cat = "wordstat", dic = "LIWC", ykd = "yoshikoder", lcd = "yoshikoder", 
                     lc3 = "lexicoder", yml = "YAML")
        
        if (!file.exists(file))
            stop("File does not exist: ", file)
        if (is.null(format)) {
            ext <- stri_trans_tolower(tools::file_ext(file))
            if (ext %in% names(formats)) {
                format <- formats[[ext]]
            } else {
                stop("Unknown dictionary file extension: ", ext)
            }
        } else {
            format <- match.arg(format, formats)
        }
        
        if (format == "wordstat") {
            x <- read_dict_wordstat(file, encoding)
        } else if (format == "LIWC") {
            # x <- read_dict_liwc(file, encoding)
            x <- list2dictionary(read_dict_liwc_old(file, encoding))
        } else if (format == "yoshikoder") {
            x <- read_dict_yoshikoder(file)
        } else if (format == "lexicoder") {
            x <- read_dict_lexicoder(file)
        } else if (format == "YAML") {
            x <- yaml::yaml.load_file(file, as.named.list = TRUE)
            x <- list2dictionary(x)
        }
    }
    if (tolower)
        x <- lowercase_dictionary(x)
    new("dictionary2", x, concatenator = concatenator)
}


#  Flatten a hierarchical dictionary into a list of character vectors
# 
#  Converts a hierarchical dictionary (a named list of named lists, ending in character
#  vectors at the lowest level) into a flat list of character vectors.  Works like
#  \code{unlist(dictionary, recursive=TRUE)} except that the recursion does not go to the
#  bottom level.  Called by \code{\link{dfm}}.
# 
#  @param tree list to be flattened
#  @param levels integer vector indicating levels in the dictionary
#  @param level internal argument to pass current levels
#  @param key_tree internal argument to pass for parent keys
#  @param dict internal argument to pass flattend dicitonary
#  @return A dictionary flattened to variable levels
#  @keywords internal
#  @author Kohei Watanabe
#  @export
#  @examples
#  dictPopulismEN <- 
#      dictionary(list(populism=c("elit*", "consensus*", "undemocratic*", "referend*",
#                                 "corrupt*", "propagand", "politici*", "*deceit*",
#                                 "*deceiv*", "*betray*", "shame*", "scandal*", "truth*",
#                                 "dishonest*", "establishm*", "ruling*")))
#  flatten_dictionary(dictPopulismEN)
# 
#  hdict <- list(level1a = list(level1a1 = c("l1a11", "l1a12"),
#                              level1a2 = c("l1a21", "l1a22")),
#                level1b = list(level1b1 = c("l1b11", "l1b12"),
#                               level1b2 = c("l1b21", "l1b22", "l1b23")),
#                level1c = list(level1c1a = list(level1c1a1 = c("lowest1", "lowest2")),
#                               level1c1b = list(level1c1b1 = c("lowestalone"))))
#  flatten_dictionary(hdict)
#  flatten_dictionary(hdict, 2)
#  flatten_dictionary(hdict, 1:2)

flatten_dictionary <- function(dict, levels = 1:100, level = 1, key_parent = '', dict_flat = list()) {
    dict <- unclass(dict)
    for (i in seq_along(dict)) {
        key <- names(dict[i])
        entry <- dict[[i]]
        if (!length(entry)) next
        if (level %in% levels) {
            if (key_parent != '' && key != '') {
                key_entry <- paste(key_parent, key, sep = '.')
            } else {
                key_entry <- key
            }
        } else {
            key_entry <- key_parent
        }
        is_category <- sapply(entry, is.list)
        dict_flat[[key_entry]] <- c(dict_flat[[key_entry]], unlist(entry[!is_category], use.names = FALSE))
        dict_flat <- flatten_dictionary(entry[is_category], levels, level + 1, key_entry, dict_flat)
    }
    attributes(dict_flat, FALSE) <- attributes(dict)
    return(dict_flat)
}

# Internal function to lowercase dictionary entries
#
# hdict <- list(KEY1 = list(SUBKEY1 = c("A", "B"),
#                           SUBKEY2 = c("C", "D")),
#               KEY2 = list(SUBKEY3 = c("E", "F"),
#                           SUBKEY4 = c("G", "F", "I")),
#               KEY3 = list(SUBKEY5 = list(SUBKEY7 = c("J", "K")),
#                           SUBKEY6 = list(SUBKEY8 = c("L"))))
# lowercase_dictionary(hdict)
#
lowercase_dictionary <- function(dict) {
    dict <- unclass(dict)
    for (i in seq_along(dict)) {
        if (is.list(dict[[i]])) {
            dict[[i]] <- lowercase_dictionary(dict[[i]])
        } else {
            if (is.character(dict[[i]])) {
                dict[[i]] <- stri_trans_tolower(dict[[i]])
            }
        }
    }
    return(dict)
}

# Internal function to convert a list to a dictionary
list2dictionary <- function(dict) {
    for (i in seq_along(dict)) {
        if (is.list(dict[[i]])) {
            dict[[i]] = list2dictionary(dict[[i]])
        } else {
            if (is.character(dict[[i]])) {
                dict[[i]] = list(stri_enc_toutf8(dict[[i]]))
            } else {
                dict[[i]] = list(dict[[i]])
            }
        }
    }
    return(dict)
}


#' check if an object is a dictionary
#' 
#' Return \code{TRUE} if an object is a \pkg{quanteda} \link{dictionary}.
#' @param x any object
#' @export
is.dictionary <- function(x) {
    is(x, "dictionary2")
}


# Import a Lexicoder dictionary
# dict <- read_dict_lexicoder('/home/kohei/Documents/Dictionary/Lexicoder/LSDaug2015/LSD2015.lc3')
read_dict_lexicoder <- function(path) {
    
    lines <- stri_read_lines(path, encoding = 'utf-8') # Lexicoder 3.0 is always UTF-8
    lines <- stri_trim_both(lines)
    lines_yaml <- ifelse(stri_detect_regex(lines, '^\\+'),
                         stri_replace_all_regex(lines, '^+(.+)$', '"$1":'),
                         stri_replace_all_regex(lines, '^(.+)$', ' - "$1"'))
    lines_yaml <- stri_replace_all_regex(lines_yaml, '[[:control:]]', '') # clean
    yaml <- paste0(lines_yaml, collapse = '\n')
    dict <- yaml::yaml.load(yaml, as.named.list = TRUE)
    dict <- list2dictionary(dict)
    return(dict)
}

# Import a Wordstat dictionary
# dict <- read_dict_wordstat('/home/kohei/Documents/Dictionary/LaverGarry.txt', 'utf-8')
# dict <- read_dict_wordstat('/home/kohei/Documents/Dictionary/Wordstat/ROGET.cat', 'utf-8')
# dict <- read_dict_wordstat('/home/kohei/Documents/Dictionary/Wordstat/WordStat Sentiments.cat', 'iso-8859-1')
read_dict_wordstat <- function(path, encoding = 'auto') {
    
    lines <- stri_read_lines(path, encoding = encoding, fallback_encoding = 'windows-1252')
    lines <- stri_trim_right(lines)
    lines_yaml <- ifelse(stri_detect_regex(lines, ' \\(\\d\\)$'),
                         stri_replace_all_regex(lines, '^(\\t*)(.+) \\(\\d\\)$', '$1- "$2"'),
                         stri_replace_all_regex(lines, '^(\\t*)(.+)$', '$1- "$2": '))
    
    lines_yaml <- stri_replace_all_regex(lines_yaml, '\t', '  ') # needs two spaces
    lines_yaml <- stri_replace_all_regex(lines_yaml, '[[:control:]]', '') # clean
    yaml <- paste0(lines_yaml, collapse = '\n')
    dict <- yaml::yaml.load(yaml, as.named.list = TRUE)
    dict <- list2dictionary_wordstat(dict, FALSE)
    return(dict)
}

# Internal functin for read_dict_wordstat
list2dictionary_wordstat <- function(entry, omit = TRUE, dict = list()) {
    if (omit) {
        for (i in seq_along(entry)) {
            key <- names(entry[i])
            if (is.list(entry[i])) {
                dict[[key]] <- list2dictionary_wordstat(entry[[i]], FALSE)
            }
        }
    } else {
        if (length(entry)) {
            is_category <- sapply(entry, is.list)
            category <- entry[is_category]
            for (i in seq_along(category)) {
                dict <- list2dictionary_wordstat(category[[i]], TRUE, dict)
            }
            dict[[length(dict) + 1]] <- unlist(entry[!is_category], use.names = FALSE)
        }
    }
    return(dict)
}


# Import a LIWC-formatted dictionary
# read_dict_liwc('/home/kohei/Documents/Dictionary/LIWC/LIWC2007_English.dic')        # WORKS
# dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2007_English.dic")      # WORKS
# dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2015_English_Flat.dic") # WORKS
# dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2001_English.dic")       # FAILS
# dictionary(file = "~/Dropbox/QUANTESS/dictionaries/LIWC/LIWC2007_English080730.dic") # FAILS
read_dict_liwc <- function(path, encoding = 'auto') {
    
    lines <- stri_read_lines(path, encoding = encoding, fallback_encoding = 'windows-1252')
    lines <- stri_trim_both(lines)
    lines <- lines[lines != '']
    
    sections <- which(lines == '%')
    lines_key <- lines[(sections[1] + 1):(sections[2] - 1)]
    lines_value <- lines[(sections[2] + 1):(length(lines))]
    
    keys <- stri_extract_last_regex(lines_key, '[^\t]+')
    keys_id <- stri_extract_first_regex(lines_key, '\\d+')
    
    values <- stri_extract_first_regex(lines_value, '[^\t]+')
    lines_value <- stri_replace_first_regex(lines_value, '[^\t]+\t', '') # for safety
    values_ids <- stri_extract_all_regex(lines_value, '\\d+')
    
    keys <- stri_replace_all_regex(keys, '[[:control:]]', '') # clean
    values <- stri_replace_all_regex(values, '[[:control:]]', '') # clean
    
    dict <- split(rep(values, lengths(values_ids)), as.factor(unlist(values_ids, use.names = FALSE)))
    dict <- dict[order(as.numeric(names(dict)))]
    names(dict) <- keys[match(names(dict), keys_id)]
    dict <- list2dictionary(dict)
    return(dict)
    
}

# Import a Yoshikoder dictionary
# dict <- read_dict_yoshikoder('/home/kohei/Documents/Dictionary/Yoshikoder/laver-garry-ajps.ykd')
read_dict_yoshikoder <- function(path){
    
    xml <- XML::xmlParse(path)
    root <- XML::xpathSApply(xml, "/dictionary/cnode")
    dict <- nodes2list(root[[1]])
    dict <- list2dictionary(dict)
    return(dict)
}

# Internal function for read_dict_yoshikoder
nodes2list <- function(node, dict = list()){
    nodes <- XML::xpathSApply(node, "cnode")
    if (length(nodes)) {
        for (i in seq_along(nodes)) {
            key <- XML::xmlGetAttr(nodes[[i]], name = "name")
            dict[[key]] <- nodes2list(nodes[[i]], dict[[key]])
        }
    } else {
        dict <- unname(XML::xpathSApply(node, "pnode/@name"))
    }
    return(dict)
}

#' convert quanteda dictionary objects to the YAML format
#' 
#' Converts a \pkg{quanteda} dictionary object constructed by the 
#' \link{dictionary} function into the YAML format. The YAML 
#' files can be editied in text editors and imported into 
#' \pkg{quanteda} again.
#' @param x dictionary object
#' @return \code{as.yaml} returns a dictionary in the YAML format
#' @export
#' @examples
#' \dontrun{
#' dict <- dictionary(file = '/home/kohei/Documents/Dictionary/LaverGarry.txt', format = 'wordstat')
#' yaml <- as.yaml(dict)
#' cat(yaml, file = '/home/kohei/Documents/Dictionary/LaverGarry.yaml')
#' }
as.yaml <- function(x) {
    yaml <- yaml::as.yaml(simplify_dictionary(x, TRUE), indent.mapping.sequence = TRUE)
    yaml <- stri_enc_toutf8(yaml)
    return(yaml)
}


# Internal function for as.yaml to simplify dictionary objects
simplify_dictionary <- function(entry, omit = TRUE, dict = list()) {
    entry <- unclass(entry)
    if (omit) {
        dict <- simplify_dictionary(entry, FALSE)
    } else {
        if (length(entry)) {
            is_category <- sapply(entry, is.list)
            category <- entry[is_category]
            if (any(is_category)) {
                for (i in seq_along(category)) {
                    dict[[names(category[i])]] <- simplify_dictionary(category[[i]], TRUE, dict)
                }
                dict[['__']] <- unlist(entry[!is_category], use.names = FALSE)
            } else {
                dict <- unlist(entry, use.names = FALSE)
            }
        }
    }
    return(dict)
}

# return the nested depth of a dictionary
# a dictionary with no nesting would have a depth of 1
dictionary_depth <- function(this, thisdepth = -1) {
    # http://stackoverflow.com/a/13433689/1270695
    this <- unclass(this)
    if (!is.list(this)) {
        return(thisdepth)
    } else {
        return(max(unlist(lapply(this, dictionary_depth, thisdepth = thisdepth + 1))))    
    }
}
