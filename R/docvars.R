#' get or set for document-level variables
#' 
#' Get or set variables associated with a document in a \link{corpus}, or get
#' these variables from a \link{tokens} or \link{dfm} object.
#' @param x \link{corpus}, \link{tokens}, or \link{dfm} object whose
#'   document-level variables will be read or set
#' @param field string containing the document-level variable name
#' @return \code{docvars} returns a data.frame of the document-level variables,
#'   dropping the second dimension to form a vector if a single docvar is
#'   returned.
#' @examples 
#' # retrieving docvars from a corpus
#' head(docvars(data_corpus_inaugural))
#' tail(docvars(data_corpus_inaugural, "President"), 10)
#' 
#' @export
#' @keywords corpus
docvars <- function(x, field = NULL) {
    UseMethod("docvars")
}

#' @noRd
#' @export
docvars.corpus <- function(x, field = NULL) {
    dvars <- documents(x)
    dvars <- dvars[names(dvars) != 'texts']
    get_docvars(dvars, field, FALSE)
}

#' @noRd
#' @export
docvars.tokens <- function(x, field = NULL) {
    dvars <- attr(x, "docvars")
    get_docvars(dvars, field, FALSE)    
}

#' @noRd
#' @export
docvars.dfm <- function(x, field = NULL) {
    dvars <- x@docvars
    get_docvars(dvars, field, FALSE)
}

#' @rdname docvars
#' @param value the new values of the document-level variable
#' @note Another way to access and set docvars is through indexing of the corpus
#'   \code{j} element, such as \code{data_corpus_irishbudget2010[, c("foren", 
#'   "name"]} or for a single docvar, 
#'   \code{data_corpus_irishbudget2010[["name"]]}.  The latter also permits 
#'   assignment, including the easy creation of new document varibles, e.g. 
#'   \code{data_corpus_irishbudget2010[["newvar"]] <- 
#'   1:ndoc(data_corpus_irishbudget2010)}. See \code{\link{[.corpus}} for 
#'   details.
#'   
#'   Assigning docvars to a \link{tokens} object is not supported.  (You should
#'   only be manipulating these variables at the corpus level.)
#' @return \code{docvars<-} assigns \code{value} to the named \code{field}
#' @examples 
#' # assigning document variables to a corpus
#' corp <- data_corpus_inaugural
#' docvars(corp, "President") <- paste("prez", 1:ndoc(corp), sep = "")
#' head(docvars(corp))
#' 
#' # alternative using indexing
#' head(corp[, "Year"])
#' corp[["President2"]] <- paste("prezTwo", 1:ndoc(corp), sep = "")
#' head(docvars(corp))
#' 
#' @export
"docvars<-" <- function(x, field = NULL, value) {
    UseMethod("docvars<-")
}


#' @noRd
#' @export
"docvars<-.corpus" <- function(x, field = NULL, value) {
    if (is.null(field)) stop("You should specify field.")
    if ("texts" %in% field) stop("You should use texts() instead to replace the corpus texts.")
    documents(x)[field] <- value
    return(x)
}

## internal only
"docvars<-.tokens" <- function(x, field = NULL, value) {
    
    if (is.null(field) && (is.data.frame(value) || is.null(value))) {
        attr(x, "docvars") <- value
    } else {
        if (!is.data.frame(attr(x, "docvars"))) {
            dvars <- data.frame(value, stringsAsFactors = FALSE)
            colnames(dvars) <- field
            attr(x, "docvars") <- dvars
        } else {
            attr(x, "docvars")[[field]] <- value
        }
    }
    return(x)
}

## internal only
"docvars<-.dfm" <- function(x, field = NULL, value) {
    
    if (is.null(field) && (is.data.frame(value) || is.null(value))) {
        x@docvars <- value
    } else {
        if (!is.data.frame(x@docvars)) {
            meta <- data.frame(value, stringsAsFactors = FALSE)
            colnames(meta) <- field
            x@docvars <- meta
        } else {
            x@docvars[[field]] <- value
        }
    }
    return(x)
}

#' get or set document-level meta-data
#' 
#' Get or set the document-level meta-data.
#' @param x a \link{corpus} object
#' @param field character, the name of the metadata field(s) to be queried or set
#' @return For \code{texts}, a character vector of the texts in the corpus.
#'   
#'   For \code{texts <-}, the corpus with the updated texts.
#' @note Document-level meta-data names are preceded by an underscore character,
#'   such as \code{_language}, but when named in in the \code{field} argument,
#'   do \emph{not} need the underscore character.
#' @examples 
#' mycorp <- corpus_subset(data_corpus_inaugural, Year > 1990)
#' summary(mycorp, showmeta = TRUE)
#' metadoc(mycorp, "encoding") <- "UTF-8"
#' metadoc(mycorp)
#' metadoc(mycorp, "language") <- "english"
#' summary(mycorp, showmeta = TRUE)
#' @export
#' @keywords corpus
metadoc <- function(x, field = NULL) 
    UseMethod("metadoc")


#' @noRd
#' @export
metadoc.corpus <- function(x, field = NULL) {
    dvars <- documents(x)
    dvars <- dvars[,names(dvars) != "texts"]
    if (!check_docvars(x, field)) return(NULL)
    get_docvars(dvars, field, TRUE)
}

#' @noRd
#' @export
metadoc.tokens <- function(x, field = NULL) {
    if (!check_docvars(x, field)) return(NULL)
    get_docvars(dvars, field, TRUE)
}

#' @noRd
#' @export
metadoc.dfm <- function(x, field = NULL) {
    if (!is.null(field)) {
        field <- paste0("_", field)
        if (!check_docvars(x, field)){
            return(NULL)
        }
    }
    get_docvars(dvars, field, TRUE)
}

#' @rdname metadoc
#' @param value the new value of the new meta-data field
#' @export
"metadoc<-" <- function(x, field = NULL, value) 
    UseMethod("metadoc")

#' @noRd
#' @export
"metadoc<-" <- function(x, field = NULL, value) {
    if (is.null(field)) {
        stop("You should specify field.")
    } else {
        field <- paste("_", field, sep="")
        documents(x)[field] <- value
    }
    return(x)
}

## internal function to return the docvars for all docvars functions
get_docvars <- function(dvars, fields = NULL, internal = FALSE) {
    if (is.null(dvars))
        dvars <- data.frame()
    if (internal) {
        dvars <- dvars[,stri_startswith_fixed(names(dvars), '_'), drop = FALSE]
    } else {
        dvars <- dvars[,!stri_startswith_fixed(names(dvars), '_'), drop = FALSE]
    }
    if (!is.null(fields)) {
        fields <- fields[fields %in% names(dvars)]
        dvars <- dvars[, fields, drop = TRUE]
    }
    return(dvars)
}

## helper function to check fields
check_docvars <- function(dvars, fields = NULL, internal = FALSE) {
    if (is.null(dvars)) 
        dvars <- data.frame()
    if (internal) {
        dvars <- dvars[,stri_startswith_fixed(names(dvars), '_'), drop = FALSE]
    } else {
        dvars <- dvars[,!stri_startswith_fixed(names(dvars), '_'), drop = FALSE]
    }
    return(fields %in% names(dvars))
}

