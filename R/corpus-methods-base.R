#' base method extensions for corpus objects
#' 
#' Extensions of base R functions for corpus objects.
#' @name corpus-class
#' @param x a corpus object
#' @keywords internal corpus
#' @seealso \code{\link{summary.corpus}}
NULL

#' @export
#' @rdname corpus-class
#' @method print corpus
print.corpus <- function(x, ...) {
    cat("Corpus consisting of ", format(ndoc(x), big.mark=","), " document",
        if (ndoc(x) > 1L) "s" else "", sep = "")
    if (!is.null(docvars(x))) 
        cat(" and ", format(ncol(docvars(x)), big.mark=","), " docvar", 
            if (ncol(docvars(x)) > 1L) "s" else "", sep="")
    if (is.corpuszip(x)) {
        cat(" (compressed ", 100 - round(x$compression_rate, 1), "%)", sep = "")
    }
    cat(".\n")
    
    #         ", ",
    #         ifelse(is.null(corp$tokens), "un", ""),
    #         "indexed.\n", sep="")
    #     cat("Settings:")
    #      tempSettings <- unlist(settings(corp))
    #      for (i in seq_along(tempSettings)) {
    #          print(tempSettings[i])
    #      }
}

#' @return \code{is.corpus} returns \code{TRUE} if the object is a corpus
#' @rdname corpus-class
#' @export
is.corpus <- function(x) {
    "corpus" %in% class(x)
}

#' @return \code{is.corpuszip} returns \code{TRUE} if the object is a compressed corpus
#' @rdname corpus-class
#' @export
is.corpuszip <- function(x) {
    "corpuszip" %in% class(x)
}


#' summarize a corpus or a vector of texts
#' 
#' Displays information about a corpus or vector of texts.  For a corpus, this 
#' includes attributes and metadata such as date of number of texts, creation 
#' and source.  For texts, prints to the console a desription of the texts,
#' including number of types, tokens, and sentences.
#' 
#' @param object corpus or texts to be summarized
#' @param n maximum number of texts to describe, default=100
#' @param verbose set to \code{FALSE} to turn off printed output, for instance
#'   if you simply want to assign the output to a \code{data.frame}
#' @param showmeta for a corpus, set to \code{TRUE} to include document-level
#'   meta-data
#' @param toLower convert texts to lower case before counting types
#' @param ... additional arguments passed through to \code{\link{tokenize}}
#' @export
#' @method summary corpus
#' @keywords internal corpus
#' @examples
#' # summarize corpus information
#' summary(data_corpus_inaugural)
#' summary(data_corpus_inaugural, n=10)
#' mycorpus <- corpus(data_char_ukimmig2010, 
#'                    docvars = data.frame(party=names(data_char_ukimmig2010))) 
#' summary(mycorpus, showmeta=TRUE)  # show the meta-data
#' mysummary <- summary(mycorpus, verbose=FALSE)  # (quietly) assign the results
#' mysummary$Types / mysummary$Tokens             # crude type-token ratio
summary.corpus <- function(object, n = 100, verbose = TRUE, showmeta = FALSE, toLower = FALSE, ...) {
    #     if (!(addedArgs <- names(list(...)) %in% )
    #         warning("Argument", ifelse(length(addedArgs)>1, "s ", " "), names(addedArgs), " not used.", sep = "")
    
    if (verbose) {
        cat("Corpus consisting of ", ndoc(object), " document",
            ifelse(ndoc(object)>1, "s", ""), 
            ifelse(is.corpuszip(object), paste0(" (compressed ", 100 - round(object$compression_rate, 1), "%)"), ""),
            ifelse(ndoc(object) <= n, "", 
                   paste(", showing ", n, " document", ifelse(n>1, "s", ""), sep="")),
            ".\n", sep="")
    }
    
    if (verbose) cat("\n")
    outputdf <- data.frame(summary(texts(object), n, verbose = FALSE, toLower = toLower, ...))
    if (!is.null(docvars(object)))
        outputdf <- cbind(outputdf, docvars(object)[1:min(c(n, ndoc(object))),, drop=FALSE])
    # if (detail) outputdf <- cbind(outputdf, metadoc(object))
    if (showmeta)
        outputdf[names(metadoc(object))] <- metadoc(object)[1:min(c(n, ndoc(object))),,drop=FALSE]
    if (verbose) {
        print(head(outputdf, n), row.names=FALSE)
        cat("\nSource:  ", unlist(metacorpus(object, "source")), "\n", sep="")
        cat("Created: ",   unlist(metacorpus(object, "created")), "\n", sep="")
        cat("Notes:   ",   unlist(metacorpus(object, "notes")), "\n\n", sep="")
    }
    # invisibly pass the summary of the texts
    return(invisible(outputdf))
}


#' @rdname corpus-class
#' @param c1 corpus one to be added
#' @param c2 corpus two to be added
#' @details The \code{+} operator for a corpus object will combine two corpus 
#'   objects, resolving any non-matching \code{\link{docvars}} or 
#'   \code{\link{metadoc}} fields by making them into \code{NA} values for the 
#'   corpus lacking that field.  Corpus-level meta data is concatenated, except 
#'   for \code{source} and \code{notes}, which are stamped with information 
#'   pertaining to the creation of the new joined corpus.
#'   
#'   The `c()` operator is also defined for corpus class objects, and provides
#'   an easy way to combine multiple corpus objects.
#'   
#'   There are some issues that need to be addressed in future revisions of 
#'   quanteda concerning the use of factors to store document variables and 
#'   meta-data.  Currently most or all of these are not recorded as factors, 
#'   because we use \code{stringsAsFactors=FALSE} in the 
#'   \code{\link{data.frame}} calls that are used to create and store the 
#'   document-level information, because the texts should always be stored as 
#'   character vectors and never as factors.
#' @export
`+.corpus` <- function(c1, c2) {
    ## deal with metadata first
    # note the source and date/time-stamp the creation
    metacorpus(c1, "source") <- paste("Combination of corpuses", deparse(substitute(c1)),
                                      "and", deparse(substitute(c2)))
    metacorpus(c1, "created") <- date()
    # concatenate the other fields if not identical already
    for (field in names(metacorpus(c2))) {
        if (field %in% c("source", "created")) next
        if (!identical(metacorpus(c1, field), metacorpus(c2, field)))
            metacorpus(c1, field) <- paste(metacorpus(c1, field), metacorpus(c2, field))
    }
    
    row.names <- c(rownames(c1$documents), rownames(c2$documents))
    c1$documents <- data.frame(
        data.table::rbindlist(list(c1$documents, c2$documents), use.names = TRUE, fill = TRUE)
    )
    #  Put rownames back in because the hadleyverse discards them
    rownames(c1$documents) <- make.unique(row.names, sep='')
    
    # settings
    ### currently just use the c1 settings
    
    # special handling for docnames if item is corpuszip
    if (is.corpuszip(c1)) {
        x <- c(texts(c1), texts(c2))
        x[1 : (length(x)-1)] <- paste0(x[1 : (length(x)-1)], quanteda_document_delimiter)
        c1$texts <- memCompress(x, 'gzip')
        c1$docnames <- rownames(c1$documents)
    }

    return(c1)
}


#' @rdname corpus-class
#' @param recursive logical used by `c()` method, always set to `FALSE`
#' @examples 
#' 
#' # concatenate corpus objects
#' corpus1 <- corpus(data_char_ukimmig2010[1:2])
#' corpus2 <- corpus(data_char_ukimmig2010[3:4])
#' corpus3 <- corpus(data_char_ukimmig2010[5:6])
#' summary(c(corpus1, corpus2, corpus3))
#' @export
c.corpus <- function(..., recursive = FALSE) {
    x <- list(...)
    if (length(x) == 1) return(x[[1]])
    result <- x[[1]] + x[[2]]
    if (length(x) == 2) return(result)
    for (i in 3:length(x))
        result <- result + x[[i]]
    metacorpus(result, "source") <- paste0("Concatenation by c.corpus(", names(x), ")")
    return(result)
}


#' @export
#' @param i index for documents or rows of document variables
#' @param j index for column of document variables
#' @param drop if \code{TRUE}, return a vector if extracting a single document
#'   variable; if \code{FALSE}, return it as a single-column data.frame.  See
#'   \code{\link{drop}} for further details.
#' @method [ corpus
#' @rdname corpus-class
#' @examples 
#' 
#' # ways to index corpus elements
#' data_corpus_inaugural["1793-Washington"]    # 2nd Washington inaugural speech
#' data_corpus_inaugural[2]                    # same
#' # access the docvars from data_corpus_irishbudget2010
#' data_corpus_irishbudget2010[, "year"]
#' # same
#' data_corpus_irishbudget2010[["year"]]            
#' 
#' # create a new document variable
#' data_corpus_irishbudget2010[["govtopp"]] <- 
#'     ifelse(data_corpus_irishbudget2010[["party"]] %in% c("FF", "Greens"), 
#'            "Government", "Opposition")
#' docvars(data_corpus_irishbudget2010)
`[.corpus` <- function(x, i, j = NULL, ..., drop = TRUE) {
    if (is.null(j))
        return(texts(x)[i, ...])
    else {
        if (!is.null(docvars(x)))
            x$documents <- x$documents[-1]  # remove texts
        return(x$documents[i, j, ..., drop = drop])
    }
}

#' @export
#' @method [[ corpus
#' @rdname corpus-class
`[[.corpus` <- function(x, i, ...) {
    if (is.null(docvars(x)))
        stop("cannot index docvars this way because none exist")
    x$documents[i, ...]
}

#' @export
#' @param value a vector that will form a new docvar
#' @method [[<- corpus
#' @rdname corpus-class
`[[<-.corpus` <- function(x, i, value) {
    x$documents[i] <- value
    x
}

#' @export
#' @param object the corpus about which you want structural information
#' @param ... not used
#' @method str corpus
#' @importFrom utils str
#' @rdname corpus-class
str.corpus <- function(object, ...) {
    # message("OK, but note: accessing corpus internals directly voids your warranty.")
    str(unclass(object))
}

