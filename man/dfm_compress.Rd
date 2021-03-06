% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfm_compress.R, R/fcm-methods.R
\name{dfm_compress}
\alias{dfm_compress}
\alias{dfm_group}
\alias{fcm_compress}
\title{recombine a dfm or fcm by combining identical dimension elements}
\usage{
dfm_compress(x, margin = c("both", "documents", "features"))

dfm_group(x, groups = NULL)

fcm_compress(x)
}
\arguments{
\item{x}{input object, a \link{dfm} or \link{fcm}}

\item{margin}{character indicating on which margin to compress a dfm, either 
\code{"documents"}, \code{"features"}, or \code{"both"} (default).  For fcm
objects, \code{"documents"} has no effect.}

\item{groups}{either: a character vector containing the names of document 
variables to be used for grouping; or a factor or object that can be 
coerced into a factor equal in length or rows to the number of documents}

\item{...}{additional arguments passed from generic to specific methods}
}
\value{
\code{dfm_compress} returns a \link{dfm} whose dimensions have been
  recombined by summing the cells across identical dimension names
  (\link{docnames} or \link{featnames}).  The \link{docvars} will be
  preserved for combining by features but not when documents are combined.

\code{dfm_group} returns a \link{dfm} whose documents are equal to
  the unique group combinations, and whose cell values are the sums of the
  previous values summed by group.  This currently erases any docvars in the dfm.
}
\description{
"Compresses" or groups a \link{dfm} or \link{fcm} whose dimension names are
the same, for either documents or features.  This may happen, for instance,
if features are made equivalent through application of a thesaurus.  It may
also occur after lower-casing or stemming the features of a dfm, but this
should only be done in very rare cases (approaching never: it's better to do
this \emph{before} constructing the dfm.)  It could also be needed after a 
\code{\link{cbind.dfm}} or \code{\link{rbind.dfm}} operation.

\code{dfm_group} allows combining dfm documents by a grouping 
  variable, which can also be one of the \link{docvars} attached to the dfm.  This is
  identical in functionality to using the \code{"groups"} argument in
  \code{\link{dfm}}.
}
\note{
\code{fcm_compress} works only when the \link{fcm} was created with a document
context.
}
\examples{
# dfm_compress examples
mat <- rbind(dfm(c("b A A", "C C a b B"), tolower = FALSE),
             dfm("A C C C C C", tolower = FALSE))
colnames(mat) <- char_tolower(featnames(mat))
mat
dfm_compress(mat, margin = "documents")
dfm_compress(mat, margin = "features")
dfm_compress(mat)

# no effect if no compression needed
compactdfm <- dfm(data_corpus_inaugural[1:5])
dim(compactdfm)
dim(dfm_compress(compactdfm))

# dfm_group examples
mycorpus <- corpus(c("a a b", "a b c c", "a c d d", "a c c d"), 
                   docvars = data.frame(grp = c("grp1", "grp1", "grp2", "grp2")))
mydfm <- dfm(mycorpus)
dfm_group(mydfm, groups = "grp")
dfm_group(mydfm, groups = c(1, 1, 2, 2))

# equivalent
dfm(mydfm, groups = "grp")
dfm(mydfm, groups = c(1, 1, 2, 2))
# compress an fcm
myfcm <- fcm(tokens("A D a C E a d F e B A C E D"), 
             context = "window", window = 3)
## this will produce an error:
# fcm_compress(myfcm)

txt <- c("The fox JUMPED over the dog.",
         "The dog jumped over the fox.")
toks <- tokens(txt, remove_punct = TRUE)
myfcm <- fcm(toks, context = "document")
colnames(myfcm) <- rownames(myfcm) <- tolower(colnames(myfcm))
colnames(myfcm)[5] <- rownames(myfcm)[5] <- "fox"
myfcm
fcm_compress(myfcm)
}
