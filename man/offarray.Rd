\name{offarray}
\alias{offarray}
\alias{offarray.default}
\alias{offarray.table}
\alias{offarray.data.frame}
\alias{as.array.offarray}
\alias{as.data.frame.offarray}
\alias{firstel}
\alias{firstel<-}
\alias{lastel}
\alias{dimrange}
\alias{dimseq}
\alias{strguts.offarray}
\alias{slindoff}
\alias{whichoff}
\title{Arrays with non-1 offsets}
\description{
An \code{offarray} is an array where (some) indices may not start at 1. Easiest example is

\preformatted{
  dumbo <- offarray( 1:6, dimseq=list( X=4:6, Y=c( 'aleph', 'beth'))
  print( dumbo)
  # now e.g. dumbo[ 5, 'aleph'] will give 2
}

Things should "just work" as you expect. However, compared to base-R and to the \pkg{Oarray} package which inspired \code{offarray}, there are a few things that might trip you up. The comprehensive list below looks much worse than it actually is; after the first 5, you might do better to simply read the EXAMPLES.

\itemize{
\item For speedy loop operations on \code{offarray} objects, you should definitely use \code{\link{autoloop}} rather than writing nested for-loops; it's \emph{much} faster and \emph{no} harder.
\item from v2.0 onwards, \code{offarray} and \code{\link{autoloop}} code \emph{should} be compatible with package \pkg{RTMB}, with one tiiiiny modification. You will need to load the package \pkg{offartmb}.
\item Behind the scenes, each \code{offarray} has an attribute "offset", a vector which holds the lowest index value for that dimension, or NA if that dimension has non-NULL \code{dimnames}. So in the above case, the offset would be \code{c(4,NA)}. NA-offset dimensions work (almost) exactly like in normal R{} when it comes to subsetting; they start at index 1, and negative value means "drop that element", etc.
\item You shouldn't need to access the offset directly, andbut there are helper routines to help you with relevant stuff. See \bold{Utility functions} below.
\item To pass an \code{offarray} to some existing R{} array/matrix routine, you \emph{may} need to \code{unclass} it first (basically if that R{} code is gonna subset it explicitly), or just \code{c} it the dimensions are irrelevant. That depends on the other code, and it's up to you to figure out.
}

Now we get into more detailed stuff

\itemize{
\item Negative offsets are interpreted just like positive ones, so \code{x[-1]} means "give me the minus-one-th element of \code{x}". (It's not that weird; there are plenty of algorithms where indices can be negative, e.g. Wynn's epsilon method which naturally start at index -1). In base-R, on the other hand, \code{x[-1]} means "discard the first element of \code{x}". To get the base-R behaviour in one specific dimension, you can set its offset to NA.
\item Subset-extraction keeps (and adjusts if necessary) each non-NA offset, provided that its corresponding subset is increasing and consecutive. Thus, in a 1D case, \code{x[2]} and \code{x[2][2]} and \code{x[2][2][2]} will always return the same thing, whereas base-R would give NAs after the first; and after \code{y <- x[2]}, the offset of \code{y} will be 2.
\item Subsetting with a \emph{non} consecutive-increasing index, eg \code{x[c(5,5),]} or \code{x[5:4,,]}, will set the corresponding offset to NA. This will trigger a warning unless the offset was NA already, or you have explicitly told R{} to do it via \code{NOOF}; see \bold{Examples}.
\item As in base-R, the idiom \code{x[]} still returns all of \code{x}, as an \code{offarray}. And an "explicitly missing" subset, such as the first index in \code{x[,5:6]}, means "all of that dimension".
\item By default, dimensions are \bold{not} dropped, so that \code{x[5,3,4:6]} will still return a 3D offarray. Use \code{x[ SLICE=5, SLICE=3, 4:6]} to drop those two dimensions, to give you 1D 3-element \code{offarray}. You can set the additional argument \code{drop=TRUE} to auto-slice, which is convenient if there's several such indices.
\item Vector (1D) subsets are available that work like R{}'s normal "treat the whole array as a 1-indexed vector, then extract the specified elements". They are indicated by \code{x[ VECSUB=1:5] <- 17} or \code{x[ VECSUB=x>0]}. The return value is always just a vector, of the same type as \code{x}. Note that, if \code{x} is a vector (of class \code{offarray}), then \code{x[1:5]} treats the \code{1:5} relative to the offset (so if the offset is -3, \code{x[1:5]} would take the 5th--9th elements), whereas \code{x[VECSUB=1:5]} always takes the first five elements.
\item Matrix-subsetting works (and respects any non-NA offsets), but you need to tell R{} you want it, via \code{x[ MATSUB=mymat]}. If some of your dimensions are character-index rather than numeric-index, you can use a \code{data.frame} or even \code{list} (one element per dimension) instead of a numeric \code{matrix}, again with \code{x[ MATSUB=my_list_or_data.frame]}. The return value is always just a vector, of the same type as \code{x}.
\item Logical subsets (which must be the same length as the corresponding dimension) are first mapped to numeric ones, then the above rules are applied.
\item Character-indexed dimensions, ie with non-NULL \code{dimnames} (of mode \code{character}, of course) can be mixed with normal integer-indexed ones, just like in base R{}. You can subset from character-indexed dimensions using either characters (safer and clearer) or logicals or integers; if integers are used, they always are treated as starting from 1, ie you can't set a non-1 offset for character-indexed dimensions. The offset for such dimensions will be \code{NA}; as noted above, you can also specify offset \code{NA} for non-character-indexed dimensions, so that they are subsettable just like base-R.
\item There are some weird subsetting options allowed in base-R, that produce IMO inconsistent and unuseful results. I have generally banned these in \code{offarray}, to protect you from yourself! One example is subset-values that are NA or 0 or out-of-range; in \code{offarray}, they produce errors. (Except, you can of course use 0 as a perfectly legimimate index provided that the corresponding offset is zero or negative.)
\item If any subset is non-empty and zero-length, the return value is a zero-length vector of the same type as \code{x}, eg from \code{x[rep(F,length(x))]} or \code{x[integer(0)]}.
\item Subset-replacement, eg \code{x[5:6,i2>0]}, follows the same rules, \emph{mutatis mutandum}.
}
\subsection{Constructing and deconstructing an offarray}{
You can construct an \code{offarray} directly from numeric (or other atomic) values, much like you would a normal array. The easiest way is usually by setting the \code{dimseq} argument (which then bypasses all other arguments except the values \code{x}), like so:

\preformatted{
  test <- offarray( 1:6, dimseq=list( X=3:5, Y=c( 'A', 'b')))
}

which means the first dimension ("X"), is numeric-indexed with offset 3, and the second ("Y") is character-indexed.

If you just want a dimension to behave like base-R (ie starting at 1 but with negative subscripts meaning "drop"), but not to have \code{dimnames}, then the following will work (by setting the offset for that dimension to NA):

\preformatted{
  test <- offarray( 1:6, dimseq=list( X=3:5, Y=character( 2)))
}

Instead of \code{dimseq}, you can specify \code{dim/first/last} directly; any 2 of the 3 should do, and they will be sanity-checked. However, if any dimension is supposed to be character-indexed, you'd also need to pass \code{dimnames=list(<something>)} too--- so, just using \code{dimseq=...} is probably easier.

For a 1D offarray, you can use a vector \code{dimseq} instead of a list of length 1. For example:

\preformatted{
  eco <- offarray( runif( 4), dimseq=0:3)
  # same as
  veco <- offarray( runif( 4), dimseq=list( 0:3))
}

You can also convert directly to an \code{offarray} from a \code{table} (ie something returned by \code{table()}) or from a \code{data.frame}. Technically, those two cases are S3 methods for the generic "constructor" function \code{offarray()}. Note that \code{table} might not give you the full ranges you expect, if there are no observations in categories where you might expect some. The safest solution is to add the \code{dimseq} argument explicitly; see \bold{Examples}.

To "deconstruct", \code{as.array} has a method for \code{offarray} objects. Unless \code{make_dimnames==FALSE}, it turns all the numeric dimensions with non-NA offsets into \emph{character} dimnames that incorporate the offset.

The almost-inverse to \code{offarray( <data.frame>)} is \code{as.data.frame} which has a method for \code{offarray} objects; see \bold{Examples}. Technically: the output is a \code{data.frame} with one column per dimension of the input, plus a column for the contents, which will be called "response" unless you set the \code{name_of_response} argument. The other columns will have names "D1", "D2", etc, unless either (i) the input has a \emph{named} \code{dimnames} attribute, in which the names will be used, or (ii) the argument "add_names" is set to a character vector naming the dimensions. If the input has any \code{dimnames}, then its non-NULL elements will be used in place of 1,2,3,... etc for the entries in the corresponding columns. Character \code{dimnames} are kept as \code{character} columns in the output, \emph{never} converted to horrible factors--- regardless of \code{stringsAsFactors}.

\code{offarray} offers partial support for naming your dimnames. For example, if your first dimension is Sex and your second dimension is Age, you can create your \code{offarray} like this:

\preformatted{
  myoff <- offarray( ....., dimnames=list( SEX=c( 'F', 'M'), AGE=NULL))
}

the NULL signifying that Age is numeric. However, I'm not sure how well named-dimnames will propagate or behave in all conceivable operations on an \code{offarray}, so the \emph{safest} course is probably to not use them.

If you don't name the dimnames, then \code{offarray} will try to figure them out from any names supplied in the vector arguments \code{first}, \code{last}, and/or \code{dim}; it will also check that any such names are consistent with each other, and with names-of-dimnames if the latter is supplied. For example:

\preformatted{
  myoff <- offarray( ..., first=c( SEX=1, 0), last=c( 2, AGE=20), dimnames=list( c( 'F', 'M'), NULL)
}

will work, and will produce an \code{offarray} the names of whose dimnames are \code{SEX} and \code{AGE} respectively. But this next one won't work, because there's a name clash:

\preformatted{
  myoff <- offarray( 1:42, first=c( SEX=1, 0), last=c( GENDER=2, AGE=20), dimnames=list( c( 'F', 'M'), NULL))
}

Calling \code{dim} on an offarray will often return a \emph{named} vector, related to the previous paragraph. This has pros, eg being able to write \code{dim(myoff)["SEX"]} or \code{dimseq(x)$SEX}, but potentially also cons, eg due to R{}'s habit of augmenting names when things are joined via \code{c()}. Time will tell whether it was a wise decision.
}
\subsection{Utility functions}{
\code{firstel} (and \code{firstel<-}), \code{lastel}, \code{dimrange}, and \code{dimseq} are convenience functions for getting the range of an array index, without you having to faff around with offsets directly. They work with regular arrays/matrices, as well as \code{offarray} objects. Note \code{dimseq(x)[[i]]} returns \code{seq_len(dim(x)[i]} if \code{dimnames(x)[i]} is NULL and \code{attr(x,"offset")[i]} is NA. Watch out if you're going to put the results back into another call to \code{offarray}--- see \code{\link{which}} and EXAMPLES. To set the offset manually, you can do eg \code{firstel(x,3)<-5} or \code{firstel(x)<-c(3,6)} (set both offsets of a 2D \code{offarray}) or \code{firstel(x,4)<-NA} (to set base-R-style subscripting for the 4th dimension). To set a non-NA offset for some dimension, there must be no \code{dimnames} for that dimension (since character-indexed dimensions must always have an NA offset), so eg \code{dimnames(x)[[3]]<-NULL;firstel(x,3)<-10} would be the safe if weird way.

\code{slindoff} is the analog of \code{slice.index} for offarrays, returning values in the "coordinate system" of the \code{offarray}. I deliberately haven't tried to make \code{slice.index} generic since--- as far as reasonably possible--- "standard" operations on offarrays, i.e. ops that don't need to know about offarray-ness, should just work.

\code{whichoff} is like \code{which( x, arr.ind=TRUE, useNames=FALSE)} but returns a matrix with \code{x}-like dimensions (ie the offsets are incorporated). Thus you can use the result to matrix-subset \code{x}, ie via \code{x[MATSUB=<result of whichoff>]}). Note that "for technical reasons" you usually need to supply the logical operation as a second argument, rather than doing it "inline" as you would with \code{which}; see \bold{Examples}.

Specific methods for \code{offarray} class are currently: subset (eg \code{X[...]}); subset-replacement (eg \code{X[...] <- Y}); \code{as.array}; \code{print}; \code{t} (transpose); \code{aperm}; \code{str} (which calls \code{strguts.offarray}); \code{rev}; \code{head} and \code{tail} (which preserve offarray-ness for 1D and 2D offarrays, but act otherwise like normal \code{head} and \code{tail}); \code{dim<-} & \code{dimnames<-} which avoid problems caused by base-R's treatment of \code{dim}; and \code{as.data.frame}. Also, \code{\link{pmax}} and \code{\link{pmin}} are modified to be offarray-aware.

See also \code{\link{autoloop}} for a convenient loop-free way to vectorize some computations on \code{offarray}s (and on standard arrays).
}
\subsection{Features}{
\itemize{
\item \code{row} and \code{col} don't work properly even with 2D \code{offarrays}.
\item Even 1D \code{offarray} objects, which are like vectors "really", still have a \code{dim} attribute.
}
}
}
\usage{
# Don't read this--- look at EXAMPLES and ARGUMENTS
offarray( x, ...) # generic
\method{offarray}{default}( x= NA, first = rep(1L, length(dim)),
  last = first + dim - 1L, dim = NULL, dimnames = NULL,
  dimseq, ...) # S3 method for default
\method{offarray}{table}( x, template=NULL, dimseq=NULL,
    warn_trim=getOption( "offarray_table_warn_trim", FALSE),
    ...) # S3 method for table
\method{offarray}{data.frame}( x, data.col, ...) # S3 method for data.frame
\method{as.data.frame}{offarray}( x, row.names=NULL, optional= NULL, add_names = NULL,
    name_of_response = "response", ...) # S3 method for offarray
\method{as.array}{offarray}( x, ..., make_dimnames=TRUE) # S3 method for offarray
firstel( x, which=NULL)
firstel( x, i) <- value
lastel( x, which=NULL)
dimrange( x, which=NULL)
dimseq( x, which=NULL, drop=TRUE)
slindoff( offa, MARGIN)
whichoff( x, expr=.)
strguts.offarray( x)
}
\arguments{
\item{ x}{(offarray.default) elements of the array, as per \code{matrix} or \code{array}; or a \code{table} or \code{data.frame}, in which case the converter method will be called. In all other functions documented here, \code{x} should be an existing \code{offarray}.}
\item{ first, last, dim}{set any 2 out of these 3--- obvious meaning, as per \code{array}. If any of these has \code{names}, then these names will also be applied to \code{dimnames}, and will print on output. For character-indexed dimensions (ie with non-null \code{dimnames}), \code{first} must be 1 and \code{last} must be the number of elements.}
\item{ dimnames}{Set this only if you want one or more indices to be primarily "character-addressed" (eg sex being "F" or "M"). It's OK to have some null ones, eg \code{dimnames=list( Year=NULL, Sex=c( "F", "M"))}.}
\item{ dimseq}{You can use this to set up an \code{offarray} directly--- it's often the easiest way. It should always be a list.See \code{\link{dimseq}} details for more info; you could also get it from calling \code{\link{dimseq}} on another \code{offarray}.}
\item{ data.col}{(offarray.data.frame) which column should become the contents of the \code{offarray}}
\item{ template}{(offarray.table) \code{template} can be an existing \code{offarray}, in which the result will have the same dimensions. This gets round the issue that \code{table} won't return index-values that don't occur in that particular dataset, but which might in other similar datasets. Alternatively, \code{dimseq} lets you specify the output dimensions directly, just as you would for \code{offarray} itself; I think you can use \code{dimseq=dimseq(mytemp)} instead of \code{template=mytemp}.}
\item{ warn_trim}{(offarray.table) if \code{template} or \code{dimseq} is supplied and \code{x} has some dimensions which don't fully fit into the desired output, \code{x} will be trimmed to fit; this parameter controls whether a warning is issued (and you can use \code{options(warn=2)} to force a crash in that case). In some applications, auto-trimming is good, but in others it's bad; you can control the general behaviour via \code{options(offarray_table_warn_trim=TRUE/FALSE)}, rather than at every call to \code{offarray<.table>}.}
\item{ add_names}{(as.data.frame.offarray) how to set the column names of the results. Should be logical or character, or the default of NULL. NULL uses information in \code{dimnames}, including \code{names(dimnames)} if those exist; FALSE means the elements will be called 1, 2, 3 etc (as per \code{slice.index}), and the columns "D1", "D2", etc, which is also where things end up if NULL doesn't give anything useful. Set to your own character vector if \code{dimnames} doesn't help and you want to clearly label the columns.}
\item{ name_of_response}{(as.data.frame.offarray) what to call the output column that holds the array \emph{contents} (as opposed to its dimensions).}
\item{ row.names}{(as.data.frame.offarray) as per \code{data.frame()}}
\item{ optional, ...}{ignored; present only for compatibility with generic S3 definitions.}
\item{ make_dimnames}{(as.array.offarray) whether to create extra dimnames for indices that used to have offsets.}
\item{ which, i}{if only certain dimensions are of interest to \code{firstel/lastel/dimrange/dimseq}. Default is to return results for all dimensions. \code{firstel( x, "Sex")} strips the \code{names} attribute of the result, and is preferable to \code{firstel( x)[ "Sex"]} if the assignee might be used again in \code{offarray}--- see \bold{Examples}.}
\item{ value}{(in \code{firstel<-}) first index value for that dimension; will be coerced to \code{integer}, or NA to use base-R indexing for that dimension (subtly different to \code{value=1}).}
\item{ drop}{only in \code{dimseq} and if \code{length(which)==1}, should a vector rather than list be returned?}
\item{ offa}{(slindoff) an \code{offarray}}
\item{ MARGIN}{(slindoff) 1 for row, 2 for column, etc. Or, a string which matches into \code{names( dimnames( offa))}. Scalar only.}
\item{ expr}{(whichoff) an R{} "test" expression containing a \code{.}, which will be evaluated for you after substituting the \code{.} with the first argument \code{x}. It should return a \code{logical} result. If you just passed in the evaluated test like you would with \code{which}, you'd lose the offsets; see \bold{Examples}.}
}
\value{\code{offarray} returns, yes, an \code{offarray}; and \code{as.data.frame} returns a \code{data.frame}... \code{firstel} and \code{lastel} return vectors, named if the dimnames are too. \code{dimrange} returns a two-column matrices, with rows named according to dimnames. \code{dimseq} returns a list containing the indices each dimension, as character or integer; see also \code{which} and \code{drop}.'strguts.offarray' returns a string showing the dimension names and ranges--- for use eg in a \code{print} method for something with an \code{offarray} component. \code{slindoff} returns an \code{offarray} of the same shape; the attributes preserved are \code{dim}, \code{dimnames}, \code{offset}, and \code{class}.
}
\details{The first element of each index is stored in the \code{offset} attribute. This will be set to NA for indices with explicit \code{dimnames}, which is useful. It can also be NA for a dimension without \code{dimnames} where base-R indexing applies; you can do that via \code{firstel(x,3)<-NA}, or at creation by \code{offarray(...,dimseq=list( character(5)),...)}. Sometimes the latter helps, but my advice is to avoid mixing offarray-style and base-R-style indices within the same object.
}
\examples{
## Not fully checked!!
# Manual construction
test <- offarray( 1:6, first=c( X=3, Y=4), last=c( 5, 5))
test
#        Y
#  X      [,4] [,5]
#    [3,]    1 4
#    [4,]    2 5
#    [5,]    3 6
#
# Easier:
test2 <- offarray( 1:6, dimseq=list( X=3:5, Y=4:5))
# Easiest!
test3 <- offarray( 6:1, dimseq=dimseq( test))
## From a table:
set.seed( 1)
df <- data.frame( X=sample(  LETTERS[1:3], 5, TRUE), Y=sample( 8:10, 5, TRUE))
tab <- with( df, table( X, Y))
offo <- offarray( tab)
# reordered version of 'df', unless 'df' has duplicated rows
df2 <- as.data.frame( offo, name_of_response='count')
# and back from data.frame to offarray
offo2 <- offarray( df2, 'count')
identical( offo, offo2)
## With explicit dimnames (offset disregarded for those indices)
# Next will fail because dimnamed dims must start at 1
try( test <- offarray( 1:6, first=c( X=3, Y=4), last=c( 5, 5), dimnames=list( NULL, c( 'A', 'b'))))
# This is OK:
test <- offarray( 1:6, first=c( X=3, Y=1), last=c( 5, 2), dimnames=list( NULL, c( 'A', 'b')))
# Or:
test <- offarray( 1:6, dimseq=list( X=3:5, Y=c( 'A', 'b')))
t( test)
#     X
#  Y   [,3] [,4] [,5]
#    A    1    2    3
#    b    4    5    6
#
test[3,1]
#        Y
#  X      A
#    [3,] 1
#
# NB offset changes automatically if subsetting is consecutive:
test[4,]
#        Y
#  X      A b
#    [4,] 2 5
test[4,][4,] ## SAME!
#        Y
#  X      A b
#    [4,] 2 5
test[ SLICE=4,] # first dim dropped, as requested
#  Y
#  A b
#  2 5
#
test[3,1, nooff=TRUE] # loses offarrayness; NB row labels in printout
#        Y
#  X      A
#    [1,] 1
test[c(3,3), 1, nooff=TRUE]
#       Y
#  X      A
#    [1,] 1
#    [2,] 1
test[ NOOFF=c( 3,3),1 ] # same
try( test[c(3,3), 1]) # omitted NOOFF --- warning
# 1 vs NA offset
test1 <- offarray( 1:4, first=1, dim=4)
test1[2][2]
#   [2]
#     2
firstel( test1, 1) <- NA
# test1 looks exactly the same, but...
try( test1[2][2]) # OOB
test[ MATSUB=rbind( c( 3, 2), c( 3, 1), c( 5, 2))] # 4 1 6
test[ MATSUB=list( c( 3,5,5), c( 'b', 'A', 'b') )] # 4 3 6
test[ 3, 'A'] <- 999
test[ VECSUB=2] <- 17
## Disregarding that "4"--- NB using try() just to avoid RCMD CHECK nannyism :/
try( test[3,4])
# Error in structure(1:6, .Dim = structure(c(3L, 2L), .Names = c("X", "Y" :  subscript out of bounds
## Without try(), you'd see this:
# Error in 1:6[1, 4, drop = FALSE] : subscript out of bounds
## Y-offset is really 1
test[3,1]
#      Y
# X      b
#   [3,] 4
## still an array
## ... but Y is probably meant to be referred to "by character"
test[3,'b']
#      Y
# X      b
#   [3,] 4
## Dropping single-element dimensions:
test[ 3, SLICE='b']
#  X
#  [3,]
#     4
## ie from a 2D to a 1D offarray
test[ SLICE=3, SLICE='b']
#  [1] 4
## ie full drop
firstel( test)
#  X Y
#  3 1
firstel( test, 'X')
#  [1] 3
## no name
dimrange( test)
#    [,1] [,2]
#  X    3    5
#  Y    1    2
dimseq( test, 1)
# [1] 3 4, 5
flubbo <- table( X=c( 1, 1, 3), Y=c( 'A', 'B', 'A'))
offarray( flubbo)
## That's OK: it figures out that X=2 is also wanted
#        Y
#  X      A B
#    [1,] 1 1
#    [2,] 0 0
#    [3,] 1 0
## But if no observations at the end of a range...
flubbo2 <- table( X=c( 1, 1), Y=c( 'A', 'B'))
offarray( flubbo2)
## ... X=2 and X=3 are missing (of course)
## So let's make sure it includes them:
offarray( flubbo2, dimseq=list( X=1:3, Y=c( 'A', 'B')) )
#        Y
#  X      A B
#    [1,] 1 1
#    [2,] 0 0
#    [3,] 0 0
## And test the trimming facility
offarray( flubbo2, dimseq=list( X=2:3, # X=1 will be discarded
    Y=c( 'A', 'B')), warn_trim=TRUE)
# ... warning
slindoff( test, 'X')
#        Y
#  X      A b
#    [3,] 3 3
#    [4,] 4 4
#    [5,] 5 5
slindoff( test, 2)
## NB no offset operational since dimnames()[[2]] is non-empty
#        Y
#  X      A b
#    [3,] 1 2
#    [4,] 1 2
#    [5,] 1 2
whichoff( test>4) # unlike which( test>4, arr.ind=T)
## NB obvious approach *doesn't* work:
whichoff( test>4) # loses offsets, cos 'test>4' just returns arrayi
}
\keyword{misc}
