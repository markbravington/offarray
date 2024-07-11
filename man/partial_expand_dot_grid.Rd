\name{partial_expand_dot_grid}
\alias{partial_expand_dot_grid}
\title{Like expand.grid but a few rows at a time}
\description{\code{\link{expand.grid}} can produce a overgigantic object, so you can instead call \code{partial_expand_dot_grid} repeatedly in a loop to generate the "next" lot of rows.
}
\usage{partial_expand_dot_grid( dots, n = prod(lengths(dots)), already = 0, force_data.frame = FALSE, switch_var=0)
}
\arguments{
\item{ dots}{list of expandees. Unlike \code{expand.grid}, you have to put them explicitly into a list, rather than using \code{...} facility.}
\item{ n}{how many rows to produce this time. Defaults to all remaining rows. If you overflow the "natural" size of the grid, it will cycle back to the start, and will always give you \code{n} rows.}
\item{ already}{how many rows in previous runs (default 0, for the first time).}
\item{ force_data.frame}{whether to convert the result to a \code{data.frame}, or leave it as a list. The latter might be fine if you just want an environment to evaluate expressions in, or to use for \code{MATSUB=} subsetting in \code{offarray}. Note that conversion will enforce \code{stringsAsFactors=FALSE} because I hate the alternative.}
\item{ switch_var}{if non-0, then an attribute will be added showing the rows where the \code{switch_var}-th expandee changed value.}
}
\examples{
expand.grid( A=1:3, B=4:5)
partial_expand_dot_grid( list( A=1:3, B=4:5), force=TRUE) # the same, thanks to defaults
partial_expand_dot_grid( list( A=1:3, B=4:5), n=6, already=0, force=TRUE) # explicit defaults
partial_expand_dot_grid( list( A=1:3, B=4:5), n=3, already=2)
}
\keyword{misc}
