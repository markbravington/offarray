\name{reclasso}
\alias{reclasso}
\title{Prepare offarray code for RTMB etc}
\description{
Suppose you have \code{\link{offarray}}-using code that computes a log-likelihood or suchlike, based on parameters. And you've debugged it, well done! Now you would like to also run under package \pkg{RMTB}, say (or some other hypothetical package where the "parameters" might not just be "numbers"). To make things work nicely, you just wrap the body of your code in a call to \code{reclasso}, like this:

\preformatted{
  myfun <- function( parzzz, <...>) reclasso(by=parzzz, {
    <old code of myfun>
  })
}

This will have \emph{no effect} if you just run \code{myfun} in R{} normally, so you don't need to keep modifying your code when you do/don't want to use it with \code{RTMB} etc.

But if \code{parzzz} has a specific class--- eg if called from \code{RTMB::MakeADFun}, then \code{parzzz} will be class "advector"--- then a special form of \code{reclasso} gets called, to pre-tweak your code so it works nicely with that class. (For \code{RTMB} package, it has to modify addition, multiplication, etc, plus \code{\link{REPORTO}}, and to tweak environments.) This behaviour depends on having loaded an appropriate "helper" library, which for \code{RTMB} is the \pkg{offartmb} package.

If you are annoyed by having to wrap your code like that, you can abbreviate it to this:

\preformatted{
  myfun <- function( parzzz, <...>) {<old code>} |> reclasso( by=parzzz)
}
\subsection{Details}{
The main reason for \code{reclasso} is that R{}'s multiple-dispatch rules for addition etc simply don't work right, unless you go to great lengths with S4. Which sounds like a lot of work for \emph{me}, and I'm not sure it's even guaranteed to work fully without hacking, since package \pkg{RTMB} is of necessity quite hacky itself. Instead, I have implemented my own versions of "+" etc which \emph{do} dispatch correctly, taking account of and preserving "offarray" and/or "advector" etc classes. So the trick is to replace calls to base-R "+" etc, with calls to my more culturally-attuned versions.

Oh, and the reasons that I don't just provide a replacement for \code{MakeADFun} that auto-does this, are:

\itemize{
\item because this notion might eventually be used not with the \pkg{RTMB} package but with something else, so I didn't want to tie it in (no idea what, this is just a note)
\item your function might call other functions you wrote, all of which need their own \code{reclasso}.
}

So it's better for it to be \emph{your} responsibility to add \code{reclasso} calls to \emph{your} functions as needed. Calling \code{reclasso} costs almost nothing run-time-wise, BTW.
}
}
\usage{
## Never use it from the command line!
## Only use it as a wrapper for function code,
## EG if your original function looks like this, and
## 'p' might be modified by some callers:
#   myorigfun <- function( p, otherpars) \{<body>\}
## then either of
#  myfun <- function( p, otherpars) reclasso( by=p, \{<body of myfun\})
#  myfun <- function( p, otherpars) \{<body>\} |> reclasso(by=p)
reclasso( expr, by)
}
\arguments{
\item{ expr}{Code of your function}
\item{ by}{What variable to look at, to decide whether \code{expr} needs to be pre-modified before evaluation. Expected use-case is for \code{by} to be a vector of parameters.}
}
\examples{
\dontrun{
reformat.my.hard.drive()
}
}
\keyword{misc}
