\name{descRibe-package}
\alias{descRibe-package}
\alias{descRibe}
\docType{package}

\title{descRibe: Taxonomic descriptions in natural language}

\description{
descRibe, is an R package that attempts to provide function for creating natural-language taxonomic descriptions from tables of characters and scored character states. While a number of other programs (e.g. DELTA, LUCID, xper2) can do similar things, the motivation for creating an R package is to allow descriptions to be built from a table of individual specimens, and to allow the resulting description to be exported into a LaTeX document via Sweave.

The complete list of functions can be displayed with \code{library(help=descRibe)}.

More information, including a tutorial (at least, a tutorial is intended sometime in the future) on the use of descRibe can be found at \code{https://github.com/sdjbrown/descRibe}.
}

\details{
\tabular{ll}{
Package: \tab descRibe\cr
Type: \tab Package\cr
Version: \tab 0.0-0\cr
Date: \tab 2012-05-06\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}

A few of the key functions provided by spider: 

DNA barcoding:
\code{\link{bestCloseMatch}}, \code{\link{nearNeighbour}}, \code{\link{threshID}}, \code{\link{threshOpt}}.

Sliding window:
\code{\link{slidingWindow}}, \code{\link{slideAnalyses}}, \code{\link{slideBoxplots}}.

Nucleotide diagnostics:
\code{\link{nucDiag}}.

Morphological techniques:
\code{\link{paa}}.
}
\author{
Samuel Brown

Maintainer:
Samuel Brown <s_d_j_brown@hotmail.com>
}
\references{
Brown S. D. J., Collins R. A., Boyer S., Lefort M.-C., Malumbres-Olarte J., Vink C. J., & Cruickshank R. H. In Press. SPIDER: an R package for the analysis of species identity and evolution, with particular reference to DNA barcoding. _Molecular Ecology Resources_
}

%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
\keyword{ package }
\seealso{

}
