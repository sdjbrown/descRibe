\name{read.descRibe}
\alias{read.descRibe}

\title{Read in a descRibe dataframe}

\description{
Collapses a vector, with a different seperator between the last two elements.
}

\usage{
read.descRibe(file, sep = ";")
}

\arguments{
  \item{file}{Name of the file to be read in.}
  \item{sep}{The delimitation character for the table. Default of a semicolon}
}

\details{
This function reads a text-format spreadsheet into a dataframe. The difference between this and \code{\link{read.table}} is that this function distinguishes those rows with an \code{"attribute"} name, and which contain important information such as the prefix and suffix for the character.
}

\value{
A dataframe with additional attributes as described below:

\code{Type}: The mode of the character.

\code{Prefix}: String that should appear before the character value.

\code{Suffix}: String that should appear after the character value.

\code{MajorCategory}: Wider category that the character sentence should appear in (e.g. Head, Thorax, etc).

\code{MinorCategory}: What sentence should the character appear in.

}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{read.table}}.
}

\examples{

}

\keyword{Format}
