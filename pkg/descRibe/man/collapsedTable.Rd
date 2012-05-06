\name{collapsedTable}
\alias{collapsedTable}

\title{Collapses a univariate table into a character vector}

\description{
Collapses a univariate table into a character vector in the form "Name (Value)"
}
\usage{
collapsedTable(vec, proportion = TRUE, rounding = 2)
}

\arguments{
  \item{vec}{A vector containing the variables to give to \code{table}.}
  \item{proportion}{Logical. Should the table return values as a proportion? Default of TRUE.}
  \item{rounding}{Decimal places the value should be returned to.}
}

\details{
This function can be used to summarise the variation in discrete characters.
}

\value{
A character vector with the number of elements being equal to the number of categories in the table.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{table}}.
}
\examples{

testVec <- sample(LETTERS[1:5], 30, replace = TRUE)

collapsedTable(testVec)
collapsedTable(testVec, proportion = FALSE)
collapsedTable(testVec, rounding = 4)
}

\keyword{Summary}
