\name{findName}
\alias{findName}
\title{
Find a component of a recursive object
}
\description{
This function searches recursive objects for components with a name
matching a pattern.
}
\usage{
findName(pattern, x, path = c())
}
\arguments{
  \item{pattern}{
A regexp pattern to match the desired name(s).
}
  \item{x}{
The recursive object to search.
}
  \item{path}{
For internal use:  names to prepend to the path.
}
}
\details{
A utility/debugging function to search a large recursive object
for a particular component name.
}
\value{
A list with one entry per hit.  Each entry in the list is a named vector
giving the path to the hit, numerically in the values, and as an R expression
by concatenating the names.
}

\examples{
x <- list( a = list( b = list(c(d=1, e=2))))
locations <- findName("e", x)
locations
x[[locations[[1]]]]
expr <- paste0(c("x", names(locations[[1]])), collapse = "")
expr
eval(parse(text=expr))
}
