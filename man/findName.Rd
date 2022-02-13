\name{findName}
\alias{findName}
\alias{findClass}
\title{
Find a component of a recursive object
}
\description{
\code{findName} searches recursive objects for components with a name
matching a pattern.  \code{findClass} does the same sort of search
for a particular class.
}
\usage{
findName(pattern, x, path = c())
findClass(class, x, path = c())
}
\arguments{
  \item{pattern}{
A regexp pattern to match the desired name(s).
}
  \item{class}{
A class name to search for.
  }
  \item{x}{
The recursive object to search.
}
  \item{path}{
For internal use:  names to prepend to the path.
}
}
\details{
Utility/debugging functions to search a large recursive object
for a particular component name or class.
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
