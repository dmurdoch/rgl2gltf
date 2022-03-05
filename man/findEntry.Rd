\name{findEntry}
\alias{findEntry}
\alias{namePattern}
\alias{hasClass}
\title{
Find a component of a recursive object
}
\description{
\code{findEntry} searches recursive objects for components
matching a condition.  \code{namePattern} creates a test of
whether the component name matches a pattern.  \code{hasClass}
creates a test of whether the component has a class.
}
\usage{
findEntry(x, test, ..., path = c())
namePattern(pattern)
hasClass(class)
}
\arguments{
  \item{x}{
The recursive object to search.
}
  \item{test}{
A test function.  See Details below.
  }
  \item{pattern}{
A regexp pattern to match the desired name(s).
}
  \item{class}{
A class name to search for.
  }
  \item{...}{
Optional additional arguments to pass to the test function.
  }
  \item{path}{
For internal use:  names to prepend to the path.
  }
}
\details{
Utility/debugging functions to search a large recursive object
for a particular component name or class.

The \code{test} function should have header \code{function(name, value)}, and may include other arguments which will
be taken from \code{...}.
}
\value{
\code{findEntry} returns
a list with one entry per hit.  Each entry in the list is a named vector
giving the path to the hit, numerically in the values, and as an R expression
by concatenating the names.  The test functions will be passed
single names and values, and should return a single logical result.
}

\examples{
x <- list( a = list( b = list(c(d="A", e="B"), 1L, 1:3)))
locations <- findEntry(x, namePattern("e"))
locations

#This shows how the result can be used:
x[[locations[[1]]]]
expr <- paste0(c("x", names(locations[[1]])), collapse = "")
expr
eval(parse(text=expr))

findEntry(x, hasClass("integer"))
}
