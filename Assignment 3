setwd('c:/Desktop/Rdata')
makeCacheMatrix <- function(x)) {
        InvM <- NULL
        set <- function(y) {
                x <<- y
                InvM <<- NULL
         }
  get <- function() x
  setsolve <- function(solve) InvM <<- solve
  getsolve <- function() InvM
  list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
                }
cacheSolve <- function(x, ...) {
         InvM <- x$getsolve()
         if(!is.null(InvM)) {
                        return(InvM)
                                }
  data <- x$get()
  InvM <- solve(data, ...)
  x$setsolve(InvM)
  InvM
}
