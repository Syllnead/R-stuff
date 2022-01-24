setwd('c:/Desktop/Rdata')                               ## Assigment 2: Lexical Scoping
makeCacheMatrix <- function(x)) {                       ## This function takes the given matrix object as an argument x
        InvM <- NULL                                    ## Here makeCacheMatrix creates necessary objects, with InvM also being assigned with a scoping assignment
        set <- function(y) {
                x <<- y
                InvM <<- NULL
         }
  get <- function() x                                   ## Defining two separate functions for 2 cases of cacheSolve input
  setsolve <- function(solve) InvM <<- solve
  getsolve <- function() InvM                          
  list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
                }
cacheSolve <- function(x, ...) {                        ## cacheSolve finds the inverse matrix, unless the matrix has not been changed. Then the InvM is assigned from cache
         InvM <- x$getsolve()
         if(!is.null(InvM)) {
                        return(InvM)
                                }                       ## Final output
  data <- x$get()
  InvM <- solve(data, ...)
  x$setsolve(InvM)
  InvM
}
