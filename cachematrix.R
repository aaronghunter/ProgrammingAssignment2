## Together, these functions produce a matrix, and the inversion of it, and cache the latter.
## These functions assume the supplied matrix can be inverted.

## makeCacheMatrix produces a special "matrix" (list) which caches an inversion. 

makeCacheMatrix <- function(x = matrix()) {
      m = NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(solve) m <<- solve
      getmatrix <- function() m
      list (set=set, get=get, 
            setmatrix=setmatrix,
            getmatrix=getmatrix)
}


## cacheSolve inverts a matrix, unless the inversion is cached, in which case it returns it.

cacheSolve <- function(x, ...) {
      m <- x$getmatrix()
      if(!is.null(m)) { ## Check For Cache
            message("Retrieving Cached Data")
            return(m)
      }
      matrix <- x$get()
      m <- solve(matrix, ...)
      x$setmatrix(m)
      m        ## Return a matrix that is the inverse of 'x'
}
