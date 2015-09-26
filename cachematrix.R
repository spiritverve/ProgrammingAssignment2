## This is my matrix inversion program. We will be caching the inverse 
## of a matrix rather than compute it repeatedly. 
## I am using the makevector example in the assignment as a template.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
      inv <- NULL
      ##create empty value
      
      set <- function(y) {
          x <<- y
          inv <<- NULL
      }
      ##define set
      
      get <- function() x
      ##define get
      
      setinv <- function(inverse) inv <<- inverse
      ## define setinv
      
      getinv <- function() inv
      ## define getinv
      
      list(set = set,
          get = get,
          setinv = setinv,
          getinv = getinv)
      ## creates the list containing the functions 
      }


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
          inv <- x$getinv()
          ##assign value of inv to value of getinv
  
          if (!is.null(inv)) {
              message("getting cached data")
              return(inv)
          }
          ##check to see if a vlaue has already been cached. if yes return it with message 
          
          z <- x$get()
          ## define z as get function
          
          inv <- solve(z, ...)
          ##assign inv the value of solve using z
          
          x$setinv(inv)
          ##set the inverse value
          
          inv
          ##display inv
  
  }
