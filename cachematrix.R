
## This function creates a special matrix object, essentially a list to set and get the matrix value as well as set and get the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    get <- function() x
    set_inverse <- function(m_inverse) i <<- m_inverse
    get_inverse <- function() i
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
		 
}


## This function computes the inverse of the special matrix object returned by makeCacheMatrix above. 
## If the inverse was previously calculated and the matrix is the same, then the cachesolve function will get the inverse from the cache

cacheSolve <- function(x, ...) {
        
		  i <- x$get_inverse()
  
  if(!is.null(i)) {
    
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  
  i
  
}
