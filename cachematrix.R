
## This function creates a list of functions to set and get the matrix value 
## as well as set and get the inverse matrix value
makeCacheMatrix <- function(x=matrix())
  {
    
    cache_data <- NULL ## set cache value to NULL
    set_matrix <- function(y) ## nested function to set the matrix value
      {
        x <<- y
        cache_data <<- NULL  ## resets cache data since there is a new matrix value
      }
    
    get_matrix <- function() x  ## returns the value of the matrix
    set_inverse <- function(m_inverse) cache_data <<- m_inverse ##sets the cache data to the inverse of the matrix
    get_inverse <- function() cache_data ##returns the inverse of the matrix from the cache data
    
    
    list(set_matrix = set_matrix, 
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
 
}


## This function computes the inverse of the special matrix object returned by makeCacheMatrix above. 
## If the inverse was previously calculated and the matrix is the same, then the cacheSolve function will get the inverse from the cache
cacheSolve <- function(x, ...) 
  
  {
  
  cache_data <- x$get_inverse() ## store inverse matrix in cache data
  
  ## check if data is already cached, return it if so
  if(!is.null(cache_data)) 
  {
    
    message("getting cached data")
    return(cache_data)
  }
  
  
  matrix_data <- x$get_matrix() ## store the matrix value
  cache_data <- solve(matrix_data, ...) ## use solve function to calculate inverse of the matrix and store in cache
  x$set_inverse(cache_data) ## set the inverse value to the cache data and display it
  cache_data
  
}
