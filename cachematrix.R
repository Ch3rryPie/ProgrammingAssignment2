# This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
  
  m <- NULL # define the cache m                    
  
  set <- function(y) {                      
    x <<- y  # assign the input matrix y to the variable x 
    m <<- NULL  # re-initialize m            
  }
  
  get <- function() x  # return the matrix x                         
  setinv <- function(solve) m <<- solve  # set m equal to the inverse of the matrix x
  getinv <- function() m   # return the cached inverse of x           
  list(set = set, get = get,                    
       setinv = setinv,
       getinv = getinv)
}



# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), then 
# the cacheinv should retrieve the inverse from the cache.

cacheSolve<- function(x, ...) {  

  m <- x$getinv() 
  
  if(!is.null(m)) {                 
    message("getting cached data")
    return(m)
  }
 
  data <- x$get()                               
  m <- solve(data, ...)
  x$setinv(m)
  m
}
