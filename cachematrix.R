### Below there are two functions. 
# The first one, makeCacheMatrix, creates a special matrix which is able to store its inverse in cache memory
# The second function, cacheSolve, checks whether there is an inverse stored in cache, before calculating the inverse
# of a special matrix


# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing four functions.
# The four functions do the following:
# Function 1. set the value of the matrix (function name is "set")
# Function 2. get the value of the matrix (function name is "get")
# Function 3. set the value of the inverse of the matrix (function name is "setinverse")
# Function 4. get the value of the inverse of the matrix (function name is "getinverse")

makeCacheMatrix <- function(x = matrix()) {
  
  # Initially the inverse of the underlying matrix is NULL
  m <- NULL                                       
  
  # Function 1. set the value of the matrix
  set <- function(y) {
    # current matrix is replaced by matrix given as input
    x <<- y
    # The inverse is set to NULL
    m <<- NULL
  }
  
  # Function 2. get the value of the matrix
  get <- function() x
  
  # Function 3. set the value of the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  # FUnction 4. get the value of the inverse of the matrix
  getinverse <- function() m
  
  # Combine all the above functions to one list, this list is returned as output
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}



# The following function calculates the inverse of the special "vector" created with the above function.
# However, it first checks to see if the mean has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  
    # Gets the inverse from the cached data
    m <- x$getinverse()
    
    # If it indeed was calculated before (the value of the inverse is not NULL), skip calculation and use previous result
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    #Otherwise get the whole matrix
    data <- x$get()
    
    # and calculate its inverse
    m <- solve(data, ...)
    
    # Save inverse for later use
    x$setinverse(m)
    
    # Return inverse as output
    m
  
}



