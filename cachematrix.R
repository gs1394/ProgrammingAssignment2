## Writing function for Inverse of a Matrix

## Cache Matrix object to create inverse

makeCacheMatrix <- function(x = matrix()) {

    inverse_matrix <- NULL
    
    set <- function(matrix) {
        m <<- matrix
        inverse_matrix <<- NULL
    }

    get <- function() {
        return(m)
    }
  
    setInverse <- function(inverse) {
        inverse_matrix <<- inverse
    }
    
    getInverse <- function() {
      
        return(inverse_matrix)
    }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)    
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {

    m <- x$getInverse()
    
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
  
    data <- x$get()
    
    m <- solve(data) %*% data
    
    x$setInverse(m)
    
    return(m)
    
}
