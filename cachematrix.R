## The following 2 functions work in conjunction to preserve computational 
## resources when determining the inverse of a matrix.


## The makeCacheMatrix function creates a "special" version of a matrix object.
## The function creates a list object that:
## 1) sets the value of the matrix
## 2) gets the value of the matrix
## 3) sets the value of the inverse matrix
## 4) gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## The cacheSove funtion only accepts an object of type makeCacheMatrix which is a 
## special matrix.  It determines the inverse matrix, stores it in cache, 
## and returns the value inverse matrix.  
## If the inverse of the makeCacheMatrix object is already known, this function 
## will return that value from the cache instead of computing it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}