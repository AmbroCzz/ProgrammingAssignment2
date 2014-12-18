## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly

### Usage example:
# > source('cachematrix.R')
# > m <- matrix(1:4, 2,2)
# > v <- makeCacheMatrix(m)
# > cachesolve(v)
# > cachesolve(v)
# from the second cachesolve execution it will return a cached value (with message)


## The function, `makeCacheMatrix` creates a special "vector", which is
## really a list containing a function to
## 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverted matrix 
## 4.  get the value of the inverted matrix

## Input (x): An invertible matrix

makeCacheMatrix <- function(x = matrix()) 
{
    invMatrix <- NULL
    set <- function(y) 
    {
        x <<- y
        invMatrix <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solve) invMatrix <<- solve
    
    getsolve <- function() invMatrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


# The following function calculates the inverted matrix using the special "vector" 
# created with the above function. 
# However, it first checks to see if the inverted matrix has already been calculated. 
# If so, it gets the inverted matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverted matrix of the data and sets the value of 
# the inverted matrix in the cache via the setsolve function.

# Input (x): the makeCacheMatrix output

cachesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getsolve()
    if(!is.null(invMatrix)) {
        message("getting cached inverted Matrix")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setsolve(invMatrix)
    invMatrix
}

