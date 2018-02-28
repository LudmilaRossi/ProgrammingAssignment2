## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The following function "makeCacheMatrix" creates a special "matrix" 
## object that can cache its inverse. Contains a function to: 
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse of the matrix
## 4) Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
            mcm <- NULL
            set <- function(y) {
                    x <<- y
                    mcm <<- NULL
                                }
            get <- function() x
            setinv <- function(inv) mcm <<- inv
            getinv <- function() mcm
            list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Write a short comment describing this function...

## This following function computes the inverse of the special "matrix" 
## returned by the function "makeCacheMatrix" above. If the inverse 
## has already been calculated (and the matrix has not changed), 
## then function cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mcm <- x$getinv()
        if(!is.null(mcm)) {
                message("Getting Cached Data")
                return(mcm)
        }
        data <- x$get()
        mcm <- solve(data, ...)
        x$setinv(mcm)
        mcm
}

## TESTING 
C <- matrix(c(2,4,6,8,10,12),2,2)
C1 <- makeCacheMatrix(C)
cacheSolve(C1)
