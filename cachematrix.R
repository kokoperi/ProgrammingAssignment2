## makeCacheMatrix receives a matrix and provides four functions, 
## which are set(),get(), setinv(),getinv(). It returns a list 
## that contains the four functions and a matrix data. The matrix
## data can be only accessed through the functions.

## For example, you can use makeCacheMatrix as follows.
##     > a <- matrix(c(1,0,0,2), nrow = 2, ncol = 2) 
##     > a$get()
## It returns:
##          [,1] [,2]
##     [1,]    1    0
##     [2,]    0    2
##
## Otherwise, you can use the function as follows.
##     > a<-makeCacheMatrix()
##     > a$set(matrix(c(1,0,0,2), nrow = 2, ncol = 2))
## 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of the specified matrix if 
## the inverse has not been calculated. Once cacheSolve calculate 
## the inverse, it stores the result in the matrix. 

## Before storing the inverse, a$getinv() returns NULL. 
## For example, cacheSolve(a) returns:
##          [,1] [,2]
##     [1,]    1  0.0
##     [2,]    0  0.5

## After that, a$getinv() gives the results.
##     > a$getinv()
##          [,1] [,2]
##     [1,]    1  0.0
##     [2,]    0  0.5

## cacheSolve(a) returns the stored values if it exists.
##     > cacheSolve(a)
##     getting cached data
##          [,1] [,2]
##     [1,]    1  0.0
##     [2,]    0  0.5

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
