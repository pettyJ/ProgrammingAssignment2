## These functions compute the inverse of an inversable matrix 
## and store the results in cache memory.
## The code first checks the cache to see if the inverse has 
## already been computed before recomputing it.
## To Use: 
## 1. Create a matrix using the makeCacheMatrix function. 
##      ex: mymatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
## 2. Use the cacheSolve function to compute the inverse. 
##      The first time you run it, it will compute the inverse.
##      Subsequent executions (for the same matrix) will print 
##      a message and get the inverse from cache memory.

## makeCacheMatrix creates a matrix object. 
## It initializes the matrix inverse to NULL and stores this result in the cache.
## The setinverse function computes the matrix inverse and updates the cache 
##      with the result.
## Its set, get, setinverse, and getinverse functions are called by
##      the cacheSolve function. The user can also call them directly. 
##      ex: mymatrix$getinverse() 
## It returns a list of the set, get, setinverse and getinverse functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                    x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve finds the inverse of an inversible matrix.
## It uses the makeCacheMatrix function.
## First, cacheSolve uses makeCacheMatrix's getinverse() function to see 
##      if the inverse has already been calculated and stored. It stores the
##      result of that function in a matrix object 'm'.
## If an inverse is found in cache memory, cacheSolve prints a message
##      and returns the inverse.
## Else, cacheSolve computes the matrix inverse and stores the result in 'm'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
