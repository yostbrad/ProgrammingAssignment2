##The function makeCacheMatrix creates a special "matrix
##This "matrix" is actually a list that contains a function to
##1)set the value of the matrix 2)get the value of the matrix
##3)set the inverse and 4)get the value of the inverse

## make CacheMatrix creates the chache matrix 
##(assuming a square and invertible matrix)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInverse(i)
    i
}
