## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a list with four separate functions
## set() lets the user to create a matrix
## get() retrieves the stored matrix and prints
## setInverse() lets the user to set the inverse
## getInverse() lets the inverse to be printed from the cache

makeCacheMatrix <- function(x = matrix()) {

        myMatrixInverse <- NULL
        
        set <- function(y){
                x<<-y
                myMatrixInverse <- NULL
        }
        
        get <- function() x
        
        setInverse <- function(myInverseCache) myMatrixInverse <<- myInverseCache
        
        getInverse <- function() myMatrixInverse
        
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)


}


## Write a short comment describing this function

## cacheSolve verifies if there is a cached solution then creates a 
## solution if the matrix has changed or there was no solution cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        myMatrixInverse <- x$getInverse()
        
        if(!is.null(myMatrixInverse)) {
                message("getting cached data")
                return(myMatrixInverse)
        }
        data <- x$get()
        myMatrixInverse <- solve(data, ...)
        x$setInverse(myMatrixInverse)
        myMatrixInverse
        
}
