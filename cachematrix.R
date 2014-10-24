## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
