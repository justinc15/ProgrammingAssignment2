## 1. The first function makeCacheMatrix would take as input a matrix and creates a 
## special "matrix" object that would : 1) set and get the value of the matrix ; 
## and 2) set and get the value of the matrix's inverse
## 2. The second function cacheSolve would return the inverse of the matrix. Before 
## the returning process, it would check if the given inverse is stored in the cache.
## If not, then the inverse of the matrix would be computed and then returned.

## makeCacheMatrix takes matrix x as an input and returns a special "matrix" object - 
## specifically, a list containing functions to get and set the values for both the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(mat) {
          x <<- mat
          inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes matrix x as an input and checks whether the matrix inverse has been calculated.
## In the case that it has not been computed, the function would compute and returns its inverse ; 
## otherwise, the function would simply return the inverse of x. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        matrix_to_solve<- x$get()
        inverse <- solve(matrix_to_solve, ...)
        x$setinverse(inverse)
        inverse
}
