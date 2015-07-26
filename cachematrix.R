## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

### Jairo's comments

### Matrix inversion is usually a costly computation, if there is need to compute many times 
### the same, store the result of inverse of a matrix in the cache and return the result 
### when need is more efficient than compute it repeatedly.

### following two functions are used to cache the inverse of a matrix.

### makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the inverse property
    inv <- NULL
    
    ### set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ### get the value of the matrix    
    get <- function() x
    
    ### set the value of inverse of the matrix    
    setinverse <- function(inverse) inv <<- inverse
    
    ### get the value of inverse of the matrix    
    getinverse <- function() inv
    
    ### Return list of the methods
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

### Jairo's comments

### This function returns the inverse of the matrix. 
### 1. It first checks if the inverse has already been computed. If exist, it returns the result 
###    and skips the computation. 
### 2. If not, it computes the inverse, stores the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    ## If the inverse already set, return it
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    
    ## Get the matrix from our object
    data <- x$get()
    
    ## Compute the inverse using matrix multiplication
    inv <- solve(data)
    
    ## Set the value to the object and return it
    x$setinverse(inv)
    inv
}
