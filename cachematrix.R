## Two functions that are used 
## to calculate an inverse of a matrix and 
## to cache its inverse

## Function  makeCacheMatrix creates a special list to cache a matrix and its inverse
## Function  cacheSolve      inverts matrix or takes the inverse from cache
## 
## Example of use:
##          
##          m  <- matrix(rnorm(1:16), 4, 4)
##          ll <- makeCacheMatrix(m) 
##          cacheSolve(ll)
##
## 



## --------------- makeCacheMatrix------------------------------------- 
## Takes a matrix as an argument and creates a special object 
##  which stores the matrix and cache's its inverse
##
## Arguments:
##  x - invertible square matrix  
##
## The returned object contains the following functions:
##
##  get          -- Retrieves stored matrix  
##  set          -- Stores new matrix and resets inverse  
##  getInverse   -- Retrieves cached inverted matrix
##  setInverse   -- Saves in the cache passed argument as inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() {
        x
    }
    
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    
    getInverse <- function() {
        inv
    }
    
    setInverse <- function(t) {
        inv <<- t              
    }
    
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## --------------- cacheSolve------------------------------------------ 
## Returns the inverse of the matrix passed as an argument 
##  to function makeCacheMatrix
##
## Arguments:
##  x - result of function makeCacheMatrix
 
cacheSolve <- function(x, ...) {
    ## Check first if the inverse already exists in the cache
    inv <- x$getInverse() 
    if (is.matrix(inv)) { 
        message("Get result from cache")  
        return(inv)
    } else {    
        dd <- x$get()
        inv <- solve(dd,...) ##inverses the matrix
        x$setInverse(inv)    ##caches the result
        inv
    }
}
