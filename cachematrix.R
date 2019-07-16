## The below functions "makeCacheMatrix" and "cacheSolve" is used to create a special vector containing
## the matrix and computing the Inverse of matrix with implimentation of Cache to reduce computation cost.

## The makeCacheMatrix Function creates a spceial vector i.e list with 4 functions defined 
## to get,set the matrix data and rest 2 to get and set the Inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    
    set <- function(y){
        x <<- y     #assigning the x as y
        I <<- NULL  #making it NULL as a new Matrix is recieved
    }
    
    get <- function() x
    
    getInverse <- function() I
    
    setInverse <- function(inverse) I <<- inverse
    
    list(set=set,get=get,getInverse=getInverse,setInverse=setInverse)
}


## cacheSolve function computes the inverse of a square invertible matrix 
## The function first will check for a cached value and if not found then it will 
## compute the inverse and will store it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        message("Retrieved Cached Data")
        return(i)
    }
    message("Computing Inverse")
    matrix <- x$get()
    matrix
    i <- solve(matrix)
    x$setInverse(i)
    i
}
