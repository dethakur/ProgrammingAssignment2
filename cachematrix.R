## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and saves the inverse in "inv" variable
## setinv is used to set the inverse
## getinv is used to get the inverse
## Return - the function returns a list object which contains all the methods. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(new_val){
                x <<- new_val
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv_val) inv <<- inv_val
        getinv <- function() inv
        list(get = get,set = set,getinv = getinv,setinv = setinv);
}


## This function takes a makeCacheMatrix object
## It puts the inverse of the matric in "inv" and returns that. "inv" is the inverted matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if( !is.null(inv)){
                message("cached inverse!")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        inv
}
