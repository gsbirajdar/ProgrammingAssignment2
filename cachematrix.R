## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                inverse <- NULL ##command initializes the outpu matrix inverse to NULL
                set <- function(y){
                        x <<- y
                        inverse <- NULL
                }
                
                get <- function() x
                setinv <- function(solve) inverse <<- solve
                getinv <- function() inverse
                list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
