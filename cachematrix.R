##This file contains two functions that given a matrix as input, return
##the inverse of it, either from cache, or by computing it if doesn't exist
##in cache

## The first function makeCacheMatrix creates a list of function that will be called
##from 'cacheSolve' the functions are to:
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
                ##command initializes the output matrix inverse to NULL
                inverse <- NULL
                ##function to set the value of matrix 'x' to a new variable 'y'
                ##and set the inverse back to NULL
                ##this function is unnecessary and is never called from from 'cacheSolve'
                set <- function(y){
                        x <<- y
                        inverse <- NULL
                }
                ##function to get the matrix x
                get <- function() x
                ##function to set the value of inverse
                setinv <- function(solve) inverse <<- solve
                ##function to get inverse
                getinv <- function() inverse
                ##it will prepare the list of function created in makeCacheMatrix that
                ##will be used later in cacheSolve
                list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##This function will check if given matrix's inverse exist in cache,
##else it will compute inverse and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inverse <- x$getinv()
                ##following command will check if inverse of given matrix exists in cache
                ##if yes it will return enverse from cache
                if(!is.null(inverse)){
                        message("Answer is cached, returning it..")
                        return(inverse)
                }
                ##if inverse doesn't exist in cache, following function will
                ## get the matrix 'x'
                matt <- x$get()
                ##it will check if matrix is inversible
                ##if not it will return the message, "Sorry, matrix is not invertible"
                ##and set the preexisting value to inverse..
                ##in assignment it was told to assume that matrix WILL BE
                ##invertible, but I have added this condition to avoid error
                test <- det(matt)
                if (test == 0){
                        message("Sorry, matrix is not invertible")
                        x$setinv(inverse)
                }
                ##if matrix is invertible, following commands will compute the
                ##inverse, cache it, and return the inverse
                else {
                        inverse <- solve(matt, ...)
                        x$setinv(inverse)
                        inverse        
                }
                
}