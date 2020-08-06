## Put comments here that give an overall description of what your
## functions do

## This function makes a special matriz object that can cache its inverse
## assumptions are that matrix are always invertible (same number of columns 
## and rows)

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y = matrix()) {
                x <<- y*
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve<- function() s 
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function computes the inverse of the special matriz given by 
## makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data!")
                return(s)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        return(inverse)
}

## Error in x$getinverse : $ operator is invalid for atomic vectors











