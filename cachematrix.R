## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of set, get,setinverse, and getinverse
## It is called by another function which passes in a matrix and asks 
## for the inverse.
## If it has the inverse it returns it, otherwise it is passed
## the inverse which it caches.

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                  x <<- y
                  inv <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) inv <<- solve
            getinverse <- function() inv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix that it is passed
## it first sees if the inverse has been cached by the passed function
## and returns it.  If not it solves for the inverse and gives it
## to the other function to cache for future use

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
            inv <- x$getinverse()
            if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setinverse(inv)
            inv
}
