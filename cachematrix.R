## The following two functions first creates a special "matrix" object 
## that can cache its inverse by function named makeCacheMatrix, and 
## then computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.


## This function creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inversed matrix
## 4. get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function first checks to see if the invered matrix has already 
## been calculated. If so, it gets the invered matrix from the cache 
## and skips the computation. Otherwise, it inverses the matrix and 
## sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
                ## Return a matrix that is the inverse of 'x'
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
