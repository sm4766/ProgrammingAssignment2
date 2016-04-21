## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The following pair of functions cache the inverse of a matrix.
## It is assumed that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.
## The function creates a list with functions to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}

## Sample Test run

##> x <- rbind(c(1, 2), c(3, 4))
##> makeCacheMatrix <- function(x = matrix()) {
##+     i <- NULL
##+     set <- function(y) {
##+         x <<- y
##+         i <<- NULL
##+     }
##+     get <- function() x
##+     setinverse <- function(inverse) i <<- inverse
##+     getinverse <- function() i
##+     list(set = set, get = get,
##+          setinverse = setinverse,
##+          getinverse = getinverse)
##+ }
##> m = makeCacheMatrix(x)
##> m$get()
##     [,1] [,2]
##[1,]    1    2
##[2,]    3    4
##> cacheSolve <- function(x, ...) {
##+     ## Return a matrix that is the inverse of 'x'
##+     
##+     i <- x$getinverse()
##+     if(!is.null(i)) {
##+         message("getting cached data")
##+         return(i)
##+     }
##+     data <- x$get()
##+     i <- solve(data)
##+     x$setinverse(i)
##+     i
##+ }
##> 
##> cacheSolve(m)
##     [,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
##> cacheSolve(m)
##getting cached data
##     [,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
##>

 