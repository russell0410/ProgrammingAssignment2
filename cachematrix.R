## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## These functions cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y = matrix()){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x,...){
        inv <- x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}

#To Test:
#a<-makeCacheMatrix()
#a$set(matrix(c(1,2,3,0,1,4,5,6,0),nrow=3,ncol=3))
#cacheSolve(a)
#cacheSolve(a)  #this will return the cached inverse


