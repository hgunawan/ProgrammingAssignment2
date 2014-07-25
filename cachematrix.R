## Inverse Matrix cache
## 

## makeCacheMatrix create a special "matrix" which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinverse <- function(im) m <<- im
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculate inverse of the matrix, get from cache if calculation existed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}