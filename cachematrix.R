## This function creates computes the inverse of a matrix and holds it in cache
## to avoid repetition of this time consuming operation. 

## Creates a special matrix to hold the calculated result (inverse of the matrix)
## Produces a list vector that will be subset and used in the cacheSolve function
## to determine if the inverse of the matrix is held in cache or needs to be
## recomputed

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #returns original matrix
        get <- function() x
        #calculates the inverse of the matrix
        setinverse <- function(solve) m<<- solve
        #returns the inverse of the matrix if it has already been calculated
        getinverse <- function() m
        
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## Return the inverse of a matrix. If the inverse is held in cache and the matrix
## hasn't changed then return the inverse from cache. If the inverse has not been
## calculated or the matrix has changed, compute the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Subset the list vector created in makeCacheMatrix
        m <- x$getinverse()
        # if the inverse hasn't been calculated, calculate it
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}