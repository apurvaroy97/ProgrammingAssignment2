## Caching the Inverse of a Matrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(a = matrix())
 {
        inver <- NULL
        set <- function(b)
        {
                a <<- b
                inver <<- NULL
        }
        get <- function() a
        setInverse <- function(inverse) inver <<- inverse
        getInverse <- function() inver
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated , 
##then it should retrieve the inverse from the cache.


cacheSolve <- function(a, ...)
 {
        
        inver <- a$getInverse()
        if (!is.null(inver))
        {
                message("Fetching the Cached Data")
                return(inver)
        }
        matr <- a$get()
        inver <- solve(matr, ...)
        a$setInverse(inver)
        inver
}
