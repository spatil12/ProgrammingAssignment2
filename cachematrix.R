


## Creating functions to cache the inverse of a matrix,
## so that we do not have to inverse the matrix over and over again.

## Caching the inverse of the matrix with this function (testing commit)

makeCacheMatrix <- function(x = matrix()) {
    i <- matrix()
    setMatrix <- function(y = matrix()){
        x <<- y
        inv <<- null
    }
    getMatrix <- function() x
    setInv <- function(inverse) i <<- inverse
    getInv <- function() i
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)
}

## function to calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
    i <- x$getInv()
    if(!is.na(i)){
        message("getting cached matrix")
        return(i)
    }
    matr <- x$getMatrix()
    i <- solve(matr)
    x$setInv(i)
    matr
}