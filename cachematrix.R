
## Final copy

## Creating functions to cache the inverse of a matrix,
## so that we do not have to inverse the matrix over and over again. Saves computation time.

## Caching the inverse of the matrix with this function

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

## function to calculate the inverse of a matrix, but it checks to see if the inverse of the Matrix is already
## computed and returns the inverse without calculating it all over again.

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