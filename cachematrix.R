
# makes cached matrix

makeCacheMatrix <- function(x = matrix()) {
    inv1 <- NULL
    set1 <- function(y) {
        x <<- y
        inv1 <<- NULL
    }
    get1 <- function() x
    setInv <- function(inv) inv1 <<- inv
    getInv <- function() inv1
    list(set1 = set1, get1 = get1, setInv = setInv, getInv = getInv)
}

# uses makeCacheMatrix, computes the inverse matrix.
# if the inverse has been calculated => retrieves from the cashe

cacheSolve <- function(x, ...) {
    inv1 <- x$getInv()
    if (!is.null(inv1)) {
        return(inv1)
    }
    mat1 <- x$get()
    inv1 <- solve(mat1, ...)
    x$setInv(inv1)
    inv1
}
