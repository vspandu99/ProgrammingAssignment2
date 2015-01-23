## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(mat2) {
                x <<- mat2
                inv_mat <<- NULL
        }
        get <- function() x
        setInv <- function(matrix) inv_mat <<- matrix
        getInv <- function() inv_mat
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix function above.
## If the inverse has already been calculated and the matrix has not been changed, then this function retrieves the
## inverse of the matrix from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getInv()
        if(!is.null(inv_mat)) {
                message("getting Cahced data")
                return (inv_mat)
        }
        new_mat <- x$get()
        inv_mat <- solve(new_mat)
        x$setInv(inv_mat)
        inv_mat
}
