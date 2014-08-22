## These functions create an special matrix capable of cache its inverse
## and calculate - if it's not yet calculated - and get the inverse of the special
## matrix

## This function create the special matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    get <- function() x # returns the original matrix

    # sets the cache with the inverse of the matrix x and returns it. There is no set function 
    # (as in mean vector example) to make sure that the inverse matrix is the correct
    # inverse of matrix x and not an arbitrary value. I guess that this approach
    # is more bug safe than the given example. It's also more encapsulated too.
    solve_or_get <- function(...) {
        if (is.null(inv)) {
            message("not cached. calculating")
            inv <<- solve(x, ...)
        }
        inv
    }
    
    list(set = set,
         get = get,
         solve_or_get = solve_or_get)
}


## This function receives a makeCacheMatrix object an returns its inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Note that this function is not really necessary anymore due the
    ## encapsulation done in makeCacheMatrix function
    x$solve_or_get(...)
}
