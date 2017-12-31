## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    
    solve <- x$getSolve()

    # check whether cached values exists
    if(is.null(solve)){
        # get matrix values
        y <- x$get()
        
        # calculate inverse matrix
        solve = solve(y)
         
        # cache inverse matrix
        x$setSolve(solve)
    }
    else {
        message("cached data loaded")
    }
    
    # return inverse matrix
    solve
}
