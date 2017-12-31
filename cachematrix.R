## creates augmented matrix with corrosponding getter / setter methods
## for its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    # set matrix, reset cached value of inverse matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    # get original matrix
    get <- function() x
    
    # set inverse
    setSolve <- function(solve) s <<- solve
    
    # get inverse
    getSolve <- function() s
    
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## methods returns the inverse of a matrix. Inverse is only calculated at the first 
## function call. All following calls will return cached values 

cacheSolve <- function(x, ...) {

    solve <- x$getSolve()
    
    # check whether cached values exists
    # if non existend calculate inverse matrix
    if(is.null(solve)){
        
        # get original matrix
        y <- x$get()
        
        # calculate inverse
        solve = solve(y)
         
        # cache inverse
        x$setSolve(solve)
    }
    else {
        message("cached data loaded")
    }
    
    # return inverse
    solve
}
