## two functions implementing cachable matrix.

## The first function, returns a list of setter and getter of matrix and inverse matrix.

makeCacheMatrix <- function(x = matrix()) {

	inverse_x = NULL
    
    set <- function(y){
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    get_inverse <- function() inverse_x
    set_inverse <- function(inverse) inverse_x <<- inverse
        
    # return this CacheMatrix as a list         
    list(set = set,get = get, get_inverse = get_inverse, set_inverse = set_inverse )
}


## A function that takes a CacheMatrix returned by the former function,
## and sets inverse matrix of that object if it was not cached, and it's invertable.
## If it was cached, then we pring it and do nothing.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse = x$get_inverse()
    
	    # if it's cached, then we return the cached value.
	    if(!is.null(inverse)){
	        # got cached inverse matrix
	        message("get cached inverse matrix")
	        return(inverse)
	    }
	    
	    # not cached, then we calculate the inverse matrix 
	    original_matrix = x$get()
	    
	    # check if this original matrix is invertable
	    if(det(original_matrix) == 0){
	        message("this matrix is not invertable")
	    }else{
	        inverse = solve(original_matrix, ...)
	        x$set_inverse(inverse)
	        inverse
	    }
}


test <- function(){
	x <- makeCacheMatrix(matrix(rnorm(16), c(4,4)))
	
	# original matrix
	x$get()

	# inverse matrix
	cacheSolve(x)
}

test()