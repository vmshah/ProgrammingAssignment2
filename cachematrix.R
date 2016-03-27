## The functions defined here can be used to calculate the 
##  inverse of an invertible matrix while caching the calculated
##  inverse internally so that subsequent calls to calculate the 
##  inverse do not incur the cost of actually calculating the 
##  inverse.

## The makeCacheMatrix function creates a special "matrix" object 
##  which is a list containing 4 functions that do the following
##	a. set the matrix
##  b. get the matrix
##	c. set the inverse of the matrix
## 	d. fet the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		mat_inv <- NULL # initialize the inverse of the matrix
		# set the matrix
		set <- function(y) {
				# Lexical scoping in R allows y from a different evironment
				# to be assigned to x
				x <<- y  
				# set the inverse to NULL as it has not been calculated yet
				mat_inv <<- NULL
		}
		
		# Returns the stored matrix
		get <- function()  x 
		
		# set the inverse 
		setinverse <- function(inverse) mat_inv <<- inverse
		
		# get the inverse
		getinverse <- function() mat_inv
		
		# create the list of functions
		list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function returns the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'traceback
        
        # get the inverse from the special "matrix"
        inv <- x$getinverse()
        
        if (!is.null(inv)) {
        		message("Getting cached data")
        		return(inv)
        }
        # get the matrix
      	mat <- x$get()
      	message("Calculating the inverse")

      	# calculate the inverse
      	inv <- solve(mat)

      	# set the inverse in the special "matrix" object
      	x$setinverse(inv)
      	
      	return(inv)
}
