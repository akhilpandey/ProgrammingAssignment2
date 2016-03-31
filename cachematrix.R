## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	## Initializing m
	m<- NULL
	
	## defining set function
	set<-function(y) {
		x<<-y
            m<<-NULL
	}
	
	## defining get function
	get<-function() x
	
	## defining setsolve function
	setsolve<-function(solve) m <<- solve
	
	## defining getsolve function
	getsolve<-function() m
	list(set=set, get=get,
		setsolve=setsolve,
		getsolve=getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Assumption: Matrix is invertible
	m<-x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	data <- x$get()
	## Computing the inverse of a square matrix can be done with the solve function in R. 
        ## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
	m<-solve(data, ...)
	x$setsolve(m)
	# Return the inverse
	m
}
## Example code to run through the functions##
## 1. Create the matrix
## z<-makeCacheMatrix(matrix(1:4,2,2))

## 2. run cacheSolve on the matrix.  Since the inverse has not yet been
## calculated, it will do so for the first time
## > cacheSolve(z)

## 3. Run cacheSolve again.  This time it will retreive the inverse matrix
## from the cache
## > cacheSolve(z)
