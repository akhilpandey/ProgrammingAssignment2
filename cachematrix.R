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
		setmean=setmean,
		getmean=getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
}
