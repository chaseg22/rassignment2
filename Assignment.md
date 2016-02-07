# rassignment2

### Function to create a matrix and assign it to the Cache variable 
### Creates a list which allows us to view the created matrix and the inverse once computed
makeCacheMatrix<- function(x = as.matrix) {
	inv<- NULL
	set<- function(y) {
		x<<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set,
	get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}

### finds the inverse of a functiion if it has not been computed using the solve function
cacheSolve<- function(x, ...) {
	inv<-x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data<- x$get()
	inv<- solve(data,...)
	x$setinverse(inv)
	inv
}
