##Caches solvable matrices

##creates a special matrix who's solution can be cached
makeCacheMatrix <- function(m = matrix()) {
	s <- NULL
	set <- function(m1) {
		m <<- m1
		s <<- NULL
	}
	get <- function() m
	setsolution <- function(solution) s <<- solution
	getsolution <- function() s
	list(set = set, get = get,
		setsolution = setsolution,
		getsolution = getsolution)
}

#Caches and returns cached solution to special matrix
cacheSolve <- function(m, ...) {
	s <- m$getsolution()
	if(!is.null(s)) {
		message("getting cached solution")
		return(s)
	}
	matrix <- m$get()
	s <- solve(matrix, ...)
	m$setsolution(s)
	s
}