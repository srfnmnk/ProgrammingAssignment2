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