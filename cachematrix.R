## The makeCacheMatrix function allows you to get and set the matrix and its inverse, it lets you access the matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(x) m <<- x
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks whether the matrix has an inverse already cached, if it does it returns the inverse, if not it calculates the inverse and sets it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


amatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
amatrix$get()
amatrix$getinverse()
cacheSolve(amatrix)
cacheSolve(amatrix)
amatrix$getinverse()
amatrix$set(matrix(c(2,2,1,4),2,2))
amatrix$get()
amatrix$getinverse()
cacheSolve(amatrix)
cacheSolve(amatrix)
amatrix$getinverse()
