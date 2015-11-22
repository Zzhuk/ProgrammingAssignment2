##Next 2 functions create and cache inverse of a matrix

##This function creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the inverse
##get the inverse

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL ##NULL the inverse if matrix has been changed
  }
  get <- function() {x}
  setInverse <- function(inverse) {m_inv <<- inverse}
  getInverse <- function() m_inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##The following function calculates the inverse of the special "matrix" created with the above function. 
##It first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  m_inv <- x$getInverse()
  if (!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  matrix <- x$get()
  m_inv <- solve(matrix, ...)
  x$setInverse(m_inv)
  m_inv
}


## examples from the assignment
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
