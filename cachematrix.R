## Creates functons that build a cached matrix object that can 
## store a matrix inverse and compute a matrix inverse and
## cache it

## Creates an object that can cache a matrix's inverse
## Code derived from makeVector function given in Assignment
## 2's directions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinvmatrix <- function(solve) m <<- solve
  getinvmatrix <- function() m
  list(set=set, get=get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## Computes the inverse of the makeCacheMatrix object 
## if the inverse has already been calculated , then 
## cacheSolve should retrieve the inverse from the cache.
## Code derived from cachemean function given in Assignment
## 2's directions

cacheSolve <- function(x, ...) {
  m <- x$getinvmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmatrix(m)
  m
}
