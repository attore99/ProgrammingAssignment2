#this function stores a matrix and allows to then store its inverse
makeMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #here, setter and getter functions are defined
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#this function allows to compute the inverse - if not already computed
cacheinv <- function(x, ...) {
  inv <- x$getinv()
  #check if inverse is already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}