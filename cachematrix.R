makeCacheMatrix <- function(x = matrix()) {
  res <- NULL
  set <- function(y) {
    x <<- y
    res <<- NULL
  }
  get <- function() x
  setres <- function(r) res <<- r
  getres <- function() res
  list(set = set, 
       get = get,
       setres = setres,
       getres = getres)  
}

cacheSolve <- function(x, ...) {
  res <- x$getres()
  if(!is.null(res)) {
    message("getting cached data")
    return(res)
  }
  a <- x$get()
  res <- solve(a)
  x$setres(res)
  res
}
