## Put comments here that give an overall description of what your
## functions do

## following is a function which creates a special matrix which can 
## cache the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## following is a function which computes the inverse of the special 
## matrix which has been created above. If the inverse has already 
## cached, it simply returns that otherwise computes the inverse

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)){
          message("getting inversed matrix")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
