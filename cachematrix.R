## Below are two functions that are used to create a special object 
## that caches inverse of a matrix. 
## 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m << NULL
  }
  get <- function() x
  setinvm <- function(invm) inv_m <<- invm
  getinvm <- function() inv_m
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)  
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_m <- x$getinvm()
    if(!is.null(inv_m)){
      message("getting cached reverse matrix")
      return(inv_m)
    }
    data<- x$get()
    inv_m <- solve(data)
    x$setinvm(inv_m)
    inv_m
}
