## Two functions that cache the inverse of a matrix.

## The first fuction will create a matrix that can be cached.
## More details in the function's comments
makeCacheMatrix <- function(x = matrix()) {
 ## Initialize inverse
  i <- NULL
  ## Here we set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Here we get the matrix, which return x
  get <- function() {x
  }
  ## setinverse and getinverse are similar to the previous set
  ## and get fuctions. They will store/return the inverse
  setinverse <- function(inverse) {i <<- inverse
  }
  getinverse <- function() {i
  }
  ## We store all the fuctions in a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function will first solve the matrix
## (find his inverse). If the inverse was previously
## calculated, it will return the i cached.
cacheSolve <- function(x, ...) {
  ## Return the inverse of x
  i <- x$getinverse()
  ## Return i if not NULL (so already set)
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## This part is like a else from the previous if
  ## get the matrix
  data <- x$get()
  ## Find the inverse
  i <- solve(data)
  ## set the inverse i
  x$setinverse(i)
  ## return the matrix
  i
}
