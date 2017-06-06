## Instructions: 
## Put comments here that give an overall description of what your functions do: 

# Matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse of a matrix rather than compute it repeatedly. 
# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, then it retrive the result from cache
# and skips the computation. 
# If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  
  # checks if the inverse has already been computed
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    
  # If so, then it retrive the result from cache and skips the computation. 
    return(inv)
  }
  
  # If not, it computes the inverse, sets the value in the cache via setinverse function.
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


########################## Try it out ########################

x<-rbind(c(1, -1/2), c(-1/2, 1))
m<-makeCacheMatrix(x)
cacheSolve(m)
cacheSolve(m)


#> cacheSolve(m)
#[,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333

# The second time it just retrive the result from cache.

#> cacheSolve(m)
#getting cached data.
#[,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333

