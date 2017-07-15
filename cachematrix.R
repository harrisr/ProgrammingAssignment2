#----------------------------------------------------------------
#  Student/Programmer:  Raymond C. Harris
#  Completed:           7/15/2017
#  Assignment GIT Repo: https://github.com/harrisr/ProgrammingAssignment2
#----------------------------------------------------------------
#  This function creates a special "matrix" object
#  that can cache its inverse.
#
#  PART #1:
#  on the command line do this:
#
#  source("cachematrix.R")
#
#  mtx <- matrix( c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
#
#  class(mtx)       # (just to check it)
#
#  my_matrix <- makeCacheMatrix(mtx)
#
#  my_matrix$get()     #  to see the matrix
#
#       [,1] [,2]
#  [1,]    1    2
#  [2,]    3    4
#
#
#  (  Prior to using the "cacheMatrix" function listed below...
#     ... to see the inverse of the matrix, you can type:  )
#
#  solve( my_matrix$get() )
#
#  you should see:
#
#       [,1] [,2]
#  [1,] -2.0  1.0
#  [2,]  1.5 -0.5
#----------------------------------------------------------------


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


#----------------------------------------------------------------
#  Computing the inverse of a square matrix can be done with the `solve`
#  function in R. For example, if `X` is a square invertible matrix, then
#  `solve(X)` returns its inverse.
#----------------------------------------------------------------
#  This function computes the inverse of the special
#  "matrix" returned by `makeCacheMatrix` above. If the inverse has
#  already been calculated (and the matrix has not changed), then
#  `cacheSolve` should retrieve the inverse from the cache.
#
#  PART #2:
#  on the command line do this:
#
#  source("cachematrix.R")
#
#  my_inverse <- cacheSolve(my_matrix)
#
#  ( if you used the 2X2 matrix  (1, 2, 3, 4) as listed above, 
#    if you type on the command line:  )
#
#  my_inverse
#
#  you should see:
#
#       [,1]  [,2]
#  [1,] -2.0   1.0
#  [2,]  1.5  -0.5
#
#  (  then if you type again:  )
#
#  my_inverse <- cacheSolve(my_matrix)
#
#  (  you should now see:  )
#
#  "getting matrix inverse from cached data"
#
#  then type again:
#
#  my_inverse
#----------------------------------------------------------------

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting matrix inverse from cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------


# This code is copied in here from the README.md in order to
# "kick the tires" on it, and make notes...
#
#    1.  FIRST, make this call, like so:
#
#        my_vector <- makeVector(   c(3, 4, 5, 6, 7)  )
#
#        my_vector now contains 4 functions:
#           set() / get() / setmean() / getmean()
#
#        these can be called like so:
#
#        my_vector$get()         ...  returns  3, 4, 5, 6, 7
#        my_vector$getmean()     ...  returns   NULL
#        my_vector$setmean(999)  ...  returns   NULL
#        my_vector$getmean()     ...  returns   999
#        my_vector$set(c(1, 2, 3, 4))  ...  returns   _nothing_
#----------------------------------------------------------------


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


#----------------------------------------------------------------
#    2.  SECOND, we can now make this call, like so:
#
#        my_mean <- cachemean(  my_vector  )
#
#        if you now type "my_mean" on the command line in the console you see:
#
#        "5"   (which is the mean of 3, 4, 5, 6, 7)
#
#        if you now type on the command line again:
#
#        my_mean <- cachemean(  my_vector  )
#
#        you see:
#
#        "getting cached data"
#
#        if you now type "my_mean" on the command line in the console you see:
#
#        "5"   (which was retrieved from the cache)
#----------------------------------------------------------------

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

