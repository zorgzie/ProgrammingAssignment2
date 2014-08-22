## Put comments here that give an overall description of what your
## functions do
## Set value of matrix
## Get value of matrix
##Set value of inverse
##get value of inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) ## function taking optional matrix as argument
{
      
      cached.inverse <- NULL ##creating shell for inverse of x
      set <- function(y = matrix()) ##overwriting matrix x with any new matrix
        {
          x <<- y ##overwriting x
          cached.inverse <<- NULL ##correcting cached inverse when overwritten with y                 
        }

      get <- function() x ## returning x
      setinverse <- function(new.inverse) cached.inverse <<- new.inverse ## Creating new function with new.inverse as argument before overwriting cached.inverse with new.inverse
      getinverse <- function() cached.inverse ## Returning cached.inverse
      list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse) ##Creating list with 4 functions names set, get, setinverse, and getinverse
}

cacheSolve <- function(x) ## line says x does not have to be a matrix - it can take anything
{ 
  cached.inverse <- x$getinverse() ##lets user see the inverse
  if(!is.null(cached.inverse)) ## if cached.inverse has some value, it will return the cached.inverse
    {
      message("getting cached data") ##giveing user a message
      return(cached.inverse) ## Returning the value of cached.inverse
    }
  data <- x$get() ## getting matrix from x
  cached.inverse <- solve(data) ##Cached.inverse is null before solving for inverse matrix. Assigning inverted matrix to cached.matrix 
  x$setinverse(cached.inverse) ##Issue of scope.  User is changing the value of x
  return (cached.inverse) ##calling the Return function to return the new cached.inverse
}


## Write a short comment describing this function


