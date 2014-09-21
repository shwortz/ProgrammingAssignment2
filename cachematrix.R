## The two functions work together to take a square matrix and check if ##its inverse has been calculated. If the inverse has been calculated,  ##it is already cached --ie, saved to a variable-- and no computation is ##necessary. If the inverse has not been calculated and cached, then it ##is calculated and cached so that the inverse can be accessed from the ##cache variable, eliminating the need to do the calculation again.    

## Passing a square matrix into this function will erase the value of the ##inverse variable --ie, set it to NULL -- and store the matrix in a ##variable. 

 makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
              x <<- y
              inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
 }  


## Passing the output of makeCacheMatrix() into this function will ##retrieve the value of the inverse stored in makeCacheMatrix(), and then ##check if the inverse is NULL or has already been calculated and cached. ##If it has been calculated and cached, the value is returned along with ##the message, "getting cached data". If the inverse value is NULL in ##makeCacheMatrix(), then cachSolve() calculates the inverse, stores it ##in makeCacheMatrix(), and returns it.  

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
  }
