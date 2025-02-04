> makeCacheMatrix <- function(x = matrix()) {
+     inv <- NULL  # Initialize inverse as NULL
+     set <- function(y) {
+         x <<- y  # Store new matrix
+         inv <<- NULL  # Reset inverse when matrix changes
+     }
        
+     get <- function() x  # Return the matrix
+     setInverse <- function(inverse) inv <<- inverse  # Store the inverse
+     getInverse <- function() inv  # Retrieve the inverse
+     list(set = set, get = get,
+          setInverse = setInverse,
+          getInverse = getInverse)  # Return a list of functions
+ }

> cacheSolve <- function(x, ...) {
+     inv <- x$getInverse()  # Check if inverse is already cached
+     if (!is.null(inv)) {  # If cached, return it
+         message("getting cached data")
+         return(inv)
+     }

+     data <- x$get()  # Get the matrix
+     inv <- solve(data, ...)  # Compute the inverse
+     x$setInverse(inv)  # Store the inverse in cache
+     inv  # Return the computed inverse
+ }
