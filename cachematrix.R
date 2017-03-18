##
## Author : RY ; Date Created : 03/18/2017 (as part of CourseEra Data Science Week3 assignment)
## This function creates a special 'matrix' object that can cache its inverse.
##

# set the value of the matrix
# get the value of the matrix
# set the value if inverse of the matrix using solve function
# get the value if inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
            i <- NULL
            set <- function(y) {
                    x <<- y
                    i <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) i <<- solve
            getinverse <- function() i
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}

##
## This function computes the inverse of the special 'matrix' returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.
## this function assumes the input matrix is always invertible.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()

        if(!is.null(i)) {

                message("getting cached inverse data")

                return(i)

        }

        data <- x$get()

        i <- solve(data, ...)

        x$setinverse(i)

        i

}
