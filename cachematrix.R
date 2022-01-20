# This process consists of 2 functions. The first function
# will assign input to the variable makeCacheMatrix (as a matrix).
# The first part of this function initializes the values x (as a function 
# argument and the variable z by setting to NULL.
# 
# The next section defines the four key elements/behaviors: the "getters"
# and "setters". The set code/definition function accepts an argument named y.
#
# The <<- operands or (assignment operators) assign the value on the right-hand
# side to that on the left. The set operation accomplishes two things. 1. Assign
# the input argument to the x object in the parent environment. 2. Assign a
# NULL value to the z object also in the parent environment. This will clear any
# z value cached due to a prior execution of cacheSolve.
#
# The next set defines the "getter" for the vector x. The x is not defined within
# the get() - it is retrieved from the parent environment.
#
# Next, the setter for the "solve" is defined. Again, the <<- is used to assign 
# the input argument to the value of z in the parent environment. 
#
# Lastly, uses lexical scoping to find the correct symbol z to retrieve its
# value. This is the getinv <- function() z of the code.
#
# The last line of code assigns each of the functions as an element within
# a list() and returns it to the parent environment. Upon completion, it returns
# a formed object of type makeCacheMatrix() to be used later.

makeCacheMatrix <- function(x=matrix())    {
        z <- NULL
        set <- function(y)      {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setinv <- function(solve) z <<- solve
        getinv <- function() z
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}
# The purpose is to populate or retrieve inverse matrix (solve) from an object
# type makeCacheMatrix(). CacheSolve, as with makeCacheMatrix, begins with a
# single argument, x. 
#
# Next, an attempt to retrieve an inverse value from the object passed in as the
# argument. It calls the x$getinv() on the input object. A check for a null
# value is done. If this is not null, there is a valid inverse and can return it
# to the parent environment. 
#
# If the result of !is.null is FALSE, then compute the inverse matrix using the
# value from the input object. It uses setinv() function of the input object
# to set the inverse value. It returns that value to the parent environment.
# Ultimately, a inverse matrix is returned to the user.

cacheSolve <- function(x,...)      {
        z <- x$getinv()
        if(!is.null(z))         {
                message("Using Cache Data")
                return(z)
        }
        data <- x$get()
        z <- solve(data,...)
        x$setinv(z)
        z
}
# For testing purposes, I have collected some examples. (see below). One of the 
# testing scenarios will create an error. This error equates to a situation 
# where the inverse matrix does not exist.
#
# These examples were provided by 
# https://www.chilimath.com/lessons/advanced-algebra/inverse-of-a-2x2-matrix/
# This site also includes the answers. 
#
t1 <- makeCacheMatrix(matrix(c(5,-7,2,-3),nrow=2,ncol=2))
t2 <- makeCacheMatrix(matrix(c(-3,5,1,-2),nrow=2,ncol=2))
t3 <- makeCacheMatrix(matrix(c(1,3,2,4),nrow=2,ncol=2))
t4 <- makeCacheMatrix(matrix(c(4,2,2,1),nrow=2,ncol=2))
t5 <- makeCacheMatrix(matrix(c(1,3,-1,4),nrow=2,ncol=2))

