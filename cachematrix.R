## I didn't start from zero, instead I used cachemean as an example structure for this functions
## and tried to understand its functionallity as best as I can. That's why I extended so much in
## comments, more than document the code I used it to explain to myself (and peers who will review
## the assigmente) what I understood from the R help, the forums, and a couple of seaches on Google.

makeCacheMatrix <- function(x = matrix()) {
        
        ## A) Parameters
        ## x: is a invertible matrix
        
        ## B) Objetive
        ## This function creates a vector of functions, each of wich are used one by one.
        ## In order to use the scooping advantages of R, this function should be run first to
        ## "store" or "cache" the inverse of the matrix, and save resources in case that it is 
        ## required later.
        ## The functions created are used to (given a matrix of name 'M'):
        ## 1) get: retrieve the cached matrix 'M'
        ## 2) set: store the matrix 'M' in cache
        ## 3) getinv: retrieve the previously calculated inverse of the matrix 'M'
        ## 4) setinv: store the inverse of the matrix 'M'
        
        ## C) Procedure
        
        ## Sets the value of 'inv' to NULL to clear previous stored values
        ## This remarks the importance of running this function first, otherwise one can
        ## obtain a result, but maybe incorrect if the matrix has changed
        inv <- NULL
        
        ## c.1) Function 'set'
        ## Takes the matrix and stores it. The operator '<<-' forces to look at the parent
        ## environment for an existing definition. Then, siince it is a new matrix, 
        ## clears any previous stored inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## c.2) Funtion 'get'
        ## Simply returns the value of the matrix
        get <- function() {x}
        
        ## c.3) Function 'setinv'
        ## This function searches in the parent environment if there is an existing definition 
        ## of 'inv'. For example in the function cacheSolve we got:
        ## inv <- solve(data, ...): calculates the inverse of the matrix
        ## x$setinv(inv): calls 'setinv', wich will look for a previous definition on 'inv' and
        ## will find the previously calculated value
        ## This means that the inverse of 'M' is not calculated here, if we follow the example of
        ## cachemean maybe we were tempted to use 'solve' as parameters for this function, it's ok
        ## but it may be a little confusing, we can use any other name.
        setinv <- function(mtxInv) {inv <<- mtxInv}
        
        ## c.4) Function 'getinv'
        ## Simply returns a previously calculated inverse
        getinv <- function() {inv}
        
        ## c.5) Creates the list of functions that are the result of this one
        ## According to what I read, the left side of each element corresponds to the name
        ## by wich the function should be identified and called, and the right refers to the 
        ## previously defined functions. I don't see a reason for wich the names couldn`t be
        ## different
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
        
        ## A) Parameters:
        ## x: is an invertible matrix
        
        ## B) Objetive: Return a matrix that is the inverse of 'x'
        
        ## C) Procedure
        
        ## c.1) Gets the value of inv
        inv <- x$getinv()
        
        ## c. 2) If the value of inv is not NULL, it means that there is a previously 
        ## calculated inverse. Returns the stored value and exits this function.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## c.3) If the value of 'inv' was NULL, we should calculate a new inverse
        ## Why not use simply 'x'? I guess the reason is consistency. This way we should run
        ## first 'makeCacheMatrix' or get an error. I think it is to be sure that the inverse
        ## always be correspond to the correct matrix.
        ## The last four lines are to retrieve the matrix, calculate its inverse, store it
        ## for later use, and return it to the calling environment
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
}