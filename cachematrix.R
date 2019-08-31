## Programming Assignment 2: Lexical Scoping
## Sathya Raj Eswaran : Aug/30/2019

##Scope of the Function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        inverseVal <- NULL  
        
        ## initialize inverseVal as NULL . This will hold value of matrix inverse
        set <- function(y) {  
                ## This function will assign new value
                x <<- y        
                ## value of matrix in Initial environment
                inverseVal <<- NULL 
                ## This will reset inverseVal to NULL
        }
        
        get <- function() x     
        ## Get fucntion returns value of the matrix argument
        
        setInverse <- function(CreateInverse) inverseVal <<- CreateInverse 
        
        ## assigns value of inverseVal in Initial environment
        getInverse <- function() inverseVal   
        
        ## gets the value of inverseVal when it has been called
        list(set = set, get = get,  setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
        
        inverseVal <- x$getInverse()
        
        #if inverse matrix is not NULL .
        
        if(!is.null(inverseVal)) {
                message("getting cached data")
                ## We get a message "getting cached data"
                return(inverseVal)
                #return the inverted matrix
        }
        data <- x$get() ##getting the original Matrix data 
        
        inverseVal <- solve(data) ## calling the solve function to inverse the matrix
        
        x$setInverse(inverseVal) ##Set value to the inverted matrix 
        
        inverseVal
        #return the Final invertible matrix
}




