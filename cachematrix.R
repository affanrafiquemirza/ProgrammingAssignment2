## Put comments here that give an overall description of what your
## functions do

## Below are the two functions that create a special object, using the <<- operator, that stores a matrix and cache
## its inverse

## Write a short comment describing this function

## The function "makeCacheMatrix" takes a matrix as an argument and sets its value, gets the value of matrix using the
## getMat function, getting the inverse of matrix using getinverseMat and setting the inverse using setinverseMat. Here
## we use the <<- operator to assign the value to an object in an environment that is different from the current environment  

makeCacheMatrix <- function(x = matrix()) {
        
        inverseMat <- NULL 
        setMat <- function(y) { 
                x <<- y 
                inverseMat <<- NULL 
        } 
        
        getMat <- function() x  
        setInverseMat <- function(inverse) inverseMat <<- inverse  #set the value of the invertible matrix 
        
        getInverseMat <- function() inverseMat
        list(setMat = setMat, getMat = getMat,
             setInverseMat = setInverseMat, getInverseMat = getInverseMat) 
}


## Write a short comment describing this function

## Using the output of the fucntion above as an argument, the function below "cacheSolve" first checks whether the 
## the inverse is null or has some value in it. If the inverseMat variable is empty, using the matrix in the 
## "cacheMatrix" function, we calculate the inverse using the solve function and set the setInverseMat function of the
## matrix function. In case its not NULL, the function returns the set value in the inverseMat variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMat <- x$getInverseMat() 
        
        if(!is.null(inverseMat)) {                       
                message("Getting Cached Invertible Matrix")     
                return(inverseMat)                            
        }
        data <- x$getMat()                      
        inverseMat <- solve(data, ...)            
        x$setInverseMat(inverseMat)                         
        inverseMat                              
}
