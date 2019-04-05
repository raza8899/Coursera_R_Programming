## This code basically calculates the inverse of matrix and what more it does is interesting, 
## it uses the cached data for increasing the processing speed i.e it checks if the inverse of
## matrix is already present in the cached memory. If so then it uses it otherwise it calculates it.

## This function helps to store the inverse of matrix  in cached memory and 
## returns the cached data if present.
makeCacheMatrix <- function(matrix = matrix()) {
    inverse_of_matrix <- NULL
    setmatrix <- function(y){
        matrix<<- y
## since we are changing the matrix , hence we initialize the inverse of matrix as null
        inverse_of_matrix<<-NULL
    }
  
    getmatrix <- function() matrix
    set_inverse_of_matrix <- function(inverse_of_matrix) inverse_of_matrix<<- inverse_of_matrix
    get_inverse_of_matrix <- function() inverse_of_matrix
    list(setmatrix=setmatrix,getmatrix=getmatrix,set_inverse_of_matrix=set_inverse_of_matrix,
         get_inverse_of_matrix=get_inverse_of_matrix)

}


## What this function do is basically check if the inverse is already present in cached memory
## or not , and if it is present it returns it, otherwise it calculates the inverse of matrix.

cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse_of_matrix()
        if(!is.null(inverse)){
            message("Using the cached data and returning it")
            return(inverse)
        }
        else{
            data <- x$getmatrix()
            inverse<- solve(data,...)
            x$set_inverse_of_matrix(inverse)
            inverse
            
        }
}
