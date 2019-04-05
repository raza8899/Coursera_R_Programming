## This code basically calculates the inverse of matrix and what does more is interesting, 
## it uses the cached data to increasing the processing speed i.e it checks if the inverse of
## matrix is already present in the cached memory. If so then it uses it otherwise it calculates it.

## This function makes the calculated inverse of matrix stored in cached memory of R
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<-y
        m<<-NULL
    }
    get <- function() x
    set_inverse_matrix <- function(inverse_of_matrix) m<<- inverse_of_matrix
    get_inverse_matrix <- function() m
    list(set=set,get=get,set_inverse_matrix=set_inverse_matrix,
         get_inverse_matrix=get_inverse_matrix)

}


## What this function do is basically check if the inverse is already present in cached memory
## or not , and if it is present it returns it, otherwise it calculates the mean .

cacheSolve <- function(x, ...) {
        m <- x$get_inverse_matrix()
        if(!is.null(m)){
            message("Using the cached data and returning it")
            return(m)
        }
        else{
            data <- x$get()
            m <- solve(data,...)
            x$set_inverse_matrix(m)
            m
            
        }
}
