  

#######################################################################################################################
#                                                                                                                     # 
# Name  :   func_acc                                                                                                  # 
# Author:   Patrick Tomberger                                                                                         # 
# Date  :   February 14th, 2019                                                                                       # 
# Notes :                                                                                                             # 
#                                                                                                                     # 
# Version history:                                                                                                    # 
#                                                                                                                     # 
# v1:         + Contains a set of often used user-written function to be accessed from a single file                  #
#                                                                                                                     # 
#######################################################################################################################


###################################################################################
# I. Define the funciton:                                                         #
###################################################################################

# Small fnction to aggregate a matrix/dataframe by given vectors. 
# Aggregation vectors for rows have to be named row_a
# Aggregation vectors for columns have to be named col_a

agg <- function(m) {
  
  m <- as.data.frame( m )
  m <- rowsum( m, row_a, reorder = F )
  m <- t( m )
  m <- rowsum( m, col_a, reorder = F )
  m <- as.data.frame( t(m) )
  
} # END FUNCTION.


# Define new operator that ignores divisions by zero and sets to zero: 
"%/0%" <- function(x,y) { 
  
  res                 <- x / y 
  res[ is.na(res)   ] <- 0
  res[ res == "Inf" ] <- 0
  return(res) 
  
} # END FUNCTION.


# Define new operator that ignores divisions by zero and sets to zero: 
"%/1%" <- function(x,y) { 
  
  res                 <- x / y 
  res[ is.na(res)   ] <- 1
  res[ res == "Inf" ] <- 1
  return(res) 
  
} # END FUNCTION.

# Define new operator that ignores divisions of a positive nominator by zero: 
# "%/i%" <- function(x,y) { 
  
#   res <- x / y ; res[ res == "Inf" ] <- 0
#   return(res) 
  
# } # END FUNCTION.

# Function that extracts WDI data in the desired format: 
own_wdi <- function( data, ind, period ) {
  
  # Load data, keep required parts and label nicely
  data           <- WDI( country = "all", indicator = ind[1], start = period[1], end = period[2], extra = T, cache = NULL )
  data           <- data[data$region != "Aggregates", ]
  data           <- data[, c("country", "iso3c", "year", ind[1])]
  colnames(data) <- c("reg", "iso", "yr", ind[2])
  
  # Get rid of the NAs
  data <- subset(data, ( !is.na(data[, 1])) )
  
} # END FUNCTION. 


# Set all other sectors explicitly to zero: 
lstrings <- function( x, n ) { # BEGIN FUNCTION. 
  
  substr( x, nchar( x ) - n + 1, nchar( x ) )
  
} # END FUNCTION. 



# Function that calculate the shares of variable w.r.t. a group it belongs to:  
calc_shr <- function( x, y, z ) { # BEGIN FUNCTION. 
  
  # x ... data frame where the varaible to split is located.
  # y ... name of the variable for which the shares should be calculated. 
  # z ... vecto with the parameters that are rquired for the aggregation function. .
  
  # Crate the list required for the aggregation: 
  tmp_l                          <- list()
  
  # Write the variables for the aggregation as different lists: 
  for( i in 1:length(z) ) { # BEGIN FOR. 
    
    tmp_l[[i]]                   <- x[, z[i]]
    
  } # END FOR. 
  
  # Calculate the total of the variable: 
  tmp                            <- aggregate( x[, y], by = tmp_l, FUN = sum )
  
  # Give proper variable names such that we can match to:
  tmp2                           <- paste( y, 't', sep = "_" )
  colnames(tmp)                  <- c( z, tmp2 )
  
  # Match back to calculate the shares: 
  x                              <- left_join( x, tmp, by = z )
  
  # Calculate the shares: 
  x[, paste(y, 'sh', sep = "_")] <- x[, y] %/0% x[, tmp2]
  
  # Clean-up a bit: 
  x[, c(y, tmp2)]                <- NULL

  # Check whether the shares sum up to one: 
  check                          <- aggregate( x[, paste(y, 'sh', sep = "_")], by = tmp_l, FUN = sum )
  
  if( sum( abs(check$x - 1) ) > 1e-10 ) { # BEGIN IF. 
    
    # Print a warning!
    print( "ERROR in share calculation!!" )
    
    # Break the function: 
    break # Actually only loops can be broken, but the effect is as intended -- i get an error message. 
    
  } # END IF. 
  
  # Return the shares: 
  return(x)
  
} # END FUNCTION. 
