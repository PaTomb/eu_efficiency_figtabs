  
  
  #######################################################################################################################
  #                                                                                                                     # 
  # Name:     monty_vartia_table                                                                                        # 
  # Author:   Patrick Tomberger                                                                                         # 
  # Date:     October 13th, 2020                                                                                        # 
  # Notes:                                                                                                              # 
  #                                                                                                                     # 
  # Version history:                                                                                                    # 
  #                                                                                                                     # 
  # v1:         + Table that shows the ratios for the Monty-Vartia Decomposition of Energy usage for the regions.       # 
  #                                                                                                                     # 
  # v2:         + Accounts for the changes done to the data in the previous sub codes.                                  # 
  #                                                                                                                     # 
  # v3:         + Does the table also for the MV decomposition of energy efficiency.                                    # 
  #                                                                                                                     # 
  #######################################################################################################################
  
  
###################################################################################
# IV. Table for the Monty-Vartia ratios for the regions:                          #
###################################################################################
  
# 1. Prepare the raw data: 
###################################################################################
  
# Transform to ratios again, first energy usage: 
plot_reg_t[[1]]$movar_e <- ( plot_reg_t[[1]]$movar_e / 100 ) + 1
plot_reg  [[1]]$movar_e <- ( plot_reg  [[1]]$movar_e / 100 ) + 1

# Second, energy efficiency: 
plot_reg_t[[2]]$movar_i <- ( plot_reg_t[[2]]$movar_i / 100 ) + 1
plot_reg  [[2]]$movar_i <- ( plot_reg  [[2]]$movar_i / 100 ) + 1

# Take total energy changes as a blueprint:
reg_mv_e                                       <- plot_reg_t[[1]] 
colnames(reg_mv_e)[length(colnames(reg_mv_e))] <- unique( reg_mv_e$fac )
reg_mv_e$fac                                   <- NULL
reg_mv_e$monty_e                               <- NULL

# Make a data frame for the energy efficiency as well: 
reg_mv_i                                       <- plot_reg_t[[2]]; rm(plot_reg_t) 
colnames(reg_mv_i)[length(colnames(reg_mv_i))] <- unique( reg_mv_i$fac )
reg_mv_i$fac                                   <- NULL

# Add the other factors:
tmp                                            <- unique( plot_reg[[1]]$fac )
plot_reg[[1]]$monty_e                          <- NULL
  
for( i in 1:length(tmp) ) { # BEGIN FOR. 

  # Pull each factor out by force: 
  tmp2                                   <- plot_reg[[1]][plot_reg[[1]]$fac == tmp[i], ]
  tmp3                                   <- plot_reg[[2]][plot_reg[[2]]$fac == tmp[i], ]
  
  # Label it nicely: 
  colnames(tmp2)[length(colnames(tmp2))] <- unique( tmp2$fac )
  tmp2$fac                               <- NULL
  
  colnames(tmp3)[length(colnames(tmp3))] <- unique( tmp3$fac )
  tmp3$fac                               <- NULL
  
  # Match with the existing totals: 
  reg_mv_e                               <- left_join( reg_mv_e, tmp2, by = c("reg", "inv") ); rm(tmp2)
  reg_mv_i                               <- left_join( reg_mv_i, tmp3, by = c("reg", "inv") ); rm(tmp3)
  
} # END FOR. 

# Clean-up: 
rm(plot_reg)


# 2. Make a final check of the data: 
###################################################################################

# For final production the final goods trade factor is set to one temporarily: 
reg_mv_e$trd[reg_mv_e$inv == "Fin. Prod."] <- 1
reg_mv_i$trd[reg_mv_i$inv == "Fin. Prod."] <- 1

# Multiply all the factors up: 
reg_mv_e$test <- reg_mv_e$act * reg_mv_e$ehh * reg_mv_e$int * reg_mv_e$mix * reg_mv_e$str * reg_mv_e$sup * reg_mv_e$trd
reg_mv_i$test <- reg_mv_i$act * reg_mv_i$ehh * reg_mv_i$int * reg_mv_i$mix * reg_mv_i$str * reg_mv_i$sup * reg_mv_i$trd

# Evaluate the test: 
reg_mv_e$test            <- abs( reg_mv_e$test - reg_mv_e$Total )
reg_mv_i$test            <- abs( reg_mv_i$test - reg_mv_i$Total )

if( max( reg_mv_e$test ) > 1e-10 | max( reg_mv_i$test ) > 1e-10 ) print( "Monty-Vartia makes troubles!!" )

reg_mv_e$test            <- NULL
reg_mv_i$test            <- NULL


# 3. Finalize the table:  
###################################################################################

# Make factors to strings again: 
reg_mv_e$reg       <- as.character( reg_mv_e$reg )
reg_mv_e$inv       <- as.character( reg_mv_e$inv )

reg_mv_i$reg       <- as.character( reg_mv_i$reg )
reg_mv_i$inv       <- as.character( reg_mv_i$inv )

# Raound the ratios: 
tmp                <- c("Total", tmp)
reg_mv_e[, tmp]    <- round( reg_mv_e[, tmp], digits = 4 ) 
reg_mv_i[, tmp]    <- round( reg_mv_i[, tmp], digits = 4 ); rm(tmp)

# Bring regions and inventories to final order: 
order              <- as.data.frame( cbind(c("EU 28", "USA", "Japan", "R.o. OECD", "EFTA", "China")) )
order              <- as.data.frame( cbind(order[rep(seq_len(nrow(order)), times = length(inv), each = 1), ]) )
order$inv          <- rep( c("Prod.", "Fin. Prod.", "Fin. Cons."), times = 1, each = length(unique(order$V1)) )
colnames(order)[1] <- "reg"

reg_mv_e           <- left_join( order, reg_mv_e, by = c("reg", "inv") ) 
reg_mv_i           <- left_join( order, reg_mv_i, by = c("reg", "inv") ); rm(order)

# Set the entries for the trade factor in final production to "NA":
reg_mv_e$trd[reg_mv_e$inv == "Fin. Prod."] <- "NA"
reg_mv_i$trd[reg_mv_i$inv == "Fin. Prod."] <- "NA"

# Rename the total: 
reg_mv_e$total      <- reg_mv_e$Total
reg_mv_i$total      <- reg_mv_i$Total

# Make Latex friendly: 
reg_mv_e$col        <- "&"
reg_mv_i$col        <- "&"
reg_mv_e$end        <- "\\\\"
reg_mv_i$end        <- "\\\\"
reg_mv_e            <- reg_mv_e[, c("reg", "col", "inv", "col", "act", "col", "int", "col", "sup", "col", "str", "col", 
                                    "trd", "col", "mix", "col", "ehh", "total", "end")]
reg_mv_i            <- reg_mv_i[, c("reg", "col", "inv", "col", "act", "col", "int", "col", "sup", "col", "str", "col", 
                                    "trd", "col", "mix", "col", "ehh", "total", "end")]







