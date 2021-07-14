  
  
  #######################################################################################################################
  #                                                                                                                     # 
  # Name:     big_table                                                                                                 # 
  # Author:   Patrick Tomberger                                                                                         # 
  # Date:     October 17th, 2020                                                                                        # 
  # Notes:                                                                                                              # 
  #                                                                                                                     # 
  # Version history:                                                                                                    # 
  #                                                                                                                     # 
  # v1:         + Creates the very big table for the energy text.                                                       # 
  #                                                                                                                     # 
  # v2:         + Uses the updated value-added inventories (GTAP 10).                                                   # 
  #                                                                                                                     # 
  # v3:         + Slight modification: insteas of VA intensities we show the share regional energy mix shares.          # 
  #                                                                                                                     # 
  # v4:         + Also here Switzerland and EFTA are now one.                                                           # 
  #                                                                                                                     # 
  # v5:         + Do not discuss per capita energy usage.                                                               # 
  #                                                                                                                     # 
  #######################################################################################################################
  
  
###################################################################################
# II. Big Table for important regions:                                            #
###################################################################################

# 1. Prepare the raw data: 
###################################################################################

# Keep only the inventories in 1997 -- 2014: 
egy           <- c( "fos", "nuc", "rsn", "hyd", "bfl", "wnd", "sol", "rsr" )
inv           <- c("p", "fp", "c")

big_list      <- list()
big_list[[1]] <- egy_inv[, c("reg", "yr", paste("p" , egy, sep = "_"))][egy_inv$yr %in% c(1997, 2014), ]
big_list[[2]] <- egy_inv[, c("reg", "yr", paste("fp", egy, sep = "_"))][egy_inv$yr %in% c(1997, 2014), ]
big_list[[3]] <- egy_inv[, c("reg", "yr", paste("c" , egy, sep = "_"))][egy_inv$yr %in% c(1997, 2014), ]


# Bring the data into the long format: 
for( i in 1:length(big_list) ){ # BEGIN FOR. 

  # Create a total energy entry: 
  big_list[[i]][, paste(inv[i], "tot", sep = "_")] <- rowSums( big_list[[i]][, paste( inv[i], egy, sep = "_" )] )
  
  # Add biofuels and other renewables: 
  big_list[[i]][, paste(inv[i], "orn", sep = "_")] <- big_list[[i]][, paste(inv[i], "bfl", sep = "_")] + big_list[[i]][, paste(inv[i], "rsr", sep = "_")]
  big_list[[i]][, paste(inv[i], "bfl", sep = "_")] <- NULL
  big_list[[i]][, paste(inv[i], "rsr", sep = "_")] <- NULL
  
  # Bring to right format: 
  tmp           <- c( "tot", "fos", "nuc", "rsn", "orn", "hyd", "wnd", "sol" )
  big_list[[i]] <- big_list[[i]][, c("reg", "yr", paste( inv[i], tmp, sep = "_" ))]; rm(tmp)

  # Bring to long format: 
  colnames(big_list[[i]]) <- c( "reg", "yr", "Total Energy", "Fossil Fuels", "Nuclear", "Other nr.", "Other r.", "Hydro", "Wind", "Solar" )
  tmp                     <- colnames(big_list[[i]])[3:length(colnames(big_list[[i]]))]
  big_list[[i]]           <- reshape(  data     = big_list[[i]], 
                                      idvar     = c( "reg", "yr" ), 
                                      varying   = tmp, 
                                      times     = tmp, 
                                      v.name    = c( paste("egy", inv[i], sep = "_") ),
                                      direction = "long" ); rm(tmp)
  
  # Remove ugly rownames: 
  rownames(big_list[[i]]) <- NULL
  
  # Bring from Kilo- to megatons: 
  big_list[[i]][,  paste("egy", inv[i], sep = "_")] <- big_list[[i]][,  paste("egy", inv[i], sep = "_")] / 1000
  
  # Reasonable colneames: 
  colnames(big_list[[i]])[3] <- "inv"

} # END FOR. 


# Put all into one dataframe: 
big_table       <- big_list[[1]]
big_table       <- left_join( big_table, big_list[[2]], by = c("reg", "yr", "inv") )
big_table       <- left_join( big_table, big_list[[3]], by = c("reg", "yr", "inv") ); rm(big_list)

# Match with population data: 
big_table       <- left_join( big_table, wb_data[, c("reg", "yr", "pop")], by = c("reg", "yr") )

# Aggregate the EU, Rest of OECD, EFTA and RoW:
big_table$reg[big_table$reg %in% tmp_eu$reg      ] <- "eu"
big_table$reg[big_table$reg %in% roecd           ] <- "roecd"
big_table$reg[big_table$reg %in% c("che", "xef") ] <- "efta"

i_reg                                        <- c("eu", "usa", "jpn", "roecd", "efta", "chn")
big_table$reg[!big_table$reg %in% i_reg]     <- "row"

big_table                                    <- aggregate( big_table[, c("egy_p", "egy_fp", "egy_c", "pop")], 
                                                           by = list(big_table$reg, big_table$yr, big_table$inv), FUN = sum )
colnames(big_table)[1:3]                     <- c("reg", "yr", "inv")


# 2. Calculate the variables in the table:  
###################################################################################

# Row serves as itself: 
tmp                <- aggregate( big_table[, c("egy_p", "egy_fp", "egy_c")], by = list(big_table$yr, big_table$inv), FUN = sum )
colnames(tmp)      <- c("yr", "inv", "p_shr", "fp_shr", "c_shr")

# Match back and calculate the shares: 
big_table          <- left_join( big_table, tmp, by = c("yr", "inv") ); rm(tmp)
big_table$p_shr    <- round( big_table$egy_p  / big_table$p_shr  * 100, digits = 1 )
big_table$fp_shr   <- round( big_table$egy_fp / big_table$fp_shr * 100, digits = 1 )
big_table$c_shr    <- round( big_table$egy_c  / big_table$c_shr  * 100, digits = 1 )

# Calculate energy per capita: 
# big_table$prod_pc  <- round( big_table$egy_p  / big_table$pop         , digits = 1 )
# big_table$cons_pc  <- round( big_table$egy_c  / big_table$pop         , digits = 1 )

# Calculate the energy-mix shares: 
tmp                <- big_table[, c("reg", "yr", "egy_p", "egy_fp", "egy_c")][big_table$inv == "Total Energy", ]
tmp2               <- c("mix_shr_p", "mix_shr_fp", "mix_shr_c")
colnames(tmp)[3:5] <- tmp2

big_table          <- left_join( big_table, tmp, by = c("reg", "yr") ); rm(tmp)
big_table[, tmp2]  <- round( big_table[, c("egy_p", "egy_fp", "egy_c")] / big_table[, tmp2] * 100, digits = 1); rm(tmp2)


# 3. Bring to final format:   
###################################################################################

# Bring to right order: 
order           <- as.data.frame( cbind(c("eu", "usa", "jpn", "roecd", "efta", "chn")) )
order           <- order[rep(seq_len(nrow(order)), times = length(unique(big_table$yr)), each = length(unique(big_table$inv))), ]
order           <- as.data.frame( cbind(order) ); colnames(order) <- "reg"

order$yr        <- rep( c(1997, 2014), times = 1, each = length(order$reg) / 2 )

tmp             <- c("Total Energy", "Fossil Fuels", "Nuclear", "Other nr.", "Other r.", "Hydro", "Wind", "Solar" )
order$inv       <- rep( tmp, times = length(unique(order$reg)) * 2, each = 1 )
big_table       <- left_join( order, big_table, by = c("reg", "yr", "inv") ); rm(order)
  
# Bring to final format: 
big_table         <- big_table[, c("yr", "reg", "inv", "egy_p", "mix_shr_p", "p_shr", "egy_fp", "mix_shr_fp", "fp_shr", "egy_c", "mix_shr_c", "c_shr")]
big_table$egy_p   <- round( big_table$egy_p , digits = 1 )
big_table$egy_fp  <- round( big_table$egy_fp, digits = 1 )
big_table$egy_c   <- round( big_table$egy_c , digits = 1 )

# Eliminate Row: 
big_table         <- big_table[big_table$reg != "row", ]

# Make Latex friendly: 
big_table$col     <- "&"
big_table$end     <- "\\\\"
big_table         <- big_table[, c("yr", "reg", "col", "inv", "col", "egy_p", "col", "mix_shr_p", "col", "p_shr", "col", "egy_fp" , "col", "mix_shr_fp", 
                                   "col", "fp_shr" , "col", "egy_c", "col"  , "mix_shr_c", "col", "c_shr", "end")]



    