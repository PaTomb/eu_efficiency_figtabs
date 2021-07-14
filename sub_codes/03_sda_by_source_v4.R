  
  
  #######################################################################################################################
  #                                                                                                                     # 
  # Name:     sda_by_source                                                                                             # 
  # Author:   Patrick Tomberger                                                                                         # 
  # Date:     October 12th, 2020                                                                                        # 
  # Notes:                                                                                                              # 
  #                                                                                                                     # 
  # Version history:                                                                                                    # 
  #                                                                                                                     # 
  # v1:         + Version of sda_cnt that keeps the region of origin/destination by income group.                       # 
  #                                                                                                                     # 
  # v2:         + Includes Switzerland to EFTA.                                                                         #
  #                                                                                                                     # 
  # v3:         + Include the absolute and relative changes of HH energy usage.                                         #
  #                                                                                                                     # 
  # v4:         + HH energy usage is now directly included in the raw data files.                                       #
  #                                                                                                                     # 
  #######################################################################################################################
  
  
###################################################################################
# III. Domestic/foreign contribution of the factors:                              #
###################################################################################

# 1. Separate tha data according to inventory and energy vs. intensity: 
###################################################################################

# Take the raw data from sda_cnt: 
tmp_pr      <- list()
tmp_pr[[1]] <- sda_cnt[[1]][sda_cnt[[1]]$inv == "Production", ]
tmp_pr[[2]] <- sda_cnt[[2]][sda_cnt[[2]]$inv == "Production", ]

tmp_fp      <- list()
tmp_fp[[1]] <- sda_cnt[[1]][sda_cnt[[1]]$inv != "Production", ]
tmp_fp[[2]] <- sda_cnt[[2]][sda_cnt[[2]]$inv != "Production", ]


# 2. Relable the source (fooptprints) or destination (prod.):   
###################################################################################

# Extract the WB income regions from the CH4 dataset: 
wb_ig   <- unique( wb_data[, c("reg", "wb_ig")] )

# For production: define the destination region as domestic if it is the same as the source, all other regions are assigned to their WB group
# Footprints: same as for production, but we re-label the destination regions. 
# European Union: all members are high income, except BGR and ROU --> treat as H
# Rest of OECD: all members are high income, except MEX and TUR   --> treat as H
tmp     <- rbind( wb_ig[3, ], wb_ig[3, ], wb_ig[3, ] )
tmp$reg <- c("eu", "roecd", "efta")
wb_ig2  <- rbind( wb_ig, tmp ); rm(tmp)
wb_ig2  <- wb_ig2[!wb_ig2$reg %in% roecd     , ] 
wb_ig2  <- wb_ig2[!wb_ig2$reg %in% tmp_eu$reg, ]

# Re-label the respective region: 
tmp     <- unique( wb_ig$wb_ig )

for( i in 1:length(tmp_fp) ) { # BEGIN FOR OVER SDA. 
  
  # If region of origin is equal to region of destination, label as domestic: 
  tmp_pr[[i]]$reg_j[tmp_pr[[i]]$reg_i   == tmp_pr[[i]]$reg_j]  <- "dom"
  tmp_pr[[i]]$reg_ja[tmp_pr[[i]]$reg_ia == tmp_pr[[i]]$reg_ja] <- "dom"
  tmp_fp[[i]]$reg_i[tmp_fp[[i]]$reg_i   == tmp_fp[[i]]$reg_j]  <- "dom"
  tmp_fp[[i]]$reg_ia[tmp_fp[[i]]$reg_ia == tmp_fp[[i]]$reg_ja] <- "dom"
  
  # The remaining regions should be labelled according to their income groups: 
  for( j in 1:length(tmp) ) { # BEGIN FOR OVER INCOME GROUPS. 
    
    tmp_pr[[i]]$reg_j [tmp_pr[[i]]$reg_j  %in% wb_ig$reg [wb_ig$ wb_ig == tmp[j]]] <- tmp[j]
    tmp_pr[[i]]$reg_ja[tmp_pr[[i]]$reg_ja %in% wb_ig2$reg[wb_ig2$wb_ig == tmp[j]]] <- tmp[j]
    tmp_fp[[i]]$reg_i [tmp_fp[[i]]$reg_i  %in% wb_ig$reg [wb_ig$ wb_ig == tmp[j]]] <- tmp[j]
    tmp_fp[[i]]$reg_ia[tmp_fp[[i]]$reg_ia %in% wb_ig2$reg[wb_ig2$wb_ig == tmp[j]]] <- tmp[j]
    
  } # END FOR OVER INCOME GROUPS. 
  
} # END FOR OVER SDA. 

# Clean-up: 
rm( wb_ig, wb_ig2 )


# 3. Aggregate inventory specific the source or destination regions:    
###################################################################################

# Prepare the original data: 
sda_cnt           <- list()
sda_cnt[[1]]      <- tmp_pr
sda_cnt[[2]]      <- tmp_fp

# Assign now the correct list elements: 
sda_cnt[[1]][[1]] <- tmp_pr[[1]]
sda_cnt[[1]][[2]] <- tmp_fp[[1]]
sda_cnt[[2]][[1]] <- tmp_pr[[2]]
sda_cnt[[2]][[2]] <- tmp_fp[[2]]; rm(tmp_pr, tmp_fp)

sda_reg           <- sda_cnt


# 4. Aggregate the country-based SDAs:     
###################################################################################

# If required, move CHE to EFTA: 
# if( che_to_efta == T ) { # BEGIN IF-ELSE. 
  
  # Rename Switzerland: 
#   for( i in 1:length(sda_reg[[1]]) ) { # BEGIN FOR.   

#     sda_reg[[i]][[1]]$reg_ia[sda_reg[[i]][[1]]$reg_i == "che"] <- "xef" 
#     sda_reg[[i]][[2]]$reg_ja[sda_reg[[i]][[2]]$reg_j == "che"] <- "xef" 
    
    # Deal with the regions of destination -- Region of destination should be WB IG!!!
    # sda_reg[[1]][[i]]$reg_ja[sda_reg[[1]][[i]]$reg_j == "che"] <- "xef" 
    # sda_reg[[2]][[i]]$reg_ja[sda_reg[[2]][[i]]$reg_j == "che"] <- "xef" 
    
#   } # END FOR. 
  
  # Call the regions different: 
#   efta <- "EFTA"

# } else {
  
#   efta <- "Rest of EFTA"
  
# } # END IF-ELSE. 

# Production-based energy: 
tmp1                        <- aggregate( sda_cnt[[1]][[1]]$monty_e                       , by = list(sda_cnt[[1]][[1]]$reg_i, sda_cnt[[1]][[1]]$reg_j, sda_cnt[[1]][[1]]$inv, sda_cnt[[1]][[1]]$fac), FUN = sum  )
tmp2                        <- aggregate( sda_cnt[[1]][[1]][, c("movar_e", "movar_e_agg")], by = list(sda_cnt[[1]][[1]]$reg_i, sda_cnt[[1]][[1]]$reg_j, sda_cnt[[1]][[1]]$inv, sda_cnt[[1]][[1]]$fac), FUN = prod )

colnames(tmp1)              <- c("reg", "s_d", "inv", "fac", "monty_e"               )
colnames(tmp2)              <- c("reg", "s_d", "inv", "fac", "movar_e", "movar_e_agg")

sda_cnt[[1]][[1]]           <- left_join( tmp1, tmp2, by = c("reg", "s_d", "inv", "fac")); rm(tmp1, tmp2)

# Footprint-based energy: 
tmp1                        <- aggregate( sda_cnt[[1]][[2]]$monty_e                       , by = list(sda_cnt[[1]][[2]]$reg_j, sda_cnt[[1]][[2]]$reg_i, sda_cnt[[1]][[2]]$inv, sda_cnt[[1]][[2]]$fac), FUN = sum  )
tmp2                        <- aggregate( sda_cnt[[1]][[2]][, c("movar_e", "movar_e_agg")], by = list(sda_cnt[[1]][[2]]$reg_j, sda_cnt[[1]][[2]]$reg_i, sda_cnt[[1]][[2]]$inv, sda_cnt[[1]][[2]]$fac), FUN = prod )

colnames(tmp1)              <- c("reg", "s_d", "inv", "fac", "monty_e"               )
colnames(tmp2)              <- c("reg", "s_d", "inv", "fac", "movar_e", "movar_e_agg")

sda_cnt[[1]][[2]]           <- left_join( tmp1, tmp2, by = c("reg", "s_d", "inv", "fac")); rm(tmp1, tmp2)

# Production-based intensity: 
sda_cnt[[2]][[1]]           <- aggregate( sda_cnt[[2]][[1]][, c("movar_i", "movar_i_agg")], by = list(sda_cnt[[2]][[1]]$reg_i, sda_cnt[[2]][[1]]$reg_j, sda_cnt[[2]][[1]]$inv, sda_cnt[[2]][[1]]$fac), FUN = prod )
colnames(sda_cnt[[2]][[1]]) <- c("reg", "s_d", "inv", "fac", "movar_i", "movar_i_agg")

# Footprint-based intensity: 
sda_cnt[[2]][[2]]           <- aggregate( sda_cnt[[2]][[2]][, c("movar_i", "movar_i_agg")], by = list(sda_cnt[[2]][[2]]$reg_j, sda_cnt[[2]][[2]]$reg_i, sda_cnt[[2]][[2]]$inv, sda_cnt[[2]][[2]]$fac), FUN = prod )
colnames(sda_cnt[[2]][[2]]) <- c("reg", "s_d", "inv", "fac", "movar_i", "movar_i_agg")


# 4. Aggregate the region-based SDAs:     
###################################################################################

# Production-based energy: 
tmp1                        <- aggregate( sda_reg[[1]][[1]]$monty_e                       , by = list(sda_reg[[1]][[1]]$reg_ia, sda_reg[[1]][[1]]$reg_ja, sda_reg[[1]][[1]]$inv, sda_reg[[1]][[1]]$fac), FUN = sum  )
tmp2                        <- aggregate( sda_reg[[1]][[1]][, c("movar_e", "movar_e_agg")], by = list(sda_reg[[1]][[1]]$reg_ia, sda_reg[[1]][[1]]$reg_ja, sda_reg[[1]][[1]]$inv, sda_reg[[1]][[1]]$fac), FUN = prod )

colnames(tmp1)              <- c("reg", "s_d", "inv", "fac", "monty_e"               )
colnames(tmp2)              <- c("reg", "s_d", "inv", "fac", "movar_e", "movar_e_agg")

sda_reg[[1]][[1]]           <- left_join( tmp1, tmp2, by = c("reg", "s_d", "inv", "fac")); rm(tmp1, tmp2)

# Footprint-based energy: 
tmp1                        <- aggregate( sda_reg[[1]][[2]]$monty_e                       , by = list(sda_reg[[1]][[2]]$reg_ja, sda_reg[[1]][[2]]$reg_ia, sda_reg[[1]][[2]]$inv, sda_reg[[1]][[2]]$fac), FUN = sum  )
tmp2                        <- aggregate( sda_reg[[1]][[2]][, c("movar_e", "movar_e_agg")], by = list(sda_reg[[1]][[2]]$reg_ja, sda_reg[[1]][[2]]$reg_ia, sda_reg[[1]][[2]]$inv, sda_reg[[1]][[2]]$fac), FUN = prod )

colnames(tmp1)              <- c("reg", "s_d", "inv", "fac", "monty_e"               )
colnames(tmp2)              <- c("reg", "s_d", "inv", "fac", "movar_e", "movar_e_agg")

sda_reg[[1]][[2]]           <- left_join( tmp1, tmp2, by = c("reg", "s_d", "inv", "fac")); rm(tmp1, tmp2)

# Production-based intensity: 
sda_reg[[2]][[1]]           <- aggregate( sda_reg[[2]][[1]][, c("movar_i", "movar_i_agg")], by = list(sda_reg[[2]][[1]]$reg_ia, sda_reg[[2]][[1]]$reg_ja, sda_reg[[2]][[1]]$inv, sda_reg[[2]][[1]]$fac), FUN = prod )
colnames(sda_reg[[2]][[1]]) <- c("reg", "s_d", "inv", "fac", "movar_i", "movar_i_agg")

# Footprint-based intensity: 
sda_reg[[2]][[2]]           <- aggregate( sda_reg[[2]][[2]][, c("movar_i", "movar_i_agg")], by = list(sda_reg[[2]][[2]]$reg_ja, sda_reg[[2]][[2]]$reg_ia, sda_reg[[2]][[2]]$inv, sda_reg[[2]][[2]]$fac), FUN = prod )
colnames(sda_reg[[2]][[2]]) <- c("reg", "s_d", "inv", "fac", "movar_i", "movar_i_agg")


# 5. Marry production and consumption and keep only the important regions:      
###################################################################################

# Merry marrying and kicking: 
for( i in 1:length(sda_cnt) ) { # BEGIN FOR. 
  
  # Merry marrying: 
  sda_cnt[[i]] <- rbindlist( sda_cnt[[i]] )
  sda_reg[[i]] <- rbindlist( sda_reg[[i]] )
  
  # Merry kicking: 
  sda_cnt[[i]] <- sda_cnt[[i]][sda_cnt[[i]]$reg %in% tmp_eu$reg, ]
  sda_reg[[i]] <- sda_reg[[i]][sda_reg[[i]]$reg %in% i_reg     , ]
  
  # Re-name the inventories: 
  sda_cnt[[i]]$inv[sda_cnt[[i]]$inv == "Production"      ] <- "Prod."
  sda_cnt[[i]]$inv[sda_cnt[[i]]$inv == "Fin. Production" ] <- "Fin. Prod."
  sda_cnt[[i]]$inv[sda_cnt[[i]]$inv == "Fin. Consumption"] <- "Fin. Cons."
  
  sda_reg[[i]]$inv[sda_reg[[i]]$inv == "Production"      ] <- "Prod."
  sda_reg[[i]]$inv[sda_reg[[i]]$inv == "Fin. Production" ] <- "Fin. Prod."
  sda_reg[[i]]$inv[sda_reg[[i]]$inv == "Fin. Consumption"] <- "Fin. Cons."
  
  # Rename the regions itself: 
  sda_cnt[[i]]$reg                              <- toupper( sda_cnt[[i]]$reg )
  
  sda_reg[[i]]$reg[sda_reg[[i]]$reg == "eu"   ] <- "EU 28"
  sda_reg[[i]]$reg[sda_reg[[i]]$reg == "usa"  ] <- "USA"
  sda_reg[[i]]$reg[sda_reg[[i]]$reg == "jpn"  ] <- "Japan"
  # sda_reg[[i]]$reg[sda_reg[[i]]$reg == "che"  ] <- "Switzerland"
  # sda_reg[[i]]$reg[sda_reg[[i]]$reg == "xef"  ] <- efta
  sda_reg[[i]]$reg[sda_reg[[i]]$reg == "efta" ] <- "EFTA"
  sda_reg[[i]]$reg[sda_reg[[i]]$reg == "roecd"] <- "R.o. OECD"
  sda_reg[[i]]$reg[sda_reg[[i]]$reg == "chn"  ] <- "China"
  
  # Rename the Income Groups: 
  sda_cnt[[i]]$s_d[sda_cnt[[i]]$s_d == "dom"  ] <- "Domestic"
  sda_cnt[[i]]$s_d[sda_cnt[[i]]$s_d == "H"    ] <- "High Income"
  sda_cnt[[i]]$s_d[sda_cnt[[i]]$s_d == "UM"   ] <- "Upper Middle"
  sda_cnt[[i]]$s_d[sda_cnt[[i]]$s_d == "LM"   ] <- "Lower Middle"
  sda_cnt[[i]]$s_d[sda_cnt[[i]]$s_d == "L"    ] <- "Low Income"
  
  sda_reg[[i]]$s_d[sda_reg[[i]]$s_d == "dom"  ] <- "Domestic"
  sda_reg[[i]]$s_d[sda_reg[[i]]$s_d == "H"    ] <- "High Income"
  sda_reg[[i]]$s_d[sda_reg[[i]]$s_d == "UM"   ] <- "Upper Middle"
  sda_reg[[i]]$s_d[sda_reg[[i]]$s_d == "LM"   ] <- "Lower Middle"
  sda_reg[[i]]$s_d[sda_reg[[i]]$s_d == "L"    ] <- "Low Income"
  
} # END FOR. 


# 6. Add totals to the SDA data: 
###################################################################################

# Add total changes for the energy SDAs to the country data: 
tmp1           <- aggregate( sda_cnt[[1]]$monty_e                       , by = list(sda_cnt[[1]]$reg, sda_cnt[[1]]$s_d, sda_cnt[[1]]$inv), FUN = sum  )
tmp2           <- aggregate( sda_cnt[[1]][, c("movar_e", "movar_e_agg")], by = list(sda_cnt[[1]]$reg, sda_cnt[[1]]$s_d, sda_cnt[[1]]$inv), FUN = prod )

colnames(tmp1) <- c("reg", "s_d", "inv", "monty_e"               )
colnames(tmp2) <- c("reg", "s_d", "inv", "movar_e", "movar_e_agg")

tmp            <- left_join( tmp1, tmp2, by = c("reg", "s_d", "inv") ); rm(tmp1, tmp2)
tmp$fac        <- "Total"

sda_cnt[[1]]   <- rbind( sda_cnt[[1]], tmp[, colnames(sda_cnt[[1]])] ); rm(tmp)

# Add total changes for the intensity SDAs to the country data: 
tmp            <- aggregate( sda_cnt[[2]][, c("movar_i", "movar_i_agg")], by = list(sda_cnt[[2]]$reg, sda_cnt[[2]]$s_d, sda_cnt[[2]]$inv), FUN = prod )
colnames(tmp)  <- c("reg", "s_d", "inv", "movar_i", "movar_i_agg")
tmp$fac        <- "Total"
sda_cnt[[2]]   <- rbind( sda_cnt[[2]], tmp[, colnames(sda_cnt[[2]])] ); rm(tmp)

# Add total changes for the energy SDAs to the region data: 
tmp1           <- aggregate( sda_reg[[1]]$monty_e                       , by = list(sda_reg[[1]]$reg, sda_reg[[1]]$s_d, sda_reg[[1]]$inv), FUN = sum  )
tmp2           <- aggregate( sda_reg[[1]][, c("movar_e", "movar_e_agg")], by = list(sda_reg[[1]]$reg, sda_reg[[1]]$s_d, sda_reg[[1]]$inv), FUN = prod )

colnames(tmp1) <- c("reg", "s_d", "inv", "monty_e"               )
colnames(tmp2) <- c("reg", "s_d", "inv", "movar_e", "movar_e_agg")

tmp            <- left_join( tmp1, tmp2, by = c("reg", "s_d", "inv") ); rm(tmp1, tmp2)
tmp$fac        <- "Total"

sda_reg[[1]]   <- rbind( sda_reg[[1]], tmp[, colnames(sda_reg[[1]])] ); rm(tmp)

# Add total changes for the intensity SDAs to the region data: 
tmp            <- aggregate( sda_reg[[2]][, c("movar_i", "movar_i_agg")], by = list(sda_reg[[2]]$reg, sda_reg[[2]]$s_d, sda_reg[[2]]$inv), FUN = prod )
colnames(tmp)  <- c("reg", "s_d", "inv", "movar_i", "movar_i_agg")
tmp$fac        <- "Total"
sda_reg[[2]]   <- rbind( sda_reg[[2]], tmp[, colnames(sda_reg[[2]])] ); rm(tmp)


# 8. Make factors our of regions, inventories, and sourses:  
###################################################################################

# Define the order of the factors (except the single countries):
tmp1 <- c("Prod.", "Fin. Prod.", "Fin. Cons.")
tmp2 <- c("EU 28", "USA", "Japan", "EFTA", "R.o. OECD", "China") 
tmp3 <- c("Domestic", "High Income", "Upper Middle", "Lower Middle", "Low Income")

for( i in 1:length(sda_cnt) ) { # BEGIN FOR. 
  
  # Regions as factors: 
  sda_cnt[[i]]  <- transform( sda_cnt[[i]], reg = factor(reg, levels = c(unique(sda_cnt[[i]]$reg))) )
  sda_reg[[i]]  <- transform( sda_reg[[i]], reg = factor(reg, levels = c(tmp2))                     )
  
  # Source/destinations as factors: 
  sda_cnt[[i]]  <- transform( sda_cnt[[i]], s_d = factor(s_d, levels = c(tmp3))                     )
  sda_reg[[i]]  <- transform( sda_reg[[i]], s_d = factor(s_d, levels = c(tmp3))                     )
  
  # Inventories as factors: 
  sda_cnt[[i]]  <- transform( sda_cnt[[i]], inv = factor(inv, levels = c(tmp1))                     )
  sda_reg[[i]]  <- transform( sda_reg[[i]], inv = factor(inv, levels = c(tmp1))                     )  
  
  # Nicer name for the components: 
  sda_cnt[[i]]$fac[sda_cnt[[i]]$fac == "tot"  ] <- "Total"
  sda_cnt[[i]]$fac[sda_cnt[[i]]$fac == "int"  ] <- "int"
  sda_cnt[[i]]$fac[sda_cnt[[i]]$fac == "vai"  ] <- "vint"
  sda_cnt[[i]]$fac[sda_cnt[[i]]$fac == "tch"  ] <- "sup"
  sda_cnt[[i]]$fac[sda_cnt[[i]]$fac == "str_e"] <- "mix"
  sda_cnt[[i]]$fac[sda_cnt[[i]]$fac == "str_s"] <- "str"
  sda_cnt[[i]]$fac[sda_cnt[[i]]$fac == "str_c"] <- "trd"
  sda_cnt[[i]]$fac[sda_cnt[[i]]$fac == "vol"  ] <- "act"
  sda_cnt[[i]]$fac[sda_cnt[[i]]$fac == "hh"   ] <- "ehh"
  
  sda_reg[[i]]$fac[sda_reg[[i]]$fac == "tot"  ] <- "Total"
  sda_reg[[i]]$fac[sda_reg[[i]]$fac == "int"  ] <- "int"
  sda_reg[[i]]$fac[sda_reg[[i]]$fac == "vai"  ] <- "vint"
  sda_reg[[i]]$fac[sda_reg[[i]]$fac == "tch"  ] <- "sup"
  sda_reg[[i]]$fac[sda_reg[[i]]$fac == "str_e"] <- "mix"
  sda_reg[[i]]$fac[sda_reg[[i]]$fac == "str_s"] <- "str"
  sda_reg[[i]]$fac[sda_reg[[i]]$fac == "str_c"] <- "trd"
  sda_reg[[i]]$fac[sda_reg[[i]]$fac == "vol"  ] <- "act"
  sda_reg[[i]]$fac[sda_reg[[i]]$fac == "hh"   ] <- "ehh"
  
} # END FOR. 

# Clean-up: 
rm(tmp1, tmp2, tmp3, i, j)


