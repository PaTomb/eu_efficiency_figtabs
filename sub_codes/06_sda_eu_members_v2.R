  
  
  #######################################################################################################################
  #                                                                                                                     # 
  # Name:     sda_eu_members                                                                                            # 
  # Author:   Patrick Tomberger                                                                                         # 
  # Date:     July 23rd, 2020                                                                                           # 
  # Notes:                                                                                                              # 
  #                                                                                                                     # 
  # Version history:                                                                                                    # 
  #                                                                                                                     # 
  # v1:         + SDA plots for the EU-members.                                                                         # 
  #                                                                                                                     # 
  # v2:         + Adapts code for the changes made in the raw data.                                                     # 
  #             + Expresses changes in intensity in percentages until we find a better solution.                        # 
  #                                                                                                                     # 
  #######################################################################################################################
  
  
###################################################################################
# IV. SDA olots for the EU member states:                                         #
###################################################################################

# 1. Prepare the raw data we will use for the plot: 
###################################################################################

# Aggregate over all destination countries (prod) or source countries (fin. prod and cons):
tmp_reg                <- list()  

# Production: 
tmp_reg[[1]]           <- sda_cnt[[1]][sda_cnt[[1]]$inv == "Prod.", ]
tmp_reg[[2]]           <- sda_cnt[[1]][sda_cnt[[1]]$inv == "Prod.", ]
tmp_reg[[3]]           <- sda_cnt[[2]][sda_cnt[[2]]$inv == "Prod.", ]
tmp_reg[[4]]           <- sda_cnt[[2]][sda_cnt[[2]]$inv == "Prod.", ]

tmp_reg[[1]]           <- aggregate( tmp_reg[[1]]$monty_e    , by = list(tmp_reg[[1]]$reg, tmp_reg[[1]]$inv, tmp_reg[[1]]$fac), FUN = sum  ) 
tmp_reg[[2]]           <- aggregate( tmp_reg[[2]]$movar_e    , by = list(tmp_reg[[2]]$reg, tmp_reg[[2]]$inv, tmp_reg[[2]]$fac), FUN = prod ) 
tmp_reg[[3]]           <- aggregate( tmp_reg[[3]]$movar_i_agg, by = list(tmp_reg[[3]]$reg, tmp_reg[[3]]$inv, tmp_reg[[3]]$fac), FUN = prod ) 
tmp_reg[[4]]           <- aggregate( tmp_reg[[4]]$movar_i    , by = list(tmp_reg[[4]]$reg, tmp_reg[[4]]$inv, tmp_reg[[4]]$fac), FUN = prod ) 

colnames(tmp_reg[[1]]) <- c("reg", "inv", "fac", "value")
colnames(tmp_reg[[2]]) <- c("reg", "inv", "fac", "value")
colnames(tmp_reg[[3]]) <- c("reg", "inv", "fac", "value")
colnames(tmp_reg[[4]]) <- c("reg", "inv", "fac", "value")

# Final production and final consumption: 
tmp2_mo_e              <- sda_cnt[[1]][sda_cnt[[1]]$inv %in% c("Fin. Prod.", "Fin. Cons."), ]
tmp2_mv_e              <- sda_cnt[[1]][sda_cnt[[1]]$inv %in% c("Fin. Prod.", "Fin. Cons."), ]
tmp2_mv_ia             <- sda_cnt[[2]][sda_cnt[[2]]$inv %in% c("Fin. Prod.", "Fin. Cons."), ]
tmp2_mv_i              <- sda_cnt[[2]][sda_cnt[[2]]$inv %in% c("Fin. Prod.", "Fin. Cons."), ]

tmp2_mo_e              <- aggregate( tmp2_mo_e $monty_e    , by = list(tmp2_mo_e $reg, tmp2_mo_e $inv, tmp2_mo_e $fac), FUN = sum  ) 
tmp2_mv_e              <- aggregate( tmp2_mv_e $movar_e    , by = list(tmp2_mv_e $reg, tmp2_mv_e $inv, tmp2_mv_e $fac), FUN = prod ) 
tmp2_mv_ia             <- aggregate( tmp2_mv_ia$movar_i_agg, by = list(tmp2_mv_ia$reg, tmp2_mv_ia$inv, tmp2_mv_ia$fac), FUN = prod ) 
tmp2_mv_i              <- aggregate( tmp2_mv_i $movar_i    , by = list(tmp2_mv_i $reg, tmp2_mv_i $inv, tmp2_mv_i $fac), FUN = prod ) 

colnames(tmp2_mo_e )   <- c("reg", "inv", "fac", "value")
colnames(tmp2_mv_e )   <- c("reg", "inv", "fac", "value")
colnames(tmp2_mv_ia)   <- c("reg", "inv", "fac", "value")
colnames(tmp2_mv_i )   <- c("reg", "inv", "fac", "value")

tmp_reg[[1]]        <- rbind( tmp_reg[[1]], tmp2_mo_e  ); rm(tmp2_mo_e )
tmp_reg[[2]]        <- rbind( tmp_reg[[2]], tmp2_mv_e  ); rm(tmp2_mv_e )
tmp_reg[[3]]        <- rbind( tmp_reg[[3]], tmp2_mv_ia ); rm(tmp2_mv_ia)
tmp_reg[[4]]        <- rbind( tmp_reg[[4]], tmp2_mv_i  ); rm(tmp2_mv_i )

# Keep only the relevant regions and give them proper names: 
imp_reg             <- toupper( c("aut", "bel", "dnk", "prt", "esp", "ita", "mlt", "cyp", "irl", "gbr", "nld", "lux", "deu", "fra", "swe", "fin", "est", "lva", 
                                  "ltu", "pol", "cze", "hun", "svk", "svn", "hrv", "rou", "bgr", "grc") )


for( i in 1:length(tmp_reg) ) { # BEGIN FOR. 
  
  # Keep only the relevant regions: 
  tmp_reg[[i]]                                  <- tmp_reg[[i]][tmp_reg[[i]]$reg %in% imp_reg, ]
  
  # Make characters out of the regions: 
  tmp_reg[[i]]$reg                              <- as.character( tmp_reg[[i]]$reg )

  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "aut"] <- "Austria"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "bel"] <- "Belgium"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "dnk"] <- "Denmark"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "prt"] <- "Portugal"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "esp"] <- "Spain"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "ita"] <- "Italy"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "mlt"] <- "Malta"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "cyp"] <- "Cyprus"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "irl"] <- "Ireland"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "gbr"] <- "United Kingdom"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "nld"] <- "Netherlands"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "lux"] <- "Luxembourg"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "deu"] <- "Germany"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "fra"] <- "France"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "swe"] <- "Sweden"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "fin"] <- "Finland"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "est"] <- "Estonia"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "lva"] <- "Latvia"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "ltu"] <- "Lithuania"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "pol"] <- "Poland"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "cze"] <- "Czechia"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "hun"] <- "Hungary"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "svk"] <- "Slovakia"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "svn"] <- "Slovenia"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "hrv"] <- "Croatia"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "rou"] <- "Roumania"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "bgr"] <- "Bulgaria"
  # tmp_reg[[i]]$reg[tmp_reg[[i]]$reg == "grc"] <- "Greece"
  
} # END FOR.

# Add total energy changes / intensity ratios to the data: 
# tmp1              <- aggregate( tmp_reg[[1]]$value, by = list(tmp_reg[[1]]$reg, tmp_reg[[1]]$inv), FUN = sum  )
# tmp2              <- aggregate( tmp_reg[[2]]$value, by = list(tmp_reg[[2]]$reg, tmp_reg[[2]]$inv), FUN = prod )
# tmp3              <- aggregate( tmp_reg[[3]]$value, by = list(tmp_reg[[3]]$reg, tmp_reg[[3]]$inv), FUN = prod )
# tmp4              <- aggregate( tmp_reg[[4]]$value, by = list(tmp_reg[[4]]$reg, tmp_reg[[4]]$inv), FUN = prod )

# colnames(tmp1)    <- c("reg", "inv", "value")
# colnames(tmp2)    <- c("reg", "inv", "value")
# colnames(tmp3)    <- c("reg", "inv", "value")
# colnames(tmp4)    <- c("reg", "inv", "value")

# tmp1$fac          <- "tot"
# tmp2$fac          <- "tot"
# tmp3$fac          <- "tot"
# tmp4$fac          <- "tot"

# tmp_reg[[1]]      <- rbind( tmp_reg[[1]], tmp1 ); rm(tmp1)
# tmp_reg[[2]]      <- rbind( tmp_reg[[2]], tmp2 ); rm(tmp2)
# tmp_reg[[3]]      <- rbind( tmp_reg[[3]], tmp3 ); rm(tmp3)
# tmp_reg[[4]]      <- rbind( tmp_reg[[4]], tmp4 ); rm(tmp4)


# 2. Create the order of regions per inventory and split each SDA by inventory: 
###################################################################################

# Order according to changes in each sda and inventory:  
order             <- list()
order_sda1        <- list()
order_sda2        <- list()
order_sda3        <- list()
order_sda4        <- list()

tmp  <- c("Prod.", "Fin. Prod.", "Fin. Cons.")

for( i in 1:length(inv) ) { # Begin For. 

  # Grab the values for each inventory: 
  order_sda1[[i]] <- tmp_reg[[1]][tmp_reg[[1]]$fac == "Total" & tmp_reg[[1]]$inv == tmp[i] , ]
  order_sda2[[i]] <- tmp_reg[[2]][tmp_reg[[2]]$fac == "Total" & tmp_reg[[2]]$inv == tmp[i] , ]
  order_sda3[[i]] <- tmp_reg[[3]][tmp_reg[[3]]$fac == "Total" & tmp_reg[[3]]$inv == tmp[i] , ]
  order_sda4[[i]] <- tmp_reg[[4]][tmp_reg[[4]]$fac == "Total" & tmp_reg[[4]]$inv == tmp[i] , ]
  
  # Order the regions according to those values: 
  order_sda1[[i]] <- order_sda1[[i]]$reg[order(-order_sda1[[i]]$value)]
  order_sda2[[i]] <- order_sda2[[i]]$reg[order(-order_sda2[[i]]$value)]
  order_sda3[[i]] <- order_sda3[[i]]$reg[order(-order_sda3[[i]]$value)]
  order_sda4[[i]] <- order_sda4[[i]]$reg[order(-order_sda4[[i]]$value)]

} # END FOR. 

# All SDA orderings into one file: 
order[[1]]        <- order_sda1; rm(order_sda1)
order[[2]]        <- order_sda2; rm(order_sda2)
order[[3]]        <- order_sda3; rm(order_sda3)
order[[4]]        <- order_sda4; rm(order_sda4)


# Make factors out of the inventories: 
# for( i in 1:length(tmp_reg) ) { # BEGIN FOR. 
  
#   tmp_reg[[i]]    <- transform( tmp_reg[[i]], inv = factor(inv, level =  c("Prod.", "Fin. Prod.", "Fin. Cons.")) )
  
# } # END FOR. 

# Split data for each SDA according to inventory: 
tmp1 <- list()
tmp2 <- list()
tmp3 <- list()
tmp4 <- list()

for( i in 1:length(tmp_reg) ) { # BEGIN FOR. 

  # Each temporary file should have one list for each inventory: 
  tmp1[[i]] <- tmp_reg[[1]][tmp_reg[[1]]$inv == tmp[i], ]
  tmp2[[i]] <- tmp_reg[[2]][tmp_reg[[2]]$inv == tmp[i], ]
  tmp3[[i]] <- tmp_reg[[3]][tmp_reg[[3]]$inv == tmp[i], ]
  tmp4[[i]] <- tmp_reg[[4]][tmp_reg[[4]]$inv == tmp[i], ]
    
} # END FOR.  

# Clean-up: 
rm(tmp)

# Plug into the main file: 
tmp_reg[[1]] <- tmp1; rm(tmp1)
tmp_reg[[2]] <- tmp2; rm(tmp2)
tmp_reg[[3]] <- tmp3; rm(tmp3)
tmp_reg[[4]] <- tmp4; rm(tmp4)

# Keep a file for the table: 
table_eu <- tmp_reg[[4]][1:3]


# 3. Bring regions to factors: 
###################################################################################

for( i in 1:length(order) ) { # BEGIN FOR OVER SDA. 
  
  for( j in 1:length(order[[1]]) ) { # BEGIN FOR OVER INVENTORIES. 
    
    # Make factors and order them properly: 
    tmp_reg[[i]][[j]]     <- transform(tmp_reg[[i]][[j]], reg = factor(reg, levels = c(order[[i]][[j]])) )
    
    # Since we are at it, tive nicer names to the factors: 
    # tmp_reg[[i]][[j]]$fac[tmp_reg[[i]][[j]]$fac == "tot"  ] <- "Total"
    # tmp_reg[[i]][[j]]$fac[tmp_reg[[i]][[j]]$fac == "int"  ] <- "Energy Intensity"
    # tmp_reg[[i]][[j]]$fac[tmp_reg[[i]][[j]]$fac == "vai"  ] <- "VA Intensity"
    # tmp_reg[[i]][[j]]$fac[tmp_reg[[i]][[j]]$fac == "tch"  ] <- "Value Chains"
    # tmp_reg[[i]][[j]]$fac[tmp_reg[[i]][[j]]$fac == "str_e"] <- "Fuel Mix"
    # tmp_reg[[i]][[j]]$fac[tmp_reg[[i]][[j]]$fac == "str_s"] <- "Structure"
    # tmp_reg[[i]][[j]]$fac[tmp_reg[[i]][[j]]$fac == "str_c"] <- "Trade"
    # tmp_reg[[i]][[j]]$fac[tmp_reg[[i]][[j]]$fac == "vol"  ] <- "Activity"
    
  } # END FOR OVER INVENTORIES. 
  
} # END FOR OVER SDA. 


# 4. Bring ratio SDAs to a nicer form and calculate the totals: 
###################################################################################

# Subtract one from the Monty-Vartia ratios: 
for( i in 1:length(inv) ) { # BEGIN FOR. 
  
  tmp_reg[[2]][[i]]$value  <- ( tmp_reg[[2]][[i]]$value - 1 ) * 100
  tmp_reg[[3]][[i]]$value  <- ( tmp_reg[[3]][[i]]$value - 1 ) * 100
  tmp_reg[[4]][[i]]$value  <- ( tmp_reg[[4]][[i]]$value - 1 ) * 100
  
} # END FOR. 
  
# Separate total energy/intensity growth: 
plot_eum   <- tmp_reg
plot_eum_t <- tmp_reg

for( i in 1:length(tmp_reg) ) { # BEGIN FOR OVER SDA.  
  
  for( j in 1:length(inv) ) { # BEGIN FOR OVER INVENTORIES. 
    
    plot_eum_t[[i]][[j]] <- plot_eum_t[[i]][[j]][plot_eum_t[[i]][[j]]$fac == "Total", ]
    plot_eum  [[i]][[j]] <- plot_eum  [[i]][[j]][plot_eum  [[i]][[j]]$fac != "Total", ]
  
  } # END FOR OVER INVENTORIES. 
    
} # END FOR OVER SDA. 

# Clean-up: 
rm(tmp_reg)


# 5. Calculate total energy usage/intensity by inventory and add to totals:
###################################################################################

# Make countries small again: 
imp_reg <- tolower( imp_reg )

# Keep only the regions of interest and the first year of the period under consideration: 
egy_tot <- egy_inv[egy_inv$reg %in% imp_reg & egy_inv$yr == d_yr[1], ] 
va_tot  <- va_inv [ va_inv$reg %in% imp_reg &  va_inv$yr == d_yr[1], ] 

# Add the different energy commodities and bring to megatons:
egy_tot$pr_tot <- rowSums( egy_tot[, paste(  "p", com, sep = "_" )] ) / 1000
egy_tot$fp_tot <- rowSums( egy_tot[, paste( "fp", com, sep = "_" )] ) / 1000
egy_tot$cn_tot <- rowSums( egy_tot[, paste(  "c", com, sep = "_" )] ) / 1000
egy_tot        <- egy_tot[, c("reg", "yr", "pr_tot", "fp_tot", "cn_tot")]

# Merge with value added and calculate national intensities:
va_tot$unit                                <- NULL
eva_tot                                    <- left_join( egy_tot, va_tot, by = c("reg", "yr") ); rm(egy_tot, va_tot)
eva_tot[, c("pr_int", "fp_int", "cn_int")] <- eva_tot[, c("pr_tot", "fp_tot", "cn_tot")] / eva_tot[, c("va_pr", "va_fp", "va_cn")] * 1000

# Keep only the important values: 
tmp                                        <- c("pr_tot", "fp_tot", "cn_tot", "pr_int", "fp_int", "cn_int")
eva_tot                                    <- eva_tot[, c("reg", "yr", tmp)]

# Bring to long format: 
tmp                     <- colnames(eva_tot)[3:length(colnames(eva_tot))]
eva_tot                 <- reshape(  data      = eva_tot, 
                                     idvar     = c( "reg", "yr" ), 
                                     varying   = tmp, 
                                     times     = tmp, 
                                     v.name    = c( "value" ),
                                     direction = "long" ); rm(tmp)

# Prepare for matching: 
rownames(eva_tot)                <- NULL
eva_tot$inv                      <- substr( eva_tot$time, 1, 2 )
eva_tot$inv[eva_tot$inv == "pr"] <- "Prod."
eva_tot$inv[eva_tot$inv == "fp"] <- "Fin. Prod."
eva_tot$inv[eva_tot$inv == "cn"] <- "Fin. Cons."
eva_tot$val                      <- lstrings( eva_tot$time, 3 )

# Make factors out of the inventories: 
eva_tot$reg                      <- toupper  ( eva_tot$reg )
eva_tot                          <- transform( eva_tot, inv = factor(inv, levels = c("Prod.", "Fin. Prod.", "Fin. Cons.")) )

# Forget non-needed data: 
eva_tot                          <- eva_tot[, c("reg", "inv", "val", "value")]


# Bring to the same format as the order and plot data: 
eva1 <- list()
eva2 <- list()
eva3 <- list()
eva4 <- list()

tmp  <- c("Prod.", "Fin. Prod.", "Fin. Cons.")


for( i in 1:length(tmp) ) { # BEGIN FOR. 
  
  # For each SDA its inventory: 
  eva1[[i]] <- eva_tot[eva_tot$inv == tmp[i], ]; colnames(eva1[[i]])[4] <- "value_t"
  eva2[[i]] <- eva_tot[eva_tot$inv == tmp[i], ]; colnames(eva2[[i]])[4] <- "value_t"
  eva3[[i]] <- eva_tot[eva_tot$inv == tmp[i], ]; colnames(eva3[[i]])[4] <- "value_t"
  eva4[[i]] <- eva_tot[eva_tot$inv == tmp[i], ]; colnames(eva4[[i]])[4] <- "value_t"
  
} # END FOR. 

# Clean-up: 
rm(tmp)

# Plug all little Evas into the big Eva: 
eva      <- list()
eva[[1]] <- eva1; rm(eva1)
eva[[2]] <- eva2; rm(eva2)
eva[[3]] <- eva3; rm(eva3)
eva[[4]] <- eva4; rm(eva4)


# 6. Add the totals now to the plots for the aggregated SDA: 
###################################################################################

# First, make factors out of the regions and bring to final format: 
for( i in 1:length(eva) ) { # BEGIN FOR OVER SDA. 
  
  for( j in 1:length(inv) ) { # BEGIN FOR OVER INVENTORIES. 
    
    # Make factors and order them: 
    eva[[i]][[j]] <- transform( eva[[i]][[j]], reg = factor( reg, levels = c(order[[i]][[j]]) ) )
  
  } # END FOR OvER INVENTORIES. 
  
} # END FOR OVER SDA. 

# Match the Evas with the totals and calculate the variables of interest: 
for( i in 1:length(plot_eum_t) ) { # BEGIN FOR OVER SDA. 
  
  for( j in 1:length(inv) ) { # BEGIN FOR OVER INVENTORIES. 
  
    # For Production we have to match with total values and calculate percentage changes: 
    if( i == 1 ) { # BEGIN IF-ELSE. 
      
      # Match the data: 
      plot_eum_t[[i]][[j]]       <- left_join( plot_eum_t[[i]][[j]], eva[[i]][[j]][eva[[i]][[j]]$val == "tot", ], by = c("reg", "inv") )

      # Calculate percentage changes: 
      plot_eum_t[[i]][[j]]$d_pct <- round( (plot_eum_t[[i]][[j]]$value / plot_eum_t[[i]][[j]]$value_t) * 100, digits = 0 ) 
      plot_eum_t[[i]][[j]]$d_pct <- paste( plot_eum_t[[i]][[j]]$d_pct, "%", sep = " " )
      
    } else {
      
      # Match the data: 
      plot_eum_t[[i]][[j]]       <- left_join( plot_eum_t[[i]][[j]], eva[[i]][[j]][eva[[i]][[j]]$val == "int", ], by = c("reg", "inv") )
      
      # Keep the original percentage changes for the ratios: 
      plot_eum_t[[i]][[j]]$d_pct <- round( plot_eum_t[[i]][[j]]$value, digits = 1 ) 
        
    } # END IF-ELSE. 
    
    # Round whatever there is to round: 
    if( i < 2 ) { # BEGIN IF-ELSE: 
      
      plot_eum_t[[i]][[j]]$value_t <- round(  plot_eum_t[[i]][[j]]$value_t, digits = 0 )
      
    } else {
      
      plot_eum_t[[i]][[j]]$value_t <- round(  plot_eum_t[[i]][[j]]$value_t, digits = 2 )

    } # END IF-ELSE. 
    
  } # END FOR OVER INVENTORIES. 
  
} # END FOR OVER SDA. 


# 7. Calculate minima and maxima to make the plots more easy: 
###################################################################################

for( i in 1:length(plot_eum_t) ) { # BEGIN FOR OVER SDA. 
  
  for( j in 1:length(inv) ) { # BEGIN FOR OVER INVENTORY. 
    
    # Sum all positive values and match back to main file: 
    tmp1                      <- plot_eum[[i]][[j]][plot_eum[[i]][[j]]$value > 0, ]
    tmp1                      <- aggregate( tmp1$value, by = list(tmp1$reg), FUN = sum )
    colnames(tmp1)            <- c("reg", "max")
    tmp1$reg                  <- as.character( tmp1$reg )
    
    # Make factors and order them properly:  
    tmp1                      <- transform( tmp1, reg = factor( reg, levels = c(order[[i]][[j]]) ) )

    # Do the same for the minimum values: 
    tmp2                      <- plot_eum[[i]][[j]][plot_eum[[i]][[j]]$value < 0, ]
    tmp2                      <- aggregate( tmp2$value, by = list(tmp2$reg), FUN = sum )
    tmp2                      <- min( tmp2$x )

    # Match with the totals: 
    plot_eum_t[[i]][[j]]      <- left_join( plot_eum_t[[i]][[j]], tmp1, by = "reg" ); rm(tmp1)
    plot_eum_t[[i]][[j]]$min  <- tmp2; rm(tmp2)
    
    # If a country has no positives: set to zero: 
    plot_eum_t[[i]][[j]]$max[is.na(plot_eum_t[[i]][[j]]$max)] <- 0
    
  } # END FOR OVER INVENTORY. 
  
} # END FOR OVER SDA. 

#  Clean-up: 
rm(imp_reg)

# Define a color vector: 
cols              <- c(`int`="gray40", `sup` = "bisque4", `str` = "white", `mix` = "black", `ehh` = "brown4", `trd` = "burlywood",
                       `act` = "lightgrey", prod = "black", fprod = "black", cons = "black") 


# 2. Create a function to make the plots and make them:                                                            
###################################################################################

# The function for the plot: 
make_eum_plot <- function(x, y, z) {
  
  # x ... changes of the factors
  # y ... changes of total energy usage / intensity
  # z ... data that is plotted ("egy_m", "egy_mv", "int_mv")
  
  # x <- plot_eum[[3]][[1]] 
  # y <- plot_eum_t[[3]][[1]]
  # z <- "int_mv"
  # ticks     <- seq(-5, 10, 0.05)
  # ticks     <- seq(-150, 150, 25)
  
  if( z == "egy_m"  ) { # BEGIN IF. 
    
    x$y  <- x$value
    y$y  <- y$value 
    lab  <- paste( "Change in energy usage (Mtoe)", d_yr[1], "--", d_yr[2], sep = " " )

  } # END IF. 
  
  if( z == "egy_mv" ) { # BEGIN IF. 
    
    x$y <- x$value
    y$y <- y$value
    lab <- paste( "Change in energy usage (percent)", d_yr[1], "--", d_yr[2], sep = " " )
    
  } # END IF. 
  
  if( z == "int_mva" | z == "int_mv") { # BEGIN IF. 
    
    x$y <- x$value
    y$y <- y$value
    lab <- paste( "Change in energy intensity (percent)", d_yr[1], "--", d_yr[2], sep = " " )
    
  } # END IF. 
  
  # Make the plot: 
  x <- ggplot() + 
    
    # Draw the raw plots for total energy change and the decomposed changes: 
    geom_bar  ( data = x, aes( x = inv, y = y, fill = fac), stat = "identity", position = "stack", colour =" black") + 
    facet_grid(~ reg) + 
    geom_point( data = y, aes( x = inv, y = y, fill = inv), size = 3) + facet_grid(~ reg) +
    geom_text( data = y, aes(x = inv, y = min, label = value_t), size = 5, vjust = 1.5 ) + 
    geom_text( data = y, aes(x = inv, y = max, label = d_pct), size = 5, vjust = -0.3 ) + 
    
    # Define the Colors and the background: 
    scale_fill_manual( values = cols ) + 
    theme( panel.background = element_rect(fill = 'white', colour = 'white')) + 
    theme( panel.grid.major = element_line(colour = 'black', size = 0.3, linetype = "dashed")) + 
    theme( panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank() ) + 
    scale_y_continuous( breaks = ticks ) + 
    theme( strip.background = element_rect(colour = "white", fill = "white")) + 
    
    # Determine the labels: 
    ylab( lab ) +
    # ylab( expression( paste( Delta, " Energy Usage (ktoe)" )) )  +
    xlab( "" )  +
    theme( axis.text.x = element_blank(), axis.ticks = element_blank() ) + 
    # theme( axis.text.x = element_text(colour = 'black', size = 12) ) +
    theme( axis.text.y = element_text(colour = 'black', size = 12) ) +
    theme( plot.title = element_text(size = 18, margin = margin(t = 10, b = -30)) ) +
    theme( axis.title.y = element_text(size = 15) ) + 
    
    # Labels for the facet_grid: 
    theme( strip.text.x = element_text(size = 16) ) + 

    # Decide whether legend should be shown and how it is formatted: 
    theme( legend.position = "none" )  +                                                          # Legend Position
    guides( fill=guide_legend(nrow = 1, byrow = T) ) + 
    theme( legend.key.width = unit(1.2, "cm") ) + 
    labs( paste( "European Union", d_yr[1], "--", d_yr[2], sep=" ") ) +                           # Legend Title
    theme( legend.text = element_text(size = 15) ) +                                              # Legend Font
    
    # Sold line for the zero entry:   
    geom_hline( yintercept = 0, size = 0.3 ) 
  
} # END FUNCTION. 

