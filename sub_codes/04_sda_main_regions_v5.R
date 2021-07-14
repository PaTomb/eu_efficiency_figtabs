  
  
  #######################################################################################################################
  #                                                                                                                     # 
  # Name:     main_regions                                                                                              # 
  # Author:   Patrick Tomberger                                                                                         # 
  # Date:     June 29th, 2020                                                                                           # 
  # Notes:                                                                                                              # 
  #                                                                                                                     # 
  # Version history:                                                                                                    # 
  #                                                                                                                     # 
  # v1:         + Bar-plots for the decomposition of the main regions: EU, US, JPN, ROECD, CHN, CHE, EFTA.              #
  #             + Three sub-plots for each inventory, the same inventory is compared for all regions in one plot.       # 
  #                                                                                                                     # 
  # v2:         + Adapted for the new format of the raw data.                                                           #
  #                                                                                                                     # 
  # v3:         + Adapted to use the already prepared raw data.                                                         #
  #             + Create a support plot that decomposes factors accoring to source (domestic and by WB IG)              # 
  #                                                                                                                     # 
  # v4:         + Stores data on average percentage changes of energy intensity and only for sector efficiency.         #
  #                                                                                                                     # 
  # v5:         + Uses the already present HH data in the raw data file.                                                #
  #                                                                                                                     # 
  #######################################################################################################################
  

###################################################################################
# I. SDA for the regions of the descriptives section:                             #
###################################################################################
  
  
# 1. Prepare the raw data we will use for the plot: 
###################################################################################
  
# Separate the data for the plot from the raw data: 
plot_reg                <- sda_reg
plot_sup                <- sda_reg

# Aggregate over the source/desination regions of the energy data: 
tmp1                    <- aggregate( plot_reg[[1]]$monty_e    , by = list(plot_reg[[1]]$reg, plot_reg[[1]]$inv, plot_reg[[1]]$fac), FUN = sum  ) 
tmp2                    <- aggregate( plot_reg[[1]]$movar_e_agg, by = list(plot_reg[[1]]$reg, plot_reg[[1]]$inv, plot_reg[[1]]$fac), FUN = prod ) 
 
colnames(tmp1)          <- c("reg", "inv", "fac", "monty_e")
colnames(tmp2)          <- c("reg", "inv", "fac", "movar_e")

plot_reg[[1]]           <- left_join( tmp1, tmp2, by = c("reg", "inv", "fac") ); rm(tmp1, tmp2)

# Aggregate over source/destination regions of the intensity data: 
plot_reg[[2]]           <- aggregate( plot_reg[[2]]$movar_i_agg, by = list(plot_reg[[2]]$reg, plot_reg[[2]]$inv, plot_reg[[2]]$fac), FUN = prod )
colnames(plot_reg[[2]]) <- c("reg", "inv", "fac", "movar_i")

# Subtract one from the Monty-Vartia ratios and change to percentages: 
plot_reg[[1]]$movar_e     <- ( plot_reg[[1]]$movar_e     - 1 ) * 100
plot_reg[[2]]$movar_i     <- ( plot_reg[[2]]$movar_i     - 1 ) * 100
plot_sup[[1]]$movar_e_agg <- ( plot_sup[[1]]$movar_e_agg - 1 ) * 100
plot_sup[[2]]$movar_i_agg <- ( plot_sup[[2]]$movar_i_agg - 1 ) * 100


# Normalze contribution of sources/destinations to the overall absolute change of each factor: 
tmp                        <- aggregate( plot_sup[[1]]$monty_e, by = list(plot_sup[[1]]$reg, plot_sup[[1]]$inv, plot_sup[[1]]$fac), FUN = sum )
colnames(tmp)              <- c("reg", "inv", "fac", "monty_scaled")
plot_sup[[1]]              <- left_join( plot_sup[[1]], tmp, by = c("reg", "inv", "fac") ); rm(tmp)
plot_sup[[1]]$monty_scaled <- plot_sup[[1]]$monty_e / plot_sup[[1]]$monty_scaled * 100

# Separate total energy/intensity growth: 
plot_reg_t <- plot_reg

for( i in 1:length(plot_reg) ) { # BEGIN FOR. 
  
  plot_reg_t[[i]]       <- plot_reg_t[[i]][plot_reg_t[[i]]$fac == "Total", ]
  plot_reg  [[i]]       <- plot_reg  [[i]][plot_reg  [[i]]$fac != "Total", ]
  plot_sup  [[i]]       <- plot_sup  [[i]][plot_sup  [[i]]$fac != "Total", ]
  
} # END FOR. 


# 2. Create the plot for factor changes by region:                                 
###################################################################################

# Reorder the factors for the legend: 
# tmp1 <- c("act", "int", "sup", "str", "trd", "mix", "ehh")

# for( i in 1:length(sda_cnt) ) { # BEGIN FOR. 
  
  # Factor as factors, haha: 
#   plot_reg[[i]]  <- transform( plot_reg[[i]], fac = factor(fac, levels = tmp1) )
  
# }

# Define a color vector: 
cols                    <- c(`int`="gray40", `sup` = "bisque4", `str` = "white", `mix` = "black", `ehh` = "brown4", 
                             `trd` = "burlywood", `act` = "lightgrey", `vint` = "indianred4", prod = "black", fprod = "black", cons = "black") 

# The function for the plot: 
make_plot <- function(x, y, z) {
  
  # x ... changes of the factors
  # y ... changes of total energy usage / intensity
  # z ... data that is plotted ("egy_m", "egy_mv", "int_mv")
  
  # x <- plot_reg[[1]]
  # y <- plot_reg_t[[1]]
  # z <- "egy_m"
  # ticks           <- seq(-3000, 3000, 500)
  
  if( z == "egy_m"  ) { # BEGIN IF. 
    
    x$y <- x$monty_e
    y$y <- y$monty_e 
    lab <- paste( "Change in energy usage (Mtoe)", d_yr[1], "--", d_yr[2], sep = " " )
    
  } # END IF. 
  
  if( z == "egy_mv" ) { # BEGIN IF. 
    
    x$y <- x$movar_e
    y$y <- y$movar_e
    lab <- paste( "Change in energy usage (percent)", d_yr[1], "--", d_yr[2], sep = " " )
    
  } # END IF. 
  
  if( z == "int_mv" ) { # BEGIN IF. 
    
    x$y <- x$movar_i
    y$y <- y$movar_i
    lab <- paste( "Change in energy intensity (percent)", d_yr[1], "--", d_yr[2], sep = " " )
    
  } # END IF. 
  
  # Make the plot: 
  x <- ggplot() + 
    
    # Draw the raw plots for total energy change and the decomposed changes: 
    geom_bar  ( data = x, aes( x = inv, y = y, fill = fac), stat = "identity", position = "stack", colour =" black") + facet_grid(~ reg) + 
    geom_point( data = y, aes( x = inv, y = y, fill = inv), size = 3) + facet_grid(~ reg) +
    # geom_point( data = x, aes( x = inv, y = y, fill = fac), stat = "identity", position = "stack", colour =" black") + facet_grid(~ reg) +
    
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
    theme( axis.text.x = element_text(colour = 'black', size = 12) ) +
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


# 3. Create the plot for contribution of surces/destinations to factor changes:                                  
###################################################################################

# Define a color vector: 
col2                    <- c(`Domestic`="gray80", `High Income` = "gray60", `Upper Middle` = "gray40", `Lower Middle` = "gray20", `Low Income` = "white") 

# The function for the plot: 
supp_plot <- function(x, y) {
  
  # x ... contribution to the factors
  # z ... data that is plotted ("egy_m", "egy_mv", "int_mv")
  
  # x <- sda_reg[[1]]
  # z <- "egy_m"
  # ticks           <- seq(-3000, 3000, 500)
  
  if( y == "egy_m"  ) { # BEGIN IF. 
    
    x$y <- x$monty_scaled
    lab <- paste( "Change in energy usage (Mtoe)", d_yr[1], "--", d_yr[2], sep = " " )
    
  } # END IF. 
  
  if( y == "egy_mv" ) { # BEGIN IF. 
    
    x$y <- x$movar_e_agg
    lab <- paste( "Change in energy usage (percent)", d_yr[1], "--", d_yr[2], sep = " " )
    
  } # END IF. 
  
  if( y == "int_mv" ) { # BEGIN IF. 
    
    x$y <- x$movar_i_agg
    lab <- paste( "Change in energy intensity (percent)", d_yr[1], "--", d_yr[2], sep = " " )
    
  } # END IF. 
  
  # Make the plot: 
  x <- ggplot() + 
    
    # Draw the raw plots for total energy change and the decomposed changes: 
    geom_bar  ( data = x, aes( x = inv, y = y, fill = s_d), stat = "identity", position = "stack", colour =" black") + 
    facet_grid(fac ~ reg) +
    
    # Define the Colors and the background: 
    scale_fill_manual( values = col2 ) + 
    theme( panel.background = element_rect(fill = 'white', colour = 'white')) + 
    theme( panel.grid.major = element_line(colour = 'black', size = 0.3, linetype = "dashed")) + 
    theme( panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank() ) + 
    scale_y_continuous( breaks = ticks ) + 
    theme( strip.background = element_rect(colour = "white", fill = "white")) + 
    
    # Determine the labels: 
    ylab( lab ) +
    # ylab( expression( paste( Delta, " Energy Usage (ktoe)" )) )  +
    xlab( "" )  +
    theme( axis.text.x = element_text(colour = 'black', size = 12) ) +
    theme( axis.text.y = element_text(colour = 'black', size = 11) ) +
    theme( plot.title = element_text(size = 18, margin = margin(t = 10, b = -30)) ) +
    theme( axis.title.y = element_text(size = 15) ) + 
    
    # Labels for the facet_grid: 
    theme( strip.text.x = element_text(size = 16) ) + 
    theme( strip.text.y = element_text(size = 14) ) + 
    
    # Decide whether legend should be shown and how it is formatted: 
    theme( legend.position = "none" )  +                                                          # Legend Position
    guides( fill=guide_legend(nrow = 1, byrow = T) ) + 
    theme( legend.key.width = unit(1.2, "cm") ) + 
    labs( paste( "European Union", d_yr[1], "--", d_yr[2], sep=" ") ) +                           # Legend Title
    theme( legend.text = element_text(size = 15) ) +                                              # Legend Font
    
    # Sold line for the zero entry:   
    geom_hline( yintercept = 0, size = 0.3 ) 
  
} # END FUNCTION. 


# 4. Average yearly changes of sector and overall energy intesity:                                   
###################################################################################

# Isolate the raw data: 
av_sec              <- plot_reg[[2]][plot_reg[[2]]$fac == "int", ]
av_tot              <- plot_reg_t[[2]]

# Calculate average yearly changes: 
av_sec$movar_i      <- round( av_sec$movar_i / ( d_yr[2] - d_yr[1] ), digits = 2 ) 
av_tot$movar_i      <- round( av_tot$movar_i / ( d_yr[2] - d_yr[1] ), digits = 2 )

# Relabel the percentage changes: 
colnames(av_sec)[4] <- paste( "ds_", substr(d_yr[1], 3, 4), "_", substr(d_yr[2], 3, 4), sep = "" )
colnames(av_tot)[4] <- paste( "dt_", substr(d_yr[1], 3, 4), "_", substr(d_yr[2], 3, 4), sep = "" )

# Merge together: 
av_sec$fac          <- NULL
av_tot$fac          <- NULL
av_cng              <- left_join( av_tot, av_sec, by = c("reg", "inv") ); rm(av_tot, av_sec)

# Add energy efficiency for total regions at the beginning of the period: 
tmp_pr              <- cbind( egy_inv[, c("reg", "yr")], rowSums(egy_inv[, paste('p' , com, sep = "_")]) ); colnames(tmp_pr)[3] <- "egy"  
tmp_fp              <- cbind( egy_inv[, c("reg", "yr")], rowSums(egy_inv[, paste('fp', com, sep = "_")]) ); colnames(tmp_fp)[3] <- "egy"  
tmp_cn              <- cbind( egy_inv[, c("reg", "yr")], rowSums(egy_inv[, paste('c' , com, sep = "_")]) ); colnames(tmp_cn)[3] <- "egy"  

tmp_pr$inv          <- "Prod."
tmp_fp$inv          <- "Fin. Prod."
tmp_cn$inv          <- "Fin. Cons."

tmp_e               <- rbind( tmp_pr, tmp_fp, tmp_cn ); rm(tmp_pr, tmp_fp, tmp_cn)
tmp_e               <- tmp_e[, c("reg", "yr", "inv", "egy")][tmp_e$yr %in% d_yr, ]

# Prepare the value added data:
tmp_v                                     <- melt( va_inv, c('reg', "yr", 'unit') )
tmp_v$unit                                <- NULL
tmp_v$variable                            <- as.character( tmp_v$variable )

tmp_v$variable[tmp_v$variable == "va_pr"] <- "Prod."
tmp_v$variable[tmp_v$variable == "va_fp"] <- "Fin. Prod."
tmp_v$variable[tmp_v$variable == "va_cn"] <- "Fin. Cons."

colnames(tmp_v)[3:4]                      <- c("inv", "va") 

# Merge with the energy data: 
tmp_eff                                   <- left_join( tmp_e, tmp_v, by = c("reg", "yr", "inv")  ); rm(tmp_e, tmp_v)

# Keep only the important regions: 
tmp                                       <- c("usa", "jpn", "chn", efta, roecd, tmp_eu$reg)
tmp_eff                                   <- tmp_eff[tmp_eff$reg %in% tmp, ]; rm(tmp)

# Aggregate EU, EFTA, and ROECD: 
tmp_eff$reg[tmp_eff$reg %in% roecd]       <- "R.o. OECD"
tmp_eff$reg[tmp_eff$reg %in% efta ]       <- "EFTA"
tmp_eff$reg[tmp_eff$reg %in% tmp_eu$reg ] <- "EU 28"

tmp_eff                                   <- aggregate( tmp_eff[, c("egy", "va")], by = list(tmp_eff$reg, tmp_eff$yr, tmp_eff$inv), FUN = sum )
colnames(tmp_eff)[1:3]                    <- c("reg", "yr", "inv")

# Rename the remaining regions: 
tmp_eff$reg[tmp_eff$reg ==  "usa"]        <- "USA"
tmp_eff$reg[tmp_eff$reg ==  "jpn"]        <- "Japan"
tmp_eff$reg[tmp_eff$reg ==  "chn"]        <- "China"

# Calculate economy-wide energy efficiency: 
tmp_eff$eff                               <- tmp_eff$egy / tmp_eff$va

# Bring efficiency data to final order: 
tmp                                       <- tmp_eff[, c("reg", "yr", "inv", "eff")][tmp_eff$yr == d_yr[1], ]
colnames(tmp)[4]                          <- paste( 'eff', substr(d_yr[1], 3, 4), sep = "_")
tmp$yr                                    <- d_yr[2]
tmp_eff                                   <- left_join( tmp, tmp_eff[, c("reg", "yr", "inv", "eff")], by = c("reg", "yr", "inv") )
colnames(tmp_eff)[5]                      <- paste( 'eff', substr(d_yr[2], 3, 4), sep = "_"); rm(tmp)

# Round the efficiency data: 
tmp_eff[, 4:5]                            <- round( tmp_eff[, 4:5], digits = 2 )           

# Bring to average changes to final order: 
av_cng$reg          <- as.character( av_cng$reg )
av_cng$inv          <- as.character( av_cng$inv )
order               <- as.data.frame( cbind(unique(av_cng$reg)) )

order               <- as.data.frame( cbind(order[rep(seq_len(nrow(order)), times = 1, each = length(unique(av_cng$inv))), ]) )
colnames(order)     <- "reg"
order$inv           <- rep( unique(av_cng$inv), times = length(unique(order$reg)), each = 1 )

av_cng              <- left_join( order, av_cng, by = c("reg", "inv") ); rm(order)

# Match with efficiency data and rename OECD: 
av_cng$reg[av_cng$reg == "R.o. OECD"] <- "R.o. OECD"
av_cng                                   <- left_join( av_cng, tmp_eff, by = c("reg", "inv") ); rm(tmp_eff)











