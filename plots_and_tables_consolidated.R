    
  
  #######################################################################################################################
  #                                                                                                                     # 
  # Name:     lmdi_decomposition                                                                                        # 
  # Author:   Patrick Tomberger                                                                                         # 
  # Date:     October 13th, 2020                                                                                        # 
  # Notes:                                                                                                              # 
  #                                                                                                                     # 
  # Version history:                                                                                                    # 
  #                                                                                                                     # 
  # v1:         + Creates all the used plots and tables from the SDA data.                                              # 
  #                                                                                                                     # 
  # v2:         + Takes HH energy usage directly from the raw data.                                                     # 
  #                                                                                                                     # 
  #######################################################################################################################
  

###################################################################################
# I. Set directory and load packages:                                             #
###################################################################################

# Set directory, load packages and define global options: 
setwd( '/home/patrick/Desktop/consolidated_plots_tables' )  # Insert correct path here!
options( stringsAsFactors = F ) 
library(dplyr         )
library(data.table    )
library(reshape2      )
library(ggplot2       )

# Make a list for all year combinations: 
d_yr_l      <- list()
d_yr_l[[1]] <- c(1997, 2014)
d_yr_l[[2]] <- c(1997, 2007)
d_yr_l[[3]] <- c(2007, 2014)
  
yr   <- c(1997, 2001, 2004, 2007, 2011, 2014)
n_yr <- length(yr)
com  <- c("fos", "nuc", "rsn", "hyd", "bfl", "wnd", "sol", "rsr")
ne   <- length(com) # Number of energy commodities
nf   <- 6           # NUmber of factors

# Load useful user-written functions: 
source( './input_files/glob_functions_v1.R' )

# Load the raw energy and value added inventories and the methane dataset: 
egy_inv                           <- read.csv( './input_files/reg_egy_inv.csv', header = T )
wb_data                           <- read.csv( './input_files/wb_data.csv'    , header = T )
wb_data$reg[wb_data$reg == "rom"] <- "rou"

# Do the tables/figures in a loop: 
for( k in 1:length(d_yr_l) ) { # BEGIN FOR. 
  
  # Determine desired years and energy type: 
  d_yr <- d_yr_l[[k]]

  if( d_yr[1] == 1997 ) { # BEGIN IF-ELSE. 
    
    va_inv                            <- read.csv( './input_files/va_inventories_97-14.csv' )
  
  } else {
    
    va_inv                            <- read.csv( './input_files/va_inventories_07-14.csv' )
    
  } # END IF-ELSE. 
  
  # Load the raw SDA data: 
  load( paste('./input_files/sda_data_', d_yr[1], '-', d_yr[2], '.RData', sep = "") )
  
  # Load matching files for EU and define the rest of OECD and EFTA regions: 
  roecd                               <- c("aus", "can", "chl", "mex", "nzl", "kor", "tur") # Note: Israel is part of XME in our data. 
  efta                                <- c("xef", "che")                                    # Some older versions did not need it. 
  tmp_eu                              <- read.csv( './input_files/eu.csv', header = T )
  
  
  ###################################################################################
  # I. Global plot for energy usage, all commodities:                               #
  ###################################################################################
  
  # Calculate the table: 
  source( './sub_codes/01_global_table_v1.R' )
  
  # Write to file: 
  if( k == 1 ) { 
    
    write.csv( glob_tab, './output_files/table_1.csv', row.names = F )
    
  }
    
  rm(glob_tab)
  
  
  ###################################################################################
  # II. Big table for the important regions:                                        #
  ###################################################################################
  
  # Calculate the table: 
  source( './sub_codes/02_big_table_v5.R' )
  
  # Write to file: 
  if( k == 1 ) { 
    
    write.csv( big_table, './output_files/table_2.csv', row.names = F )

  }
    
  rm(big_table, egy, i)
  
  
  ###################################################################################
  # III. SDA by source:                                                             #
  ###################################################################################
  
  source( './sub_codes/03_sda_by_source_v4.R' )
  
  
  ###################################################################################
  # IV. SDA plots for the regions of the descriptives section:                      #
  ###################################################################################
  
  source( './sub_codes/04_sda_main_regions_v5.R' )
  
  if( k == 1 ) {
    
    # Keep the relevant part of the table: 
    tab_avgr_int        <- av_cng
    tab_avgr_int$eff_14 <- NULL
    tab_avgr_int$yr     <- NULL
    
  }
  
  if( k == 2 ) {
    
    # Add what we need from the second iteration: 
    tab_avgr_int$dt_97_07 <- av_cng$dt_97_07
    tab_avgr_int$ds_97_07 <- av_cng$ds_97_07
    
  }
  
  if( k == 3 ) {
    
    # Add what we keep from the last iteration: 
    tab_avgr_int$dt_07_14 <- av_cng$dt_07_14
    tab_avgr_int$ds_07_14 <- av_cng$ds_07_14
    tab_avgr_int$eff_07   <- av_cng$eff_07
    
    # Bring to final order: 
    tab_avgr_int          <- tab_avgr_int[, c( "reg", "inv", "dt_97_14", "dt_97_07", "dt_07_14", "ds_97_14", "ds_97_07", "ds_07_14", "eff_97", "eff_07" )]
    
    # Write to file: 
    write.csv( tab_avgr_int, paste( './output_files/table_c5.csv', sep = ""), row.names = F ); rm(tab_avgr_int)
    
  }
  
  # 1. Create  and save t}}he SDA for the regions:                                                             
  ###################################################################################
  
  # Makethe plot for energy (Monty-Vartia):
  ticks             <- seq(-100, 500, 50)
  plot_reg_egy_mv   <- make_plot( plot_reg[[1]], plot_reg_t[[1]], "egy_mv" ); plot_reg_egy_mv
  
  # Makethe plot for intensity (Monty-Vartia):
  ticks           <- seq(-100, 250, 20)
  plot_reg_int_mv <- make_plot( plot_reg[[2]], plot_reg_t[[2]], "int_mv" ); plot_reg_int_mv
  
  # Write the plots to file: 
  if( d_yr[1] == 1997 & d_yr[2] == 2014 ) { # BEGIN IF. 
  
    ggsave( paste( './output_files/figure_1.jpg', sep = ""), plot = plot_reg_egy_mv, device = "jpeg", scale = 1, width = 420, height = 150, units = c("mm"), dpi = 300 )
    ggsave( paste( './output_files/figure_2.jpg', sep = ""), plot = plot_reg_int_mv, device = "jpeg", scale = 1, width = 420, height = 150, units = c("mm"), dpi = 300 )
    
  } # END IF. 
    
  if( k == 2 ) { # BEGIN IF. 
    
    ggsave( paste( './output_files/figure_3a.jpg', sep = ""), plot = plot_reg_int_mv, device = "jpeg", scale = 1, width = 420, height = 150, units = c("mm"), dpi = 300 )
    
  } # END IF. 
  
  if( k == 3 ) { # BEGIN IF. 
    
    ggsave( paste( './output_files/figure_3b.jpg', sep = ""), plot = plot_reg_int_mv, device = "jpeg", scale = 1, width = 420, height = 150, units = c("mm"), dpi = 300 )
    
  } # END IF. 
  
  # Clean-up: 
  rm(plot_reg_egy_mv, plot_reg_int_mv)
  
 
  ###################################################################################
  # V. Table for the Monty-Vartia ratios for the regions:                           #
  ###################################################################################
  
  source( './sub_codes/05_monty_vartia_table_v3.R' )
  
  # Write to file: 
  if( d_yr[1] == 1997 & d_yr[2] == 2014 ) { # BEGIN IF. 
    
    write.csv( reg_mv_e, paste('./output_files/table_c1.csv', sep = ""), row.names = F )
    write.csv( reg_mv_i, paste('./output_files/table_c2.csv', sep = ""), row.names = F )
    
  } # END IF.  
  
  if( k == 2 ) { # BEGIN IF. 
    
    write.csv( reg_mv_i, paste('./output_files/table_c3.csv', sep = ""), row.names = F )
    
  } # END IF.  
  
  if( k == 3 ) { # BEGIN IF. 
    
    write.csv( reg_mv_i, paste('./output_files/table_c4.csv', sep = ""), row.names = F )
    
  } # END IF.
  
  rm(reg_mv_e, reg_mv_i)
  
  
  ###################################################################################
  # VI. SDA plots for the EU member states:                                         #
  ###################################################################################
  
  # 1. Run the calculations: 
  ###################################################################################
  
  source( './sub_codes/06_sda_eu_members_v2.R' )
  
  
  # 2. Make nice Excel tables: 
  ###################################################################################
  
  # File with the factors: 
  tmp_l <- c("act", "int", "sup", "str", "trd", "mix", "ehh", "Total")
  
  # Bring to wide Format: 
  for( i in 1:length(table_eu) ) { # BEGIN FOR. 
    
    # Take a raw file: 
    tmp        <- table_eu[[i]][, c("reg", "inv")][table_eu[[i]]$fac == "act", ]
    
    # Note the period: 
    tmp$period <- paste( substr(d_yr[1], 3, 4), '-', substr(d_yr[2], 3, 4), sep = "" )
    
    # Now add all the factors: 
    for( j in 1:length(tmp_l) ) { # BEGIN FOR. 
      
      # Match with the factor: 
      tmp <- left_join( tmp, table_eu[[i]][, c("reg", "inv", "value")][table_eu[[i]]$fac == tmp_l[j], ], by = c("reg", "inv") )
      
      # Give nice column names: 
      colnames(tmp)[j+3] <- tmp_l[j]
      
    } # END FOR. 
    
    # Remove Malta and Cyprus: 
    tmp <- tmp[!tmp$reg %in% c("MLT"), ]
    
    # Plug into the main }}}file: 
    table_eu[[i]] <- tmp; rm(tmp)
    
  } # END FOR. 
  
  # Collapse to one file: 
  table_eu <- rbindlist(table_eu)
  
  if( k == 1 ) {
    
    # Keep the first period: 
    table_c6f <- table_eu; rm(table_eu)
    
  }
  
  if( k == 2 ) {
    
    # Add the new periods: 
    table_c6f <- rbind( table_c6f, table_eu ); rm(table_eu)
    
  }
  
  if( k == 3 ) {
    
    # Add the new periods: 
    table_c6f <- rbind( table_c6f, table_eu ); rm(table_eu)
    
    # Write to file: 
    write.csv( table_c6f, file = paste( "./output_files/table_c6f.csv", sep = ""), row.names = F )
    
  }

  
  # 2. Make the plots: 
  ###################################################################################
  
  # Ticks: 
  tick_l      <- list()
  tick_l[[1]] <- seq( -150, 150,  25  )
  tick_l[[2]] <- seq( -150, 500,  50  )
  tick_l[[3]] <- seq(  -10,  10,   1  )
  tick_l[[4]] <- seq( -100, 500,  50  )
  
  # SDA tpyes: 
  sda         <- c("egy_m", "egy_mv", "int_mva", "int_mv")
  inv         <- c("pr", "fp", "cn")
  
  # Make the plots: 
  for( i in 1:length(plot_eum) ) { # BEGIN FOR OVER SDA.
    
    for( j in 1:length(inv) ) { # BEGIN FOR OVER INVENTORIES. 
      
      # Delete Malta and Cyprus: 
      plot_eum  [[i]][[j]] <- plot_eum  [[i]][[j]][!plot_eum  [[i]][[j]]$reg %in% c("MLT"), ]
      plot_eum_t[[i]][[j]] <- plot_eum_t[[i]][[j]][!plot_eum_t[[i]][[j]]$reg %in% c("MLT"), ]
      
      # Make the plot: 
      ticks <- tick_l[[i]]
      tmp   <- make_eum_plot( plot_eum[[i]][[j]], plot_eum_t[[i]][[j]], sda[i] ); rm(ticks)
  
      # Save the plot: 
      if( i == 4 ) { # BEGIN IF. 
        
        if( k == 1 ) { # BEGIN IF. 
        
          ggsave( paste( './output_files/figure_c', j, 'a.jpg', sep = ""), plot = tmp, device = "jpeg", scale = 1, width = 420, height = 150, units = c("mm"), dpi = 300 )
          rm(tmp)
        
        } # END IF. 
        
        if( k == 2 ) { # BEGIN IF. 
          
          ggsave( paste( './output_files/figure_c', j, 'b.jpg', sep = ""), plot = tmp, device = "jpeg", scale = 1, width = 420, height = 150, units = c("mm"), dpi = 300 )
          rm(tmp)
          
        } # END IF. 
        
        if( k == 3 ) { # BEGIN IF. 
          
          ggsave( paste( './output_files/figure_c', j, 'c.jpg', sep = ""), plot = tmp, device = "jpeg", scale = 1, width = 420, height = 150, units = c("mm"), dpi = 300 )
          rm(tmp)
          
        } # END IF. 
        
      } # END IF. 
      
    } # END FOR OVER INVENTORIES. 
    
  } # END FOR OVER SDA. 
  
  # Clean-up: 
  rm( sda, tick_l, plot_eum, plot_eum_t, eva, order )
  
  
  ###################################################################################
  # VII. Averge yearly intensity-changes and scatterplot:                           #
  ###################################################################################
  
  # Do we want all countries or remove RoW?
  all <- "yes" # Comment out if not needed. 
  xtw <- "yes" # Do we want XTW or not?

  if( k == 1 ) {
    
    # Calculate the table: 
    source( './sub_codes/07_scatter_reg_v9.R' )
    
    # Write to file: 
    write.csv( tab_agr, './output_files/table_c9.csv', row.names = F ); rm(tab_agr)
  
  }
  
} # END FOR. 
  
  
  








