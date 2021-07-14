  
  
  #######################################################################################################################
  #                                                                                                                     # 
  # Name:     global_table                                                                                              # 
  # Author:   Patrick Tomberger                                                                                         # 
  # Date:     June 25th, 2020                                                                                           # 
  # Notes:                                                                                                              # 
  #                                                                                                                     # 
  # Version history:                                                                                                    # 
  #                                                                                                                     # 
  # v1:         + Tables with global energy usage (mtoe) and energy commodities as shares.                              # 
  #                                                                                                                     # 
  #######################################################################################################################
  

###################################################################################
# IX. Global table:                                                               #
###################################################################################

# Global energy usage and share of the six commodities over time: 
egy          <- c( "p_fos", "p_nuc", "p_rsn", "p_hyd", "p_bfl", "p_wnd", "p_sol", "p_rsr" )
glob_tab     <- egy_inv[, c("reg", "yr", egy)]

# Aggregate biofuels and other renewable sources and update the egy labels: 
glob_tab$orn <- glob_tab$p_bfl + glob_tab$p_rsr
egy          <- c( egy[-c(5,8)], "orn" )

# Create a total: 
glob_tab$tot <- rowSums( glob_tab[, egy] )
egy          <- c("tot", egy)

# Aggreate to the global level: 
glob_tab              <- aggregate( glob_tab[, egy], by = list(glob_tab$yr), FUN = sum )
colnames(glob_tab)[1] <- "yr"

# Calculate the shares of the seven energy commodities: 
tmp             <- egy[-1]
glob_tab[, tmp] <- round( glob_tab[, tmp] / glob_tab$tot * 100, digits = 1 ); rm(tmp, egy)

# Switch from Kilo- to Megatons: 
glob_tab$tot  <- round( glob_tab$tot / 1000, digits = 1 )

# Bring to final format: 
glob_tab$col <- "&"
glob_tab$end <- "\\\\"
glob_tab     <- glob_tab[, c("yr" , "col"  , "tot", "col", "p_fos", "col", "p_nuc", "col", "p_rsn", "col", "p_hyd", "col", "p_wnd",
                             "col", "p_sol", "col", "orn", "end")]

colnames(glob_tab) <- c("yr" , "col"  , "tot", "col", "fossil", "col", "nuclear", "col", "oth_nr", "col", "hydro", "col", "wind",
                        "col", "solar", "col", "oth_r", "end")







