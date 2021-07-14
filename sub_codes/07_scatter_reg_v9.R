  
  
  #######################################################################################################################
  #                                                                                                                     # 
  # Name:     scatter_reg                                                                                               # 
  # Author:   Patrick Tomberger                                                                                         # 
  # Date:     March 1st, 2021                                                                                           # 
  # Notes:                                                                                                              # 
  #                                                                                                                     # 
  # Version history:                                                                                                    # 
  #                                                                                                                     # 
  # v9:         + Calculates table C.9 in the Appendix.                                                                 #
  #                                                                                                                     # 
  #######################################################################################################################
  

###################################################################################
# VII. Averge yearly intensity-changes and scatterplot:                           #
###################################################################################

# 1. Laod and prepare the raw data: 
###################################################################################

# Keep the original sda_cnt
tmp     <- sda_cnt

# The SDA data for each period: 
load( paste('./input_files/sda_data_1997-2007.RData', sep = "") )
sda_p1  <- sda_cnt[[2]]; rm(sda_cnt)

load( paste('./input_files/sda_data_2007-2014.RData', sep = "") )
sda_p2  <- sda_cnt[[2]]; rm(sda_cnt)

load( paste('./input_files/sda_data_1997-2014.RData', sep = "") )
sda_p3  <- sda_cnt[[2]]; rm(sda_cnt)

# Restore the original sda_cnt: 
sda_cnt <- tmp; rm(tmp)

# Put the separate files together: 
sda_p1$period <- "p1"
sda_p2$period <- "p2"
sda_p3$period <- "p3"
av_growth     <- rbind( sda_p1, sda_p2, sda_p3 ); rm(sda_p1, sda_p2, sda_p3)

# Define the groups: 
tmp1          <- unique( av_growth$reg_i[av_growth$reg_ia == "roecd"] ) # OECD not explicitly in the data already
tmp2          <- c("usa", "jpn", "che", "xef")                          # OECD we have already in the data
tmp3          <- c("chn", "bra", "rus", "ind")                          # BRICS without South Africa which is in a composite region  

# Keep only produciton-based inventories and aggregate the MV indices for the individual countries: 
# Note: Keep sector energy-intensity separated and aggregate all the others
av_growth$fac[av_growth$fac %in% c("ehh", "tch", "str_s", "str_c")] <- "other"
av_growth$fac[av_growth$fac == "str_e"                            ] <- "mix"
av_growth$fac[av_growth$fac == "vol"                              ] <- "act"

# Separate the inventories: 
tmp_pr                             <- av_growth[av_growth$inv == "Production"      , ]
tmp_fp                             <- av_growth[av_growth$inv == "Fin. Production" , ]
tmp_cn                             <- av_growth[av_growth$inv == "Fin. Consumption", ]

# Aggregate the inventories: 
tmp_pr                             <- aggregate( tmp_pr$movar_i, by = list(tmp_pr$reg_i, tmp_pr$inv, tmp_pr$fac, tmp_pr$period), FUN = prod )
tmp_fp                             <- aggregate( tmp_fp$movar_i, by = list(tmp_fp$reg_j, tmp_fp$inv, tmp_fp$fac, tmp_fp$period), FUN = prod )
tmp_cn                             <- aggregate( tmp_cn$movar_i, by = list(tmp_cn$reg_j, tmp_cn$inv, tmp_cn$fac, tmp_cn$period), FUN = prod )

# Label the inventories and plug them back together: 
av_growth                          <- rbind( tmp_pr, tmp_fp, tmp_cn ); rm(tmp_pr, tmp_fp, tmp_cn)
colnames(av_growth)                <- c("reg", "inv", "fac", "period", "mv_int")

# Define the groups  
av_growth$group                                                  <- "row"
av_growth$group[av_growth$reg %in% tmp_eu$reg[tmp_eu$eu15 == 1]] <- "eu_15"
av_growth$group[av_growth$reg %in% tmp_eu$reg[tmp_eu$eu15 == 0]] <- "eeu"
av_growth$group[av_growth$reg %in% tmp1]                         <- "oecd"
av_growth$group[av_growth$reg %in% tmp2]                         <- "oecd"
av_growth$group[av_growth$reg %in% tmp3]                         <- "brics"

# transform into yearly average percentages:
av_growth$mv_int[av_growth$period == "p1"] <- ( av_growth$mv_int[av_growth$period == "p1"] - 1 ) / 10 * 100
av_growth$mv_int[av_growth$period == "p2"] <- ( av_growth$mv_int[av_growth$period == "p2"] - 1 ) /  7 * 100
av_growth$mv_int[av_growth$period == "p3"] <- ( av_growth$mv_int[av_growth$period == "p3"] - 1 ) / 17 * 100


# 2. Create the table for the text, but calculate energy efficiency for all years: :  
###################################################################################

# Make an own data frame for the tables:  
tab_agr        <- av_growth

# Make own dataframe and round the average growth rates: 
tab_agr$mv_int <- round( tab_agr$mv_int, digits = 2 )

# Bring to right format: 
tmp1a   <- tab_agr[tab_agr$fac ==   "int", ]
tmp1b   <- tab_agr[tab_agr$fac == "other", ]

tmp1    <- tmp1a[tmp1a$period == "p3", ][, c("reg", "group", "inv", "mv_int")]
tmp1$p1 <- tmp1a$mv_int[tmp1a$period == "p1"]
tmp1$p2 <- tmp1a$mv_int[tmp1a$period == "p2"]; rm(tmp1a)

tmp2    <- tmp1b[tmp1b$period == "p3", ][, c("reg", "group", "inv", "mv_int")]
tmp2$p1 <- tmp1b$mv_int[tmp1b$period == "p1"]
tmp2$p2 <- tmp1b$mv_int[tmp1b$period == "p2"]; rm(tmp1b)

tab_agr <- cbind( tmp1, tmp2[, c("mv_int", "p1", "p2")] ); rm(tmp1, tmp2)

# Give nice column names: 
colnames( tab_agr ) <- c("reg", "group", "inv", "pc_i_tot", "pc_i_p1", "pc_i_p2", "pc_o_tot", "pc_o_p1", "pc_o_p2")

# Order the groups: 
tmp1    <- tab_agr[tab_agr$group == "eu_15", ]
tmp2    <- tab_agr[tab_agr$group == "eeu"  , ]
tmp3    <- tab_agr[tab_agr$group == "oecd" , ]
tmp4    <- tab_agr[tab_agr$group == "brics", ]
tmp5    <- tab_agr[tab_agr$group == "row"  , ]
tab_agr <- rbind( tmp1, tmp2, tmp3, tmp4, tmp5 ); rm(tmp1, tmp2, tmp3, tmp4, tmp5) 

# Calculate economy-wide sector intensity and -efficiency and their average growth rate in the two periods:  
va_inv_a <- read.csv( './input_files/va_inventories_97-14.csv' )
va_inv_b <- read.csv( './input_files/va_inventories_07-14.csv' )

va_inv_a <- va_inv_a[va_inv_a$yr == 1997, ]
va_inv_c <- va_inv_b[va_inv_b$yr == 2014, ]
va_inv_b <- va_inv_b[va_inv_b$yr == 2007, ]

va_inv   <- rbind( va_inv_a, va_inv_b, va_inv_c ); rm(va_inv_a, va_inv_b, va_inv_c)

# Calculate total sector energy intensity in 1997 and 2007:  
egy_tot <- egy_inv[ egy_inv$yr %in% c(1997, 2007, 2014), ] 
va_tot  <- va_inv [ va_inv$yr  %in% c(1997, 2007, 2014), ]; rm( va_inv)

# Add the different energy commodities and bring to megatons:
egy_tot$pr_tot <- rowSums( egy_tot[, paste(  "p", com, sep = "_" )] ) / 1000
egy_tot$fp_tot <- rowSums( egy_tot[, paste( "fp", com, sep = "_" )] ) / 1000
egy_tot$cn_tot <- rowSums( egy_tot[, paste(  "c", com, sep = "_" )] ) / 1000
egy_tot        <- egy_tot[, c("reg", "yr", "pr_tot", "fp_tot", "cn_tot")]

# Calcualte energy efficiency and its growth rates in all periods: 
eff_data                                 <- left_join( egy_tot, va_tot[, c("reg", "yr", "va_pr", "va_fp", "va_cn")], by = c("reg", "yr") )
eff_data[, paste('eff', inv, sep = '_')] <- eff_data[, paste(inv, 'tot', sep = '_')] / eff_data[, paste('va', inv, sep = '_')] * 1000

tmp_1                                        <- eff_data[, c("reg", "yr")][eff_data$yr == 1997, ]
tmp_1[, paste('eff_agr_p1', inv, sep = '_')] <- ( eff_data[, paste('eff', inv, sep = '_')][eff_data$yr == 2007, ] /
                                                  eff_data[, paste('eff', inv, sep = '_')][eff_data$yr == 1997, ] - 1 ) * 100 / 10 

tmp_1[, paste('eff_agr_p2', inv, sep = '_')] <- ( eff_data[, paste('eff', inv, sep = '_')][eff_data$yr == 2014, ] /
                                                  eff_data[, paste('eff', inv, sep = '_')][eff_data$yr == 2007, ] - 1 ) * 100 / 7 

eff_data                                     <- tmp_1; rm(tmp_1)

# Bring eff-data to final format: 
eff_data$yr                                           <- NULL
eff_data                                              <- melt(eff_data, id.vars = c("reg"), variable.name = "period")
colnames(eff_data)[3]                                 <- "eff_agr"

eff_data$inv                                          <- "XXX"
eff_data$inv[substr(eff_data$period, 12, 13) == "pr"] <- "Production"
eff_data$inv[substr(eff_data$period, 12, 13) == "fp"] <- "Fin. Production"
eff_data$inv[substr(eff_data$period, 12, 13) == "cn"] <- "Fin. Consumption"
eff_data$period                                       <- substr( eff_data$period, 9, 10 )

# Remove the year 2014 from egy_tot again: 
egy_tot <- egy_tot[egy_tot$yr != 2014, ]

# Remove HH energy usage in order to get economy-wide sector intensity: 
egy_hh              <- read.csv( './input_files/household_energy_usage.csv', header = T )

egy_tot             <- left_join( egy_tot, egy_hh, by = c("reg", "yr") ); rm(egy_hh)
tmp                 <- c("pr_tot", "fp_tot", "cn_tot")
egy_tot[, tmp]      <- egy_tot[, tmp]  - egy_tot$egy_hh
egy_tot$egy_hh      <- NULL

va_tot$unit         <- NULL
eva_tot             <- left_join( egy_tot, va_tot, by = c("reg", "yr") ); rm(egy_tot, va_tot)

eva_tot[, c("pr_int", "fp_int", "cn_int")] <- eva_tot[, c("pr_tot", "fp_tot", "cn_tot")] / eva_tot[, c("va_pr", "va_fp", "va_cn")] * 1000

eva_tot$period                             <- "xxx"
eva_tot$period[eva_tot$yr == 1997]         <- "p1"
eva_tot$period[eva_tot$yr == 2007]         <- "p2"

# Stack all energy intensitis according to its inventory: 
tmp1     <- eva_tot[, c("reg", "yr", "period", "pr_int")]; colnames(tmp1)[4] <- "s_int"
tmp2     <- eva_tot[, c("reg", "yr", "period", "fp_int")]; colnames(tmp2)[4] <- "s_int"
tmp3     <- eva_tot[, c("reg", "yr", "period", "cn_int")]; colnames(tmp3)[4] <- "s_int"

tmp1$inv <- "Production"
tmp2$inv <- "Fin. Production"
tmp3$inv <- "Fin. Consumption"

# Bind together and match with the tables: 
eva_tmp  <- rbind( tmp1, tmp2, tmp3 ); rm(tmp1, tmp2, tmp3)
tab_agr  <- left_join( tab_agr, eva_tmp[, c("reg", "inv", "s_int")][eva_tmp$yr == 1997, ], by = c("reg", "inv") ); colnames(tab_agr)[10] <- "s_int_97"
tab_agr  <- left_join( tab_agr, eva_tmp[, c("reg", "inv", "s_int")][eva_tmp$yr == 2007, ], by = c("reg", "inv") ); colnames(tab_agr)[11] <- "s_int_07"

# Round and clean-up:
tab_agr$s_int_97 <- round( tab_agr$s_int_97, digits = 2 )
tab_agr$s_int_07 <- round( tab_agr$s_int_07, digits = 2 ); rm(eva_tmp)

# Bring to final order: 
order1           <- tab_agr[, c("reg", "group", "inv")][tab_agr$inv == "Production"      , ] 
order2           <- tab_agr[, c("reg", "group", "inv")][tab_agr$inv == "Fin. Production" , ] 
order3           <- tab_agr[, c("reg", "group", "inv")][tab_agr$inv == "Fin. Consumption", ] 
order            <- rbind( order1, order2, order3 ); rm(order1, order2, order3)

tab_agr          <- left_join( order, tab_agr, by = c("reg", "group", "inv") ); rm(order)

# Give reasonable column-names: 
colnames(tab_agr) <- c( "reg", "group", "inv", "eff_fac_97-14", "eff_fac_97-07", "eff_fac_07-14", "pc_o_tot", "pc_o_p1", "pc_o_p2", "sec_int_97", "sec_int_07" )

# Delete non-needed inventories: 
tab_agr           <- tab_agr[tab_agr$inv == "Production", ]
tab_agr$inv       <- NULL

# Latex format (we ignore average change in the overall factors): 
tab_agr$tab <- "&"
tab_agr$end <- "\\\\"
tab_agr     <- tab_agr[, c("group", "reg", "tab", "eff_fac_97-14", "tab", "eff_fac_97-07", "tab", "eff_fac_07-14", "tab", "sec_int_97", "tab", "sec_int_07", "end")]










