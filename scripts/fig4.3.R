  # Setup -------------------------------------------------------------------

# rm(list=ls())
# 
# # Get the current user (Rebecca or Becc, if work or home)
# user<-Sys.info()[7]
# 
# # Set working directory
# base_dir <-
#   file.path("C:/Users",user,
#             "Google Drive/PhD/Ch4/Resubmission")
# dat_dir <- file.path(base_dir,"Data/FINAL")
# mod_dir <- file.path(dat_dir, "models")
# fig_dir <- file.path(base_dir,"Figures/results")

# Load libraries
library(ggplot2)
library(cowplot)

# Load raw data
flir <- readRDS("data/ch4/flir_rep_2017-07-27.Rds")
load("data/ch4/deadwood_2017-07-27.Rds")
load("data/ch4/hole_2017-07-27.Rds")
load("data/ch4/litter_2017-07-27.Rds")
treeBA_median <- readRDS("data/ch4/treeBA_median.rds")

# Load models
q1_results <- readRDS("data/ch4/models/q1_main_results.Rds")

all_dat <- c("flir",
             "deadwood_rep", "hole_rep","litter_rep")

# Remove any data we don't need
rm(list = ls()[!(ls() %in% c(all_dat, "all_dat","q1_results","treeBA_median",
                             "base_dir","dat_dir","fig_dir","mod_dir","user"))])

q1_results <- 
  q1_results[which(gsub(".results","",names(q1_results)) %in% all_dat)]

# Source local functions
source("scripts/mround.R")
source("scripts/plotting_fn.R")

# Define font sizes
text_size <- 8
title_size <- 10
lab_size <- 7

# Prepare data ------------------------------------------------------------

# Check dimensions
correct_dim <- data.frame(dat = all_dat,
                          rows = c(600,
                                   1680, 1649, 1710))
dim_test <- sapply(all_dat, function(x){
  dat <- eval(parse(text = x))
  results <- nrow(dat) == correct_dim$rows[correct_dim$dat == x]
  return(results)
})
dim_test

flir$micro_temp <- flir$percentile_5
deadwood_rep$micro_temp <- deadwood_rep$rep_temp
hole_rep$micro_temp <- hole_rep$rep_temp
litter_rep$micro_temp <- litter_rep$rep_temp

key_vars <- c("forest_type", 
              "primary_BA","logged_BA",
              "tree_stand_BA","ambient",
              "microhabitat", "micro_temp")
micro <- rbind(flir[,key_vars],
               deadwood_rep[,key_vars],
               hole_rep[,key_vars],
               litter_rep[,key_vars])
nrow(micro) == sum(correct_dim$rows)

micro$microhabitat <-
  factor(micro$microhabitat,
         levels = unique(micro$microhabitat))

# Add panel labels
micro$top_labs <- 
  ifelse(micro$microhabitat == "Surface","(a)",
         ifelse(micro$microhabitat == "Deadwood", "(b)",
                ifelse(micro$microhabitat == "Tree hole","(c)","(d)")))
micro$bottom_labs <- 
  ifelse(micro$microhabitat == "Surface","(e)",
         ifelse(micro$microhabitat == "Deadwood", "(f)",
                ifelse(micro$microhabitat == "Tree hole","(g)","(h)")))

# Plot against ambient ----------------------------------------------------

# primary_BA <- unique(micro$primary_BA)
# logged_BA <- unique(micro$logged_BA)

fig3_top <-
  sapply(all_dat, function(x){
    
    if(x == "flir"){
      RV <- "percentile_5"
    }else{
      RV <- "rep_temp"
    }
    
    result <- pred(dat = x,
                   df_name = x,
                   results_df = q1_results,
                   RV = RV,
                   vary_EV = "ambient_log",
                   constant_EV = "tree_stand_BA",
                   constant_val = treeBA_median)
  })

fig3_top <- do.call("rbind", fig3_top)

# Change response variable name
colnames(fig3_top)[names(fig3_top)=="RV"] <- "micro_temp"

# Add microhabitat variable
fig3_top$microhabitat <- gsub("_rep","",fig3_top$dat)
fig3_top$microhabitat <-
  factor(fig3_top$microhabitat,
         levels = unique(fig3_top$microhabitat),
         labels = c("Surface", "Deadwood", "Tree hole", "Leaf litter"))

# Re-level forest type
fig3_top$forest_type <- factor(fig3_top$forest_type,
                               levels <- c("Primary", "Logged"))

# Get ambient
fig3_top$ambient <- exp(fig3_top$ambient_log)

# Plot!
p1 <- plot_continuous(pred_df = fig3_top,
                      raw_df = micro,
                      RV = "micro_temp",
                      vary_EV = "ambient",
                      constant_EV = "tree_stand_BA",
                      x_lab = paste("Macroclimate temperature (", "\U00B0","C)",sep=""),
                      y_lab = "", 
                      panel_labs = "",
                      facet_scale = "fixed",
                      point_alpha = 0.2,
                      leg_pos = "return",
                      lab_hjust = 1.02,
                      lab_vjust = 1.45)
p1$p$p <-
    p1$p$p + 
    scale_x_continuous(breaks = seq(20,40,5),
                       limits = c(20,40))

# Plot against tree BA ----------------------------------------------------

fig3_bottom <-
  sapply(all_dat, function(x){
    
    if(x == "flir"){
      RV <- "percentile_5"
    }else{
      RV <- "rep_temp"
    }
    
    result <- pred(dat = x,
                   df_name = x,
                   results_df = q1_results,
                   RV = RV,
                   vary_EV = "tree_stand_BA",
                   constant_EV = "ambient_log",
                   constant_val = "median")
  })

fig3_bottom <- do.call("rbind", fig3_bottom)

# Change response variable name
colnames(fig3_bottom)[names(fig3_bottom)=="RV"] <- "micro_temp"

# Add microhabitat variable
fig3_bottom$microhabitat <- gsub("_rep","",fig3_bottom$dat)
fig3_bottom$microhabitat <-
  factor(fig3_bottom$microhabitat,
         levels = unique(fig3_bottom$microhabitat),
         labels = c("Surface", "Deadwood", "Tree hole", "Leaf litter"))

# Add ambient
fig3_bottom$ambient <- exp(fig3_bottom$ambient_log)

# Plot!
p2 <- plot_continuous(pred_df = fig3_bottom, 
                      raw_df = micro, 
                      RV = "micro_temp", 
                      vary_EV = "tree_stand_BA",
                      constant_EV = "ambient",
                      x_lab = paste("Tree stand basal area (m","\U00B2","/ha)",sep=""),
                      panel_labs = "",
                      facet_scale = "fixed",
                      y_lab = "", 
                      point_alpha = 0.2,
                      leg_pos = "none",
                      lab_hjust = 1.67,
                      lab_vjust = 1.45)
p2$p <-
    p2$p + 
    scale_x_continuous(breaks = seq(0,100,20))

# Combine and save --------------------------------------------------------

combi_colour <-
  ggdraw() +
  draw_plot(ggplot(), x = 0, y = 0.95, width = 0.05, height = 0.05) +
  draw_plot(p1$p$p_leg, x = 0.05, y = 0.95, width = 0.95, height = 0.05) +
  draw_plot(p1$p$p, x = 0, y = 0.475, width = 1, height = 0.475) +
  draw_plot(p2$p, x = 0, y = 0, width = 1, height = 0.475) +
  draw_text(paste("Microclimate temperature (", "\U00B0","C)",sep=""),
            x = 0.01, y= 0.5,size = title_size,angle=90) +
    draw_plot_label(label = c("(a)", "(b)", "(c)", "(d)",
                              "(e)","(f)", "(g)", "(h)"),
                    x = c(0.26, 0.490, 0.724, 0.955,
                          0.26, 0.495, 0.724, 0.955),
                    y = rep(c(0.86, 0.390), each = 4),
                    size = lab_size)

# ggsave(plot = combi_colour, filename = "old/fig4.3.png",
#        width = 16.6/2.54, height = 10/2.54)


saveRDS(combi_colour, file = "figs/fig4.3.Rds")





