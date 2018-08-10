# Setup -------------------------------------------------------------------

rm(list=ls())

# Get the current user (Rebecca or Becc, if work or home)
user<-Sys.info()[7]

# Load libraries
library(ggplot2)
library(cowplot)

# Load raw data
flir <- readRDS("data/ch4/flir_day_2017-07-27.Rds")
load("data/ch4/deadwood_2017-07-27.Rds")
load("data/ch4/hole_2017-07-27.Rds")
load("data/ch4/litter_2017-07-27.Rds")

# Load models
q2_results <- readRDS("data/ch4/models/q2_main_results.Rds")

all_dat <- c("flir",
             "deadwood_day", "hole_day","litter_day")

# Remove any data we don't need
rm(list = ls()[!(ls() %in% c(all_dat, "all_dat","q2_results","base_dir",
                             "dat_dir","fig_dir","mod_dir","user"))])

# Source local functions
source("scripts/mround.R")
source("scripts/plotting_fn.R")
# Define font sizes
lab_size <- 7
text_size <- 7
title_size <- 9
# Prepare data ------------------------------------------------------------

# Check dimensions
correct_dim <- data.frame(dat = all_dat,
                          rows = c(115,336, 330, 342))
dim_test <- sapply(all_dat, function(x){
  dat <- eval(parse(text = x))
  results <- nrow(dat) == correct_dim$rows[correct_dim$dat == x]
  return(results)
})
dim_test

flir$micro_IPR <- flir$percentile_5_IPR
deadwood_day$micro_IPR <- deadwood_day$day_IPR 
hole_day$micro_IPR <- hole_day$day_IPR
litter_day$micro_IPR <- litter_day$day_IPR

key_vars <- c("forest_type", "tree_stand_BA",
              "microhabitat", "micro_IPR")
micro <- rbind(flir[,key_vars],
               deadwood_day[,key_vars],
               hole_day[,key_vars],
               litter_day[,key_vars])
nrow(micro) == sum(correct_dim$rows)

micro$microhabitat <-
  factor(micro$microhabitat,
         levels = unique(micro$microhabitat))

# Add panel labels
micro$top_labs <- 
  ifelse(micro$microhabitat == "Surface","(a)",
         ifelse(micro$microhabitat == "Deadwood", "(b)",
                ifelse(micro$microhabitat == "Tree hole","(c)","(d)")))

# Plot against tree BA ----------------------------------------------------

fig4 <-
  sapply(all_dat, function(x){
    
    if(x == "flir"){
      RV <- "percentile_5_IPR"
    }else{
      RV <- "day_IPR"
    }
    
    result <- pred(dat = x,
                   df_name = x,
                   results_df = q2_results,
                   RV = RV,
                   vary_EV = "tree_stand_BA",
                   constant_EV = "none",
                   constant_val = "NA")
  })

fig4 <- do.call("rbind", fig4)

# Change response variable name
colnames(fig4)[names(fig4)=="RV"] <- "micro_IPR"

# Add microhabitat variable
fig4$microhabitat <- gsub("_day","",fig4$dat)
fig4$microhabitat <-
  factor(fig4$microhabitat,
         levels = unique(fig4$microhabitat),
         labels = c("Surface", "Deadwood", "Tree hole", "Leaf litter"))

# Plot!
p1 <- plot_continuous(pred_df = fig4, 
                      raw_df = micro, 
                      RV = "micro_IPR", 
                      vary_EV = "tree_stand_BA",
                      constant_EV = "ambient",
                      x_lab = paste("Tree stand basal area (m","\U00B2","/ha)",sep=""),
                      y_lab = paste("Microclimate temp. range (", "\U00B0","C)",sep=""), 
                      panel_labs = "",
                      text_size = text_size,
                      title_size = title_size,
                      facet_scale = "fixed",
                      point_alpha = 0.5,
                      leg_pos = "top", 
                      lab_hjust = 1.67,
                      lab_vjust = 1.45)
p1$p <-
    p1$p + 
    scale_x_continuous(breaks = seq(0,100,20))

# Write -------------------------------------------------------------------

combi <-
    ggdraw() +
    draw_plot(p1$p, x = 0, y = 0, width = 1, height = 1) +
    draw_plot_label(label = c("(a)", "(b)", "(c)", "(d)"),
                    x = c(0.25, 0.485, 0.72, 0.95),
                    y = rep(0.69, each = 4),
                    size = lab_size)

ggsave(plot = combi, filename = "old/fig4.4.png",
       width = 16.6/2.54, height = 10/2.54)

saveRDS(combi, file = "figs/fig4.4.Rds")




