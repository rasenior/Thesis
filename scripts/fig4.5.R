# Setup -------------------------------------------------------------------

rm(list=ls())

# Get the current user (Rebecca or Becc, if work or home)
user<-Sys.info()[7]

# Set working directory
base_dir <-
    file.path("C:/Users",user,
              "Google Drive/PhD/Ch4/Resubmission")
dat_dir <- file.path(base_dir,"Data/FINAL")
mod_dir <- file.path(dat_dir, "models")
fig_dir <- file.path(base_dir,"Figures/results")

# Load libraries
library(tidyr)
library(lme4)
library(ggplot2)
library(cowplot)

# Load raw data
flir <- readRDS(file.path(dat_dir,"flir_rep_2017-07-27.Rds"))
plot_info <- readRDS(file.path(dat_dir,"plot_info_2017-07-27.Rds"))
load(file.path(dat_dir,"logger_vol_2017-07-27.Rdata"))
treeBA_median <- readRDS(file.path(dat_dir,"treeBA_median.rds"))

# Load models
load(file.path(mod_dir, "q3_main_results.Rdata"))

# Source local functions
source(file.path("C:/Users",user,
                 "Google Drive/Programming/R/functions/mround.R"))
source("scripts/plotting_fn.R")
# Define font sizes
lab_size <- 7
# options(scipen = 999)

# Prepare data ------------------------------------------------------------

flir$cold.patch.AI <- flir$cold.obs.edges / flir$cold.max.edges

micro_results <- c("deadwood_vol", "hole_vol", "litter_vol")

# Because the y axis is always the same, can use facets
micro <- rbind(deadwood_vol, hole_vol, litter_vol)

# Add panel labels
micro$bottom_labs <- 
    ifelse(micro$microhabitat == "Deadwood", "(d)",
           ifelse(micro$microhabitat == "Tree hole","(e)","(f)"))


# Plot patch range --------------------------------------------------------

fig5_a <- do.call("rbind",pred(dat = "flir",
                               df_name = "patch.range",
                               results_df = q3_results,
                               RV = "patch.range",
                               vary_EV = "tree_stand_BA",
                               constant_EV = "none",
                               constant_val = "NA"))

# Change response variable name
colnames(fig5_a)[names(fig5_a)=="RV"] <- "patch.range"

p5a <- plot_continuous(pred_df = fig5_a, 
                       raw_df = flir, 
                       RV = "patch.range", 
                       vary_EV = "tree_stand_BA",
                       constant_EV = "none",
                       x_lab = "",
                       y_lab = paste("Patch temperature range (", "\U00B0","C)\n",sep=""), 
                       facet_scale =  "free",
                       text_size = 6,
                       panel_labs = "",
                       point_alpha = 0.5,
                       leg_pos = "return", 
                       lab_hjust = 1.7,
                       lab_vjust = 1.44)



# Plot patch area ---------------------------------------------------------

fig5_b <- do.call("rbind",pred(dat = "flir",
                               df_name = "cold.patch.area",
                               results_df = q3_results,
                               RV = "cold.patch.area",
                               vary_EV = "tree_stand_BA",
                               constant_EV = "none",
                               constant_val = "NA"))

# Change response variable name
colnames(fig5_b)[names(fig5_b)=="RV"] <- "cold.patch.area"

p5b <- plot_continuous(pred_df = fig5_b, 
                       raw_df = flir, 
                       RV = "cold.patch.area", 
                       vary_EV = "tree_stand_BA",
                       constant_EV = "none",
                       text_size = 6,
                       x_lab = "",
                       y_lab = paste("Cool patch area (cm", "\U00B2",")",sep=""),
                       facet_scale =  "free",
                       panel_labs = "",
                       point_alpha = 0.5,
                       leg_pos = "none", 
                       lab_hjust = 1.7,
                       lab_vjust = 1.44)


# Plot patch AI -----------------------------------------------------------

fig5_c <- do.call("rbind",pred(dat = "flir",
                               df_name = "cold.patch.AI",
                               results_df = q3_results,
                               mod_family = "binomial",
                               RV = "cold.patch.AI",
                               vary_EV = "tree_stand_BA",
                               constant_EV = "none",
                               constant_val = "NA"))

# Change response variable name
colnames(fig5_c)[names(fig5_c)=="RV"] <- "cold.patch.AI"

# Change AI to percentage
flir$cold.patch.AI <- flir$cold.patch.AI * 100
fig5_c$cold.patch.AI <- fig5_c$cold.patch.AI * 100
fig5_c$plo <- fig5_c$plo * 100
fig5_c$phi <- fig5_c$phi * 100

p5c <- plot_continuous(pred_df = fig5_c, 
                       raw_df = flir, 
                       RV = "cold.patch.AI", 
                       vary_EV = "tree_stand_BA",
                       constant_EV = "none",
                       x_lab = "",
                       y_lab = "Cool patch AI (%)",
                       text_size = 6,
                       facet_scale = "free", 
                       panel_labs = "",
                       point_alpha = 0.5,
                       leg_pos = "none", 
                       lab_hjust = 1.7,
                       lab_vjust = 1.44)


# Plot microhabitat vol ---------------------------------------------------

fig5_bottom <-
    sapply(micro_results, function(x){
        
        df_name <- 
            unique(eval(parse(text = x))[,"df_name"])
        
        if(x == "litter_vol"){
            RV <- "vol"
        }else{
            RV <- "vol_log"
        }
        
        result <- pred(dat = x,
                       results_df = q3_results,
                       df_name = df_name, 
                       RV = RV,
                       vary_EV = "tree_stand_BA",
                       constant_EV = "none",
                       constant_val = "NA")
    })

fig5_bottom <- do.call("rbind", fig5_bottom)

# Change response variable name
colnames(fig5_bottom)[names(fig5_bottom)=="RV"] <- "vol"

# Add microhabitat variable
fig5_bottom$microhabitat <- gsub("_day","",fig5_bottom$dat)
fig5_bottom$microhabitat <-
    factor(fig5_bottom$microhabitat,
           levels = unique(fig5_bottom$microhabitat),
           labels = c("Deadwood", "Tree hole", "Leaf litter"))

# Back-transform for deadwood and tree holes
# Deadwood
fig5_bottom$vol[fig5_bottom$microhabitat == "Deadwood"] <- 
    exp(fig5_bottom$vol[fig5_bottom$microhabitat == "Deadwood"])
fig5_bottom$plo[fig5_bottom$microhabitat == "Deadwood"] <- 
    exp(fig5_bottom$plo[fig5_bottom$microhabitat == "Deadwood"])
fig5_bottom$phi[fig5_bottom$microhabitat == "Deadwood"] <- 
    exp(fig5_bottom$phi[fig5_bottom$microhabitat == "Deadwood"])

# Tree holes
fig5_bottom$vol[fig5_bottom$microhabitat == "Tree hole"] <- 
    expm1(fig5_bottom$vol[fig5_bottom$microhabitat == "Tree hole"])
fig5_bottom$plo[fig5_bottom$microhabitat == "Tree hole"] <- 
    expm1(fig5_bottom$plo[fig5_bottom$microhabitat == "Tree hole"])
fig5_bottom$phi[fig5_bottom$microhabitat == "Tree hole"] <- 
    expm1(fig5_bottom$phi[fig5_bottom$microhabitat == "Tree hole"])

# Plot!
p5_bottom <- plot_continuous(pred_df = fig5_bottom, 
                             raw_df = micro, 
                             RV = "vol", 
                             vary_EV = "tree_stand_BA",
                             constant_EV = "none",
                             x_lab = paste("Tree stand basal area (m","\U00B2","/ha)",sep=""),
                             y_lab = paste("Microhabitat volume (cm", "\U00B3",")",sep=""), 
                             panel.spacing = 0.5,
                             panel_labs = "",
                             text_size = 6,
                             point_alpha = 0.8,
                             facet_scale = "free_y",
                             leg_pos = "none", 
                             lab_hjust = 1.7,
                             lab_vjust = 1.44)


# Combine and save --------------------------------------------------------

combi_colour <-
    ggdraw() +
    draw_plot(ggplot(), x = 0, y = 0.95, width = 0.05, height = 0.05) +
    draw_plot(p5a$p$p_leg, x = 0.05, y = 0.95, width = 0.95, height = 0.05) +
    draw_plot(p5a$p$p, x = 0.004, y = 0.475, width = 1/3 + 0.004, height = 0.475) +
    draw_plot(p5b$p, x = 1/3 + 0.000, y = 0.475, width = 1/3 - 0.016, height = 0.475) +
    draw_plot(p5c$p, x = 2/3 - 0.026, y = 0.475, width = 1/3 - 0.015, height = 0.475) +
    draw_plot(p5_bottom$p, x = 0, y = 0, width = 0.96, height = 0.475) +
    draw_plot_label(label = "**", x = 0.81, y = 0.40, size = 10)+
    draw_plot_label(label = c("(a)", "(b)", "(c)", 
                              "(d)", "(e)","(f)"),
                    x = c(0.298, 0.607, 0.917,
                          0.298, 0.607, 0.923),
                    y = rep(c(0.87, 0.394), each = 3),
                    size = lab_size)

# ggsave(combi_colour, filename = "figs/test.png", dpi = 200,
#        width = 16.6/2.54, height = 10/2.54, units = "in")

saveRDS(combi_colour, file = "figs/fig4.5.Rds")



