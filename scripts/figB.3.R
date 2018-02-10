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
library(ggplot2)
library(cowplot)

# Load raw data
flir <- readRDS(file.path(dat_dir,"flir_rep_2017-07-27.Rds"))
load(file.path(dat_dir,"ambient_2017-07-27.Rds"))

# Load models
qs2_ambient <- readRDS(file.path(mod_dir, "som_q2_ambient_results.Rds"))
qs2_vpd <- readRDS(file.path(mod_dir, "som_q2_vpd_results.Rds"))

all_dat <- c("flir","ambient_rep")

# Remove any data we don't need
rm(list = ls()[!(ls() %in% c(all_dat, "all_dat","qs2_ambient","qs2_vpd",
                             "base_dir","dat_dir","fig_dir","mod_dir","user"))])

# Source local functions
source(file.path("C:/Users",user,
                 "Google Drive/Programming/R/functions/mround.R"))
source("scripts/plotting_fn.R")

# Prepare data ------------------------------------------------------------

# Check dimensions
correct_dim <- data.frame(dat = all_dat,
                          rows = c(600,1770))
dim_test <- sapply(all_dat, function(x){
    dat <- eval(parse(text = x))
    results <- nrow(dat) == correct_dim$rows[correct_dim$dat == x]
    return(results)
})
dim_test

key_vars <- c("forest_type", "tree_stand_BA",
              "primary_BA","logged_BA",
              "microhabitat", "ambient","VPD")
macroclimate <- rbind(flir[,key_vars],ambient_rep[,key_vars])
nrow(macroclimate) == sum(correct_dim$rows)

macroclimate$instrument <- 
    ifelse(macroclimate$microhabitat == "Surface","Hygrometer","Datalogger")

macroclimate$instrument <-
    factor(macroclimate$instrument,
           levels = c("Hygrometer","Datalogger"))

# Add panel labels
macroclimate$top_labs <- 
    ifelse(macroclimate$instrument == "Hygrometer","(a)","(b)")
macroclimate$bottom_labs <- 
    ifelse(macroclimate$instrument == "Hygrometer","(c)","(d)")

# Gather ambient and VPD
# macroclimate <- gather(macroclimate, key = "RV_var", value = "RV", ambient, VPD)


# Plot ambient temperature ------------------------------------------------

figS3_top <-
    sapply(all_dat, function(x){
        
        result <- pred(dat = x,
                       df_name = x,
                       results_df = qs2_ambient,
                       RV = "ambient_log",
                       vary_EV = "tree_stand_BA",
                       constant_EV = "none",
                       constant_val = "NA")
    })

figS3_top <- do.call("rbind", figS3_top)

# Back-transform
figS3_top$ambient <- exp(figS3_top$RV)
figS3_top$plo <- exp(figS3_top$plo)
figS3_top$phi <- exp(figS3_top$phi)
figS3_top <- figS3_top[names(figS3_top) != "RV"]

# Add instrument variable
figS3_top$instrument <- 
    ifelse(figS3_top$dat == "flir","Hygrometer","Datalogger")
figS3_top$instrument <-
    factor(figS3_top$instrument,
           levels = c("Hygrometer","Datalogger"))

# Plot!
p_top <- plot_continuous(pred_df = figS3_top, 
                         raw_df = macroclimate, 
                         RV = "ambient", 
                         vary_EV = "tree_stand_BA",
                         constant_EV = "none",
                         x_lab = "",
                         y_lab = paste("Macroclimate temperature (", "\U00B0","C)",sep=""),
                         facetting = "~ instrument",
                         facet_scale = "fixed",
                         panel_labs = "",
                         panel.spacing = 0.2,
                         point_alpha = 0.3,
                         leg_pos = "return", 
                         lab_hjust = 2,
                         lab_vjust = 1.45,
                         bw = FALSE)

# Plot VPD ----------------------------------------------------------------

figS3_bottom<-
    sapply(all_dat, function(x){
        
        results <- pred(dat = x,
                        df_name = x,
                        results_df = qs2_vpd,
                        RV = "VPD_log1p",
                        vary_EV = "tree_stand_BA",
                        constant_EV = "none",
                        constant_val = "NA")
        return(results)
        
    })

figS3_bottom <- do.call("rbind", figS3_bottom)

# Back-transform
figS3_bottom$VPD <- expm1(figS3_bottom$RV)
figS3_bottom$plo <- expm1(figS3_bottom$plo)
figS3_bottom$phi <- expm1(figS3_bottom$phi)
figS3_bottom <- figS3_bottom[names(figS3_bottom) != "RV"]

# Add instrument variable
figS3_bottom$instrument <- 
    ifelse(figS3_bottom$dat == "flir","Hygrometer","Datalogger")
figS3_bottom$instrument <-
    factor(figS3_bottom$instrument,
           levels = c("Hygrometer","Datalogger"))

p_bottom <- plot_continuous(pred_df = figS3_bottom, 
                            raw_df = macroclimate, 
                            RV = "VPD", 
                            vary_EV = "tree_stand_BA",
                            constant_EV = "none",
                            x_lab = paste("Tree stand basal area (m","\U00B2","/ha)",sep=""),
                            y_lab = ("VPD (Pa)"),
                            facetting = "~ instrument",
                            facet_scale = "fixed",
                            panel_labs = "",
                            panel.spacing = 0.2,
                            point_alpha = 0.3,
                            leg_pos = "none", 
                            lab_hjust = 2,
                            lab_vjust = 1.45,
                            bw = FALSE)


# Combine and save --------------------------------------------------------

p_leg <- p_top[[2]]
p_main <- plot_grid(p_top[[1]],p_bottom,ncol = 1,align = "hv")

combi <-
    ggdraw() +
    draw_plot(ggplot(), x = 0, y = 0.95, width = 0.05, height = 0.05) +
    draw_plot(p_leg, x = 0.05, y = 0.95, width = 0.95, height = 0.05) +
    draw_plot(p_main, x = 0, y = 0, width = 1, height = 0.95) +
    draw_plot_label(label = c("(a)", "(b)", "(c)", "(d)"),
                    x = c(0.25, 0.485, 0.72, 0.95),
                    y = rep(0.7, each = 4),
                    size = 7)

saveRDS(combi,file = "figs/figB.3.Rds")
