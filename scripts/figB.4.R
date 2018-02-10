# Setup -------------------------------------------------------------------

rm(list=ls())

# Get the current user (Rebecca or Becc, if work or home)
user<-Sys.info()[7]

# Load libraries
library(ggplot2)
library(cowplot)

# Load raw data
load("data/ch4/deadwood_all_2017-07-27.Rds")
load("data/ch4/hole_all_2017-07-27.Rds")
load("data/ch4/litter_all_2017-07-27.Rds")
treeBA_median <- readRDS("data/ch4/treeBA_median.rds")

# Split into day and night
deadwood_rep_day <- deadwood_rep_all[deadwood_rep_all$day_night=="day",]
deadwood_rep_night <- deadwood_rep_all[deadwood_rep_all$day_night=="night",]
hole_rep_day <- hole_rep_all[hole_rep_all$day_night=="day",]
hole_rep_night <- hole_rep_all[hole_rep_all$day_night=="night",]
litter_rep_day <- litter_rep_all[litter_rep_all$day_night=="day",]
litter_rep_night <- litter_rep_all[litter_rep_all$day_night=="night",]

day_df <- c("deadwood_rep_day", "hole_rep_day", "litter_rep_day")
night_df <- c("deadwood_rep_night", "hole_rep_night", "litter_rep_night")

# Load models
q1_results <- readRDS("data/ch4/models/q1_som_results.Rds")

q1_day_results <- 
  q1_results[which(grepl("_day.results",names(q1_results)))]

q1_night_results <- 
  q1_results[which(grepl("_night.results",names(q1_results)))]

# Remove any data we don't need
rm(list = ls()[!(ls() %in% c(day_df,night_df,"day_df","night_df",
                             "treeBA_median",
                             "q1_day_results","q1_night_results","base_dir",
                             "dat_dir","fig_dir","mod_dir","user"))])
# Source local functions
source(file.path("C:/Users",user,
                 "Google Drive/Programming/R/functions/mround.R"))
source("scripts/plotting_fn.R")

# Prepare data ------------------------------------------------------------

# Check dimensions
correct_dim <- data.frame(dat = day_df,
                          rows = c(2016, 1980, 2052))
dim_test <- sapply(day_df, function(x){
  dat <- eval(parse(text = x))
  results <- nrow(dat) == correct_dim$rows[correct_dim$dat == x]
  return(results)
})
dim_test

key_vars <- c("forest_type", "tree_stand_BA","day_night",
              "primary_BA","logged_BA",
              "ambient","microhabitat", "rep_temp")
micro_day <- rbind(deadwood_rep_day[,key_vars],
                   hole_rep_day[,key_vars],
                   litter_rep_day[,key_vars])
micro_night <- rbind(deadwood_rep_night[,key_vars],
                     hole_rep_night[,key_vars],
                     litter_rep_night[,key_vars])

nrow(micro_day) == sum(correct_dim$rows)
nrow(micro_night) == sum(correct_dim$rows)

micro_day$microhabitat <-
  factor(micro_day$microhabitat,
         levels = c("Deadwood","Tree hole","Leaf litter"))
micro_night$microhabitat <-
  factor(micro_night$microhabitat,
         levels = c("Deadwood","Tree hole","Leaf litter"))

# Add panel labels
micro_day$top_labs <- 
  ifelse(micro_day$microhabitat == "Deadwood","(a)",
         ifelse(micro_day$microhabitat == "Tree hole", "(b)","(c)"))
micro_night$top_labs <- 
  ifelse(micro_night$microhabitat == "Deadwood","(d)",
         ifelse(micro_night$microhabitat == "Tree hole", "(e)","(f)"))

micro <- rbind(micro_day,micro_night)

micro$day_night <- factor(micro$day_night,
                          levels = c("day","night"),
                          labels = c("Day", "Night"))

# Daytime micro vs. macro -------------------------------------------------

# primary_BA <- unique(micro$primary_BA)
# logged_BA <- unique(micro$logged_BA)

figS4_top <-
  sapply(day_df, function(x){
    
    result <- pred(dat = x,
                   df_name = x,
                   results_df = q1_day_results,
                   RV = "rep_temp",
                   vary_EV = "ambient_log",
                   constant_EV = "tree_stand_BA",
                   constant_val = treeBA_median)
  })

figS4_top <- do.call("rbind", figS4_top)

# Change response variable name
colnames(figS4_top)[names(figS4_top)=="RV"] <- "rep_temp"

# Add microhabitat variable
figS4_top$microhabitat <- gsub("_rep_day","",figS4_top$dat)
figS4_top$microhabitat <-
  factor(figS4_top$microhabitat,
         levels = c("deadwood","hole","litter"),
         labels = c("Deadwood", "Tree hole", "Leaf litter"))

# Add time of day
figS4_top$day_night <- "Day"

# Get ambient
figS4_top$ambient <- exp(figS4_top$ambient_log)


# Nighttime micro vs. macro -----------------------------------------------

figS4_bottom <-
  sapply(night_df, function(x){
    
    result <- pred(dat = x,
                   df_name = x,
                   results_df = q1_night_results,
                   RV = "rep_temp",
                   vary_EV = "ambient_log",
                   constant_EV = "tree_stand_BA",
                   constant_val = treeBA_median)
  })

figS4_bottom <- do.call("rbind", figS4_bottom)

# Change response variable name
colnames(figS4_bottom)[names(figS4_bottom)=="RV"] <- "rep_temp"

# Add microhabitat variable
figS4_bottom$microhabitat <- gsub("_rep_night","",figS4_bottom$dat)
figS4_bottom$microhabitat <-
  factor(figS4_bottom$microhabitat,
         levels = c("deadwood","hole","litter"),
         labels = c("Deadwood", "Tree hole", "Leaf litter"))

# Add time of day
figS4_bottom$day_night <- "Night"

# Get ambient
figS4_bottom$ambient <- exp(figS4_bottom$ambient_log)


# Plot! -------------------------------------------------------------------

figS4 <- rbind(figS4_top,figS4_bottom)

figS4$day_night <- factor(figS4$day_night,
                          levels = c("Day", "Night"))

# Plot!
p <- plot_continuous(pred_df = figS4,
                     raw_df = micro,
                     RV = "rep_temp",
                     vary_EV = "ambient",
                     constant_EV = "tree_stand_BA",
                     x_lab = paste("Macroclimate temperature (", "\U00B0","C)",sep=""),
                     y_lab = paste("Microclimate temperature (", "\U00B0","C)",sep=""), 
                     facetting = "day_night ~ microhabitat",
                     panel_labs = "",
                     panel.spacing = 0.1,
                     facet_scale = "free",
                     point_alpha = 0.2,
                     override_alpha = FALSE,
                     leg_pos = "top", 
                     lab_size = 2.8,
                     lab_x_pos = "x_min",
                     lab_hjust = -0.3,
                     lab_vjust = 1.45,
                     bw = FALSE)

combi <-
    ggdraw() +
    draw_plot(p, x = 0, y = 0, width = 1, height = 1) +
    draw_plot_label(label = c("(a)", "(b)", "(c)", 
                              "(d)", "(e)", "(f)"),
                    x = c(0.075, 0.368, 0.66,
                          0.075, 0.368, 0.663),
                    y = c(rep(0.79, 3), rep(0.445, 3)),
                    size = 7)

ggsave(plot = combi, filename = "figs/test.png", 
       dpi = 200, width = 16.6/2.54, height = 9/2.54, units = "in")

saveRDS(combi,file = "figs/figB.4.Rds")







