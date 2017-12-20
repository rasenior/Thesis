# Setup -------------------------------------------------------------------

rm(list=ls())

# Get the current user (Rebecca or Becc, if work or home)
user<-Sys.info()[7]

# Set working directory
base_dir <-
  file.path("C:/Users",user,
            "Google Drive/PhD/Chapter 2/Resubmission")
dat_dir <- file.path(base_dir,"Data/FINAL")
mod_dir <- file.path(dat_dir, "models")
fig_dir <- file.path(base_dir,"Figures/results")
setwd(dat_dir)

# Load libraries
library(ggplot2)
library(cowplot)

# Load raw data
load("deadwood_all_2017-07-27.Rds")
load("hole_all_2017-07-27.Rds")
load("litter_all_2017-07-27.Rds")
treeBA_median <- readRDS("treeBA_median.rds")

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
q1_results <- readRDS(file.path(mod_dir, "q1_som_results.Rds"))

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
                 "Google Drive/R/functions/mround.R"))
source(file.path(base_dir, "Code/Plotting/plotting_fn.R"))

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
                     panel_labs = "top_labs",
                     panel.spacing = 0.1,
                     scale.y = "free",
                     point_alpha = 0.2,
                     override_alpha = FALSE,
                     leg_pos = "top", 
                     lab_size = 2.8,
                     lab_x_pos = "x_min",
                     lab_hjust = -0.3,
                     lab_vjust = 1.45,
                     bw = FALSE)

saveRDS(p,file = file.path(fig_dir,"figS4_micro_vs_macro.Rds"))

ggsave(plot = p, filename = file.path(fig_dir,"figS4_micro_vs_macro.png"),
       dpi=800,width=16.6,height=13,units="cm")

# Summary stats - interaction (day) ---------------------------------------------

# What is the interaction effect in each microhabitat?
# I.e. what is the difference between forest types in microclimate temp. increase 
#      for 1 degree macroclimate temp. increase

# Define datasets with sig. interaction
day_sig_dat <- c("hole_rep_day", "litter_rep_day")

day_inter_sum<-
  sapply(day_sig_dat, function(x){
    
    dat_eval <- eval(parse(text = x))
    vary_seq = c(log(median(dat_eval[,"ambient"])),
                 log(median(dat_eval[,"ambient"])+1))
    
    result<- 
      pred(dat = x,
           df_name = x,
           results_df = q1_day_results,
           RV = "rep_temp",
           vary_EV = "ambient_log",
           vary_seq = vary_seq,
           constant_EV = "tree_stand_BA",
           constant_val = treeBA_median)$newdat
    
    # Calculate difference for each forest type
    result$RV_diff <- NA
    result$RV_diff[result$forest_type == "Primary"] <-
      max(result$RV[result$forest_type == "Primary"]) - min(result$RV[result$forest_type == "Primary"])
    result$RV_diff[result$forest_type == "Logged"] <-
      max(result$RV[result$forest_type == "Logged"]) - min(result$RV[result$forest_type == "Logged"])
    
    result$forest_effect <-
      unique(result$RV_diff[result$forest_type == "Primary"]) -
      unique(result$RV_diff[result$forest_type == "Logged"])
    
    return(list(result = result))
    
  })

day_inter_sum <- do.call("rbind", day_inter_sum)

# Change response variable name
colnames(day_inter_sum)[names(day_inter_sum)=="RV"] <- "micro_temp"
colnames(day_inter_sum)[names(day_inter_sum)=="RV_diff"] <- "micro_diff"

# Add microhabitat variable
day_inter_sum$microhabitat <- gsub("_rep_day","",day_inter_sum$dat)
day_inter_sum$microhabitat <-
  factor(day_inter_sum$microhabitat,
         levels = c("deadwood", "hole", "litter"),
         labels = c("Deadwood", "Tree hole", "Leaf litter"))

# Re-level forest type
day_inter_sum$forest_type <- factor(day_inter_sum$forest_type,
                                levels <- c("Primary", "Logged"))

# Get ambient
day_inter_sum$ambient <- exp(day_inter_sum$ambient_log)

# Add day/night variable
day_inter_sum$day_night <- "day"

# Summary stats - interaction (night) ---------------------------------------------

# What is the interaction effect in each microhabitat?
# I.e. what is the difference between forest types in microclimate temp. increase 
#      for 1 degree macroclimate temp. increase

# Define datasets with sig. interaction
night_sig_dat <- c("deadwood_rep_night", "litter_rep_night")

night_inter_sum<-
  sapply(night_sig_dat, function(x){
    
    dat_eval <- eval(parse(text = x))
    vary_seq = c(log(median(dat_eval[,"ambient"])),
                 log(median(dat_eval[,"ambient"])+1))
    
    result<- 
      pred(dat = x,
           df_name = x,
           results_df = q1_night_results,
           RV = "rep_temp",
           vary_EV = "ambient_log",
           vary_seq = vary_seq,
           constant_EV = "tree_stand_BA",
           constant_val = treeBA_median)$newdat
    
    # Calculate difference for each forest type
    result$RV_diff <- NA
    result$RV_diff[result$forest_type == "Primary"] <-
      max(result$RV[result$forest_type == "Primary"]) - min(result$RV[result$forest_type == "Primary"])
    result$RV_diff[result$forest_type == "Logged"] <-
      max(result$RV[result$forest_type == "Logged"]) - min(result$RV[result$forest_type == "Logged"])
    
    result$forest_effect <-
      unique(result$RV_diff[result$forest_type == "Primary"]) -
      unique(result$RV_diff[result$forest_type == "Logged"])
    
    return(list(result = result))
    
  })

night_inter_sum <- do.call("rbind", night_inter_sum)

# Change response variable name
colnames(night_inter_sum)[names(night_inter_sum)=="RV"] <- "micro_temp"
colnames(night_inter_sum)[names(night_inter_sum)=="RV_diff"] <- "micro_diff"

# Add microhabitat variable
night_inter_sum$microhabitat <- gsub("_rep_night","",night_inter_sum$dat)
night_inter_sum$microhabitat <-
  factor(night_inter_sum$microhabitat,
         levels = c("deadwood", "hole", "litter"),
         labels = c("Deadwood", "Tree hole", "Leaf litter"))

# Re-level forest type
night_inter_sum$forest_type <- factor(night_inter_sum$forest_type,
                                    levels <- c("Primary", "Logged"))

# Get ambient
night_inter_sum$ambient <- exp(night_inter_sum$ambient_log)

# Add night/night variable
night_inter_sum$day_night <- "night"


# Summary stats - bind interaction summaries ------------------------------

inter_sum <- rbind(day_inter_sum, night_inter_sum)


# Summary stats - tree BA -------------------------------------------------

# What is the effect of tree BA on tree hole temp.?
# I.e. what is the difference in tree hole temp. for one unit increase in tree BA?
hole_sum <- pred(dat = "litter_rep_day",
                        df_name = "litter_rep_day",
                        results_df = q1_day_results,
                        RV = "rep_temp",
                        vary_EV = "tree_stand_BA",
                        vary_seq = c(treeBA_median, treeBA_median + 1),
                        constant_EV = "ambient_log",
                        constant_val = "median")$newdat

# Calculate difference for each forest type
hole_sum$RV_diff <- NA

hole_sum$RV_diff[hole_sum$forest_type == "Primary"] <-
  max(hole_sum$RV[hole_sum$forest_type == "Primary"]) - 
  min(hole_sum$RV[hole_sum$forest_type == "Primary"])

hole_sum$RV_diff[hole_sum$forest_type == "Logged"] <-
  max(hole_sum$RV[hole_sum$forest_type == "Logged"]) - 
  min(hole_sum$RV[hole_sum$forest_type == "Logged"])

hole_sum$forest_effect <-
  hole_sum$RV[hole_sum$forest_type == "Primary" & hole_sum$tree_stand_BA == treeBA_median]-
  hole_sum$RV[hole_sum$forest_type == "Logged" & hole_sum$tree_stand_BA == treeBA_median]

colnames(hole_sum)[names(hole_sum)=="RV"] <- "micro_temp"
colnames(hole_sum)[names(hole_sum)=="RV_diff"] <- "micro_diff"


# Write -------------------------------------------------------------------

write.csv(inter_sum, "predictions/figS4_sig_interactions.csv")
write.csv(hole_sum, "predictions/figS4_sig_effects.csv")



