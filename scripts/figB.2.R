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
library(lme4)
library(ggplot2)
library(cowplot)

# Load raw data
macrohabitat <- readRDS(file.path(dat_dir,"plot_info_2017-07-27.Rds"))

# Load models
load(file.path(mod_dir, "som_q1_results.Rdata"))

# Source local functions
source(file.path("C:/Users",user,
                 "Google Drive/Programming/R/functions/mround.R"))
source(file.path("C:/Users",user,
                 "Google Drive/Programming/R/functions/empirical_logit_transformation.R"))
source("scripts/plotting_fn.R")
# Define font sizes
lab_size <- 7

# Prepare data ------------------------------------------------------------

colnames(macrohabitat)[names(macrohabitat)=="dipt%"] <- "dipt_prop"
colnames(macrohabitat)[names(macrohabitat)=="canopy%"] <- "canopy_cover"
colnames(macrohabitat)[names(macrohabitat)=="groundVeg%"] <- "ground_veg"
colnames(macrohabitat)[names(macrohabitat)=="underVeg%"] <- "under_veg"
colnames(macrohabitat)[names(macrohabitat)=="canopyVeg%"] <- "canopy_veg"

ground_veg <- macrohabitat[,c("plotID","forest_type","ground_veg")]
under_veg <- macrohabitat[,c("plotID","forest_type","under_veg")]
canopy_veg <- macrohabitat[,c("plotID","forest_type","canopy_veg")]

ground_veg$stratum <- "Ground"
under_veg$stratum <- "Understorey"
canopy_veg$stratum <- "Canopy"

colnames(ground_veg) <- c("plotID","forest_type","veg_cover","stratum")
colnames(under_veg) <- c("plotID","forest_type","veg_cover","stratum")
colnames(canopy_veg) <- c("plotID","forest_type","veg_cover","stratum")

veg <- rbind(ground_veg, under_veg, canopy_veg)

veg$stratum <- factor(veg$stratum,
                      levels = c("Ground","Understorey","Canopy"))

veg$bottom_labs <-
    ifelse(veg$stratum == "Ground","(i)",
           ifelse(veg$stratum == "Understorey", "(j)","(k)"))

veg$sig_level <-
    ifelse(veg$stratum == "Ground","",
           ifelse(veg$stratum == "Understorey", "*","**"))

# Plot tree BA ------------------------------------------------------------

figS2_d <- do.call("rbind",
                   pred(dat = "macrohabitat",
                        df_name = "tree_stand_BA",
                        results_df = q1_som_results,
                        RV = "tree_stand_BA",
                        vary_EV = "forest_type",
                        vary_EV_class = "factor",
                        constant_EV = "none",
                        constant_val = "NA"))

# Change response variable name
colnames(figS2_d)[names(figS2_d)=="RV"] <- "tree_stand_BA"

p5_d <- plot_categorical(pred_df = figS2_d, 
                         raw_df = macrohabitat, 
                         sig_level = "**",
                         sig_y = 90,
                         RV = "tree_stand_BA", 
                         EV = "forest_type",
                         x_lab = "", 
                         y_lab = paste("Tree stand BA (m","\U00B2","/ha)",sep=""), 
                         x_axis = FALSE,
                         facet_micro = FALSE,
                         panel_head = FALSE,
                         panel_labs = "(d)",
                         panel.spacing = 0.2,
                         facet_scale = "free", 
                         point_alpha = 0.7,
                         lab_hjust = 1.7,
                         lab_vjust = 1.44)



# Plot sapling BA ---------------------------------------------------------

figS2_e <- do.call("rbind",pred(dat = "macrohabitat",
                                df_name = "sapling_stand_BA",
                                results_df = q1_som_results,
                                RV = "sapling_stand_BA",
                                vary_EV = "forest_type",
                                vary_EV_class = "factor",
                                constant_EV = "none",
                                constant_val = "NA"))

# Change response variable name
colnames(figS2_e)[names(figS2_e)=="RV"] <- "sapling_stand_BA"

p5_e <- plot_categorical(pred_df = figS2_e, 
                         raw_df = macrohabitat, 
                         sig_level = "*",
                         sig_y = 0.002,
                         RV = "sapling_stand_BA", 
                         EV = "forest_type",
                         x_lab = "", 
                         y_lab = paste("Sapling stand BA (m","\U00B2","/ha)",sep=""), 
                         x_axis = FALSE,
                         facet_micro = FALSE,
                         panel_labs = "(e)",
                         panel.spacing = 0.2,
                         panel_head = FALSE,
                         facet_scale = "free", 
                         point_alpha = 0.7,
                         lab_hjust = 1.7,
                         lab_vjust = 1.44)

# Plot tree BA CV ------------------------------------------------------------

figS2_f <- do.call("rbind",pred(dat = "macrohabitat",
                                df_name = "treeBA_cv",
                                results_df = q1_som_results,
                                RV = "treeBA_cv",
                                vary_EV = "forest_type",
                                vary_EV_class = "factor",
                                constant_EV = "none",
                                constant_val = "NA"))

# Change response variable name
colnames(figS2_f)[names(figS2_f)=="RV"] <- "treeBA_cv"

p5_f <- plot_categorical(pred_df = figS2_f, 
                         raw_df = macrohabitat, 
                         sig_level = "**",
                         sig_y = 90,
                         RV = "treeBA_cv", 
                         EV = "forest_type",
                         x_lab = "", 
                         y_lab = "Tree basal area CV", 
                         x_axis = FALSE,
                         facet_micro = FALSE,
                         panel_head = FALSE,
                         panel_labs = "(f)",
                         panel.spacing = 0.2,
                         facet_scale = "free", 
                         point_alpha = 0.7,
                         lab_hjust = 1.7,
                         lab_vjust = 1.44)



# Plot sapling BA CV ---------------------------------------------------------

figS2_g <- do.call("rbind",pred(dat = "macrohabitat",
                                df_name = "saplingBA_cv",
                                results_df = q1_som_results,
                                RV = "saplingBA_cv",
                                vary_EV = "forest_type",
                                vary_EV_class = "factor",
                                constant_EV = "none",
                                constant_val = "NA"))

# Change response variable name
colnames(figS2_g)[names(figS2_g)=="RV"] <- "saplingBA_cv"

p5_g <- plot_categorical(pred_df = figS2_g, 
                         raw_df = macrohabitat, 
                         sig_level = "*",
                         sig_y = 0.002,
                         RV = "saplingBA_cv", 
                         EV = "forest_type",
                         x_lab = "", 
                         y_lab = "Sapling basal area CV", 
                         x_axis = FALSE,
                         facet_micro = FALSE,
                         panel_labs = "(g)",
                         panel.spacing = 0.2,
                         panel_head = FALSE,
                         facet_scale = "free", 
                         point_alpha = 0.7,
                         lab_hjust = 1.7,
                         lab_vjust = 1.44)

# Plot dipt. prop ---------------------------------------------------------

figS2_h <- do.call("rbind",pred(dat = "macrohabitat",
                                df_name = "dipt_prop",
                                results_df = q1_som_results,
                                RV = "dipt_prop",
                                mod_family = "binomial",
                                inc_TBA = FALSE,
                                vary_EV = "forest_type",
                                vary_EV_class = "factor",
                                constant_EV = "none",
                                constant_val = "NA"))

# Change response variable name
colnames(figS2_h)[names(figS2_h)=="RV"] <- "dipt_prop"
figS2_h$dipt_prop <- (figS2_h$dipt_prop) * 100
figS2_h$plo <- (figS2_h$plo) * 100
figS2_h$phi <- (figS2_h$phi) * 100

p5_h <- plot_categorical(pred_df = figS2_h, 
                         raw_df = macrohabitat, 
                         sig_level = "",
                         sig_y = 0.002,
                         RV = "dipt_prop", 
                         EV = "forest_type",
                         x_lab = "", 
                         y_lab = "Dipterocarp prop. (%)", 
                         x_axis = FALSE,
                         facet_micro = FALSE,
                         panel_labs = "(h)",
                         panel_head = FALSE,
                         panel.spacing = 0.2,
                         facet_scale = "free", 
                         point_alpha = 0.7,
                         lab_hjust = 1.7,
                         lab_vjust = 1.44)

# Plot canopy cover ---------------------------------------------------------

figS2_c <- do.call("rbind",pred(dat = "macrohabitat",
                                df_name = "canopy_cover_logit",
                                results_df = q1_som_results,
                                RV = "canopy_cover_logit",
                                vary_EV = "forest_type",
                                vary_EV_class = "factor",
                                constant_EV = "none",
                                constant_val = "NA"))

# Change response variable name
colnames(figS2_c)[names(figS2_c)=="RV"] <- "canopy_cover_logit"

# Use eps from raw data to back-transform
canopy_cover_eps <- unique(macrohabitat$canopy_cover_eps)

figS2_c[,c("canopy_cover","plo","phi")] <-
    sapply(c("canopy_cover_logit","plo","phi"), function(x){
        
        result <- logit_trans(y_trans = figS2_c[,x], 
                              eps = canopy_cover_eps, 
                              nearOne = TRUE, 
                              reverse = TRUE)[,"results"]
        result <- result *100
        return(result)
        
    })

p5_c <- plot_categorical(pred_df = figS2_c, 
                         raw_df = macrohabitat, 
                         sig_level = "",
                         sig_y = 0.002,
                         RV = "canopy_cover", 
                         EV = "forest_type",
                         x_lab = "", 
                         y_lab = "Canopy cover (%)", 
                         x_axis = FALSE,
                         facet_micro = FALSE,
                         panel_head = FALSE,
                         panel_labs = "(c)",
                         panel.spacing = 0.2,
                         facet_scale = "free", 
                         point_alpha = 0.7,
                         lab_hjust = 1.7,
                         lab_vjust = 1.44)

# Plot veg cover ----------------------------------------------------------


figS2_bottom <- 
    sapply(c("ground_veg","under_veg","canopy_veg"), 
           function(x){
               
               RV <- paste(x,"logit",sep="_")
               eps <- unique(macrohabitat[,paste(x,"eps",sep="_")])
               
               results <- pred(dat = "macrohabitat",
                               df_name = RV,
                               results_df = q1_som_results,
                               RV = RV,
                               vary_EV = "forest_type",
                               vary_EV_class = "factor",
                               constant_EV = "none",
                               constant_val = "NA")
               
               if(x == "canopy_veg"){
                   nearOne = FALSE
               } else{
                   nearOne = TRUE
               }
               
               results$newdat[,c("veg_cover","plo","phi")] <-
                   
                   sapply(c("RV","plo","phi"), function(x){
                       
                       result <- logit_trans(y_trans = results$newdat[,x], 
                                             eps = eps, 
                                             nearOne = nearOne, 
                                             reverse = TRUE)[,"results"]
                       result <- result *100
                       return(result)
                       
                   })
               
               return(results = results)
               
           })

figS2_bottom <- do.call("rbind",figS2_bottom)
figS2_bottom$stratum <- gsub("_veg_logit","", figS2_bottom$dat)
figS2_bottom$stratum <- factor(figS2_bottom$stratum,
                               levels = c("ground","under","canopy"),
                               labels = c("Ground","Understorey","Canopy"))
figS2_bottom <- figS2_bottom[names(figS2_bottom)!= "RV"]

p5_bottom <- plot_categorical(pred_df = figS2_bottom, 
                              raw_df = veg, 
                              sig_level = "",
                              sig_y = 90,
                              RV = "veg_cover", 
                              EV = "forest_type",
                              x_lab = "", 
                              y_lab = "Vegetation cover (%)", 
                              x_axis = TRUE,
                              facet_micro = "stratum",
                              panel_labs = "bottom_labs",
                              panel.spacing = 0.2,
                              facet_scale = "free", 
                              point_alpha = 0.7,
                              lab_hjust = 1.7,
                              lab_vjust = 1.44)

p5_bottom <- p5_bottom + 
    theme(axis.text.x = element_text(size = 10,
                                     colour = "black"))

# Combine and save ----------------------------------------------------------------

p5_top <- plot_grid(p5_d,p5_e,p5_f,p5_g,p5_h,p5_c,ncol = 2,align = "hv")

set.seed(383)
combi <-
    ggdraw() +
    draw_plot(p5_top, x = 0, y = 1/3, width = 1, height = 2/3) +
    draw_plot(p5_bottom, x = 0.008, y = 0, width = 1 - 0.008, height = 1/3)+
    draw_plot_label(label = paste("(",letters[1:(length(q1_som_results)-2)],")",sep=""),
                    x = c(
                        # 0.075, 0.572,
                        0.080, 0.580,
                        0.080, 0.580,
                        0.080, 0.582,
                        0.087, 0.390, 0.695
                    ),
                    y = c(
                        # 0.871, 0.871,
                        rep(0.836, 2),
                        rep(0.614, 2),
                        rep(0.392, 2),
                        rep(0.075, 3)
                    ),
                    size= lab_size)+
    draw_plot_label(label = c(
        # "**"  , "*",
        "**" , "*" ,
        "***",
        
        "*"  , "**"),
        x = c(
            # 0.270, 0.770,
            0.268, 0.772,
            0.260,
            0.528, 0.825
        ),
        y = c(
            0.99,  0.99,
            # 0.805, 0.805,
            0.770,
            0.295, 0.295
        ),
        size = 11)

saveRDS(combi,file = "figs/figB.2.Rds")

# Write -------------------------------------------------------------------

# write.csv(figS2_a,"predictions/figS2a.csv")
# write.csv(figS2_b,"predictions/figS2b.csv")
write.csv(figS2_c,"data/ch4/predictions/figS2c.csv")
write.csv(figS2_d,"data/ch4/predictions/figS2d.csv")
write.csv(figS2_e,"data/ch4/predictions/figS2e.csv")
write.csv(figS2_f,"data/ch4/predictions/figS2f.csv")
write.csv(figS2_bottom,"data/ch4/predictions/figS2_bottom.csv")


