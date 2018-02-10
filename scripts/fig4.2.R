# Setup -------------------------------------------------------------------

rm(list=ls())

library(Thermimage)
devtools::install_github("rasenior/PatchStatsFLIR")
library(PatchStatsFLIR)
library(RColorBrewer)
library(ggplot2)

# Get patches -------------------------------------------------------------

example <- 
  get_patches(flir_matrix = flir11835$flir_matrix,photo_no = flir11835$photo_no)

example_df <- example$flir_df
example_patches <- example$patches


# Plot in colour ----------------------------------------------------------

p_col <- plot_patches(flir_df = example_df, 
                      patches = example_patches, 
                      plot_distribution = FALSE,
                      print_plot = FALSE,
                      return_plot = TRUE,
                      save_plot = FALSE,
                      patch_labs = c("Warm patches","Cool patches"))

saveRDS(p_col,file = "figs/fig4.2.Rds")
