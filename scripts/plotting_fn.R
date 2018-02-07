
#  Random useful plotting stuff -------------------------------------------

# Define colourblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")

title_size<-8
leg_size<-8
text_size<-6

# Inverse-logit function --------------------------------------------------

inv.logit <- function(x){
  exp(x)/(1+exp(x))
}

# Function for extracting legend -----------------------------------
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Function for predicting fitted vals ------------------------------

pred <- function(dat,df_name, results_df,mod_family = "gaussian",RV, 
                 vary_EV, vary_EV_class = "numeric",
                 vary_seq = NULL,
                 constant_EV, constant_val = "median"){
  
  dat <- eval(parse(text = dat))
  
  mod <- results_df[[paste(df_name,"results",sep = ".")]][["main_mod"]]
  
  if(length(constant_val) == 1){
    if(constant_val == "median"){
      constant_val <- median(dat[,constant_EV])
    }
  }
  
  if(vary_EV_class == "factor"){
    vary_EV_vals <- levels(dat[,vary_EV])
  }else{
    if(is.null(vary_seq)){
      vary_EV_vals <- seq(min(dat[,vary_EV]),
                          max(dat[,vary_EV]),
                          length.out = 100)
    }else{
      vary_EV_vals <- vary_seq
    }
  }
  
  newdat <-
    expand.grid(vary_EV = vary_EV_vals,
                forest_type = levels(dat[,"forest_type"]),
                constant_EV = constant_val,
                RV = 0)
  
  names(newdat) <- c(vary_EV, "forest_type", constant_EV, RV)
  newdat <- newdat[,names(newdat) != "none"]

  # Remove values that exceed limits for each forest type
  primary_min_EV <- min(dat[dat[,"forest_type"] == "Primary",vary_EV])
  primary_max_EV <- max(dat[dat[,"forest_type"] == "Primary",vary_EV])
  logged_min_EV <- min(dat[dat[,"forest_type"] == "Logged",vary_EV])
  logged_max_EV <- max(dat[dat[,"forest_type"] == "Logged",vary_EV])
  
  newdat_primary <- 
    newdat[newdat$forest_type == "Primary" & newdat[,vary_EV] >= primary_min_EV & newdat[,vary_EV] <= primary_max_EV,]
  
  newdat_logged <- 
    newdat[newdat$forest_type == "Logged" & newdat[,vary_EV] >= logged_min_EV & newdat[,vary_EV] <= logged_max_EV,]
  
  newdat<-rbind(newdat_primary, newdat_logged)
  
  # Re-level forest type
  newdat$forest_type <- factor(newdat$forest_type,
                               levels <- c("Primary", "Logged"))
  
  if(mod_family == "binomial"){
    RV_logit <- paste(RV,"logit",sep="_")
    
    # Confidence intervals & back-transformation
    mm <- model.matrix(~ tree_stand_BA + forest_type,newdat)
    newdat[,RV_logit] <- mm%*%fixef(mod)
    pvar1 <- diag(mm %*% tcrossprod(vcov(mod),mm))
    
    newdat[,eval(RV)]<- inv.logit(newdat[,RV_logit])
    newdat[,"plo"]<- inv.logit(newdat[,RV_logit] - 1.96*sqrt(pvar1))
    newdat[,"phi"]<- inv.logit(newdat[,RV_logit] + 1.96*sqrt(pvar1))
    
  }else{
    newdat[,RV] <- predict(mod,newdat,re.form=NA,level = 0)
    
    # Confidence intervals
    mm <- model.matrix(terms(mod),newdat)
    pvar1 <- diag(mm %*% tcrossprod(vcov(mod),mm))
    newdat <- data.frame(
      newdat,
      plo = newdat[,RV] - 1.96 * sqrt(pvar1),
      phi = newdat[,RV] + 1.96 * sqrt(pvar1)
    )
    
  }
  
  # Add the name of the dataframe
  newdat[,"dat"] <- df_name
  
  # Change the RV name to something standardised
  colnames(newdat)[names(newdat) == RV] <- "RV"
  
  return(list(newdat = newdat))
  
}


# Function for continuous base plot -------------------------------------------

plot_continuous <- function(pred_df, 
                            raw_df, 
                            RV, 
                            vary_EV, 
                            constant_EV, 
                            x_lab, 
                            y_lab,
                            facetting = "~ microhabitat",
                            panel_labs,
                            panel.spacing = 0.2,
                            scale.y,
                            point_alpha,
                            override_alpha = FALSE,
                            leg_pos,
                            lab_x_pos = x_max,
                            lab_size = 3,
                            lab_hjust, 
                            lab_vjust,
                            bw = TRUE){
  
  x_min <- mround((min(raw_df[,vary_EV])), 10, FUN = floor)
  x_max <- mround((max(raw_df[,vary_EV])), 10, FUN = ceiling)
  x_range <- x_max - x_min
  x_incr <- 
    ifelse(x_range <= 10, 1,
           ifelse(x_range > 10 & x_range <= 20, 2,
                  ifelse(x_range > 20 & x_range <= 50, 5, 10)))
  
  
  if(leg_pos == "return"){
    leg_pos <- "top"
    leg_return <- TRUE
  }else{
    leg_return <- FALSE
  }
  
  # Plot raw points
  p <- ggplot(raw_df, aes(x = eval(parse(text = vary_EV)), 
                          y = eval(parse(text = RV))))+
    geom_point(aes(colour = forest_type, shape = forest_type), 
               alpha = point_alpha)+
    geom_line(data = pred_df,
              aes(colour = forest_type))+
    geom_ribbon(data = pred_df, 
                aes(ymin = plo, ymax = phi, fill = forest_type), alpha = 0.2)+
    xlab(x_lab)+
    ylab(y_lab)+
    theme_bw()+
    theme(axis.title.x=element_text(margin = margin(6,0,0,0),
                                    size=title_size),
          axis.title.y=element_text(margin = margin(0,6,0,0),
                                    size=title_size),
          axis.text.x=element_text(size=text_size),
          axis.text.y=element_text(size=text_size),
          strip.text=element_text(size=title_size),
          panel.spacing = unit(panel.spacing, unit = "cm"), 
          legend.title=element_blank(),
          legend.text=element_text(size=leg_size),
          legend.position= leg_pos,
          legend.key=element_blank(),
          legend.key.size=unit(10,"pt"),
          panel.grid=element_blank())+
    scale_x_continuous(breaks=seq(x_min, x_max, x_incr))+ 
    scale_colour_manual(values=c(cbPalette[6],cbPalette[7]))+
    scale_fill_manual(values=c(cbPalette[6],cbPalette[7]))
  
  # If variable held constant is treeBA, there are two vals so 
  # plotted differently
  if(constant_EV == "tree_stand_BA"){
    p <- p +
      geom_abline(colour = "grey70", linetype = "dashed")
  } 
  # else{
  #   p <- p +
  #     geom_line(data = pred_df)
  # }
  
  if(facetting == "~ microhabitat"){
    p <- p + 
      facet_wrap(~ microhabitat, nrow = 1, scales = scale.y)
    
  } else if (facetting == "day_night ~ microhabitat"){
    p <- p + 
      facet_grid(day_night ~ microhabitat, scales = scale.y)
  } else if(facetting =="~ instrument"){
    p <- p + 
      facet_wrap(~ instrument, nrow = 1, scales = scale.y)
  }
  
  if(override_alpha){
    p <- p +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) 
  }
  
  if(panel_labs == "top_labs"){
    p <- p +
      geom_text(aes(label = top_labs),
                x= eval(parse(text = lab_x_pos)),y=Inf,vjust=lab_vjust,hjust= lab_hjust,
                size= lab_size,colour="black", fontface = "plain")
  } else if(panel_labs == "bottom_labs"){
    p <- p +
      geom_text(aes(label = bottom_labs),
                x= eval(parse(text = lab_x_pos)),y=Inf,vjust=lab_vjust,hjust=lab_hjust,
                size= lab_size,colour="black", fontface = "plain")
  } else{
    p <- p +
      geom_text(aes(label = panel_labs),
                x= eval(parse(text = lab_x_pos)),y=Inf,vjust=lab_vjust,hjust=lab_hjust,
                size= lab_size,colour="black", fontface = "plain")
  }
  
  if(scale.y == "fixed"){
    y_min <- mround((min(raw_df[,RV],na.rm = TRUE)), 10, FUN = floor)
    y_max <- mround((max(raw_df[,RV],na.rm = TRUE)), 10, FUN = ceiling)
    y_range <- y_max - y_min
    y_incr <- 
      ifelse(y_range <= 10, 1,
             ifelse(y_range > 10 & y_range <= 20, 2,
                    ifelse(y_range > 20 & y_range <= 50, 5, 
                           ifelse(y_range > 50 & y_range <= 100, 10,
                                  ifelse(y_range > 100 & y_range <= 1000,100,500)))))
    p <- p +
      scale_y_continuous(breaks=seq(y_min, y_max, y_incr))
    
  }
  
  # If a b&w version should be returned as well, make it here
  if(bw){
    
    if(isTRUE(leg_return)){
      p_bw <- p +
        scale_colour_manual(values=c("black","grey70"))+
        scale_fill_manual(values=c("black","grey70"))
      
      pbw_leg <- g_legend(p_bw)
      
      p_bw <- p_bw+
        theme(legend.position = "none")
      
      p_bw = list(p_bw = p_bw, pbw_leg = pbw_leg)
      
    } else {
      p_bw <- p +
        scale_colour_manual(values=c("black","grey70"))+
        scale_fill_manual(values=c("black","grey70"))
    }
  }
  
  if(isTRUE(leg_return)){
    p_leg <- g_legend(p)
    p <- p+
      theme(legend.position = "none")
    p = list(p = p, p_leg = p_leg)
    
  } else {
    p <- p+
      theme(legend.position = leg_pos)
  }
  
  
  if(bw){
    return(list(p = p, p_bw = p_bw))
  }else{
    return(p = p)
  }
  
}


# Function for categorical base plot -------------------------------------------

plot_categorical <- function(pred_df, 
                             raw_df, 
                             RV, 
                             EV,
                             sig_level,
                             sig_y,
                             x_lab, 
                             y_lab, 
                             x_axis = TRUE,
                             facet_micro = FALSE,
                             panel_head = TRUE,
                             panel_labs,
                             panel.spacing = 0.2,
                             scale.y = "free", 
                             point_alpha = 1,
                             lab_hjust, 
                             lab_vjust){
  
  # Plot raw points
  p <- ggplot(raw_df, aes(x = eval(parse(text = EV)), 
                          y = eval(parse(text = RV))))+
    geom_jitter(aes(colour = forest_type),
                alpha = point_alpha)+
    geom_point(data = pred_df,size=2)+
    geom_errorbar(data = pred_df, aes(ymin = plo, ymax = phi),
                  width = 0.2)+
    # geom_text(aes(label = sig_level),
    #           x= 2,y= sig_y,
    #           size=3,fontface="bold",colour="black")+
    xlab(x_lab)+
    ylab(y_lab)+
    theme_bw()+
    theme(axis.title.x=element_text(margin = margin(6,0,0,0),
                                    size=title_size),
          axis.title.y=element_text(margin = margin(0,6,0,0),
                                    size=title_size),
          axis.text.x=element_text(size=text_size),
          axis.text.y=element_text(size=text_size),
          strip.text=element_text(size=title_size),
          panel.spacing = unit(panel.spacing, unit = "cm"), 
          legend.title=element_blank(),
          legend.text=element_text(size=leg_size),
          legend.position= "none",
          legend.key=element_blank(),
          legend.key.size=unit(10,"pt"),
          panel.grid=element_blank())+
    scale_colour_manual(values=c(cbPalette[6],cbPalette[7]))
  
  if(!(is.null(facet_micro))){
    p <- p+
      facet_wrap(~ eval(parse(text = facet_micro)))
  }
  
  if(!(x_axis)){
    p <- p +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }
  
  if(!(panel_head)){
    p <- p +
      theme(strip.background = element_blank(),
            strip.text = element_blank())
  }
  
  # if(panel_labs == "top_labs"){
  #   p <- p +
  #     geom_text(aes(label = top_labs),
  #               x= 2,y=Inf,vjust=lab_vjust,hjust= lab_hjust,
  #               size=3,fontface="bold",colour="black")
  # } else if(panel_labs == "bottom_labs"){
  #   p <- p +
  #     geom_text(aes(label = bottom_labs),
  #               x= 2,y=Inf,vjust=lab_vjust,hjust=lab_hjust,
  #               size=3,fontface="bold",colour="black")
  # } else {
  #   p <- p +
  #     geom_text(aes(label = panel_labs),
  #               x= 2,y=Inf,vjust=lab_vjust,hjust=lab_hjust,
  #               size=3,fontface="bold",colour="black")
  # }
  
  if(scale.y == "fixed"){
    y_min <- mround((min(raw_df[,vary_EV])), 10, FUN = floor)
    y_max <- mround((max(raw_df[,vary_EV])), 10, FUN = ceiling)
    y_range <- y_max - y_min
    y_incr <- 
      ifelse(y_range <= 10, 1,
             ifelse(y_range > 10 & y_range <= 20, 2,
                    ifelse(y_range > 20 & y_range <= 50, 5, 10)))
    p <- p +
      scale_y_continuous(breaks=seq(y_min, y_max, y_incr))
    
  }
  
  return(p = p)
  
}


