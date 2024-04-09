# Packages needed
packages <- c("DImodelsVis", "DImodels", "MASS", "ggpubr",
              "tidyverse", "PieGlyph", "gridExtra",
              "gtable", "rstudioapi", "nlme", "cowplot")
# Install a package if it's not present or has an older version
# Latest versions of packages from CRAN
cran_packs <- as.data.frame(available.packages(repos = "https://cran.r-project.org/"))[, c(1,2)]
need_install <- c()
for (package in packages){
  not_present <- FALSE
  local_version <- tryCatch(packageVersion(package), 
                            error = function(e) {not_present <<- TRUE})
  available_version <- cran_packs[cran_packs$Package == package, "Version"]
  if(isTRUE(not_present) || local_version < as.package_version(available_version)){
    need_install <- c(need_install, package)
  }
}

# Install the latest versions
if(length(need_install) > 0){
  install.packages(need_install)
}

library(DImodelsVis)
library(DImodels)
library(tidyverse)
library(PieGlyph)
library(grid)
library(gridExtra)
library(gtable)
library(rstudioapi)
library(nlme)

# Helper for creating scatter pie-chart plots
pie_chart_figure <- function(data, model,  
                             max_res = NULL, mean_vals = NULL, un = 0.7){
  set.seed(555)
  fig_data <- data %>% 
    mutate(Response = predict(model, newdata = .),
           Richness = rowSums(.[, 1:6] != 0)) %>% 
    arrange(Richness)
  
  if(is.null(max_res)){
    max_res <- max(fig_data$Response)
  }
  # Standardising response 
  fig_data <- fig_data %>% mutate(Response = Response/max_res)
  
  pie_comms <- fig_data %>% 
    filter(!(G1 == un | G2 == un | L1 == un | 
               L2 == un | H1 == un | H2 == un)) %>% 
    filter(Richness != 1) %>%
    group_by(Richness) %>% 
    arrange(Response) %>% 
    slice(c(1, n())) 
  
  tern_pies <- fig_data %>% 
    filter(G1 == 1/6 | (H1 == 0.5 & H2 == 0.5) |
             (G1 == 0.5 & G2 == 0.5) | (L1 == 0.5 & L2 == 0.5))
  
  if(is.null(mean_vals)){
    mean_vals <- fig_data %>% 
      filter(!(G1 == un | G2 == un | L1 == un | 
                 L2 == un | H1 == un | H2 == un)) %>% 
      group_by(Richness) %>% 
      summarise(Response = mean(Response)) %>% 
      .$Response
  }
  
  pl <- ggplot(data = fig_data, aes(x = Richness, y = Response))+
    geom_smooth(data = data.frame(Richness = 1:6,
                                  Response = mean_vals),
                colour = "#505050", linewidth = 1, fill = NA)+
    geom_pie_glyph(radius = 0.3, slices = 1:6, colour = NA,
                   data = fig_data %>% filter(G1 == un | G2 == un | L1 == un | 
                                                L2 == un | H1 == un | H2 == un))+
    geom_pie_glyph(radius = 0.35, slices = 1:6, colour = "black", 
                   data = fig_data %>% filter(Richness == "1"))+
    geom_pie_glyph(radius = 0.35, slices = 1:6, colour = "black", 
                   data = pie_comms)+
    geom_pie_glyph(radius = 0.35, slices = 1:6, colour = "black", lty = "12",
                   linewidth = 2, data = tern_pies)+
    theme_bw(base_size = 19)+
    labs(y = "Predicted Response", fill = "Species")+
    theme(legend.position = "top")+
    scale_x_continuous(breaks = 1:6)+
    theme(axis.text = element_text(size = 18))+
    scale_fill_manual(values = c("#66aad1", "#00507d", "#b380e5", "#6421a5", "#e58550", "#af4e1a"))
  pl
}

# Helper function for add pie-glyphs to an existing plot
add_pies <- function(plot, data, pie_data = NULL, prop, pie_radius = 0.35, 
                     linetype = "12", linewidth = 2){
  if(is.null(pie_data)){
    pie_data <- prop_to_tern_proj(data %>% 
                                    filter((G1 == 0.5 & G2 == 0.5 | 
                                              L1 == 0.5 & L2 == 0.5 | 
                                              H1 == 0.5 & H2 == 0.5) | 
                                             G1 == 1/6) %>% 
                                    mutate("Gr" = G1 + G2, 
                                           "Le" = L1 + L2,
                                           "He" = H1 + H2),
                                  prop = c("Gr", "Le", "He"))
  } else {
    pie_data <- prop_to_tern_proj(pie_data %>% 
                                    mutate("Gr" = G1 + G2, 
                                           "Le" = L1 + L2,
                                           "He" = H1 + H2),
                                  prop = c("Gr", "Le", "He"))
  }
  
  plot <- plot +
    ggnewscale::new_scale_fill() + 
    geom_pie_glyph(data = pie_data %>% mutate(.Pred = 0),
                   aes(x = .x, y =.y),
                   slices = prop,
                   colour = "black",
                   lty = linetype,
                   linewidth = linewidth, 
                   radius = pie_radius) + 
    scale_fill_manual(values = c("#66aad1", "#00507d", "#b380e5", "#6421a5", "#e58550", "#af4e1a")) +
    guides("fill" = "none", fill_new = guide_colorbar(available_aes = "fill_new"))
  
  if(nrow(custom_filter(data = pie_data,
                        prop = prop,
                        special = "richness == 1")) > 1){
    plot <- plot + 
      geom_pie_glyph(data = pie_data %>% mutate(.Pred = 0) %>%
                       custom_filter(prop = prop, special = "richness == 1"),
                     aes(x = .x, y =.y),
                     slices = prop,
                     colour = NA,
                     lty = linetype,
                     linewidth = linewidth, 
                     radius = pie_radius - 0.025) 
  }
  return(plot)
}

# Helper functions for performing inference on DI models
get_best_mono <- function(model, weights = c(1/3, 1/3, 1/3)){
  monos <- (weights %*% matrix(coef(model)[1:18], nrow = 3))
  names(monos) <- c("G1", "G2", "L1", "L2", "H1", "H2")
  names(monos)[which.max(monos)]
}

get_inf <- function(data, model, weights = c(1/3, 1/3, 1/3)){
  coeffs <- coef(model)
  var_mat <- vcov(model)
  mod_mat <- model.matrix(as.formula(model)[-2], data)
  
  weight_mat <- replicate(nrow(mod_mat), rep(weights, times = ncol(mod_mat)/length(weights))) %>% t()
  
  
  best_mono <- get_best_mono(model, weights = weights)
  
  monos <- data
  monos[, c("G1", "G2", "L1", "L2", "H1", "H2",
            "bfg_Gr_He", "bfg_Gr_Le", "bfg_He_Le", "wfg_Gr", "wfg_He", "wfg_Le")] <- 0
  monos[, best_mono] <- 1
  mono_mat <- model.matrix(as.formula(model)[-2], monos)
  
  to_mat <- (mod_mat - mono_mat) * weight_mat
  #to_mat <- to_mat/sum(w1)
  
  to_mat <- to_mat %>% as.data.frame() %>% 
    mutate(ID = rep(1:(nrow(.)/3), each = 3)) %>% 
    group_by(ID) %>% 
    summarise(across(.cols = everything(), .fns = sum)) %>% 
    select(-ID) %>% 
    as.matrix()
  
  to_pred <- as.vector(to_mat %*% coeffs)
  to_se <- apply(to_mat, 1, function(x){
    sqrt(t(x) %*% var_mat %*% x)
  })
  
  oy_mat <- mod_mat
  oy_mat[, 1:18] <- 0
  oy_mat <- oy_mat * weight_mat
  
  oy_mat <- oy_mat %>% as.data.frame() %>% 
    mutate(ID = rep(1:(nrow(.)/3), each = 3)) %>% 
    group_by(ID) %>% 
    summarise(across(.cols = everything(), .fns = sum)) %>% 
    select(-ID) %>% 
    as.matrix()
  
  oy_pred <- as.vector(oy_mat %*% coeffs)
  oy_se <- apply(oy_mat, 1, function(x){
    sqrt(t(x) %*% var_mat %*% x)
  })
  
  return(list("TO" = to_pred, "TO_se" = to_se,
              "OY" = oy_pred, "OY_se" = oy_se))
}

# Helper function for getting the position of point along a plot legend
get_legend_position <- function(value, min = 0.3, max = 1){
  legend_values <- seq(min, max, length.out = 300)
  x <- which.min(abs(legend_values - value))
  return(x)
}

# Pull legend from gtable of plot
pull_legend <- function(plot){
  leg <- cowplot::get_plot_component(plot, "guide-box", return_all = TRUE)
  leg[sapply(leg, function(x) inherits(x, "gtable"))][[1]]
}

# Helper function to mark a particular point along the legend of a plot
mark_legend <- function(leg, val, col = "sienna4", min = 0.3, max = 1){
  # gt <- ggplotGrob(plot)  #Convert plot to grob
  # leg <- gtable_filter(gt, "guide-box")
  leg$grobs[[1]]$grobs$bar$raster[(get_legend_position(val, min = min, max = max)) + c(-1, 0, 1)] <- col
  # gt$grobs[[11]] <- leg
  # plot <- ggpubr::as_ggplot(gt)
  return(leg)
}

# Helper function for adding rows to the underlying gtable of a ggplot object
shift_plot <- function(p){
  # get gtable object
  z <- ggplotGrob(p)
  
  # add label for top strip
  z <- gtable_add_rows(z, unit(0.15, 'npc'), 2)
  z <- gtable_add_rows(z, unit(1/3, "line"), 3)
  out <- ggpubr::as_ggplot(z)
  return(out)
}

# Helper functin for prepare data for creating ternary diagrams 
get_data <- function(coeffs, values, prop = species, FG = FG, 
                     AV = FALSE, scaler = 1){
  if(!AV){
    grouped_ternary_data(prop = species, FG = FG,
                         prediction = FALSE,
                         resolution = 5,
                         values = values) %>%
      bind_cols(., DI_data_FG(prop = species, data = ., 
                              FG = FG)$FG) %>% 
      add_prediction(coefficients = coeffs,
                     coeff_cols = 6:17) %>% 
      mutate(.Pred = .Pred/scaler)
  } else {
    grouped_ternary_data(prop = species, FG = FG,
                         prediction = FALSE,
                         resolution = 5,
                         values = values) %>%
      bind_cols(., AV = DI_data_E_AV(prop = species, data = ., 
                                     theta = 0.5)$AV) %>% 
      add_prediction(coefficients = coeffs,
                     coeff_cols = 6:12) %>% 
      mutate(.Pred = .Pred/scaler)
  }
}

# Helper function for creating a single ternary diagram
get_plot <- function(model, values, prop = species, FG = FG, 
                     AV = FALSE, scaler = 1){
  pl <-  grouped_ternary(model = model,
                         contour_text = FALSE,
                         resolution = 5,
                         axis_label_size = 6,
                         vertex_label_size = 7,
                         values = values,
                         lower_lim = 5,
                         upper_lim = 17,
                         nlevels = 6
  ) + 
    theme(legend.text = element_text(angle = 0, hjust = 0.5,
                                     vjust = 0.5)) 
  pl 
}

# Helper function for creating the appendix figures S2 and S3
create_plots_un <- function(data, model, mono_thresh){
  p1_data <- grouped_ternary(model = model,
                             FG = FG,
                             contour_text = FALSE,
                             lower_lim = 5,
                             upper_lim = 17,
                             values = rep(0.5, 6),
                             nlevels = 6,
                             resolution = 5,
                             axis_label_size = 6,
                             vertex_label_size = 7, plot = FALSE)
  # MF index 1 value
  wa1_val <- grouped_ternary_plot(data = p1_data %>% mutate(facet = "Prediction"),
                                  contour_text = FALSE,
                                  axis_label_size = 6, 
                                  vertex_label_size = 7,
                                  nlevels = 6,
                                  upper_lim = 17, 
                                  lower_lim = 5,
                                  tern_labels = c("Gr", "Le", "He")) + 
    geom_point(data = p1_data %>% slice_max(.Pred, n = 1),
               fill = "cyan3", colour = "black", shape = 24, size = 4) +
    # facet_wrap(~ facet) + 
    theme(legend.text = element_text(size = 16, angle = 0,
                                     hjust = 0.5, vjust = 0.5),
          legend.title = element_text(size = 18, vjust = 2), 
          legend.position = "bottom",
          plot.margin = unit(c(0, 0, 0, 0), "pt")) + 
    guides(fill = guide_colorbar(title = "Predicted\nResponse",
                                 ticks.colour = "black", 
                                 frame.colour = "black"))
  
  # MF index 1 over-performance inference
  wa1_OY <- grouped_ternary_plot(data = data %>% mutate(facet = "Over-performance"),
                                 contour_text = FALSE,
                                 axis_label_size = 6, 
                                 vertex_label_size = 7,
                                 nlevels = 2,
                                 tern_labels = c("Gr", "Le", "He")) + 
    geom_point(aes(colour = (OY_class)), size = .5)+
    guides(fill = "none", color = guide_legend(nrow = 3,
                                               override.aes = list(shape = 15, size = 5)))+
    geom_segment(data = tibble(x = c(0, 0, 1), y = c(0,0,0),
                               xend = c(1, 0.5, 0.5),
                               yend = c(0, sqrt(3)/2, sqrt(3)/2),
                               !! ".Pred" := 0),
                 aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
                 linewidth = 1) + 
    geom_point(data = p1_data %>% slice_max(.Pred, n = 1),
               fill = "cyan3", colour = "black", shape = 24, size = 4) +
    scale_colour_manual(values = c("#CCCCCC", "#505050", "black"), drop = FALSE, 
                        labels = c("0" = "Under-performance",
                                   "0.5" = "Non-significant",
                                   "1" = "Over-performance"),
                        name = "Inference") + 
    theme(legend.key.size = unit(0.08, "npc"), 
          legend.key.height = unit(0.04, "npc"),
          legend.title = element_text(size = 18, vjust = 0.5),
          legend.text = element_text(size = 16, angle = 0, 
                                     vjust = 0.5, hjust = 0), 
          legend.position = "bottom",
          plot.margin = unit(c(0, 0, 0, 0), "pt"))  
  
  # MF index 1 transgressive over-performance inference
  wa1_TO <- grouped_ternary_plot(data = data %>% mutate(facet = "Transgressive over-performance"),
                                 contour_text = FALSE,
                                 axis_label_size = 6, 
                                 vertex_label_size = 7,
                                 nlevels = 2,
                                 tern_labels = c("Gr", "Le", "He")) + 
    geom_point(aes(colour = (TO_class)), size = .5)+
    guides(fill = "none", color = guide_legend(nrow = 3,
                                               override.aes = list(shape = 15, size = 5)))+
    scale_colour_manual(values = c("#CCCCCC", "#505050", "black"), drop = FALSE,
                        labels = c("0" = "Trangressive under-performance",
                                   "0.5" = "Non-significant",
                                   "1" = "Trangressive over-performance"),
                        name = "Inference") + 
    geom_segment(data = tibble(x = c(0, 0, 1), y = c(0,0,0),
                               xend = c(1, 0.5, 0.5),
                               yend = c(0, sqrt(3)/2, sqrt(3)/2),
                               !! ".Pred" := 0),
                 aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
                 linewidth = 1) + 
    geom_point(data = p1_data %>% slice_max(.Pred, n = 1),
               fill = "cyan3", colour = "black", shape = 24, size = 4) +
    theme(legend.key.size = unit(0.08, "npc"), 
          legend.key.height = unit(0.04, "npc"),
          legend.title = element_text(size = 18, vjust = 0.5),
          legend.text = element_text(size = 16, angle = 0, 
                                     vjust = 0.5, hjust = 0), 
          legend.position = "bottom",
          plot.margin = unit(c(0, 0, 0, 0), "pt"))
  
  r3_pies <- cowplot::plot_grid(ggplot() + theme_void(),
                                wa1_val %>%  add_pies(data = fig2_data, prop = species, pie_radius = 0.3,
                                                      linetype = 1, linewidth = 1) + theme(legend.position = "none"), 
                                ggplot() + theme_void(),
                                wa1_OY %>%  add_pies(data = fig2_data, prop = species, pie_radius = 0.3,
                                                     linetype = 1, linewidth = 1)+ theme(legend.position = "none"),
                                ggplot() + theme_void(),
                                wa1_TO %>%  add_pies(data = fig2_data, prop = species, pie_radius = 0.3,
                                                     linetype = 1, linewidth = 1)+ theme(legend.position = "none"), 
                                ggplot() + theme_void(),
                                nrow = 1,
                                rel_widths = c(0.025, 1, 0.025, 1, 0.025, 1, 0.025))
  
  r3_pies
}

