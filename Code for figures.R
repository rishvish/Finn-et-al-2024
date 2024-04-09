# Helper functions for creating plots
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
source(paste0(dir, "/Helpers.R"))

# Directory to store figures
setwd(paste0(dir, "/Figures"))

################################################################################################################################################################################
############## Figure 1 
fig1_data <- data.frame(G1 = c(1, 1, 0, 0, 0, 0, 0, 0,
                               0.5, 0.5, 0.5, 0, 0, 0,
                               0.333, 0.333, 0.333, 0,
                               0.7, 0.1, 0.1, 0.1, 0.25),
                        G2 = c(0, 0, 1, 1, 0, 0, 0, 0,
                               0.5, 0, 0, 0.5, 0.5, 0,
                               0.333, 0.333, 0, 0.333,
                               0.1, 0.7, 0.1, 0.1, 0.25),
                        L1 = c(0, 0, 0, 0, 1, 1, 0, 0,
                               0, 0.5, 0, 0.5, 0, 0.5,
                               0.333, 0, 0.333, 0.333,
                               0.1, 0.1, 0.7, 0.1, 0.25),
                        L2 = c(0, 0, 0, 0, 0, 0, 1, 1,
                               0, 0, 0.5, 0, 0.5, 0.5,
                               0, 0.333, 0.333, 0.333,
                               0.1, 0.1, 0.1, 0.7, 0.25),
                        Richness = c(rep("1", 8), rep("2", 6),
                                     rep("3", 4), rep("4", 5)),
                        Response = c(14.59, 15.35, 13.39, 12.91, 9.58, 8.92, 10.5, 11.2,
                                     12.2, 18.25, 17.33, 14.5, 15.55, 13.3,
                                     15.5, 16.6, 18, 17.5,
                                     18.45, 17.75, 15.81, 16.3, 17.30))

a1 <- ggplot(data = fig1_data, aes(x = Richness, y = Response))+
  geom_point(size = 5, colour = "#707070")+
  theme_bw(base_size = 16)

b1 <- ggplot(data = fig1_data, aes(x = Richness, y = Response))+
  geom_pie_glyph(radius = 0.3, slices = 1:4, colour = "black")+
  theme_bw(base_size = 16)+
  labs(fill = "Species")+
  theme(legend.position = "top")+
  scale_fill_manual(values = c("#66aad1", "#00507d", "#b380e5", "#6421a5"))

fig1 <- cowplot::plot_grid(a1, b1, nrow = 2, 
                           labels = c("(a)", "(b)"),
                           rel_heights = c(1, 1.1))

ggsave(filename = "Figure 1.png", plot = fig1,
       width = 10, height = 12, units = "in",
       device = "png", dpi = 300)

################################################################################################################################################################################
## Simulating data for figure 2 and 3
species <- c("G1", "G2", "L1", "L2", "H1", "H2")
FG <- c("Gr", "Gr", "Le", "Le", "He", "He")
fig2_design <- DImodelsVis::get_equi_comms(6, variables = paste0("p", 1:6)) %>% 
  distinct(p1, p2, p3, p4, p5 , p6) %>% 
  rbind(ifelse(diag(0.2, 6) == 0, 0.2, 0) %>% `colnames<-`(paste0("p",1:6))) %>% 
  `colnames<-`(species) 

# Coefficients for simulating data

f1_ID_effects <- f2_ID_effects <- f3_ID_effects <- c(11.75, 10.47, 7.48, 8.66, 5.95, 4.75)

f1_int_effects <- rep(0, 6)
f2_int_effects <- rep(3, 6)
f3_int_effects <- c(9.54, 17.5, 14.2, 5.5, -2.85, 4.2)

set.seed(943)
means <- c(0, 0, 0)
f <- 0.05
var_mat <- matrix(c(0.2*f,  0.03*f, 0.05*f,
                    0.03*f, 0.3*f,  0.07*f,
                    0.05*f, 0.07*f, 0.4*f), nrow = 3)
res_mat <- MASS::mvrnorm(nrow(fig2_design), means, var_mat) 

Fs <- c("F1", "F2", "F3")
thetas <- c(1, 0.5, 1)

fig_5_model_data <- lapply(1:length(Fs), function(x) {
  IDs <- switch(x,  f1_ID_effects, f2_ID_effects, f3_ID_effects)
  Ints <- switch(x, f1_int_effects, f2_int_effects, f3_int_effects)
  
  fig2_design %>% 
    `colnames<-`(c("G1", "G2", "L1", "L2", "H1", "H2")) %>% 
    bind_cols(., DI_data_FG(prop = 1:6, data = ., theta = thetas[x],
                            FG = c("Gr", "Gr", "Le", "Le", "He", "He"))$FG) %>% 
    mutate(Plot = row_number(),
           Function = Fs[x],
           Value = ((as.matrix(.) %*% c(IDs, Ints)) + res_mat[, x])[, 1],
           Richness = rowSums(.[, 1:6] != 0)) %>% 
    mutate(Value = Value)
}) %>% bind_rows() 

f1_mod <- DI(y = "Value", prop = species, DImodel = "ID" ,
   data = fig_5_model_data %>% filter(Function == "F1"), FG = FG)
f1_mod 

f2_mod <- DI(y = "Value", prop = species, DImodel = "AV", theta = 0.5, 
   data = fig_5_model_data %>% filter(Function == "F2"), FG = FG) 
f2_mod

f3_mod <- DI(y = "Value", prop = species, DImodel = "FG", 
   data = fig_5_model_data %>% filter(Function == "F3"), FG = FG)
f3_mod

################################################################################################################################################################################
##### Figure 2
un <- 0.7
extra_comms <- matrix(c(un, (1 - un), 0, 0, 0, 0,
                        un, (1 - un)/2, 0, (1 - un)/2, 0, 0,
                        un, (1 - un)/3, (1 - un)/3, (1 - un)/3, 0, 0,
                        un, (1 - un)/4, (1 - un)/4, (1 - un)/4, (1 - un)/4, 0, 
                        un, (1 - un)/5, (1 - un)/5, (1 - un)/5, (1 - un)/5, (1 - un)/5,
                        
                        0, 0, 0, 0, (1 - un), un, 
                        0, 0, (1 - un)/2, 0, (1 - un)/2, un,
                        0, 0, (1 - un)/3, (1 - un)/3, (1 - un)/3, un,
                        0, (1 - un)/4, (1 - un)/4, (1 - un)/4, (1 - un)/4, un,
                        (1 - un)/5, (1 - un)/5, (1 - un)/5, (1 - un)/5, (1 - un)/5, un),
                      ncol = 6, byrow = TRUE) %>% 
  `colnames<-`(species) %>% 
  as.data.frame()

# Row 1
fig2_data <- fig2_design %>% 
  bind_rows(., extra_comms) %>% 
  cbind(., DI_data_FG(prop = 1:6, data = ., 
                      FG = FG)$FG) 
two_11 <- pie_chart_figure(data = fig2_data, 
                           model = f1_mod, 
                           max_res = 1) + 
  geom_text(data = data.frame(x = 3.5, y = 17, label = "italic(hat(y) == sum(hat(beta)[i] * p[i], i==1, 6))"),
            aes(x = x, y = y, label = label), size = 6, parse = TRUE)+
  scale_y_continuous(breaks = seq(4, 16, 4), limits = c(4, 18))
# two_11

two_12 <- grouped_ternary(model = f1_mod,
                          FG = FG,
                          contour_text = FALSE,
                          lower_lim = 5,
                          upper_lim = 17,
                          nlevels = 6,
                          values = rep(0.5, 6),
                          resolution = 5,
                          axis_label_size = 6,
                          vertex_label_size = 7) + 
  theme(legend.text = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))
# two_12

two_a <- cowplot::plot_grid(two_11, two_12, nrow = 1)
# two_a

# Row 2
two_21 <- pie_chart_figure(data = fig2_data, 
                           model = f2_mod, 
                           max_res = 1) + 
  geom_text(data = data.frame(x = 3.5, y = 17, label = "italic(hat(y) == sum(hat(beta)[i] * p[i], i==1, 6) + hat(delta)[av] * sum((p[i] * p[j])^0.5, 
                              atop(list(i, j) == 1, i < j), 6))"),
            aes(x = x, y = y, label = label), size = 6, parse = TRUE)+
  scale_y_continuous(breaks = seq(4, 16, 4), limits = c(4, 18))
# two_21

two_22 <- grouped_ternary(model = f2_mod,
                          contour_text = FALSE,
                          lower_lim = 5,
                          upper_lim = 17,
                          resolution = 5,
                          values = rep(0.5, 6),
                          nlevels = 6,
                          axis_label_size = 6,
                          vertex_label_size = 7
) + theme(legend.text = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))
# two_22

two_b <- cowplot::plot_grid(two_21, two_22, nrow = 1)
# two_b

# Panel C
two_31 <- pie_chart_figure(data = fig2_data,
                           model = f3_mod,
                           max_res = 1) + #21.82) + 
  geom_text(data = data.frame(x = 3.5, y = 17, label = "italic(hat(y) == sum(hat(beta)[i] * p[i], i==1, 6) + sum(hat(omega)[qq], q == 1, T) * sum(p[i] * p[j], 
                              atop(list(i, j) == 1, i < j), 6) + sum(hat(omega)[qr], atop(list(q, r) == 1, q < r), T) * sum(p[i] * p[j], 
                              atop(list(i, j) == 1, i < j), 6))"),
            aes(x = x, y = y, label = label), size = 6, parse = TRUE)+
  scale_y_continuous(breaks = seq(4, 16, 4), limits = c(4, 18))
# two_31

two_32 <- grouped_ternary(model = f3_mod,
                          FG = FG, 
                          contour_text = FALSE,
                          lower_lim = 5,
                          upper_lim = 17,
                          values = rep(0.5, 6),
                          resolution = 5,
                          nlevels = 6,
                          axis_label_size = 6,
                          vertex_label_size = 7
) + 
  theme(legend.text = element_text(angle = 0, hjust = 0.5,
                                   vjust = 0.5))
# two_32

two_c <- cowplot::plot_grid(two_31, two_32, nrow = 1)
# two_c

# Combined figure
pie_legend <- pull_legend(two_11 + guides(fill = guide_legend(override.aes = list(linetype = 1),
                                                  keyheight = unit(0.6, "cm"),  # Adjust height of legend key
                                                  keywidth = unit(0.6, "cm"))) +
                            theme(legend.title = element_text(size = 18),
                                  legend.text = element_text(size = 16)))
colour_bar_un <- pull_legend(two_12 + 
                               guides(fill = guide_colorbar(title = "Predicted\nResponse", 
                                                            ticks.colour = "black", 
                                                            frame.colour = "black"))  + 
                               theme(legend.title = element_text(size = 18, vjust = 4),
                                     legend.text = element_text(size = 16)))

fig2 <- cowplot::plot_grid(ggplot() + theme_void(), ggplot() + theme_void(),
                           pie_legend, 
                           colour_bar_un,
                           ggplot() + theme_void(), ggplot() + theme_void(),
                           two_11 + theme(legend.position = "none"), two_12 %>% add_pies(plot = ., data = fig2_data, prop = species) + theme(legend.position = "none"), 
                           two_21 + theme(legend.position = "none"), two_22 %>% add_pies(plot = ., data = fig2_data, prop = species) + theme(legend.position = "none"), 
                           two_31 + theme(legend.position = "none"), two_32 %>% add_pies(plot = ., data = fig2_data, prop = species) + theme(legend.position = "none"), 
                           ggplot() + theme_void(), ggplot() + theme_void(),
                           nrow = 7, ncol = 2, byrow = TRUE, rel_heights = c(0.05, 0.1, 0.05, 1, 1, 1, 0.05),
                           labels = c("", "", "", "", "", "", "(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "", ""),
                           label_size = 20)

ggsave(filename = "Figure 2.png", plot = fig2,
       width = 14, height = 21, units = "in",
       device = "png", dpi = 300, bg = "white")

################################################################################################################################################################################
###### Figure 3
fig3_model_data <- fig_5_model_data %>%
  group_by(Function) %>% 
  mutate(Value = Value/max(Value)) %>% 
  ungroup()

model <- gls(Value ~ 0 + Function:(G1 + G2 + L1 + L2 + H1 + H2 + 
                                     bfg_Gr_He + bfg_Gr_Le + bfg_He_Le +
                                     wfg_Gr + wfg_Le + wfg_He),
             data = fig3_model_data,
             correlation = corSymm(#value = c(0, 0, 0),
               form = ~ 1 | Plot),
             weights = varIdent(#value = c("F1" = 0.1, "F2" = 0.91),
               form = ~ 1 | Function),
             control = glsControl(maxIter = 5000000, msMaxIter = 50000000))
model

fig3_model_data <- add_prediction(model = model, data = fig3_model_data)

fig3_data <- lapply(1:length(Fs), function(x) {
  grouped_ternary_data(prop = species,
                       FG = FG,
                       values = c(0.5,0.5,0.5,0.5,0.5,0.5),
                       resolution = 5,
                       prediction = FALSE) %>% 
    bind_cols(DI_data_FG(prop = species,
                         FG = FG,
                         theta = thetas[x],
                         data = .)$FG) %>% 
    mutate(Function = factor(Fs[x], levels = Fs)) 
}) %>% 
  bind_rows() %>% 
  add_prediction(model = model) 

test_data <- fig3_data %>% 
  mutate(ID = rep(1:(nrow(.)/3), times = 3)) %>% arrange(ID)

# MF index 1
wa1 <- fig3_data %>% 
  mutate(ID = rep(1:(nrow(.)/3), times = 3)) %>% 
  group_by(ID, Gr, He, Le, .x, .y,
           G1, G2, L1, L2, H1, H2) %>% 
  summarise(.Pred = sum(.Pred * c(1/3, 1/3, 1/3))) %>% 
  mutate(Group = "MF value 1")

# MF index 2
wa2 <- fig3_data %>% 
  mutate(ID = rep(1:(nrow(.)/3), times = 3)) %>% 
  group_by(ID, Gr, He, Le, .x, .y,
           G1, G2, L1, L2, H1, H2) %>% 
  summarise(.Pred = sum(.Pred * c(2/3, 1/3, 0/3))) %>% 
  mutate(Group = "MF value 2")

# Infernece for MF index 1
w1 <-  c(1/3, 1/3, 1/3)
# This will take some time to run (about 5 minutes)
wa1_inf <- get_inf(data = test_data,
                   model = model,
                   weights = w1)

# Check TO is correct
mono_pred1 <- fig3_model_data %>% filter(!!sym(get_best_mono(model, weights = w1)) == 1)
mono_mean1 <- weighted.mean(mono_pred1$.Pred, w1)
all(
  sapply(1:nrow(wa1), function(x){
    all.equal((wa1$.Pred[x] - mono_mean1), ((wa1_inf$TO[x])))
  })
)

# Check OY is correct
oy_data <- test_data
oy_data[, species] <- 0
all.equal(wa1_inf$OY, 
          add_prediction(oy_data, model = model) %>%
            group_by(ID) %>% 
            summarise(.Pred = sum(.Pred * w1)) %>% 
            .$.Pred, 
          check.attributes = FALSE)


## Inference for MF index 2
w2 <-  c(2/3, 1/3, 0/3)
# This will take some time to run (about 5 minutes)
wa2_inf <- get_inf(data = test_data,
                   model = model,
                   weights = w2) 

# Checking TO is correct
mono_pred2 <- fig3_model_data %>% filter(!!sym(get_best_mono(model, weights = w2)) == 1)
mono_mean2 <- weighted.mean(mono_pred2$.Pred, w2)
all(
  sapply(1:nrow(wa2), function(x){
    (all.equal((wa2$.Pred[x] - mono_mean2), (wa2_inf$TO[x])))
  })
)

# Check OY is correct
oy_data <- test_data
oy_data[, species] <- 0
all.equal(wa2_inf$OY, 
          add_prediction(oy_data, model = model) %>%
            group_by(ID) %>% 
            summarise(.Pred = sum(.Pred * w2)) %>% 
            .$.Pred, 
          check.attributes = FALSE)


# All is good now create plot
wa1 <- wa1 %>% ungroup() %>% copy_attributes(fig3_data) %>% 
  mutate(TO = wa1_inf$TO,
         OY = wa1_inf$OY,
         TO_se = wa1_inf$TO_se,
         OY_se = wa1_inf$OY_se) %>% 
  mutate(TO.lwr = TO - 1.96 * TO_se,
         TO.upr = TO + 1.96 * TO_se,
         OY.lwr = OY - 1.96 * OY_se,
         OY.upr = OY + 1.96 * OY_se) %>% 
  mutate(TO_class = ifelse(TO.upr > 0 & TO.lwr < 0, 0.5, ifelse(TO.lwr > 0, 1, 0)),
         OY_class = ifelse(TO.upr > 0 & OY.lwr < 0, 0.5, ifelse(OY.lwr > 0, 1, 0)),
         Group = "MF value 1") %>% 
  mutate(TO_class = factor(TO_class, levels = c("0", "0.5", "1")),
         OY_class = factor(OY_class, levels = c("0", "0.5", "1")))

wa2 <- wa2 %>% ungroup() %>% copy_attributes(fig3_data) %>% 
  mutate(TO = wa2_inf$TO,
         OY = wa2_inf$OY,
         TO_se = wa2_inf$TO_se,
         OY_se = wa2_inf$OY_se) %>% 
  mutate(TO.lwr = TO - 1.96 * TO_se,
         TO.upr = TO + 1.96 * TO_se,
         OY.lwr = OY - 1.96 * OY_se,
         OY.upr = OY + 1.96 * OY_se) %>% 
  mutate(TO_class = ifelse(TO.upr > 0 & TO.lwr < 0, 0.5, ifelse(TO.lwr > 0, 1, 0)),
         OY_class = ifelse(TO.upr > 0 & OY.lwr < 0, 0.5, ifelse(OY.lwr > 0, 1, 0)),
         Group = "MF value 2") %>% 
  mutate(TO_class = factor(TO_class, levels = c("0", "0.5", "1")),
         OY_class = factor(OY_class, levels = c("0", "0.5", "1")))

# MF index 1 value
wa1_val <- grouped_ternary_plot(data = wa1 %>% mutate(facet = "Prediction"),
                                contour_text = FALSE,
                                axis_label_size = 6, 
                                vertex_label_size = 7,
                                nlevels = 7,
                                upper_lim = 1, 
                                lower_lim = 0.3,
                                tern_labels = c("Gr", "Le", "He")) + 
  geom_point(data = wa1 %>% slice_max(.Pred, n = 1),
             fill = "cyan3", colour = "black", shape = 24, size = 4) +
  facet_wrap(~ facet) + 
  theme(strip.text = element_text(size = 18, margin = margin(0.1,0,0.1,0, "cm")),
        strip.background = element_rect(colour = "black", fill = "#DDDDDD")) + 
  theme(legend.text = element_text(size = 16, angle = 0,
                                   hjust = 0.5, vjust = 0.5),
        legend.title = element_text(size = 18, vjust = 2), 
        legend.position = "bottom") + 
  guides(fill = guide_colorbar(title = "Predicted\nResponse",
                               ticks.colour = "black", 
                               frame.colour = "black"))

pred_scale <- pull_legend(wa1_val + theme(legend.title = element_text(vjust = 3.5))) %>% 
  mark_legend(val = mono_mean1, col = "navy")

label <- textGrob(x = 0.845, y = 0.55, label = "TO", 
                  gp = gpar(size = 5, fontface = "bold"))

pos <-  subset(pred_scale$layout, grepl("guides", name), t:r)

width = unit(1, "grobwidth", label) + unit(10, "points")
height = unit(1, "grobheight", label)+ unit(10, "points")
pred_scale <-  gtable_add_rows(pred_scale, height, pos = pos$t-1)
pred_scale <-  gtable_add_grob(pred_scale, label, 
                               t = 3, l = 3, r = 4)

# MF index 1 over-performance inference
pad_data <- matrix(NA, nrow = 3, ncol = 24) %>% as.data.frame() %>% 
  `colnames<-`(colnames(wa1)) %>% 
  mutate(OY_class = c(0, 0.5, 1),
         TO_class = c(0, 0.5, 1))

wa1_OY <- grouped_ternary_plot(data = wa1 %>% mutate(facet = "Over-performance"),
                               contour_text = FALSE,
                               axis_label_size = 6, 
                               vertex_label_size = 7,
                               nlevels = 2,
                               tern_labels = c("Gr", "Le", "He")) + 
  geom_point(aes(colour = (OY_class)), size = .5,
             data = rbind(wa1, pad_data) %>% 
               mutate(facet = "Over-performance"))+
  geom_point(data = wa1 %>% slice_max(.Pred, n = 1),
             fill = "cyan3", colour = "black", shape = 24, size = 4) +
  guides(fill = "none", color = guide_legend(nrow = 3,
                                             override.aes = list(shape = 15, size = 5)))+
  geom_segment(data = tibble(x = c(0, 0, 1), y = c(0,0,0),
                             xend = c(1, 0.5, 0.5),
                             yend = c(0, sqrt(3)/2, sqrt(3)/2),
                             !! ".Pred" := 0),
               aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
               linewidth = 1) + 
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
        legend.position = "bottom") + 
  facet_wrap(~ facet) + 
  theme(strip.text = element_text(size = 18, margin = margin(0.1,0,0.1,0, "cm")),
        strip.background = element_rect(colour = "black", fill = "#DDDDDD"))

# MF index 1 transgressive over-performance inference
wa1_TO <- grouped_ternary_plot(data = wa1 %>% mutate(facet = "Transgressive over-performance"),
                               contour_text = FALSE,
                               axis_label_size = 6, 
                               vertex_label_size = 7,
                               nlevels = 2,
                               tern_labels = c("Gr", "Le", "He")) + 
  geom_point(aes(colour = (TO_class)), size = 0.5,
             data = rbind(wa1, pad_data) %>% 
               mutate(facet = "Transgressive over-performance"))+
  geom_point(data = wa1 %>% slice_max(.Pred, n = 1),
             fill = "cyan3", colour = "black", shape = 24, size = 4) +
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
  theme(legend.key.size = unit(0.08, "npc"), 
        legend.key.height = unit(0.04, "npc"),
        legend.title = element_text(size = 18, vjust = 0.5),
        legend.text = element_text(size = 16, angle = 0, 
                                   vjust = 0.5, hjust = 0), 
        legend.position = "bottom") + 
  facet_wrap(~ facet) + 
  theme(strip.text = element_text(size = 18, margin = margin(0.1,0,0.1,0, "cm")),
        strip.background = element_rect(colour = "black", fill = "#DDDDDD"))

# MF index 2 value
wa2_val <- grouped_ternary_plot(data = wa2 %>% mutate(facet = "Prediction"),
                                contour_text = FALSE,
                                axis_label_size = 6, 
                                vertex_label_size = 7,
                                nlevels = 7,
                                upper_lim = 1, 
                                lower_lim = 0.3,
                                tern_labels = c("Gr", "Le", "He")) + 
  geom_point(data = wa2 %>% slice_max(.Pred, n = 1),
             fill = "cyan3", colour = "black", shape = 24, size = 4) +
  facet_wrap(~ facet) + 
  theme(strip.text = element_text(size = 18, margin = margin(0.1,0,0.1,0, "cm")),
        strip.background = element_rect(colour = "black", fill = "#DDDDDD")) +
  theme(legend.text = element_text(size = 16, angle = 0,
                                   hjust = 0.5, vjust = 0.5),
        legend.title = element_text(size = 18, vjust = 2), 
        legend.position = "bottom") +
  guides(fill = guide_colorbar(title = "Predicted\nResponse", 
                               ticks.colour = "black", 
                               frame.colour = "black"))

pred_scale2 <- pull_legend(wa2_val + theme(legend.title = element_text(vjust = 3.5))) %>%
  mark_legend(val = mono_mean2, col = "navy")

label <- textGrob(x = 0.913, y = 0.55, label = "TO", 
                  gp = gpar(size = 5, fontface = "bold"))

pos <-  subset(pred_scale2$layout, grepl("guides", name), t:r)

width = unit(1, "grobwidth", label) + unit(10, "points")
height = unit(1, "grobheight", label)+ unit(10, "points")
pred_scale2 <-  gtable_add_rows(pred_scale2, height, pos = pos$t-1)
pred_scale2 <-  gtable_add_grob(pred_scale2, label, 
                                t = 3, l = 3, r = 4)

# MF index 1 over-performance inference
wa2_OY <- grouped_ternary_plot(data = wa2 %>% mutate(facet = "Over-performance"),
                               contour_text = FALSE,
                               axis_label_size = 6, 
                               vertex_label_size = 7,
                               nlevels = 7,
                               upper_lim = 1, 
                               lower_lim = 0.3,
                               tern_labels = c("Gr", "Le", "He")) + 
  geom_point(aes(colour = (OY_class)), size = 0.5,
             data = rbind(wa2, pad_data) %>% 
               mutate(facet = "Over-performance"))+
  guides(fill = "none", color = guide_legend(nrow = 3,
                                             override.aes = list(shape = 15, size = 5)))+
  geom_segment(data = tibble(x = c(0, 0, 1), y = c(0,0,0),
                             xend = c(1, 0.5, 0.5),
                             yend = c(0, sqrt(3)/2, sqrt(3)/2),
                             !! ".Pred" := 0),
               aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
               linewidth = 1) + 
  geom_point(data = wa2 %>% slice_max(.Pred, n = 1),
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
        legend.position = "bottom") + 
  facet_wrap(~ facet) + 
  theme(strip.text = element_text(size = 18, margin = margin(0.1,0,0.1,0, "cm")),
        strip.background = element_rect(colour = "black", fill = "#DDDDDD"))

# MF index 1 transgressive over-performance inference
wa2_TO <- grouped_ternary_plot(data = wa2 %>% mutate(facet = "Transgressive over-performance"),
                               contour_text = FALSE,
                               axis_label_size = 6, 
                               vertex_label_size = 7,
                               nlevels = 7,
                               upper_lim = 1, 
                               lower_lim = 0.3,
                               tern_labels = c("Gr", "Le", "He")) + 
  geom_point(aes(colour = (TO_class)), size = 0.5,
             data = rbind(wa2, pad_data) %>% 
               mutate(facet = "Transgressive over-performance"))+
  guides(fill = "none", color = guide_legend(nrow = 3,
                                             override.aes = list(shape = 15, size = 5)))+
  geom_segment(data = tibble(x = c(0, 0, 1), y = c(0,0,0),
                             xend = c(1, 0.5, 0.5),
                             yend = c(0, sqrt(3)/2, sqrt(3)/2),
                             !! ".Pred" := 0),
               aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
               linewidth = 1) + 
  geom_point(data = wa2 %>% slice_max(.Pred, n = 1),
             fill = "cyan3", colour = "black", shape = 24, size = 4) +
  scale_colour_manual(values = c("#CCCCCC", "#505050", "black"), drop = FALSE,
                      labels = c("0" = "Trangressive under-performance",
                                 "0.5" = "Non-significant",
                                 "1" = "Trangressive over-performance"),
                      name = "Inference") + 
  theme(legend.key.size = unit(0.08, "npc"), 
        legend.key.height = unit(0.04, "npc"),
        legend.title = element_text(size = 18, vjust = 0.5),
        legend.text = element_text(size = 16, angle = 0, 
                                   vjust = 0.5, hjust = 0), 
        legend.position = "bottom") + 
  facet_wrap(~ facet) + 
  theme(strip.text = element_text(size = 18, margin = margin(0.1,0,0.1,0, "cm")),
        strip.background = element_rect(colour = "black", fill = "#DDDDDD"))

opt_leg <- pull_legend(
  ggplot() + geom_point(aes(x = 1, y= 1,fill = "cyan3"), colour = "black", shape = 24, size = 4) + 
    scale_fill_identity("", labels = "Optimal community", 
                        guide = "legend") + 
    xlim(-0.95, 1) +
    theme(legend.key = element_rect(colour = NA, fill = NA),
          legend.position = "top",
          legend.text = element_text(size = 16, vjust = 1)))


pl3_pies <- cowplot::plot_grid(ggplot() + theme_void(),
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

pl4_pies <- cowplot::plot_grid(ggplot() + theme_void(),
                               wa2_val %>%  add_pies(data = fig2_data, prop = species, pie_radius = 0.3,
                                                     linetype = 1, linewidth = 1) + theme(legend.position = "none"), 
                               ggplot() + theme_void(),
                               wa2_OY %>%  add_pies(data = fig2_data, prop = species, pie_radius = 0.3,
                                                    linetype = 1, linewidth = 1) + theme(legend.position = "none"),
                               ggplot() + theme_void(),
                               wa2_TO %>%  add_pies(data = fig2_data, prop = species, pie_radius = 0.3,
                                                    linetype = 1, linewidth = 1) + theme(legend.position = "none"), 
                               ggplot() + theme_void(),
                               nrow = 1,
                               rel_widths = c(0.025, 1, 0.025, 1, 0.025, 1, 0.025))

r3_pies <- (gTree(children = gList(ggplotGrob(shift_plot(pl3_pies)),
                                   rectGrob(gp = gpar(col = "black", fill = "#CCCCCC"),
                                            x = unit(0.5, "npc"), 
                                            y = unit(0.9, "npc"), 
                                            width = unit(0.985, "npc"),
                                            height = unit(0.05, "npc")),
                                   textGrob("Individual function weights for MF value A: (1/3, 1/3, 1/3)", 
                                            x = unit(0.5, "npc"), 
                                            y = unit(0.9, "npc"), 
                                            gp = gpar(col = "black", cex = 1,
                                                      fontsize = 20)))))

r4_pies <- (gTree(children = gList(ggplotGrob(shift_plot(pl4_pies)),
                                   rectGrob(gp = gpar(col = "black", fill = "#CCCCCC"),
                                            x = unit(0.5, "npc"), 
                                            y = unit(0.9, "npc"), 
                                            width = unit(0.985, "npc"),
                                            height = unit(0.05, "npc")),
                                   textGrob("Individual function weights for MF value B: (2/3, 1/3, 0)", 
                                            x = unit(0.5, "npc"), 
                                            y = unit(0.9, "npc"), 
                                            gp = gpar(col = "black", cex = 1,
                                                      fontsize = 20)))))

r5_pies <- cowplot::plot_grid(ggplot() + theme_void(),
                               pred_scale, 
                               ggplot() + theme_void(),
                               ggplot() + theme_void(),
                               ggplot() + theme_void(),
                               ggplot() + theme_void(),
                               ggplot() + theme_void(),
                               nrow = 1,
                               rel_widths = c(0.025, 1, 0.025, 1, 0.025, 1, 0.025))

r6_pies <- cowplot::plot_grid(ggplot() + theme_void(),
                              pred_scale2, 
                              ggplot() + theme_void(),
                              cowplot::plot_grid(opt_leg,
                                                 ggplot() + theme_void(),
                                                 pull_legend(wa2_OY),
                                                 nrow = 3,
                                                 rel_heights = c(1, 0.1, 1)),
                              ggplot() + theme_void(),
                              cowplot::plot_grid(pie_legend,
                                                 ggplot() + theme_void(),
                                                 pull_legend(wa2_TO), 
                                                 nrow = 3,
                                                 rel_heights = c(1, 0.1, 1)),
                              ggplot() + theme_void(),
                              nrow = 1,
                              rel_widths = c(0.025, 1, 0.025, 1, 0.025, 1, 0.025))


### Combined figure
fig3 <- cowplot::plot_grid(
  #ggplot() + theme_void(),
  ggpubr::as_ggplot(r3_pies) + theme(plot.margin = unit(c(0.01, .01, 0.01, .025), "npc")),
  r5_pies,
  # ggplot() + theme_void(),
  ggpubr::as_ggplot(r4_pies) + theme(plot.margin = unit(c(0.01, .01, 0.01, .025), "npc")),
  r6_pies,
  ggplot() + theme_void(),
  ncol = 1,
  labels = c("(a)", "", "(b)", "", ""),
  label_size = 20,
  hjust = -0.15,
  vjust = 2,
  rel_heights = c(1, 0.1, 1, 0.25, 0.025))
# fig3

ggsave(filename = "Figure 3.png", plot = fig3,
       width = 18.21, height = 15, units = "in",
       device = "png", dpi = 300, bg = "white")

################################################################################
### Supplementary figures

################################################################################
# Figure S1
fig2a_data <- data.frame(G1 = seq(0, 100, 1)/100,
                         L1 = seq(100, 0, -1)/100,
                         beta1 = mean(c(11.804)),
                         beta2 = mean(c(7.541)),
                         delta = 17.214, 
                         Richness = c(1, rep(2, 99), 1),
                         G2 = 0, L2 = 0) %>% 
  mutate(G1_cont = beta1*G1,
         L1_cont = beta2*L1,
         WA = G1_cont + L1_cont,
         Int = G1*L1*delta,
         Pred = WA + Int,
         Prop = G1)

pie_data <- fig2a_data %>% filter(L1 %in% (seq(0, 100, 10)/100))

fig_s1 <- ggplot(data = fig2a_data, aes(x = Prop, y = Pred))+
  geom_line(aes(y=`WA`, x=`G1`, group=1), linetype=5, linewidth = 1)+
  #geom_line(aes(y=max(plot_data_d$p1_cont), x=seq(1,1.07,length=101)), linetype=5, size=1)+
  geom_area(aes(x=`G1`, y=Pred, fill='#c4ef90') , alpha=1)+
  geom_area(aes(y=`WA`, fill='#b380e5') , alpha=1)+
  geom_area(aes(y=`G1_cont`, fill='#66aad1'), alpha = 1)+
  geom_line(linewidth = 1.1)+
  # geom_line(linetype = 5, linewidth = 1, 
  #           aes(x = G1, y = G1_cont))+
  geom_segment(data = data.frame(x = c(0.6, 0.6, 0.6), 
                                 y = c(0, fig2a_data$G1_cont[61], fig2a_data$WA[61]), 
                                 xend = c(0.6,0.6,0.6),
                                 yend = c(fig2a_data$G1_cont[61], fig2a_data$WA[61], fig2a_data$Pred[61])),
               aes(x = x, y = y, xend = xend, yend = yend), linewidth = 0.75, colour = "#404040",
               arrow = arrow(length = unit(0.5, "cm"), ends = "both", type = "closed"))+
  geom_text(data = data.frame(x = c(0.555, 0.555, 0.51), 
                              y = c(mean(c(0, fig2a_data$G1_cont[61])), 
                                    mean(c(fig2a_data$G1_cont[61], fig2a_data$WA[61])),
                                    mean(c(fig2a_data$Pred[61], fig2a_data$WA[61])) + .5), 
                              label = c("bold(beta[G1] %*% italic(p)[G1])", "bold(beta[L1] %*% italic(p)[L1])", 
                                        "bold(delta[G1%*%L1] %*% (italic(p)[G1] %*% italic(p)[L1])**1)")),
            aes(x = x, y = y, label = label), size = 6, 
            colour = "#222222", parse = TRUE)+
  geom_line(aes(y = max(G1_cont),
                linetype = "T"),
            linewidth = 1.1)+
  geom_line(linewidth = 1.1, 
            aes(x = G1, y = WA,
                linetype = "O",))+
  scale_x_continuous(name = "G1 Proportion", sec.axis = sec_axis(transform =~ 1-., name='L1 Proportion'))+
  scale_fill_identity(name = 'Contribution to response', guide = 'legend', 
                      breaks=c("#66aad1", "#b380e5", "#c4ef90"),
                      labels = c("G1", "L1", 'Interaction'))+
  # scale_colour_manual(name = 'Threshold', guide = 'legend',
  #                     values=c("#30130f", "#17033c"),
  #                     labels = c("Overyielding", "Transgressive Overyielding"))+
  scale_linetype_manual(name = 'Threshold', guide = 'legend',
                        values = c(4, 3),
                        labels = c("Overyielding", "Transgressive Overyielding"))+
  ggnewscale::new_scale_fill()+
  geom_pie_glyph(data = pie_data,
                 slices = c("G1", "L1"), radius = 0.35, colour = "black")+
  scale_fill_manual(values = c("#66aad1", "#b380e5"))+
  guides(fill = "none")+
  DImodelsVis:::theme_DI(font_size = 17)+
  theme(legend.position = "top")+
  labs(x = "L1 Proportion", y = "Predicted Response", fill = "Species")
# fig_s1

ggsave(filename = "Figure S1.png", plot = fig_s1,
       width = 14, height = 10, units = "in",
       device = "png", dpi = 300, bg = "white")

################################################################################################################################################################################
# Figure S2
two11_10 <- get_plot(model = f1_mod, 
                     values = c(1, 0, 1, 0, 1, 0),
                     prop = species, 
                     FG = FG)

two11_0.5 <- get_plot(model = f1_mod, 
                      values = c(.5, 0.5, .5, 0.5, .5, 0.5),
                      prop = species, 
                      FG = FG)

two11_01 <- get_plot(model = f1_mod,
                     values = c(0, 1, 0, 1, 0, 1),
                     prop = species, 
                     FG = FG)

two21_10 <- get_plot(model = f2_mod,
                     values = c(1, 0, 1, 0, 1, 0),
                     prop = species, 
                     FG = FG)

two21_0.5 <- get_plot(model = f2_mod,
                      values = c(.5, 0.5, .5, 0.5, .5, 0.5),
                      prop = species, 
                      FG = FG)

two21_01 <- get_plot(model = f2_mod,
                     values = c(0, 1, 0, 1, 0, 1),
                     prop = species, 
                     FG = FG)

two31_10 <- get_plot(model = f3_mod,
                     values = c(1, 0, 1, 0, 1, 0),
                     prop = species, 
                     AV = TRUE,
                     FG = FG)

two31_0.5 <- get_plot(model = f3_mod,
                      values = c(.5, 0.5, .5, 0.5, .5, 0.5),
                      prop = species, 
                      AV = TRUE,
                      FG = FG)

two31_01 <- get_plot(model = f3_mod,
                     values = c(0, 1, 0, 1, 0, 1),
                     prop = species, 
                     AV = TRUE,
                     FG = FG)

# Labels
strip1 <- "Gr = 100% G1;\nLe = 100% L1;\nHe = 100% H1;\n"
strip2 <- "Gr = 50%-50% G1-G2;\nLe = 50%-50% L1-L2;\nHe = 50%-50% H1-H2;\n"
strip3 <- "Gr = 100% G2;\nLe = 100% L2;\nHe = 100% H2;\n"
strips <- list(strip1, strip2, strip3)

tab <- ggplotGrob(ggplot(data = data.frame(facet = c(strip1, strip2, strip3)) %>% 
                           mutate(facet = fct_inorder(facet))) + 
                    facet_wrap(~facet) +
                    theme(strip.text = element_text(size = 16, vjust = -2,
                                                    margin = unit(c(0, 0, 0, 0), "cm")),
                          strip.background = element_rect(colour = "black", fill = "#DDDDDD"),
                          panel.spacing = unit(0.025, "npc"),
                          plot.margin = unit(c(0, 0.025, 0, 0.025), "npc")) + 
                    coord_equal())
facet_labs <- gtable_filter(tab, "strip") %>% ggpubr::as_ggplot() + 
  theme(plot.margin = unit(c(0, 0.03, 0, 0.03), "npc"))

row1 <- cowplot::plot_grid(ggplot() + theme_void(),
                           two11_10 %>% add_pies(plot = ., pie_data = fig2_data[c(1, 3, 5, 27),], 
                                                 prop = species, 
                                                 linetype = 1,
                                                 linewidth = 1,
                                                 pie_radius = 0.3) + theme(legend.position = "none"), 
                           ggplot() + theme_void(),
                           two11_0.5 %>% add_pies(plot = ., data = fig2_data, 
                                                  prop = species, 
                                                  linetype = 1,
                                                  linewidth = 1,
                                                  pie_radius = 0.3) + theme(legend.position = "none"), 
                           ggplot() + theme_void(),
                           two11_01 %>% add_pies(plot = ., pie_data = fig2_data[c(2, 4, 6, 36),], 
                                                 prop = species, 
                                                 linetype = 1,
                                                 linewidth = 1,
                                                 pie_radius = 0.3) + theme(legend.position = "none"), 
                           ggplot() + theme_void(),
                           nrow = 1,
                           rel_widths = c(0.025, 1, 0.025, 1, 0.025, 1, 0.025))
# row1

row2 <- cowplot::plot_grid(ggplot() + theme_void(),
                           two21_10 %>% add_pies(plot = ., pie_data = fig2_data[c(1, 3, 5, 27),], 
                                                 prop = species, 
                                                 linetype = 1,
                                                 linewidth = 1,
                                                 pie_radius = 0.3) + theme(legend.position = "none"), 
                           ggplot() + theme_void(),
                           two21_0.5 %>% add_pies(plot = ., data = fig2_data, 
                                                  prop = species, 
                                                  linetype = 1,
                                                  linewidth = 1,
                                                  pie_radius = 0.3) + theme(legend.position = "none"), 
                           ggplot() + theme_void(),
                           two21_01 %>% add_pies(plot = ., pie_data = fig2_data[c(2, 4, 6, 36),], 
                                                 prop = species, 
                                                 linetype = 1,
                                                 linewidth = 1,
                                                 pie_radius = 0.3) + theme(legend.position = "none"), 
                           ggplot() + theme_void(),
                           nrow = 1,
                           rel_widths = c(0.025, 1, 0.025, 1, 0.025, 1, 0.025))
# row2

row3 <- cowplot::plot_grid(ggplot() + theme_void(),
                           two31_10 %>% add_pies(plot = ., pie_data = fig2_data[c(1, 3, 5, 27),], 
                                                 prop = species, 
                                                 linetype = 1,
                                                 linewidth = 1,
                                                 pie_radius = 0.3) + theme(legend.position = "none"), 
                           ggplot() + theme_void(),
                           two31_0.5 %>% add_pies(plot = ., data = fig2_data, 
                                                  prop = species, 
                                                  linetype = 1,
                                                  linewidth = 1,
                                                  pie_radius = 0.3) + theme(legend.position = "none"), 
                           ggplot() + theme_void(),
                           two31_01 %>% add_pies(plot = ., pie_data = fig2_data[c(2, 4, 6, 36),], 
                                                 prop = species, 
                                                 linetype = 1,
                                                 linewidth = 1,
                                                 pie_radius = 0.3) + theme(legend.position = "none"), 
                           ggplot() + theme_void(),
                           nrow = 1,
                           rel_widths = c(0.025, 1, 0.025, 1, 0.025, 1, 0.025))
# row3

figS2 <- cowplot::plot_grid(
  ggplot() + theme_void(),
  cowplot::plot_grid(
    pie_legend,
    colour_bar_un,
    nrow = 1, rel_widths = c(1, 1)),
  ggplot() + theme_void(),
  facet_labs,
  row1, row2, row3,
  ncol = 1,
  rel_heights = c(0.075, 0.1, 0.05, 0.15, 1, 1, 1),
  labels = c("","", "","","(a)", "(b)", "(c)"),
  label_size = 20)
# figS2

ggsave(filename = "Figure S2.png", plot = figS2,
       width = 18, height = 19, units = "in",
       device = "png", dpi = 300, bg = "white")


################################################################################################################################################################################
### Figure S3
## F1
f1 <-  c(1, 0, 0)
fa1 <- test_data %>% filter(Function == "F1")
fa1_inf <- get_inf(data = test_data,
                   model = model,
                   weights = f1)

# Check TO is correct
mono_pred1 <- fig3_model_data %>% filter(!!sym(get_best_mono(model, weights = f1)) == 1)
mono_mean1 <- weighted.mean(mono_pred1$.Pred, f1)
all(
  sapply(1:nrow(fa1), function(x){
    all.equal((fa1$.Pred[x] - mono_mean1), ((fa1_inf$TO[x])))
  })
)

# Check OY is correct
oy_data <- test_data
oy_data[, species] <- 0
all.equal(fa1_inf$OY, 
          add_prediction(oy_data, model = model) %>%
            group_by(ID) %>% 
            summarise(.Pred = sum(.Pred * f1)) %>% 
            .$.Pred, 
          check.attributes = FALSE)


## F2
f2 <-  c(0, 1, 0)
fa2 <- test_data %>% filter(Function == "F2")
fa2_inf <- get_inf(data = test_data,
                   model = model,
                   weights = f2) 

# Checking TO is correct
mono_pred2 <- fig3_model_data %>% filter(!!sym(get_best_mono(model, weights = f2)) == 1)
mono_mean2 <- weighted.mean(mono_pred2$.Pred, f2)
all(
  sapply(1:nrow(fa2), function(x){
    all.equal((fa2$.Pred[x] - mono_mean2), (fa2_inf$TO[x]))
  })
)

# Check OY is correct
oy_data <- test_data
oy_data[, species] <- 0
all.equal(fa2_inf$OY, 
          add_prediction(oy_data, model = model) %>%
            group_by(ID) %>% 
            summarise(.Pred = sum(.Pred * f2)) %>% 
            .$.Pred, 
          check.attributes = FALSE)

## F3
f3 <-  c(0, 0, 1)
fa3 <- test_data %>% filter(Function == "F3")
fa3_inf <- get_inf(data = test_data,
                   model = model,
                   weights = f3) 

# Checking TO is correct
mono_pred3 <- fig3_model_data %>% filter(!!sym(get_best_mono(model, weights = f3)) == 1)
mono_mean3 <- weighted.mean(mono_pred2$.Pred, f3)
all(
  sapply(1:nrow(fa3), function(x){
    all.equal((fa3$.Pred[x] - mono_mean3), (fa3_inf$TO[x]))
  })
)

# Check OY is correct
oy_data <- test_data
oy_data[, species] <- 0
all.equal(fa3_inf$OY, 
          add_prediction(oy_data, model = model) %>%
            group_by(ID) %>% 
            summarise(.Pred = sum(.Pred * f3)) %>% 
            .$.Pred, 
          check.attributes = FALSE)


# All is good now create plot
fa1 <- fa1 %>% ungroup() %>% copy_attributes(fig3_data) %>% 
  mutate(TO = fa1_inf$TO,
         OY = fa1_inf$OY,
         TO_se = fa1_inf$TO_se,
         OY_se = fa1_inf$OY_se) %>% 
  mutate(TO.lwr = TO - 1.96 * TO_se,
         TO.upr = TO + 1.96 * TO_se,
         OY.lwr = OY - 1.96 * OY_se,
         OY.upr = OY + 1.96 * OY_se) %>% 
  mutate(TO_class = ifelse(TO.upr > 0 & TO.lwr < 0, 0.5, ifelse(TO.lwr > 0, 1, 0)),
         OY_class = ifelse(TO.upr > 0 & OY.lwr < 0, 0.5, ifelse(OY.lwr > 0, 1, 0)),
         Group = "MF value 1") %>% 
  mutate(TO_class = factor(TO_class, levels = c("0", "0.5", "1")),
         OY_class = factor(OY_class, levels = c("0", "0.5", "1")))

fa2 <- fa2 %>% ungroup() %>% copy_attributes(fig3_data) %>% 
  mutate(TO = fa2_inf$TO,
         OY = fa2_inf$OY,
         TO_se = fa2_inf$TO_se,
         OY_se = fa2_inf$OY_se) %>% 
  mutate(TO.lwr = TO - 1.96 * TO_se,
         TO.upr = TO + 1.96 * TO_se,
         OY.lwr = OY - 1.96 * OY_se,
         OY.upr = OY + 1.96 * OY_se) %>% 
  mutate(TO_class = ifelse(TO.upr > 0 & TO.lwr < 0, 0.5, ifelse(TO.lwr > 0, 1, 0)),
         OY_class = ifelse(TO.upr > 0 & OY.lwr < 0, 0.5, ifelse(OY.lwr > 0, 1, 0)),
         Group = "MF value 2") %>% 
  mutate(TO_class = factor(TO_class, levels = c("0", "0.5", "1")),
         OY_class = factor(OY_class, levels = c("0", "0.5", "1")))

fa3 <- fa3 %>% ungroup() %>% copy_attributes(fig3_data) %>% 
  mutate(TO = fa3_inf$TO,
         OY = fa3_inf$OY,
         TO_se = fa3_inf$TO_se,
         OY_se = fa3_inf$OY_se) %>% 
  mutate(TO.lwr = TO - 1.96 * TO_se,
         TO.upr = TO + 1.96 * TO_se,
         OY.lwr = OY - 1.96 * OY_se,
         OY.upr = OY + 1.96 * OY_se) %>% 
  mutate(TO_class = ifelse(TO.upr > 0 & TO.lwr < 0, 0.5, ifelse(TO.lwr > 0, 1, 0)),
         OY_class = ifelse(TO.upr > 0 & OY.lwr < 0, 0.5, ifelse(OY.lwr > 0, 1, 0)),
         Group = "MF value 2") %>% 
  mutate(TO_class = factor(TO_class, levels = c("0", "0.5", "1")),
         OY_class = factor(OY_class, levels = c("0", "0.5", "1")))


f1_plot_un <- create_plots_un(fa1, model = f1_mod, 
                              mono_thres = 11.8)  
f2_plot_un <- create_plots_un(fa2, model = f2_mod,                              
                              mono_thres = 11.8)  
f3_plot_un <- create_plots_un(fa3, model = f3_mod, 
                              mono_thres = 11.8)  

pred_scale_un <- pull_legend(two_12 + 
                               guides(fill = guide_colorbar(title = "Predicted\nResponse", 
                                                            ticks.colour = "black", 
                                                            frame.colour = "black"))  + 
                               theme(legend.title = element_text(size = 18, vjust = 3.5),
                                     legend.text = element_text(size = 16))) %>% 
  mark_legend(val = 11.75, col = "navy", min = 5, max = 17)

label <- textGrob(x = 0.69, y = 0.55, label = "TO", 
                  gp = gpar(size = 5, fontface = "bold"))

pos <-  subset(pred_scale_un$layout, grepl("guides", name), t:r)

width = unit(1, "grobwidth", label) + unit(10, "points")
height = unit(1, "grobheight", label)+ unit(10, "points")
pred_scale_un <- gtable_add_rows(pred_scale_un, height, pos = pos$t-1)
pred_scale_un <- gtable_add_grob(pred_scale_un, label, 
                                 t = 3, l = 3, r = 4)

opt_leg <- pull_legend(
  ggplot() + geom_point(aes(x = 1, y= 1,fill = "cyan3"), colour = "black", shape = 24, size = 4) + 
    scale_fill_identity("", labels = "Optimal community", 
                        guide = "legend") + 
    xlim(-0.95, 1) +
    theme(legend.key = element_rect(colour = NA, fill = NA),
          legend.position = "top",
          legend.text = element_text(size = 16, vjust = 1)))

s3_labs <- ggplotGrob(ggplot(data = data.frame(facet = fct_inorder(c("Prediction",
                                                                     "Over-performance",
                                                                     "Transgressive over-performance")))) + 
                        facet_wrap(~facet) +
                        theme(strip.text = element_text(size = 20,
                                                        margin = unit(c(0.1, 0, 0.1, 0), "cm")),
                              strip.background = element_rect(colour = "black", fill = "#DDDDDD"),
                              panel.spacing = unit(0.025, "npc"),
                              plot.margin = unit(c(0, 0.01, 0, 0.01), "npc")) + 
                        coord_equal()) %>% 
  gtable_filter("strip") %>% 
  ggpubr::as_ggplot() + 
  theme(plot.margin = unit(c(0, 0.025, 0, 0.025), "npc"))


s3_leg <- cowplot::plot_grid(ggplot() + theme_void(),
                             pred_scale_un, 
                             ggplot() + theme_void(),
                             cowplot::plot_grid(opt_leg,
                                                ggplot() + theme_void(),
                                                pull_legend(wa2_OY),
                                                nrow = 3,
                                                rel_heights = c(1, 0.25, 1)),
                             ggplot() + theme_void(),
                             cowplot::plot_grid(pie_legend,
                                                ggplot() + theme_void(),
                                                pull_legend(wa2_TO), 
                                                nrow = 3,
                                                rel_heights = c(1, 0.25, 1)),
                             ggplot() + theme_void(),
                             nrow = 1,
                             rel_widths = c(0.025, 1, 0.025, 1, 0.025, 1, 0.025))


figS3 <- cowplot::plot_grid(
  ggplot() + theme_void(),
  s3_labs,
  f1_plot_un,
  #ggplot() + theme_void(),
  f2_plot_un,
  #ggplot() + theme_void(),
  f3_plot_un,
  s3_leg,
  ggplot() + theme_void(),
  ncol = 1,
  labels = c("","","(a)", "(b)", "(c)", "",""),
  label_size = 20,
  hjust = -0.15,
  rel_heights = c(0.05, 0.1, 1, 1, 1, 0.25, 0.05),
  greedy = FALSE
)
# figS3

ggsave(filename = "Figure S3.png", plot = figS3,
       width = 17.85, height = 21, units = "in",
       device = "png", dpi = 300, bg = "white")
