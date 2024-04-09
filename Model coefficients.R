# Helper functions for creating plots
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
source(paste0(dir, "/Helpers.R"))

# Directory to store figures
setwd(paste0(dir, "/Figures"))

# Simulating data for figures
species <- c("G1", "G2", "L1", "L2", "H1", "H2")
FG <- c("Gr", "Gr", "Le", "Le", "He", "He")
fig2_design <- DImodelsVis::get_equi_comms(6, variables = paste0("p", 1:6)) %>% 
  distinct(p1, p2, p3, p4, p5 , p6) %>% 
  rbind(ifelse(diag(0.2, 6) == 0, 0.2, 0) %>% `colnames<-`(paste0("p",1:6))) %>% 
  `colnames<-`(species) 

# ID effects
f1_ID_effects <- f2_ID_effects <- f3_ID_effects <- c(11.75, 10.47, 7.48, 8.66, 5.95, 4.75)

# Interaction effects
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

# Create Table S2
coeff_names <- c("\u03B2G1", "\u03B2G2", "\u03B2L1", "\u03B2L2", "\u03B2H1", "\u03B2H2",
                 "\u03B4AV",
                 "\u03C9GL", "\u03C9GH", "\u03C9LH", "\u03C9GG", "\u03C9LL", "\u03C9HH",
                 "\u03B8")
coeff_data <- data.frame(Coefficient = coeff_names,
                         F1 = rep("-", times = 14), 
                         F2 = rep("-", times = 14), 
                         F3 = rep("-", times = 14)) %>% 
  `rownames<-`(NULL)

for(func in Fs){
  idx <- switch (func,
    "F1" = c(1:6, 14),
    "F2" = c(1:7, 14),
    "F3" = c(1:6, 9, 8, 10, 11, 13, 12, 14))
  
  mod <- switch (func,
                 "F1" = f1_mod,
                 "F2" = f2_mod,
                 "F3" = f3_mod)
  
  theta_val <- switch (func,
                       "F1" = "-",
                       "F2" = "0.5",
                       "F3" = "1")
  
  summ <- summary(mod)$coefficients
  coeff_data[idx, func] <- c(paste(round(summ[, 1], 4),  "\u00B1", round(summ[, 2], 4)), theta_val)
}

# Save (Remember to not have the file open while running this code)
grDevices::cairo_pdf("Table S2.pdf", height = 5, width = 7)
grid.table(coeff_data, rows = NULL)
dev.off()




