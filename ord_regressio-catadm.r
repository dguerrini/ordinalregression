# Load necessary libraries
library(tidyverse)
library(MASS)   # For ordinal regression
library(ggplot2)
library(gridExtra)
library(dplyr)

data_path <- "/home/dguerrini/CascadeProjects/windsurf-project/academic-survey-analysis/data/question_docentes-ppg.csv"
survey_data <- read.csv(data_path)

# Filtrar e recodificar cat_adm
survey_data <- survey_data %>%
  filter(genero != "Prefiro não responder") %>%
  filter(raca != "Amarela") %>%
  filter(raca != "Prefiro não responder") %>%
  mutate(cat_adm = case_when(
    cat_adm %in% c("Pública Federal", "Pública Estadual") ~ "Pública",
    cat_adm %in% c("Privada com fins lucrativos", "Privada sem fins lucrativos") ~ "Privada",
    TRUE ~ cat_adm
  ))

# Preparar fatores
survey_data <- survey_data %>%
  mutate(
    genero = factor(genero),
    raca = factor(raca) %>% relevel(ref = "Branca"),
    escol_pai = factor(escol_pai),
    cat_adm = factor(cat_adm) %>% relevel(ref = "Pública")  # Usando Pública como referência
  )

# Define predictor and activity columns
predictor_cols <- c("genero", "raca", "idade", "escol_pai", "cat_adm")  # Adicionado cat_adm
activity_cols <- c("docencia", "pos_lato_s", "pos_stricto_s", "extensao", 
                  "projeto_pesq", "administracao", "orienta_grad", 
                  "orienta_stricto_s", "orienta_lato_s")

# Create activity name mapping
activity_names <- c(
  "docencia" = "docência na graduação",
  "pos_lato_s" = "docência na pós lato sensu",
  "pos_stricto_s" = "docência na pós stricto sensu",
  "extensao" = "extensão",
  "projeto_pesq" = "projeto de pesquisa",
  "administracao" = "administração",
  "orienta_grad" = "orientação na graduação",
  "orienta_stricto_s" = "orientação na pós stricto sensu",
  "orienta_lato_s" = "orientação na pós lato sensu"
)

# Função run_ordinal_analysis modificada para incluir cat_adm nos labels
run_ordinal_analysis <- function(activity, data) {
  activity_label <- activity_names[activity]
  
  if (is.na(activity_label)) {
    stop(paste("Activity", activity, "not found in activity_names mapping"))
  }
  
  data[[activity]] <- factor(data[[activity]], ordered = TRUE)
  
  model_formula <- as.formula(paste(activity, "~", paste(predictor_cols, collapse = " + ")))
  model <- polr(model_formula, data = data, Hess = TRUE)
  
  ctable <- coef(summary(model))
  coef_names <- names(coef(model))
  threshold_pattern <- "^[0-9]"
  coef_indices <- which(!grepl(threshold_pattern, coef_names))
  
  p_values <- 2 * (1 - pnorm(abs(ctable[coef_indices, "t value"])))
  ci <- confint.default(model)
  ci_coef <- ci[coef_indices, ]
  
  summary_df <- data.frame(
    Coefficient = coef_names[coef_indices],
    Estimate = coef(model)[coef_indices],
    SE = ctable[coef_indices, "Std. Error"],
    p_value = p_values,
    CI_lower = ci_coef[, 1],
    CI_upper = ci_coef[, 2]
  ) %>%
    mutate(
      percent_change = (exp(Estimate) - 1) * 100,
      CI_lower_pct = (exp(CI_lower) - 1) * 100,
      CI_upper_pct = (exp(CI_upper) - 1) * 100,
      is_significant = p_value < 0.05
    )
  
  summary_df <- summary_df %>%
    mutate(
      Coefficient = case_when(
        Coefficient == "racaParda" ~ "raça parda",
        Coefficient == "racaBranca" ~ "raça branca",
        Coefficient == "generoMasculino" ~ "masculino",
        Coefficient == "idade" ~ "idade",
        Coefficient == "cat_admPrivada" ~ "instituição privada",  # Nova categoria
        str_detect(Coefficient, "pos-ls") ~ str_replace(Coefficient, "pos-ls", "pós lato sensu"),
        str_detect(Coefficient, "pos-ss") ~ str_replace(Coefficient, "pos-ss", "pós stricto sensu"),
        startsWith(Coefficient, "escol_pai") ~ str_replace(Coefficient, "escol_pai", ""),
        TRUE ~ Coefficient
      )
    )
  
  coef_plot <- ggplot(summary_df, aes(x = Coefficient, y = percent_change)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "#E41A1C", linewidth = 0.8) +
    geom_point(aes(color = is_significant), size = 4) +
    geom_errorbar(aes(ymin = CI_lower_pct, 
                     ymax = CI_upper_pct,
                     color = is_significant), 
                 width = 0.3, 
                 linewidth = 1) +
    geom_text(aes(label = sprintf("%.1f%%", percent_change)), 
              hjust = -0.5,
              size = 3.5) +
    scale_color_manual(values = c("TRUE" = "#2171B5", "FALSE" = "#969696"),
                      labels = c("TRUE" = "p < 0,05", "FALSE" = "p ≥ 0,05"),
                      name = "Significância\nEstatística") +
    coord_flip() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", margin = margin(b = 20)),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold")
    ) +
    labs(
      title = paste("Variação Percentual nas Chances para", activity_label),
      x = "Preditores",
      y = "Variação Percentual nas Chances (%)"
    )
  
  return(list(
    model = model,
    summary = summary_df,
    plots = list(
      coefficients = coef_plot
    )
  ))
}

# Criar diretório para os plots
dir.create("plots_catadm", showWarnings = FALSE)

# Rodar análise para todas as atividades
activity_results <- list()
for(activity in activity_cols) {
  activity_results[[activity]] <- run_ordinal_analysis(activity, survey_data)
  
  ggsave(
    filename = paste0("plots_catadm/", activity, "_coefficients.png"),
    plot = activity_results[[activity]]$plots$coefficients,
    width = 12,
    height = 8,
    dpi = 300
  )
}

# Para ver os resultados
print(activity_results[["orienta_grad"]]$summary)

rm(list = ls())
