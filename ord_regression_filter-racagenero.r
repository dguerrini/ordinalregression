# Load necessary libraries
library(tidyverse)
library(MASS)   # For ordinal regression
library(ggplot2)
library(gridExtra)
library(dplyr)

# Desativar notação científica
options(scipen = 999)

data_path <- "/home/dguerrini/CascadeProjects/windsurf-project/academic-survey-analysis/data/question_docentes-ppg.csv"
survey_data <- read.csv(data_path)

# Criar dois datasets separados: público e privado
public_data <- survey_data %>%
  filter(cat_adm %in% c("Pública Federal", "Pública Estadual")) %>%
  filter(genero != "Prefiro não responder") %>%
  filter(raca != "Amarela") %>%
  filter(raca != "Prefiro não responder")

private_data <- survey_data %>%
  filter(cat_adm %in% c("Privada com fins lucrativos", "Privada sem fins lucrativos")) %>%
  filter(genero != "Prefiro não responder") %>%
  filter(raca != "Amarela") %>%
  filter(raca != "Prefiro não responder")

# Preparar fatores para ambos os datasets
public_data <- public_data %>%
  mutate(
    genero = factor(genero),
    raca = factor(raca) %>% relevel(ref = "Branca"),
    escol_pai = factor(escol_pai)
  )

private_data <- private_data %>%
  mutate(
    genero = factor(genero),
    raca = factor(raca) %>% relevel(ref = "Branca"),
    escol_pai = factor(escol_pai)
  )

# Define predictor and activity columns
predictor_cols <- c("genero", "raca", "idade", "escol_pai")
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

# Function to run ordinal regression and create plots
run_ordinal_analysis <- function(activity, data, institution_type) {
  activity_label <- activity_names[activity]
  
  if (is.na(activity_label)) {
    stop(paste("Activity", activity, "not found in activity_names mapping"))
  }
  
  # Convert activity to factor
  data[[activity]] <- factor(data[[activity]], ordered = TRUE)

  # Fit ordinal regression model
  model_formula <- as.formula(paste(activity, "~", paste(predictor_cols, collapse = " + ")))
  model <- polr(model_formula, data = data, Hess = TRUE)
  
  # Calculate p-values
  ctable <- coef(summary(model))
  
  # Pegar todos os coeficientes exceto os thresholds
  coef_names <- names(coef(model))
  threshold_pattern <- "^[0-9]" # Padrão que identifica os thresholds (começam com números)
  coef_indices <- which(!grepl(threshold_pattern, coef_names))
  
  # Calcular p-valores apenas para os coeficientes
  p_values <- 2 * (1 - pnorm(abs(ctable[coef_indices, "t value"])))
  
  # Calcular intervalos de confiança
  ci <- confint.default(model)
  ci_coef <- ci[coef_indices, ]
  
  # Criar dataframe com todos os coeficientes
  summary_df <- data.frame(
    Coefficient = coef_names[coef_indices],
    Estimate = coef(model)[coef_indices],
    SE = ctable[coef_indices, "Std. Error"],
    p_value = p_values,
    CI_lower = ci_coef[, 1],
    CI_upper = ci_coef[, 2]
  ) %>%
    # Converter para mudanças percentuais nas chances
    mutate(
      percent_change = (exp(Estimate) - 1) * 100,
      CI_lower_pct = (exp(CI_lower) - 1) * 100,
      CI_upper_pct = (exp(CI_upper) - 1) * 100,
      is_significant = p_value < 0.05
    )
  
  # Inside run_ordinal_analysis function, add label cleaning before creating the plot
  summary_df <- summary_df %>%
    mutate(
      # Limpar os nomes dos coeficientes
      Coefficient = case_when(
        Coefficient == "racaParda" ~ "raça parda",
        Coefficient == "racaBranca" ~ "raça branca",
        Coefficient == "generoMasculino" ~ "masculino",
        Coefficient == "idade" ~ "idade",
        str_detect(Coefficient, "pos-ls") ~ str_replace(Coefficient, "pos-ls", "pós lato sensu"),
        str_detect(Coefficient, "pos-ss") ~ str_replace(Coefficient, "pos-ss", "pós stricto sensu"),
        startsWith(Coefficient, "escol_pai") ~ str_replace(Coefficient, "escol_pai", ""),
        TRUE ~ Coefficient
      )
    )
  
  # Dentro do run_ordinal_analysis, antes de criar o plot
  # Ajustar o título da atividade
  activity_label <- str_replace_all(activity_label, 
                                  c("pos-ls" = "pós lato sensu",
                                    "pos-ss" = "pós stricto sensu"))
  
  # Resto do código do plot continua igual
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
      title = paste("Variação Percentual nas Chances para", activity_label, 
                   ifelse(institution_type == "public", "- IES públicas", "- IES privadas")),
      x = "Preditores",
      y = "Variação Percentual nas Chances (%)"
    )
  
  # Predicted probabilities plot for gender
  pred_data <- expand.grid(
    genero = unique(data$genero),
    raca = levels(data$raca)[1],  # Use first level instead of median
    idade = median(data$idade),
    escol_pai = levels(data$escol_pai)[1]  # Use first level instead of median
  )
  
  pred_probs <- predict(model, pred_data, type = "probs")
  pred_data <- cbind(pred_data, as.data.frame(pred_probs))
  pred_data_long <- tidyr::gather(pred_data, key = "Response", value = "Probability", 
                                 -genero, -raca, -idade, -escol_pai)
  
  # Enhanced probability plot
  prob_plot <- ggplot(pred_data_long, aes(x = Response, y = Probability, fill = genero)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) +
    labs(title = paste("Predicted Probabilities by Gender for", activity_label),
         x = "Response Level",
         y = "Probability",
         fill = "Gênero")
  
  # Return results as a list
  return(list(
    model = model,
    summary = summary_df,
    plots = list(
      coefficients = coef_plot,
      probabilities = prob_plot
    )
  ))
}

# Criar diretórios para os resultados
dir.create("plots_public", showWarnings = FALSE)
dir.create("plots_private", showWarnings = FALSE)

# Rodar análises para instituições públicas
public_results <- list()
for(activity in activity_cols) {
  public_results[[activity]] <- run_ordinal_analysis(activity, public_data, "public")
  
  ggsave(
    filename = paste0("plots_public/", activity, "_coefficients.png"),
    plot = public_results[[activity]]$plots$coefficients,
    width = 12,
    height = 8,
    dpi = 300
  )
}

# Rodar análises para instituições privadas
private_results <- list()
for(activity in activity_cols) {
  private_results[[activity]] <- run_ordinal_analysis(activity, private_data, "private")
  
  ggsave(
    filename = paste0("plots_private/", activity, "_coefficients.png"),
    plot = private_results[[activity]]$plots$coefficients,
    width = 12,
    height = 8,
    dpi = 300
  )
}

# Para ver os resultados de uma análise específica, por exemplo:
# Públicas
print("Resultados para Instituições Públicas:")
print(public_results[["orienta_grad"]]$summary)

# Privadas
print("Resultados para Instituições Privadas:")
print(private_results[["orienta_grad"]]$summary)

rm(list = ls())
