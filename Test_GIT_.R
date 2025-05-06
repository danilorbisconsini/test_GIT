# ==============================================================================
# Script de Simulação 2: Análise de Desempenho de Modelos
# Cenário: Conjunto de Treino Cumulativo (1 a 17 Veículos), Conjunto de Teste Variável (Restante dos Veículos)
# Amostragem: 1 Velocidade Aleatória por IRI (Segmento) por Veículo <<< CORRIGIDO
# Inclui Modelos em Escala Original e Logarítmica, com Categoria e Efeitos Aleatórios Específicos
# Extração de Coeficientes Fixos (p-valores, ICs).
# Adição de R² e R² Ajustado na Escala Original para Modelos Logarítmicos.
# Adição de Teste de Homocedasticidade.
# Ajustes em Modelos Agregados (removido preditor Vehicle_Category).
# Detalhes mais claros sobre motivos de pulo/falha.
# CORREÇÃO: Erro "$ operator is invalid for atomic vectors" ao lidar com modelos que falham.
# CORREÇÃO: Garantir que Type seja "Aggregated" ou "Mixed" para modelos bem-sucedidos.
# CORREÇÃO: Identificação correta da fórmula do modelo Agregado RMSAS/V.
# CORREÇÃO: Tratamento mais robusto de erros em cálculos de métricas para evitar NAs inconsistentes.
# IMPLEMENTAÇÃO: Agregação específica para modelos Aggregated (média dos preditores por IRI único).
# REMOÇÃO: Modelos Aggregated com múltiplos preditores, interações ou apenas intercepto.
# REMOÇÃO: Modelos Mixed com interações RMSAS * V ou log_RMSAS * log_V.
# CORREÇÃO (Baseada no feedback): Lógica de amostragem ajustada para usar IRI como identificador de segmento.
# ==============================================================================

# === INSTALAR E CARREGAR PACOTES ===
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("lme4", quietly = TRUE)) install.packages("lme4")
if (!requireNamespace("MuMIn", quietly = TRUE)) install.packages("MuMIn") # For R2 of mixed models
if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom") # For tidy() on lm models
if (!requireNamespace("broom.mixed", quietly = TRUE)) install.packages("broom.mixed") # For tidy() on lmer models
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl") # For saving to .xlsx
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr") # For saving to .csv
if (!requireNamespace("tcltk", quietly = TRUE)) install.packages("tcltk") # For tk_choose.dir()
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr") # For string manipulation
if (!requireNamespace("car", quietly = TRUE)) install.packages("car") # For ncvTest (homoscedasticity)


library(readxl)
library(dplyr)
library(lme4)
library(MuMIn)
library(broom)
library(broom.mixed)
library(writexl)
library(readr)
library(tcltk)
library(stringr)
library(car) # Load car package


# Desabilitar warnings (opcional, re-abilitar no final para debugging)
# options(warn = 0)
options(warn = -1)


# ==============================================================================
# === CONFIGURAÇÕES DO USUÁRIO ===
# ==============================================================================

# --- 1. Arquivo de Dados de Entrada ---
# O script pedirá para você selecionar o arquivo .xlsx interativamente.
# --- 2. Arquivos de Saída ---
# Os caminhos dos arquivos de saída serão definidos pela seleção interativa da pasta.
# Os nomes base serão definidos aqui:
output_detailed_basename_excel <- 'sim_test_variavel_detalhado.xlsx'
output_detailed_basename_csv <- 'sim_test_variavel_detalhado.csv'
output_summary_basename_excel <- 'sim_test_variavel_resumo.xlsx'
output_summary_basename_csv <- 'sim_test_variavel_resumo.csv'


# --- 3. Configurações da Simulação ---
n_iteracoes <- 1 # Número de repetições para cada cenário
speeds_to_exclude <- c() # Velocidades (em km/h) a excluir. Use c() para incluir todas.

# Número de veículos de treino a variar cumulativamente.
# De 1 até 17 veículos.
n_train_vehicles_list <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)

# Número mínimo de grupos (veículos) de treino para tentar ajustar um LMM.
MIN_GROUPS_FOR_LMM <- 2 # Pelo menos 2 grupos são necessários para LMM com (1|Vehicle)


# --- 4. Variável Alvo ---
# Agora testaremos IRI (escala original) e log_IRI
target_vars <- c("IRI", "log_IRI")


# --- 5. Definição da Categoria do Veículo ---
# Mapeamento de veículos para categorias. AJUSTE CONFORME SEUS DADOS.
vehicle_category_map <- list(
  Sedan = c("BMW 530i", "NISSAN SENTRA", "VW PASSAT", "RENAULT LOGAN", "BMW 5 SERIES"),
  Compacto_Hatchback = c("FORD FIESTA", "CLIO 2", "SMART FORTWO", "SMART FORFOUR", "MINI-1", "AUDI A1", "MINI-2", "MEGANE COUPE", "PEUGEOT 308 RCZ"),
  Van_MPV_SUV = c("TOYOTA SIENTA", "Renault Kangoo", "Audi Q2"),
  Classico_Unico = c("DACIA 1300")
)


# --- 6. Cenários de Modelos a Testar ---
# Definir explicitamente todas as combinações únicas de Tipo de Modelo, Fórmula de Efeitos Fixos e Fórmula de Efeitos Aleatórios.
# Removidos modelos agregados com Vehicle_Category como preditor fixo, conforme solicitado.
# Isso garante que cada cenário seja testado n_iteracoes vezes (sujeito a falhas/pulos).
# REMOVIDOS: Modelos agregados com múltiplos preditores, interações ou apenas intercepto.
# REMOVIDOS: Modelos mistos com interações.
model_scenarios <- list(
  # Modelos Agregados (LM) - Apenas com RMSAS ou RMSAS_div_V (e suas versões log) como preditor único
  list(Type = "Aggregated", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS", Random_Effects_Formula = NA),
  list(Type = "Aggregated", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS_div_V", Random_Effects_Formula = NA),
  
  list(Type = "Aggregated", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS", Random_Effects_Formula = NA),
  list(Type = "Aggregated", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS_div_log_V", Random_Effects_Formula = NA),
  
  
  # Modelos Mistos (LMM) - Combinando Fórmulas Fixas e de Efeitos Aleatórios relevantes
  # Mantida a inclusão de Vehicle_Category e diferentes estruturas de efeitos aleatórios
  # REMOVIDAS: Interações nos modelos mistos.
  # IRI as Target
  list(Type = "Mixed", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS", Random_Effects_Formula = "(1 | Vehicle)"),
  list(Type = "Mixed", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS", Random_Effects_Formula = "(1 + RMSAS | Vehicle)"),
  list(Type = "Mixed", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS + Vehicle_Category", Random_Effects_Formula = "(1 | Vehicle)"),
  list(Type = "Mixed", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS + Vehicle_Category", Random_Effects_Formula = "(1 + RMSAS | Vehicle)"),
  list(Type = "Mixed", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS_div_V", Random_Effects_Formula = "(1 | Vehicle)"),
  list(Type = "Mixed", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS_div_V", Random_Effects_Formula = "(1 + RMSAS | Vehicle)"), # Usando RMSAS no RE para VD original
  list(Type = "Mixed", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS_div_V + Vehicle_Category", Random_Effects_Formula = "(1 | Vehicle)"),
  list(Type = "Mixed", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS_div_V + Vehicle_Category", Random_Effects_Formula = "(1 + RMSAS | Vehicle)"),
  list(Type = "Mixed", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS + V", Random_Effects_Formula = "(1 | Vehicle)"),
  list(Type = "Mixed", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS + V", Random_Effects_Formula = "(1 + RMSAS | Vehicle)"), # REs comuns
  list(Type = "Mixed", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS + V + Vehicle_Category", Random_Effects_Formula = "(1 | Vehicle)"),
  list(Type = "Mixed", Target = "IRI", Fixed_Formula = "IRI ~ RMSAS + V + Vehicle_Category", Random_Effects_Formula = "(1 + RMSAS | Vehicle)"),
  
  # log_IRI as Target
  list(Type = "Mixed", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS", Random_Effects_Formula = "(1 | Vehicle)"),
  list(Type = "Mixed", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS", Random_Effects_Formula = "(1 + log_RMSAS | Vehicle)"),
  list(Type = "Mixed", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS + Vehicle_Category", Random_Effects_Formula = "(1 | Vehicle)"),
  list(Type = "Mixed", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS + Vehicle_Category", Random_Effects_Formula = "(1 + log_RMSAS | Vehicle)"),
  list(Type = "Mixed", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS_div_log_V", Random_Effects_Formula = "(1 | Vehicle)"),
  list(Type = "Mixed", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS_div_log_V", Random_Effects_Formula = "(1 + log_RMSAS | Vehicle)"),
  list(Type = "Mixed", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS_div_log_V + Vehicle_Category", Random_Effects_Formula = "(1 | Vehicle)"),
  list(Type = "Mixed", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS_div_log_V + Vehicle_Category", Random_Effects_Formula = "(1 + log_RMSAS | Vehicle)"),
  list(Type = "Mixed", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS + log_V", Random_Effects_Formula = "(1 | Vehicle)"),
  list(Type = "Mixed", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS + log_V", Random_Effects_Formula = "(1 + log_RMSAS | Vehicle)"),
  list(Type = "Mixed", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS + log_V + Vehicle_Category", Random_Effects_Formula = "(1 | Vehicle)"),
  list(Type = "Mixed", Target = "log_IRI", Fixed_Formula = "log_IRI ~ log_RMSAS + log_V + Vehicle_Category", Random_Effects_Formula = "(1 + log_RMSAS | Vehicle)")
)


# --- 7. Variáveis Necessárias para Todos os Cenários ---
# Esta lista ajuda a garantir que todas as variáveis usadas nos cenários sejam criadas e filtradas.
# Precisa incluir todas as variáveis potenciais de todas as fórmulas fixas e aleatórias.
all_fixed_vars_in_scenarios <- unique(unlist(lapply(model_scenarios, function(s) all.vars(as.formula(s$Fixed_Formula)))))

all_random_vars_in_scenarios <- unique(unlist(lapply(model_scenarios, function(s) {
  if (!is.na(s$Random_Effects_Formula)) {
    re_formula_str <- s$Random_Effects_Formula
    # Usar all.vars em um objeto fórmula criado de forma segura com ~
    temp_formula_obj <- as.formula(paste("~", re_formula_str))
    return(all.vars(temp_formula_obj))
  } else {
    return(character(0))
  }
})))


# Combinar variáveis fixas, aleatórias necessárias e as variáveis alvo (orig/log)
# Incluir todas as transformações potenciais e bases que podem ser usadas em qualquer cenário
# REMOVIDO: Segment_ID, pois IRI é usado como identificador de segmento agora
required_vars_all_scenarios <- unique(c(
  all_fixed_vars_in_scenarios,
  all_random_vars_in_scenarios,
  "Vehicle", "IRI",        # Vehicle e IRI são sempre necessários
  "IRI", "log_IRI",        # Original and Log target vars (IRI já incluído)
  "RMSAS", "log_RMSAS",    # Original and Log RMSAS
  "V", "log_V",            # Original and Log V
  "RMSAS_div_V",          # Derived original
  "log_RMSAS_div_log_V",  # Derived log
  "RMSASxV",              # Interaction original - Still needed for data prep, even if not in final models
  "log_RMSASxlog_V",      # Interaction log - Still needed for data prep
  "Vehicle_Category"      # Category variable
))


# ==============================================================================
# === FIM DAS CONFIGURAÇÕES DO USUÁRIO ===
# ==============================================================================


# === FUNÇÕES AUXILIARES ===

# Function to aggregate data for Aggregated models (mean of predictors per unique target value)
aggregate_data_by_target <- function(df_data, target_var, fixed_formula_str) {
  if (nrow(df_data) == 0) return(df_data[0, ]) # Return empty df with correct columns
  
  # Extract predictor variable names from the fixed formula string
  predictor_vars <- all.vars(as.formula(fixed_formula_str))
  predictor_vars <- setdiff(predictor_vars, target_var) # Remove target variable
  
  if (length(predictor_vars) == 0) {
    # If no predictors (e.g., just intercept, although removed now), return data grouped by target
    cat(paste0("		 Aviso: Nenhum preditor encontrado na fórmula fixa '", fixed_formula_str, "'. Agregando apenas pela variável alvo.\n"))
    df_agg <- df_data %>%
      group_by(across(all_of(target_var))) %>%
      summarise(N_obs = n(), .groups = 'drop')
    # Add predictor columns with NA to match expected structure downstream
    for(pred_var in predictor_vars) {
      df_agg[[pred_var]] <- NA
    }
    return(df_agg)
  }
  
  # Ensure predictor variables exist in the data
  if (!all(predictor_vars %in% colnames(df_data))) {
    stop(paste0("Variáveis preditoras ", paste(setdiff(predictor_vars, colnames(df_data)), collapse = ", "), " não encontradas nos dados para agregação."))
  }
  
  # Group by the target variable and calculate the mean of predictor variables
  df_agg <- df_data %>%
    group_by(across(all_of(target_var))) %>%
    summarise(
      across(all_of(predictor_vars), ~mean(., na.rm = TRUE)),
      N_obs_agg = n(), # Number of original observations aggregated into this row
      .groups = 'drop'
    )
  
  if (nrow(df_agg) == 0) {
    cat(paste0("		 Aviso: Agregação por variável alvo resultou em dataframe vazio para fórmula '", fixed_formula_str, "'.\n"))
  }
  
  return(df_agg)
}


# Function to calculate metrics and format results for Aggregated models (lm)
# NOW USES AGGREGATED DATA
run_aggregated_model <- function(df_train_agg, df_test_agg, formula, target_var, rep, n_train_vehicles, n_test_vehicles, analysis_name) {
  
  # Wrap model fitting in tryCatch
  model_fit_result <- tryCatch({
    model <- lm(formula, data = df_train_agg)
    # Return the model object and NULL for fail_reason if successful
    list(model = model, fail_reason = NULL)
  }, error = function(e) {
    cat(paste0("\n		 ERROR fitting Aggregated Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", target_var, ", Formula=", as.character(formula)[2], "] -> ", e$message, "\n"))
    # Capture specific error message
    fail_reason <- paste0("Failed Fit: ", gsub("\\s+", " ", trimws(e$message)))
    return(list(model = NULL, fail_reason = fail_reason)) # Return NULL model and reason on error
  })
  
  # If model fitting failed, return NA results with specific reason
  if (is.null(model_fit_result$model)) {
    result_row <- list(
      Rep = rep, Analysis = analysis_name, N_train = n_train_vehicles, N_test = n_test_vehicles,
      Type = paste0("Aggregated (", model_fit_result$fail_reason, ")"), # Include specific error message
      Target = target_var, Model_Formula = as.character(formula)[2], Random_Effects_Formula = NA,
      RMSE_train_orig_mean = NA, MAE_train_orig_mean = NA, RMSE_test_orig_mean = NA, MAE_test_orig_mean = NA,
      R2_train_Model_Scale = NA, R2_adj_train_Model_Scale = NA, R2_test_Model_Scale = NA, R2_adj_test_Model_Scale = NA,
      R2_train_orig_from_log_Mean = NA, R2_adj_train_orig_from_log_Mean = NA,
      R2_test_orig_from_log_Mean = NA, R2_adj_test_orig_from_log_Mean = NA, # R2 and Adj R2 on original scale for log models
      AIC_train_Model_Scale = NA, BIC_train_Model_Scale = NA, AIC_test_Model_Scale = NA, BIC_test_Model_Scale = NA,
      Homoscedasticity_BP_p_value_train = NA, # Homoscedasticity test p-value
      R2_train_conditional_Model_Scale = NA, R2_train_marginal_Model_Scale = NA, RE_Var = NA, RE_Cov = list(NA), RE_Variances = list(NA),
      Fixed_Effects_Coefficients = list(data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA))
    )
    return(result_row)
  }
  
  # Extract the actual model object if fitting was successful
  model <- model_fit_result$model
  
  # Determine the original scale target variable name for metric calculation
  original_target_var = "IRI" # Original scale is always IRI
  
  
  # Predictions on train and test (scale of the model) - Using aggregated data
  predictions_train_model_scale <- tryCatch({ predict(model, newdata = df_train_agg) }, error = function(e) { rep(NA, nrow(df_train_agg)) })
  predictions_test_model_scale <- tryCatch({ predict(model, newdata = df_test_agg) }, error = function(e) { rep(NA, nrow(df_test_agg)) })
  
  
  # --- Back-transform predictions to original scale if target is log_IRI ---
  predictions_train_orig <- rep(NA, nrow(df_train_agg))
  predictions_test_orig <- rep(NA, nrow(df_test_agg))
  smearing_factor <- NA
  
  if (target_var == "log_IRI") {
    # Calculate smearing factor on train data (original scale residuals)
    finite_train_indices_log = is.finite(df_train_agg[[target_var]]) & is.finite(predictions_train_model_scale)
    if(sum(finite_train_indices_log) > 0) {
      residuals_train_log = df_train_agg[[target_var]][finite_train_indices_log] - predictions_train_model_scale[finite_train_indices_log]
      smearing_factor <- tryCatch({ mean(exp(residuals_train_log), na.rm = TRUE) }, error = function(e) { NA })
    }
    
    # Apply smearing factor to get predictions in original scale
    if(is.finite(smearing_factor)){
      predictions_train_orig <- exp(predictions_train_model_scale) * smearing_factor
      predictions_test_orig <- exp(predictions_test_model_scale) * smearing_factor
    }
  } else { # If target_var is already original scale (e.g., "IRI")
    predictions_train_orig <- predictions_train_model_scale
    predictions_test_orig <- predictions_test_model_scale
  }
  
  
  # Metrics in original scale (using original_target_var and predictions_orig)
  # Note: Metrics are calculated on the AGGREGATED original target values
  finite_train_orig_indices <- is.finite(predictions_train_orig) & is.finite(df_train_agg[[original_target_var]])
  rmse_train_orig <- if(sum(finite_train_orig_indices) > 0) tryCatch({ sqrt(mean((predictions_train_orig[finite_train_orig_indices] - df_train_agg[[original_target_var]][finite_train_orig_indices])^2, na.rm = TRUE)) }, error = function(e) { NA }) else NA
  mae_train_orig <- if(sum(finite_train_orig_indices) > 0) tryCatch({ mean(abs(predictions_train_orig[finite_train_orig_indices] - df_train_agg[[original_target_var]][finite_train_orig_indices]), na.rm = TRUE) }, error = function(e) { NA }) else NA
  
  finite_test_orig_indices <- is.finite(predictions_test_orig) & is.finite(df_test_agg[[original_target_var]])
  rmse_test_orig <- if(sum(finite_test_orig_indices) > 0) tryCatch({ sqrt(mean((predictions_test_orig[finite_test_orig_indices] - df_test_agg[[original_target_var]][finite_test_orig_indices])^2, na.rm = TRUE)) }, error = function(e) { NA }) else NA
  mae_test_orig <- if(sum(finite_test_orig_indices) > 0) tryCatch({ mean(abs(predictions_test_orig[finite_test_orig_indices] - df_test_agg[[original_target_var]][finite_test_orig_indices]), na.rm = TRUE) }, error = function(e) { NA }) else NA
  
  
  # Metrics in model scale (R2, AIC, BIC) - Using aggregated data
  r2_train_model_scale <- tryCatch({ summary(model)$r.squared }, error = function(e) { NA })
  r2_adj_train_model_scale <- tryCatch({ summary(model)$adj.r.squared }, error = function(e) { NA })
  
  # R2 on test (model scale)
  finite_test_model_scale_indices <- is.finite(predictions_test_model_scale) & is.finite(df_test_agg[[target_var]])
  n_test_finite_obs <- sum(finite_test_model_scale_indices)
  
  rss_test_model_scale <- if(n_test_finite_obs > 0) tryCatch({ sum((predictions_test_model_scale[finite_test_model_scale_indices] - df_test_agg[[target_var]][finite_test_model_scale_indices])^2, na.rm = TRUE) }, error = function(e) { NA }) else NA
  tss_test_model_scale <- if(n_test_finite_obs > 0) tryCatch({ sum((df_test_agg[[target_var]][finite_test_model_scale_indices] - mean(df_test_agg[[target_var]][finite_test_model_scale_indices], na.rm = TRUE))^2, na.rm = TRUE) }, error = function(e) { NA })
  
  r2_test_model_scale <- if(is.finite(rss_test_model_scale) && is.finite(tss_test_model_scale) && tss_test_model_scale > 0) 1 - rss_test_model_scale / tss_test_model_scale else NA
  
  # Adjusted R2 on test (model scale)
  p <- tryCatch({ length(coef(model)) }, error = function(e) { NA }) # Number of coefficients (including intercept)
  r2_adj_test_model_scale <- if (is.finite(r2_test_model_scale) && is.finite(p) && n_test_finite_obs > p && (n_test_finite_obs - p - 1) > 0) {
    1 - (1 - r2_test_model_scale) * (n_test_finite_obs - 1) / (n_test_finite_obs - p - 1)
  } else { NA }
  
  
  # --- Calculate R2 and Adjusted R2 on Original Scale for Log Models ---
  # Note: Calculated on the AGGREGATED original target values
  r2_train_orig_from_log = NA
  r2_adj_train_orig_from_log = NA
  r2_test_orig_from_log = NA
  r2_adj_test_orig_from_log = NA
  
  if (target_var == "log_IRI") {
    # R2 on train (original scale)
    finite_train_orig_indices_r2 <- is.finite(predictions_train_orig) & is.finite(df_train_agg[[original_target_var]])
    n_train_finite_obs_r2 <- sum(finite_train_orig_indices_r2)
    
    rss_train_orig_r2 <- if(n_train_finite_obs_r2 > 0) tryCatch({ sum((predictions_train_orig[finite_train_orig_indices_r2] - df_train_agg[[original_target_var]][finite_train_orig_indices_r2])^2, na.rm = TRUE) }, error = function(e) { NA }) else NA
    tss_train_orig_r2 <- if(n_train_finite_obs_r2 > 0) tryCatch({ sum((df_train_agg[[original_target_var]][finite_train_orig_indices_r2] - mean(df_train_agg[[original_target_var]][finite_train_orig_indices_r2], na.rm = TRUE))^2, na.rm = TRUE) }, error = function(e) { NA })
    r2_train_orig_from_log <- if(is.finite(rss_train_orig_r2) && is.finite(tss_train_orig_r2) && tss_train_orig_r2 > 0) 1 - rss_train_orig_r2 / tss_train_orig_r2 else NA
    
    # Adjusted R2 on train (original scale) - using number of fixed effects parameters from the log model
    p_log_train <- tryCatch({ length(coef(model)) }, error = function(e) { NA })
    r2_adj_train_orig_from_log <- if (is.finite(r2_train_orig_from_log) && is.finite(p_log_train) && n_train_finite_obs_r2 > p_log_train && (n_train_finite_obs_r2 - p_log_train - 1) > 0) {
      1 - (1 - r2_train_orig_from_log) * (n_train_finite_obs_r2 - 1) / (n_train_finite_obs_r2 - p_log_train - 1)
    } else { NA }
    
    
    # R2 on test (original scale)
    finite_test_orig_indices_r2 <- is.finite(predictions_test_orig) & is.finite(df_test_agg[[original_target_var]])
    n_test_finite_obs_r2 <- sum(finite_test_orig_indices_r2)
    
    rss_test_orig_r2 <- if(n_test_finite_obs_r2 > 0) tryCatch({ sum((predictions_test_orig[finite_test_orig_indices_r2] - df_test_agg[[original_target_var]][finite_test_orig_indices_r2])^2, na.rm = TRUE) }, error = function(e) { NA }) else NA
    tss_test_orig_r2 <- if(n_test_finite_obs_r2 > 0) tryCatch({ sum((df_test_agg[[original_target_var]][finite_test_orig_indices_r2] - mean(df_test_agg[[original_target_var]][finite_test_orig_indices_r2], na.rm = TRUE))^2, na.rm = TRUE) }, error = function(e) { NA })
    r2_test_orig_from_log <- if(is.finite(rss_test_orig_r2) && is.finite(tss_test_orig_r2) && tss_test_orig_r2 > 0) 1 - rss_test_orig_r2 / tss_test_orig_r2 else NA
    
    # Adjusted R2 on test (original scale) - using number of fixed effects parameters from the log model
    p_log_test <- tryCatch({ length(coef(model)) }, error = function(e) { NA }) # Use number of fixed effects from the log model
    r2_adj_test_orig_from_log <- if (is.finite(r2_test_orig_from_log) && is.finite(p_log_test) && n_test_finite_obs_r2 > p_log_test && (n_test_finite_obs_r2 - p_log_test - 1) > 0) {
      1 - (1 - r2_test_orig_from_log) * (n_test_finite_obs_r2 - 1) / (n_test_finite_obs_r2 - p_log_test - 1)
    } else { NA }
  }
  
  
  # Information Criteria (AIC, BIC) - Using aggregated data
  aic_train_model_scale <- tryCatch({ AIC(model) }, error = function(e) { NA })
  bic_train_model_scale <- tryCatch({ BIC(model) }, error = function(e) { NA })
  aic_test_model_scale <- NA # AIC/BIC on test not applicable/common
  bic_test_model_scale <- NA
  
  # --- Homoscedasticity Test (Breusch-Pagan) ---
  homoscedasticity_bp_p_value_train = NA
  # Check if model is valid and has enough data points for the test
  p_for_ncv <- tryCatch({ length(coef(model)) }, error = function(e) { NA })
  if (!is.null(model) && is.finite(p_for_ncv) && nrow(df_train_agg) > p_for_ncv) { # Use df_train_agg
    bp_test <- tryCatch({
      ncvTest(model)
    }, error = function(e) {
      cat(paste0("\n		 ERROR running ncvTest for Aggregated Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", target_var, ", Formula=", as.character(formula)[2], "] -> ", e$message, "\n"))
      return(NULL)
    })
    if (!is.null(bp_test)) {
      homoscedasticity_bp_p_value_train <- bp_test$p
    }
  }
  
  
  # LMM specific metrics (NA for aggregated)
  r2_train_conditional = NA
  r2_train_marginal = NA
  re_var = NA
  re_cov = list(NA)
  re_variances = list(NA)
  
  
  # === Extract Fixed Effects Coefficients ===
  fixed_effects_df <- tryCatch({ broom::tidy(model) }, error = function(e) { data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA) })
  fixed_effects_list <- list(fixed_effects_df)
  
  
  # Return results as a list
  list(
    Rep = rep, Analysis = analysis_name, N_train = n_train_vehicles, N_test = n_test_vehicles,
    Type = "Aggregated", # Type is just "Aggregated" if successful
    Target = target_var, Model_Formula = as.character(formula)[2], Random_Effects_Formula = NA,
    RMSE_train_orig_mean = rmse_train_orig, MAE_train_orig_mean = mae_train_orig,
    RMSE_test_orig_mean = rmse_test_orig, MAE_test_orig_mean = mae_test_orig,
    R2_train_Model_Scale = r2_train_model_scale, R2_adj_train_Model_Scale = r2_adj_train_model_scale,
    R2_test_Model_Scale = r2_test_model_scale, R2_adj_test_Model_Scale = r2_adj_test_model_scale,
    R2_train_orig_from_log_Mean = r2_train_orig_from_log, # R2 on original scale for log models (train)
    R2_adj_train_orig_from_log_Mean = r2_adj_train_orig_from_log, # Adj R2 on original scale for log models (train)
    R2_test_orig_from_log_Mean = r2_test_orig_from_log,   # R2 on original scale for log models (test)
    R2_adj_test_orig_from_log_Mean = r2_adj_test_orig_from_log, # Adj R2 on original scale for log models (test)
    AIC_train_Model_Scale = aic_train_model_scale, BIC_train_Model_Scale = bic_train_model_scale,
    AIC_test_Model_Scale = aic_test_model_scale, BIC_test_Model_Scale = bic_test_model_scale,
    Homoscedasticity_BP_p_value_train = homoscedasticity_bp_p_value_train, # Homoscedasticity test p-value
    R2_train_conditional_Model_Scale = NA, R2_train_marginal_Model_Scale = NA, RE_Var = NA, RE_Cov = list(NA), RE_Variances = list(NA),
    Fixed_Effects_Coefficients = fixed_effects_list
  )
}


# Function to calculate metrics and format results for Mixed models (lmer)
# Adaptada para IRI e log_IRI como alvo
# Adicionado cálculo de R2 e R2 Ajustado na escala original para modelos logarítmicos
# Adicionado teste de homocedasticidade (Breusch-Pagan) nos resíduos dos efeitos fixos
# Detalhes mais claros sobre motivos de pulo/falha
# Tratamento de erro mais robusto para cálculos de métricas
run_mixed_model <- function(df_train, df_test, fixed_formula_str, random_effects_formula_str, target_var, rep, n_train_vehicles, n_test_vehicles, min_groups_for_lmm, analysis_name) {
  
  # Check if there are enough groups (vehicles) in the training data for LMM
  num_groups_train <- length(unique(df_train$Vehicle))
  if (num_groups_train < min_groups_for_lmm) {
    skip_reason <- paste0("Skipped - Insufficient Training Groups (", num_groups_train, " < ", min_groups_for_lmm, ")")
    cat(paste0("\n		 Skipping Mixed Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", target_var, ", Fixed=", fixed_formula_str, " RE=", random_effects_formula_str, "] -> ", skip_reason, "\n"))
    # Return a list of NAs for skipped models
    result_row <- list(
      Rep = rep, Analysis = analysis_name, N_train = n_train_vehicles, N_test = n_test_vehicles,
      Type = paste("Mixed (", skip_reason, ")"), Target = target_var, Model_Formula = fixed_formula_str, Random_Effects_Formula = random_effects_formula_str,
      RMSE_train_orig_mean = NA, MAE_train_orig_mean = NA, RMSE_test_orig_mean = NA, MAE_test_orig_mean = NA,
      R2_train_Model_Scale = NA, R2_adj_train_Model_Scale = NA, R2_test_Model_Scale = NA, R2_adj_test_Model_Scale = NA,
      R2_train_orig_from_log_Mean = NA, R2_adj_train_orig_from_log_Mean = NA,
      R2_test_orig_from_log_Mean = NA, R2_adj_test_orig_from_log_Mean = NA, # R2 and Adj R2 on original scale for log models
      AIC_train_Model_Scale = NA, BIC_train_Model_Scale = NA, AIC_test_Model_Scale = NA, BIC_test_Model_Scale = NA,
      Homoscedasticity_BP_p_value_train = NA, # Homoscedasticity test p-value
      R2_train_conditional_Model_Scale = NA, R2_train_marginal_Model_Scale = NA, RE_Var = NA, RE_Cov = list(NA), RE_Variances = list(NA),
      Fixed_Effects_Coefficients = list(data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA))
    )
    return(result_row)
  }
  
  # If category fixed effect is included, check for sufficient categories
  formula_includes_category_fixed <- grepl("\\+ Vehicle_Category", fixed_formula_str)
  if (formula_includes_category_fixed) {
    categories_in_train <- unique(df_train$Vehicle_Category)
    if (length(categories_in_train) < 2) {
      skip_reason <- paste0("Skipped - Insufficient Training Categories (", length(categories_in_train), " < 2) for fixed category effect.")
      cat(paste0("\n		 Skipping Mixed Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", target_var, ", Fixed=", fixed_formula_str, " RE=", random_effects_formula_str, "] -> ", skip_reason, "\n"))
      # Add NA results for skipped models
      result_row <- list(
        Rep = rep, Analysis = analysis_name, N_train = n_train_vehicles, N_test = n_test_vehicles,
        Type = paste("Mixed (", skip_reason, ")"), Target = target_var, Model_Formula = fixed_formula_str, Random_Effects_Formula = random_effects_formula_str,
        RMSE_train_orig_mean = NA, MAE_train_orig_mean = NA, RMSE_test_orig_mean = NA, MAE_test_orig_mean = NA,
        R2_train_Model_Scale = NA, R2_adj_train_Model_Scale = NA, R2_test_Model_Scale = NA, R2_adj_test_Model_Scale = NA,
        R2_train_orig_from_log_Mean = NA, R2_adj_train_orig_from_log_Mean = NA,
        R2_test_orig_from_log_Mean = NA, R2_adj_test_orig_from_log_Mean = NA, # R2 and Adj R2 on original scale for log models
        AIC_train_Model_Scale = NA, BIC_train_Model_Scale = NA, AIC_test_Model_Scale = NA, BIC_test_Model_Scale = NA,
        Homoscedasticity_BP_p_value_train = NA, # Homoscedasticity test p-value
        R2_train_conditional_Model_Scale = NA, R2_train_marginal_Model_Scale = NA, RE_Var = NA, RE_Cov = list(NA), RE_Variances = list(NA),
        Fixed_Effects_Coefficients = list(data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA))
      )
      return(result_row)
    }
  }
  
  
  # Combine fixed and random effects formulas into the full formula string
  full_formula_str <- paste(fixed_formula_str, random_effects_formula_str, sep = " + ")
  full_formula <- as.formula(full_formula_str)
  
  
  # Fit the mixed model
  model_fit_result <- tryCatch({
    model <- lmer(full_formula, data = df_train, REML = FALSE) # Use REML = FALSE for AIC/BIC comparisons
    # Return the model object and NULL for fail_reason if successful
    list(model = model, fail_reason = NULL)
  }, error = function(e) {
    fail_reason <- paste0("Failed Fit: ", gsub("\\s+", " ", trimws(e$message))) # Include specific error message
    cat(paste0("\n		 ERROR fitting Mixed Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", target_var, ", Fixed=", fixed_formula_str, " RE=", random_effects_formula_str, "] -> ", fail_reason, "\n"))
    return(list(model = NULL, fail_reason = fail_reason)) # Return NULL model and reason on error
  })
  
  # If model fitting failed, return NA results with specific reason
  if (is.null(model_fit_result$model)) {
    result_row <- list(
      Rep = rep, Analysis = analysis_name, N_train = n_train_vehicles, N_test = n_test_vehicles,
      Type = paste("Mixed (", model_fit_result$fail_reason, ")"), # Include specific error message
      Target = target_var, Model_Formula = fixed_formula_str, Random_Effects_Formula = random_effects_formula_str,
      RMSE_train_orig_mean = NA, MAE_train_orig_mean = NA, RMSE_test_orig_mean = NA, MAE_test_orig_mean = NA,
      R2_train_Model_Scale = NA, R2_adj_train_Model_Scale = NA, R2_test_Model_Scale = NA, R2_adj_test_Model_Scale = NA,
      R2_train_orig_from_log_Mean = NA, R2_adj_train_orig_from_log_Mean = NA,
      R2_test_orig_from_log_Mean = NA, R2_adj_test_orig_from_log_Mean = NA, # R2 and Adj R2 on original scale for log models
      AIC_train_Model_Scale = NA, BIC_train_Model_Scale = NA, AIC_test_Model_Scale = NA, BIC_test_Model_Scale = NA,
      Homoscedasticity_BP_p_value_train = NA, # Homoscedasticity test p-value
      R2_train_conditional_Model_Scale = NA, R2_train_marginal_Model_Scale = NA, RE_Var = NA, RE_Cov = list(NA), RE_Variances = list(NA),
      Fixed_Effects_Coefficients = list(data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA))
    )
    return(result_row)
  }
  
  # Extract the actual model object if fitting was successful
  model <- model_fit_result$model
  
  
  # Determine the original scale target variable name for metric calculation
  original_target_var = "IRI" # Original scale is always IRI
  
  
  # Predictions on train and test (scale of the model)
  # Use re.form = NULL for conditional predictions on train (default for data used for fitting)
  predictions_train_model_scale <- tryCatch({ predict(model, newdata = df_train, re.form = NULL) }, error = function(e) { rep(NA, nrow(df_train)) })
  # Use re.form = ~0 for fixed effects only predictions on test (for generalization to new vehicles)
  predictions_test_model_scale <- tryCatch({ predict(model, newdata = df_test, re.form = ~0, allow.new.levels = TRUE) }, error = function(e) { rep(NA, nrow(df_test)) })
  
  
  # --- Back-transform predictions to original scale if target is log_IRI ---
  predictions_train_orig <- rep(NA, nrow(df_train))
  predictions_test_orig <- rep(NA, nrow(df_test))
  smearing_factor <- NA
  
  if (target_var == "log_IRI") {
    # Calculate smearing factor on train data (original scale residuals from CONDITIONAL predictions)
    finite_train_indices_log = is.finite(df_train[[target_var]]) & is.finite(predictions_train_model_scale)
    if(sum(finite_train_indices_log) > 0) {
      residuals_train_log = df_train[[target_var]][finite_train_indices_log] - predictions_train_model_scale[finite_train_indices_log]
      smearing_factor <- tryCatch({ mean(exp(residuals_train_log), na.rm = TRUE) }, error = function(e) { NA })
    }
    
    # Apply smearing factor to get predictions in original scale
    # Use fixed-effects prediction on test for generalization back-transformation
    if(is.finite(smearing_factor)){
      predictions_train_orig <- exp(predictions_train_model_scale) * smearing_factor
      predictions_test_orig <- exp(predictions_test_model_scale) * smearing_factor
    }
  } else { # If target_var is already original scale (e.g., "IRI")
    predictions_train_orig <- predictions_train_model_scale
    predictions_test_orig <- predictions_test_model_scale
  }
  
  
  # Metrics in original scale (using original_target_var and predictions_orig)
  finite_train_orig_indices <- is.finite(predictions_train_orig) & is.finite(df_train[[original_target_var]])
  rmse_train_orig <- if(sum(finite_train_orig_indices) > 0) tryCatch({ sqrt(mean((predictions_train_orig[finite_train_orig_indices] - df_train[[original_target_var]][finite_train_orig_indices])^2, na.rm = TRUE)) }, error = function(e) { NA }) else NA
  mae_train_orig <- if(sum(finite_train_orig_indices) > 0) tryCatch({ mean(abs(predictions_train_orig[finite_train_orig_indices] - df_train[[original_target_var]][finite_train_orig_indices]), na.rm = TRUE) }, error = function(e) { NA }) else NA
  
  finite_test_orig_indices <- is.finite(predictions_test_orig) & is.finite(df_test[[original_target_var]])
  rmse_test_orig <- if(sum(finite_test_orig_indices) > 0) tryCatch({ sqrt(mean((predictions_test_orig[finite_test_orig_indices] - df_test[[original_target_var]][finite_test_orig_indices])^2, na.rm = TRUE)) }, error = function(e) { NA }) else NA
  mae_test_orig <- if(sum(finite_test_orig_indices) > 0) tryCatch({ mean(abs(predictions_test_orig[finite_test_orig_indices] - df_test[[original_target_var]][finite_test_orig_indices]), na.rm = TRUE) }, error = function(e) { NA }) else NA
  
  
  # Metrics in model scale (R2, AIC, BIC)
  r2_values <- tryCatch({ r.squaredGLMM(model) }, error = function(e) { cat(paste0("\n		 ERROR calculating R2 for Mixed Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", target_var, ", Fixed=", fixed_formula_str, " RE=", random_effects_formula_str, "] -> ", e$message, "\n")) ; c("R2m"=NA, "R2c"=NA) })
  r2_train_marginal_Model_Scale = if("R2m" %in% colnames(r2_values)) r2_values[,"R2m"] else NA
  r2_train_conditional_Model_Scale = if("R2c" %in% colnames(r2_values)) r2_values[,"R2c"] else NA
  
  # Calculate Adjusted R2 for train (based on Marginal R2)
  # This is a pseudo R2 adjusted for LMMs based on fixed effects parameters
  p_fixed <- tryCatch({ length(fixef(model)) }, error = function(e) { NA }) # Number of fixed effects parameters (including intercept)
  n_train_obs <- nrow(df_train)
  r2_adj_train_model_scale <- if (is.finite(r2_train_marginal_Model_Scale) && is.finite(p_fixed) && n_train_obs > p_fixed && (n_train_obs - p_fixed - 1) > 0) {
    1 - (1 - r2_train_marginal_Model_Scale) * (n_train_obs - 1) / (n_train_obs - p_fixed - 1)
  } else { NA }
  
  
  # R2 on test (model scale) - calculated based on fixed-effects prediction on test
  finite_test_model_scale_indices <- is.finite(predictions_test_model_scale) & is.finite(df_test[[target_var]])
  n_test_finite_obs <- sum(finite_test_model_scale_indices)
  
  rss_test_model_scale <- if(n_test_finite_obs > 0) tryCatch({ sum((predictions_test_model_scale[finite_test_model_scale_indices] - df_test[[target_var]][finite_test_model_scale_indices])^2, na.rm = TRUE) }, error = function(e) { NA }) else NA
  tss_test_model_scale <- if(n_test_finite_obs > 0) tryCatch({ sum((df_test[[target_var]][finite_test_model_scale_indices] - mean(df_test[[target_var]][finite_test_model_scale_indices], na.rm = TRUE))^2, na.rm = TRUE) }, error = function(e) { NA })
  
  r2_test_model_scale <- if(is.finite(rss_test_model_scale) && is.finite(tss_test_model_scale) && tss_test_model_scale > 0) 1 - rss_test_model_scale / tss_test_model_scale else NA
  
  # Adjusted R2 on test (model scale)
  p_fixed_test <- tryCatch({ length(fixef(model)) }, error = function(e) { NA }) # Number of fixed effect coefficients
  r2_adj_test_model_scale <- if (is.finite(r2_test_model_scale) && is.finite(p_fixed_test) && n_test_finite_obs > p_fixed_test && (n_test_finite_obs - p_fixed_test - 1) > 0) {
    1 - (1 - r2_test_model_scale) * (n_test_finite_obs - 1) / (n_test_finite_obs - p_fixed_test - 1)
  } else { NA }
  
  # --- Calculate R2 and Adjusted R2 on Original Scale for Log Models ---
  r2_train_orig_from_log = NA
  r2_adj_train_orig_from_log = NA
  r2_test_orig_from_log = NA
  r2_adj_test_orig_from_log = NA
  
  if (target_var == "log_IRI") {
    # R2 on train (original scale)
    finite_train_orig_indices_r2 <- is.finite(predictions_train_orig) & is.finite(df_train[[original_target_var]])
    n_train_finite_obs_r2 <- sum(finite_train_orig_indices_r2)
    
    rss_train_orig_r2 <- if(n_train_finite_obs_r2 > 0) tryCatch({ sum((predictions_train_orig[finite_train_orig_indices_r2] - df_train[[original_target_var]][finite_train_orig_indices_r2])^2, na.rm = TRUE) }, error = function(e) { NA }) else NA
    tss_train_orig_r2 <- if(n_train_finite_obs_r2 > 0) tryCatch({ sum((df_train[[original_target_var]][finite_train_orig_indices_r2] - mean(df_train[[original_target_var]][finite_train_orig_indices_r2], na.rm = TRUE))^2, na.rm = TRUE) }, error = function(e) { NA })
    r2_train_orig_from_log <- if(is.finite(rss_train_orig_r2) && is.finite(tss_train_orig_r2) && tss_train_orig_r2 > 0) 1 - rss_train_orig_r2 / tss_train_orig_r2 else NA
    
    # Adjusted R2 on train (original scale) - using number of fixed effects parameters from the log model
    p_log_train <- tryCatch({ length(fixef(model)) }, error = function(e) { NA }) # Use number of fixed effects from the log model
    r2_adj_train_orig_from_log <- if (is.finite(r2_train_orig_from_log) && is.finite(p_log_train) && n_train_finite_obs_r2 > p_log_train && (n_train_finite_obs_r2 - p_log_train - 1) > 0) {
      1 - (1 - r2_train_orig_from_log) * (n_train_finite_obs_r2 - 1) / (n_train_finite_obs_r2 - p_log_train - 1)
    } else { NA }
    
    
    # R2 on test (original scale)
    finite_test_orig_indices_r2 <- is.finite(predictions_test_orig) & is.finite(df_test[[original_target_var]])
    n_test_finite_obs_r2 <- sum(finite_test_orig_indices_r2)
    
    rss_test_orig_r2 <- if(n_test_finite_obs_r2 > 0) tryCatch({ sum((predictions_test_orig[finite_test_orig_indices_r2] - df_test[[original_target_var]][finite_test_orig_indices_r2])^2, na.rm = TRUE) }, error = function(e) { NA }) else NA
    tss_test_orig_r2 <- if(n_test_finite_obs_r2 > 0) tryCatch({ sum((df_test[[original_target_var]][finite_test_orig_indices_r2] - mean(df_test[[original_target_var]][finite_test_orig_indices_r2], na.rm = TRUE))^2, na.rm = TRUE) }, error = function(e) { NA })
    r2_test_orig_from_log <- if(is.finite(rss_test_orig_r2) && is.finite(tss_test_orig_r2) && tss_test_orig_r2 > 0) 1 - rss_test_orig_r2 / tss_test_orig_r2 else NA
    
    # Adjusted R2 on test (original scale) - using number of fixed effects parameters from the log model
    p_log_test <- tryCatch({ length(fixef(model)) }, error = function(e) { NA }) # Use number of fixed effects from the log model
    r2_adj_test_orig_from_log <- if (is.finite(r2_test_orig_from_log) && is.finite(p_log_test) && n_test_finite_obs_r2 > p_log_test && (n_test_finite_obs_r2 - p_log_test - 1) > 0) {
      1 - (1 - r2_test_orig_from_log) * (n_test_finite_obs_r2 - 1) / (n_test_finite_obs_r2 - p_log_test - 1)
    } else { NA }
  }
  
  
  # Information Criteria (AIC, BIC)
  aic_train_model_scale <- tryCatch({ AIC(model) }, error = function(e) { NA })
  bic_train_model_scale <- tryCatch({ BIC(model) }, error = function(e) { NA })
  aic_test_model_scale <- NA # AIC/BIC on test not applicable/common
  bic_test_model_scale <- NA
  
  # --- Homoscedasticity Test (Breusch-Pagan) ---
  homoscedasticity_bp_p_value_train = NA
  # Check if model is valid and has enough data points for the test
  p_fixed_for_ncv <- tryCatch({ length(fixef(model)) }, error = function(e) { NA })
  if (!is.null(model) && is.finite(p_fixed_for_ncv) && nrow(df_train) > p_fixed_for_ncv) { # Use df_train (not aggregated) for mixed model residuals
    bp_test <- tryCatch({
      ncvTest(model)
    }, error = function(e) {
      cat(paste0("\n		 ERROR running ncvTest for Mixed Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", target_var, ", Fixed=", fixed_formula_str, " RE=", random_effects_formula_str, "] -> ", e$message, "\n"))
      return(NULL)
    })
    if (!is.null(bp_test)) {
      homoscedasticity_bp_p_value_train <- bp_test$p
    }
  }
  
  
  # Random Effects Information
  re_structure <- tryCatch({ VarCorr(model) }, error = function(e) { cat(paste0("\n		 ERROR extracting VarCorr for Mixed Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", target_var, ", Fixed=", fixed_formula_str, " RE=", random_effects_formula_str, "] -> ", e$message, "\n")) ; NULL })
  re_var = NA
  re_cov_list_values <- list(NA) # Initialize list variables
  re_variances_list_values <- list(NA) # Initialize list variables
  
  if (!is.null(re_structure)) {
    re_structure_df <- as.data.frame(re_structure)
    if("grp" %in% colnames(re_structure_df) && "var1" %in% colnames(re_structure_df) && "vcov" %in% colnames(re_structure_df)) {
      # Extract variance for (Intercept), if it exists for Vehicle
      re_var_row <- re_structure_df %>% filter(grp == "Vehicle", var1 == "(Intercept)", is.na(var2))
      re_var <- if(nrow(re_var_row) > 0) re_var_row$vcov else NA
      
      # Extract all covariances for Vehicle
      re_cov_df <- re_structure_df %>% filter(grp == "Vehicle", !is.na(var2))
      re_cov_list_values <- if(nrow(re_cov_df) > 0) as.list(re_cov_df$vcov) else list(NA)
      
      # Extract all variances (Intercept and Slopes) for Vehicle
      re_variances_df <- re_structure_df %>% filter(grp == "Vehicle", is.na(var2))
      re_variances_list_values <- if(nrow(re_variances_df) > 0) as.list(re_variances_df$vcov) else list(NA)
      
    } else {
      cat(paste0("\n		 Warning: Unexpected columns in VarCorr(model). RE_Cov and RE_Variances will be NA. [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", target_var, ", Fixed=", fixed_formula_str, " RE=", random_effects_formula_str, "] -> ", e$message, "\n"))
      # Ensure list variables are NA lists if block was skipped
      re_cov_list_values <- list(NA)
      re_variances_list_values <- list(NA)
    }
  }
  
  
  # === Extract Fixed Effects Coefficients ===
  fixed_effects_df <- tryCatch({ broom.mixed::tidy(model, effects = "fixed") }, error = function(e) { cat(paste0("\n		 ERROR extracting fixed effects for Mixed Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", target_var, ", Fixed=", fixed_formula_str, " RE=", random_effects_formula_str, "] -> ", e$message, "\n")) ; data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA) })
  fixed_effects_list <- list(fixed_effects_df)
  
  
  # Return results as a list
  list(
    Rep = rep, Analysis = analysis_name, N_train = n_train_vehicles, N_test = n_test_vehicles,
    Type = "Mixed", # Type is just "Mixed" if successful
    Target = target_var, Model_Formula = fixed_formula_str, Random_Effects_Formula = random_effects_formula_str,
    RMSE_train_orig_mean = rmse_train_orig, MAE_train_orig_mean = mae_train_orig,
    RMSE_test_orig_mean = rmse_test_orig, MAE_test_orig_mean = mae_test_orig,
    R2_train_Model_Scale = r2_train_marginal_Model_Scale, # Report Marginal R2 for Mixed on model scale
    R2_adj_train_Model_Scale = r2_adj_train_model_scale, # Incluindo R2 ajustado para treino (LMM - pseudo)
    R2_test_Model_Scale = r2_test_model_scale, # R2 on test on model scale
    R2_adj_test_Model_Scale = r2_adj_test_model_scale, # Adj R2 on test on model scale
    R2_train_orig_from_log_Mean = r2_train_orig_from_log, # R2 on original scale for log models (train)
    R2_adj_train_orig_from_log_Mean = r2_adj_train_orig_from_log, # Adj R2 on original scale for log models (train)
    R2_test_orig_from_log_Mean = r2_test_orig_from_log,   # R2 on original scale for log models (test)
    R2_adj_test_orig_from_log_Mean = r2_adj_test_orig_from_log, # Adj R2 on original scale for log models (test)
    AIC_train_Model_Scale = aic_train_model_scale, BIC_train_Model_Scale = bic_train_model_scale,
    AIC_test_Model_Scale = aic_test_model_scale, BIC_test_Model_Scale = bic_test_model_scale,
    Homoscedasticity_BP_p_value_train = homoscedasticity_bp_p_value_train, # Homoscedasticity test p-value
    R2_train_conditional_Model_Scale = r2_train_conditional_Model_Scale, R2_train_marginal_Model_Scale = r2_train_marginal_Model_Scale,
    RE_Var = re_var,
    RE_Cov = re_cov_list_values,
    RE_Variances = re_variances_list_values,
    Fixed_Effects_Coefficients = fixed_effects_list
  )
}


# <<NOVA FUNÇÃO DE AMOSTRAGEM>>
# Função para amostrar uma observação (velocidade) aleatória por IRI por veículo
sample_one_speed_per_IRI_per_vehicle <- function(df_data, target_vars_to_check) {
  # target_vars_to_check é usado apenas para filtrar NAs antes da amostragem
  if (nrow(df_data) == 0) return(df_data)
  
  # Garantir que as colunas necessárias existem
  required_cols <- c("Vehicle", "IRI", target_vars_to_check)
  if (!all(required_cols %in% colnames(df_data))) {
    stop(paste0("Colunas necessárias (Vehicle, IRI, ", paste(target_vars_to_check, collapse=", "), ") não encontradas para amostragem."))
  }
  
  # Filtrar NAs nas variáveis alvo potenciais ANTES de agrupar e amostrar
  df_valid_obs <- df_data %>%
    filter(if_all(all_of(target_vars_to_check), ~!is.na(.)))
  
  # Agrupar pelo identificador real do segmento (IRI) e Veículo
  # Então amostrar 1 linha (que corresponde a uma simulação de velocidade) de cada grupo
  df_sampled <- df_valid_obs %>%
    group_by(Vehicle, IRI) %>% # Agrupar por Veículo e o identificador único do segmento (IRI)
    filter(n() > 0) %>%        # Garantir que o grupo não está vazio após filtrar NAs
    sample_n(1) %>%           # Amostrar uma simulação de velocidade para esta combinação Veículo/IRI
    ungroup()
  
  # Verificar se a amostragem resultou em dados (debug opcional)
  # n_expected_approx <- nrow(df_valid_obs) / 10 # Assume 10 speeds on average
  # if(nrow(df_sampled) < n_expected_approx * 0.8) {
  #   warning("Amostragem resultou em significativamente menos dados que o esperado.")
  # }
  
  return(df_sampled)
}


# === CARREGAR E TRANSFORMAR DADOS ===
cat("Carregando e transformando dados...\n")
input_file <- file.choose() # Seleção interativa do arquivo de entrada

df_original <- readxl::read_excel(input_file)

# Aplicar filtro de velocidade se speeds_to_exclude não estiver vazio
if (length(speeds_to_exclude) > 0) {
  df <- df_original %>% filter(!V %in% speeds_to_exclude)
} else {
  df <- df_original
}

# Remover linhas com NA nos preditores base ou alvo antes de transformar
df <- df %>% filter(!is.na(Vehicle) & !is.na(IRI) & !is.na(RMSAS) & !is.na(V))


# --- REMOVER Criação do Segment_ID antigo ---
# O código anterior que criava Segment_ID baseado em blocos de 10 foi removido
# pois estava incorreto para o objetivo da amostragem. IRI é usado diretamente agora.
# cat("Segment_ID creation based on row blocks removed. Using IRI as segment identifier.\n")


# Adicionar variáveis transformadas (log e interações)
# Manter as colunas originais e adicionar as log
df <- df %>% mutate(log_IRI = log(IRI), log_RMSAS = log(RMSAS), log_V = log(V))

# Calcular interações e divisões
# Verificar se as variáveis base existem antes de criar as transformadas
if ("RMSAS_div_V" %in% required_vars_all_scenarios) df <- df %>% mutate(RMSAS_div_V = RMSAS / V)
if ("log_RMSAS_div_log_V" %in% required_vars_all_scenarios) df <- df %>% mutate(log_RMSAS_div_log_V = log_RMSAS - log_V)
# Interação ainda criada aqui caso seja necessária para REs em modelos mistos futuros, mesmo que não em Fixed Effects
if ("RMSASxV" %in% required_vars_all_scenarios) df <- df %>% mutate(RMSASxV = RMSAS * V)
if ("log_RMSASxlog_V" %in% required_vars_all_scenarios) df <- df %>% mutate(log_RMSASxlog_V = log_RMSAS * log_V)


# === ADICIONAR CATEGORIA DO VEÍCULO ===
# Atribuir categoria a cada veículo
vehicle_to_category <- unlist(lapply(names(vehicle_category_map), function(cat) setNames(rep(cat, length(vehicle_category_map[[cat]])), vehicle_category_map[[cat]])))
df <- df %>%
  mutate(Vehicle_Category = vehicle_to_category[Vehicle])

# Verificar se Vehicle_Category é necessário em algum cenário (apenas para Mixed agora)
if (any(grepl("Vehicle_Category", unlist(lapply(model_scenarios, function(s) if(s$Type == "Mixed") s$Fixed_Formula else character(0)))))) { # Check if category is required by any Mixed formula
  # Converter para fator se necessário
  df <- df %>% mutate(Vehicle_Category = as.factor(Vehicle_Category))
  # Verificar se todos foram categorizados
  if (any(is.na(df$Vehicle_Category))) {
    uncategorized_vehicles <- unique(df$Vehicle[is.na(df$Vehicle_Category)])
    cat(paste0("Aviso: Os seguintes veículos não foram categorizados: ", paste(uncategorized_vehicles, collapse = ", "), ". Eles ainda serão usados se não forem filtrados pelos requisitos das fórmulas.\n"))
    # Nota: Veículos não categorizados terão NA em Vehicle_Category. Modelos usando Vehicle_Category como preditor fixo
    # excluirão automaticamente observações com NA nesta coluna.
  } else {
    cat("Todos os veículos foram categorizados.\n")
  }
} else {
  # Se Vehicle_Category não é usado em nenhuma fórmula Mista, garantir que não é um fator
  if ("Vehicle_Category" %in% colnames(df)) {
    df <- df %>% mutate(Vehicle_Category = as.character(Vehicle_Category))
  }
}


# Remover NAs/Infs/NaNs e filtrar por variáveis necessárias para TODOS os cenários
# Aplicar condições de filtro com base nas variáveis requeridas
filter_conditions <- lapply(required_vars_all_scenarios, function(var) {
  if (var %in% colnames(df)) {
    if (is.numeric(df[[var]])) { return(is.finite(df[[var]])) } else { return(!is.na(df[[var]])) }
  } else {
    stop(paste0("Variável requerida '", var, "' não encontrada nos dados após o carregamento e transformações. Verifique seus cenários ou dados originais."))
  }
})
combined_filter <- Reduce("&", filter_conditions)
df_filtered <- df %>% filter(combined_filter)


# Validar que para cada Veículo e IRI (segmento real), há pelo menos 1 observação nos dados filtrados (idealmente 10 antes da amostragem)
obs_per_IRI_vehicle_check <- df_filtered %>%
  group_by(Vehicle, IRI) %>%
  summarise(N_obs_speed = n(), .groups = 'drop')

if (any(obs_per_IRI_vehicle_check$N_obs_speed == 0)) {
  vehicles_with_empty_segments <- obs_per_IRI_vehicle_check %>% filter(N_obs_speed == 0)
  print(vehicles_with_empty_segments)
  stop("Erro na validação dos dados: Alguns segmentos (IRI) por veículo não têm observações após a filtragem. Verifique seus dados.")
}
# Avisar se algum grupo Vehicle+IRI tiver menos de 10 observações de velocidade ANTES da amostragem.
vehicles_with_non_10_counts <- obs_per_IRI_vehicle_check %>% filter(N_obs_speed != 10) # Assume 10 speeds originally
if (nrow(vehicles_with_non_10_counts) > 0) {
  cat("Aviso: As seguintes combinações de Veículo+IRI não têm exatamente 10 observações de velocidade após a filtragem:\n")
  # print(head(vehicles_with_non_10_counts)) # Print first few examples
  cat(paste0("Número total de combinações Veículo+IRI com contagem != 10: ", nrow(vehicles_with_non_10_counts), "\n"))
  cat("Isso pode afetar a representatividade da amostragem de velocidade para estes segmentos.\n")
} else {
  cat("Validação dos dados: Todas as combinações Veículo+IRI têm 10 observações de velocidade antes da amostragem (nos dados filtrados).\n")
}


# Obter a lista completa de veículos e IRIs nos dados filtrados
all_vehicles <- unique(df_filtered$Vehicle)
total_vehicles <- length(all_vehicles)
all_iris <- unique(df_filtered$IRI)
total_segments = length(all_iris) # Número de segmentos reais (IRIs únicos)

cat(paste0("Dados carregados e transformados. Total de veículos: ", total_vehicles, ", Total de Segmentos (IRIs únicos): ", total_segments, ".\n"))

# Verificar se o número total de veículos é suficiente para os testes planejados
# No Script 2, N_train + N_test = Total_vehicles. O N_train máximo pode ser Total_vehicles - 1 (para ter pelo menos 1 no teste)
max_possible_train_vehicles <- total_vehicles - 1
if (max(n_train_vehicles_list) > max_possible_train_vehicles) {
  stop(paste0("Número máximo de veículos de treino solicitado (", max(n_train_vehicles_list), ") excede o máximo possível (", max_possible_train_vehicles, ") com Teste Variável. Ajuste n_train_vehicles_list."))
}


# === SELEÇÃO INTERATIVA DA PASTA DE SAÍDA ===
cat("\nPor favor, selecione a pasta onde deseja salvar os arquivos de resultados da simulação.\n")
output_sim_dir <- tk_choose.dir(caption = "Selecionar Pasta de Saída para Resultados da Simulação (Teste Variável)")

if (is.na(output_sim_dir)) { stop("Nenhuma pasta de saída selecionada. Simulação abortada.") }
cat(paste0("Resultados da simulação serão salvos em: ", output_sim_dir, "\n"))

output_file_detailed_excel <- file.path(output_sim_dir, output_detailed_basename_excel)
output_file_detailed_csv <- file.path(output_sim_dir, output_detailed_basename_csv)
output_file_summary_excel <- file.path(output_sim_dir, output_summary_basename_excel)
output_file_summary_csv <- file.path(output_sim_dir, output_summary_basename_csv)


results_list <- list() # Lista para armazenar resultados de todas as iterações e cenários

# Define analysis_name before the main loop to fix scope error
analysis_name <- "Variable Train (by Vehicle), Variable Test"

# === LOOP PRINCIPAL: ITERAR SOBRE REPETIÇÕES ===
cat("\nIniciando simulação com Teste Variável...\n")
total_scenarios_to_run <- length(n_train_vehicles_list) * length(model_scenarios)
pb <- txtProgressBar(min = 0, max = n_iteracoes * total_scenarios_to_run, style = 3)
progress_counter <- 0

for (rep in 1:n_iteracoes) {
  set.seed(rep)
  
  # --- Inicializar conjuntos de treino cumulativos para esta iteração ---
  cumulative_train_vehicles_rep <- character(0) # Vetor de Vehicle IDs no treino
  cumulative_df_train_rep_sampled <- df_filtered[0, ] # Dataframe vazio com as mesmas colunas
  
  
  # === LOOP: VARY THE NUMBER OF TRAINING VEHICLES (Cumulative) ===
  for (n_train_vehicles in n_train_vehicles_list) {
    
    # --- Selecionar o(s) novo(s) VEÍCULO(s) de treino para esta etapa cumulativa ---
    num_vehicles_to_add = n_train_vehicles - length(cumulative_train_vehicles_rep)
    
    if (num_vehicles_to_add < 0) {
      stop("Lógica de treino cumulativo falhou: N_train atual menor que veículos no treino cumulativo.")
    } else if (num_vehicles_to_add > 0) {
      # Identificar VEÍCULOS disponíveis para treino NESTA iteração que AINDA NÃO ESTÃO no conjunto cumulativo
      # A piscina de veículos disponíveis para treino nesta iteração são TODOS os veículos menos aqueles já no treino cumulativo
      available_pool_for_step = setdiff(all_vehicles, cumulative_train_vehicles_rep)
      
      if (length(available_pool_for_step) < num_vehicles_to_add) {
        # Não há veículos suficientes restantes para adicionar o número desejado
        cat(paste0("\n		 Aviso: Não há veículos suficientes (", length(available_pool_for_step), ") restantes para atingir N_train = ", n_train_vehicles, " em Rep=", rep, ". Usando todos os veículos restantes (", length(available_pool_for_step), ").\n"))
        num_vehicles_to_add = length(available_pool_for_step) # Adiciona todos os restantes
        if (num_vehicles_to_add == 0) {
          # Se não sobrou nenhum veículo, pulamos esta etapa de N_train
          cat(paste0("\n		 Aviso: Nenhum veículo restante para adicionar ao treino em Rep=", rep, ", N_train_v=", n_train_vehicles, ". Pulando esta etapa de N_train.\n"))
          # Precisamos pular todos os cenários para este N_train aqui
          for (scenario in model_scenarios) {
            progress_counter <- progress_counter + 1
            setTxtProgressBar(pb, progress_counter)
            results_list[[length(results_list) + 1]] <- list(
              Rep = rep, Analysis = analysis_name, N_train = n_train_vehicles, N_test = 0, # N_test é 0 se todos os veículos estão no treino
              Type = paste(scenario$Type, "(Skipped - No More Vehicles)"), Target = scenario$Target,
              Model_Formula = scenario$Fixed_Formula, Random_Effects_Formula = scenario$Random_Effects_Formula,
              RMSE_train_orig_mean = NA, MAE_train_orig_mean = NA, RMSE_test_orig_mean = NA, MAE_test_orig_mean = NA,
              R2_train_Model_Scale = NA, R2_adj_train_Model_Scale = NA, R2_test_Model_Scale = NA, R2_adj_test_Model_Scale = NA,
              R2_train_orig_from_log_Mean = NA, R2_adj_train_orig_from_log_Mean = NA,
              R2_test_orig_from_log_Mean = NA, R2_adj_test_orig_from_log_Mean = NA, # R2 and Adj R2 on original scale for log models
              AIC_train_Model_Scale = NA, BIC_train_Model_Scale = NA, AIC_test_Model_Scale = NA, BIC_test_Model_Scale = NA,
              Homoscedasticity_BP_p_value_train = NA, # Homoscedasticity test p-value
              R2_train_conditional_Model_Scale = NA, R2_train_marginal_Model_Scale = NA, RE_Var = NA, RE_Cov = list(NA), RE_Variances = list(NA),
              Fixed_Effects_Coefficients = list(data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA))
            )
          }
          next # Pular para o próximo N_train
        }
      }
      # Amostrar os novos veículos a adicionar
      new_train_vehicles_for_step <- sample(available_pool_for_step, num_vehicles_to_add)
      
      # --- Obter DADOS de treino para o(s) novo(s) veículo(s) (1 velocidade por IRI por veículo) <<< CORRIGIDO >>> ---
      df_new_train_vehicles_full <- df_filtered %>% filter(Vehicle %in% new_train_vehicles_for_step)
      # Amostrar 1 velocidade por IRI para os novos veículos de treino
      df_new_train_vehicles_sampled <- sample_one_speed_per_IRI_per_vehicle(df_new_train_vehicles_full, target_vars)
      
      
      # --- Atualizar conjunto de treino cumulativo ---
      cumulative_train_vehicles_rep <- c(cumulative_train_vehicles_rep, new_train_vehicles_for_step)
      cumulative_df_train_rep_sampled <- bind_rows(cumulative_df_train_rep_sampled, df_new_train_vehicles_sampled)
    } # Se num_vehicles_to_add é 0, usamos o conjunto de treino cumulativo da etapa anterior
    
    # Renomear para clareza no loop de modelos
    df_train_sampled <- cumulative_df_train_rep_sampled
    n_train_vehicles_actual = length(unique(df_train_sampled$Vehicle)) # Número real de veículos no treino
    
    
    # --- Definir VEÍCULOS de TESTE para esta etapa (Restante dos veículos) ---
    test_vehicles_rep <- setdiff(all_vehicles, cumulative_train_vehicles_rep)
    n_test_vehicles <- length(test_vehicles_rep)
    
    # --- Obter DADOS de TESTE para os veículos de teste (1 velocidade por IRI por veículo) <<< CORRIGIDO >>> ---
    # Se não houver veículos de teste, o dataframe estará vazio.
    df_test_rep_full <- df_filtered %>% filter(Vehicle %in% test_vehicles_rep)
    # Amostrar 1 velocidade por IRI para os veículos de teste
    df_test_sampled <- sample_one_speed_per_IRI_per_vehicle(df_test_rep_full, target_vars)
    
    
    # Verificar se há dados suficientes nos conjuntos de treino/teste amostrados para prosseguir
    if (nrow(df_train_sampled) == 0) {
      skip_reason <- "Skipped - No Train Data Points after sampling"
      cat(paste0("\n		 Aviso: Sem dados de treino amostrados para Rep=", rep, ", N_train_v=", n_train_vehicles, ". Pulando cenários de modelo.\n"))
      # Preencher com NAs para todos os cenários deste N_train
      for (scenario in model_scenarios) {
        progress_counter <- progress_counter + 1
        setTxtProgressBar(pb, progress_counter)
        results_list[[length(results_list) + 1]] <- list(
          Rep = rep, Analysis = analysis_name, N_train = n_train_vehicles_actual, N_test = n_test_vehicles,
          Type = paste(scenario$Type, "(", skip_reason, ")"), Target = scenario$Target,
          Model_Formula = scenario$Fixed_Formula, Random_Effects_Formula = scenario$Random_Effects_Formula,
          RMSE_train_orig_mean = NA, MAE_train_orig_mean = NA, RMSE_test_orig_mean = NA, MAE_test_orig_mean = NA,
          R2_train_Model_Scale = NA, R2_adj_train_Model_Scale = NA, R2_test_Model_Scale = NA, R2_adj_test_Model_Scale = NA,
          R2_train_orig_from_log_Mean = NA, R2_adj_train_orig_from_log_Mean = NA,
          R2_test_orig_from_log_Mean = NA, R2_adj_test_orig_from_log_Mean = NA, # R2 and Adj R2 on original scale for log models
          AIC_train_Model_Scale = NA, BIC_train_Model_Scale = NA, AIC_test_Model_Scale = NA, BIC_test_Model_Scale = NA,
          Homoscedasticity_BP_p_value_train = NA, # Homoscedasticity test p-value
          R2_train_conditional_Model_Scale = NA, R2_train_marginal_Model_Scale = NA, RE_Var = NA, RE_Cov = list(NA), RE_Variances = list(NA),
          Fixed_Effects_Coefficients = list(data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA))
        )
      }
      next # Pular para o próximo N_train
    }
    
    if (nrow(df_test_sampled) == 0 && n_test_vehicles > 0) {
      skip_reason <- "Skipped - No Test Data Points after sampling"
      cat(paste0("\n		 Aviso: Sem dados de teste amostrados para Rep=", rep, ", N_train_v=", n_train_vehicles, ". Pulando cenários de modelo.\n"))
      # Preencher com NAs para todos os cenários deste N_train
      for (scenario in model_scenarios) {
        progress_counter <- progress_counter + 1
        setTxtProgressBar(pb, progress_counter)
        results_list[[length(results_list) + 1]] <- list(
          Rep = rep, Analysis = analysis_name, N_train = n_train_vehicles_actual, N_test = n_test_vehicles,
          Type = paste(scenario$Type, "(", skip_reason, ")"), Target = scenario$Target,
          Model_Formula = scenario$Fixed_Formula, Random_Effects_Formula = scenario$Random_Effects_Formula,
          RMSE_train_orig_mean = NA, MAE_train_orig_mean = NA, RMSE_test_orig_mean = NA, MAE_test_orig_mean = NA,
          R2_train_Model_Scale = NA, R2_adj_train_Model_Scale = NA, R2_test_Model_Scale = NA, R2_adj_test_Model_Scale = NA,
          R2_train_orig_from_log_Mean = NA, R2_adj_train_orig_from_log_Mean = NA,
          R2_test_orig_from_log_Mean = NA, R2_adj_test_orig_from_log_Mean = NA, # R2 and Adj R2 on original scale for log models
          AIC_train_Model_Scale = NA, BIC_train_Model_Scale = NA, AIC_test_Model_Scale = NA, BIC_test_Model_Scale = NA,
          Homoscedasticity_BP_p_value_train = NA, # Homoscedasticity test p-value
          R2_train_conditional_Model_Scale = NA, R2_train_marginal_Model_Scale = NA, RE_Var = NA, RE_Cov = list(NA), RE_Variances = list(NA),
          Fixed_Effects_Coefficients = list(data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA))
        )
      }
      next # Pular para o próximo N_train
    }
    
    cat(paste0("		 Dados de treino amostrados (N_obs=", nrow(df_train_sampled), ", N_vehicles=", n_train_vehicles_actual, "). Dados de teste amostrados (N_obs=", nrow(df_test_sampled), ", N_vehicles=", n_test_vehicles, ").\n"))
    
    
    # === LOOP: ITERAR SOBRE OS CENÁRIOS DE MODELO (Usando os mesmos dados de treino/teste amostrados) ===
    for (scenario in model_scenarios) {
      progress_counter <- progress_counter + 1
      
      current_type <- scenario$Type
      current_fixed_formula_str <- scenario$Fixed_Formula
      current_random_effects_formula_str <- scenario$Random_Effects_Formula # NA for Aggregated
      current_target_var <- scenario$Target # Get target from scenario list
      
      if (current_type == "Aggregated") {
        # Rodar modelo Agregado (LM)
        # === AGREGAR DADOS PARA MODELOS AGREGADOS ===
        df_train_agg <- aggregate_data_by_target(df_train_sampled, current_target_var, current_fixed_formula_str)
        df_test_agg <- aggregate_data_by_target(df_test_sampled, current_target_var, current_fixed_formula_str)
        
        if (nrow(df_train_agg) == 0) {
          skip_reason <- paste0("Skipped - No Train Data Points after aggregation (Target=", current_target_var, ")")
          cat(paste0("\n		 Skipping Aggregated Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", current_target_var, ", Formula=", current_fixed_formula_str, "] -> ", skip_reason, "\n"))
          result <- list(
            Rep = rep, Analysis = analysis_name, N_train = n_train_vehicles_actual, N_test = n_test_vehicles,
            Type = paste(current_type, "(", skip_reason, ")"), Target = current_target_var,
            Model_Formula = current_fixed_formula_str, Random_Effects_Formula = current_random_effects_formula_str,
            RMSE_train_orig_mean = NA, MAE_train_orig_mean = NA, RMSE_test_orig_mean = NA, MAE_test_orig_mean = NA,
            R2_train_Model_Scale = NA, R2_adj_train_Model_Scale = NA, R2_test_Model_Scale = NA, R2_adj_test_Model_Scale = NA,
            R2_train_orig_from_log_Mean = NA, R2_adj_train_orig_from_log_Mean = NA,
            R2_test_orig_from_log_Mean = NA, R2_adj_test_orig_from_log_Mean = NA, # R2 and Adj R2 on original scale for log models
            AIC_train_Model_Scale = NA, BIC_train_Model_Scale = NA, AIC_test_Model_Scale = NA, BIC_test_Model_Scale = NA,
            Homoscedasticity_BP_p_value_train = NA, # Homoscedasticity test p-value
            R2_train_conditional_Model_Scale = NA, R2_train_marginal_Model_Scale = NA, RE_Var = NA, RE_Cov = list(NA), RE_Variances = list(NA),
            Fixed_Effects_Coefficients = list(data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA))
          )
          results_list[[length(results_list) + 1]] <- result
          setTxtProgressBar(pb, progress_counter)
          next # Skip to the next scenario
        }
        
        if (nrow(df_test_agg) == 0 && n_test_vehicles > 0) {
          skip_reason <- paste0("Skipped - No Test Data Points after aggregation (Target=", current_target_var, ")")
          cat(paste0("\n		 Skipping Aggregated Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", current_target_var, ", Formula=", current_fixed_formula_str, "] -> ", skip_reason, "\n"))
          result <- list(
            Rep = rep, Analysis = analysis_name, N_train = n_train_vehicles_actual, N_test = n_test_vehicles,
            Type = paste(current_type, "(", skip_reason, ")"), Target = current_target_var,
            Model_Formula = current_fixed_formula_str, Random_Effects_Formula = current_random_effects_formula_str,
            RMSE_train_orig_mean = NA, MAE_train_orig_mean = NA, RMSE_test_orig_mean = NA, MAE_test_orig_mean = NA,
            R2_train_Model_Scale = NA, R2_adj_train_Model_Scale = NA, R2_test_Model_Scale = NA, R2_adj_test_Model_Scale = NA,
            R2_train_orig_from_log_Mean = NA, R2_adj_train_orig_from_log_Mean = NA,
            R2_test_orig_from_log_Mean = NA, R2_adj_test_orig_from_log_Mean = NA, # R2 and Adj R2 on original scale for log models
            AIC_train_Model_Scale = NA, BIC_train_Model_Scale = NA, AIC_test_Model_Scale = NA, BIC_test_Model_Scale = NA,
            Homoscedasticity_BP_p_value_train = NA, # Homoscedasticity test p-value
            R2_train_conditional_Model_Scale = NA, R2_train_marginal_Model_Scale = NA, RE_Var = NA, RE_Cov = list(NA), RE_Variances = list(NA),
            Fixed_Effects_Coefficients = list(data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA))
          )
          results_list[[length(results_list) + 1]] <- result
          setTxtProgressBar(pb, progress_counter)
          next # Skip to the next scenario
        }
        
        
        result <- run_aggregated_model(df_train_agg, df_test_agg, as.formula(current_fixed_formula_str), current_target_var, rep,
                                       n_train_vehicles_actual, n_test_vehicles, analysis_name)
        results_list[[length(results_list) + 1]] <- result
        
      } else if (current_type == "Mixed") {
        # Rodar modelo Misto (LMM)
        
        # Check conditions for running Mixed Model (enough groups, enough categories if fixed effect included)
        can_run_mixed <- (length(unique(df_train_sampled$Vehicle)) >= MIN_GROUPS_FOR_LMM)
        skip_reason <- "" # Reset skip reason
        
        formula_includes_category_fixed <- grepl("\\+ Vehicle_Category", current_fixed_formula_str)
        if (can_run_mixed && formula_includes_category_fixed) {
          categories_in_train <- unique(df_train_sampled$Vehicle_Category)
          if (length(categories_in_train) < 2) {
            can_run_mixed <- FALSE
            skip_reason <- paste0("Skipped - Insufficient Training Categories (", length(categories_in_train), " < 2) for fixed category effect.")
            cat(paste0("\n		 Skipping Mixed Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", current_target_var, ", Fixed=", current_fixed_formula_str, " RE=", current_random_effects_formula_str, "] -> ", skip_reason, "\n"))
          }
        } else if (can_run_mixed && length(unique(df_train_sampled$Vehicle)) < MIN_GROUPS_FOR_LMM) {
          can_run_mixed <- FALSE # This case should be caught by the first check, but double-checking
          skip_reason <- paste0("Skipped - Insufficient Training Groups (", length(unique(df_train_sampled$Vehicle)), " < ", MIN_GROUPS_FOR_LMM, ")")
          cat(paste0("\n		 Skipping Mixed Model [Rep=", rep, ", N_train_v=", n_train_vehicles, ", Target=", current_target_var, ", Fixed=", current_fixed_formula_str, " RE=", current_random_effects_formula_str, "] -> ", skip_reason, "\n"))
        }
        
        
        if (can_run_mixed) {
          # Mixed models use the sampled (non-aggregated) data
          result <- run_mixed_model(df_train_sampled, df_test_sampled, current_fixed_formula_str, current_random_effects_formula_str, current_target_var, rep,
                                    n_train_vehicles_actual, n_test_vehicles, MIN_GROUPS_FOR_LMM, analysis_name)
          results_list[[length(results_list) + 1]] <- result
          
        } else { # Caso em que LMM não é rodado (poucos grupos ou categorias)
          # If can_run_mixed was already FALSE due to insufficient groups, use that reason
          # Otherwise, the reason was insufficient categories (handled above)
          if (!exists("skip_reason") || is.null(skip_reason) || skip_reason == "") {
            skip_reason <- paste0("Skipped - Insufficient Training Groups (", length(unique(df_train_sampled$Vehicle)), " < ", MIN_GROUPS_FOR_LMM, ")")
          }
          
          result_row_mixed_skipped <- list(
            Rep = rep, Analysis = analysis_name, N_train = n_train_vehicles_actual, N_test = n_test_vehicles,
            Type = paste("Mixed (", skip_reason, ")"), # Include specific skip reason
            Target = current_target_var,
            Model_Formula = current_fixed_formula_str, Random_Effects_Formula = current_random_effects_formula_str,
            RMSE_train_orig_mean = NA, MAE_train_orig_mean = NA, RMSE_test_orig_mean = NA, MAE_test_orig_mean = NA,
            R2_train_Model_Scale = NA, R2_adj_train_Model_Scale = NA, R2_test_Model_Scale = NA, R2_adj_test_Model_Scale = NA,
            R2_train_orig_from_log_Mean = NA, R2_adj_train_orig_from_log_Mean = NA,
            R2_test_orig_from_log_Mean = NA, R2_adj_test_orig_from_log_Mean = NA, # R2 and Adj R2 on original scale for log models
            AIC_train_Model_Scale = NA, BIC_train_Model_Scale = NA, AIC_test_Model_Scale = NA, BIC_test_Model_Scale = NA,
            Homoscedasticity_BP_p_value_train = NA, # Homoscedasticity test p-value
            R2_train_conditional_Model_Scale = NA, R2_train_marginal_Model_Scale = NA, RE_Var = NA, RE_Cov = list(NA), RE_Variances = list(NA),
            Fixed_Effects_Coefficients = list(data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA))
          )
          results_list[[length(results_list) + 1]] <- result_row_mixed_skipped
        }
      } # Fim do if (current_type)
      setTxtProgressBar(pb, progress_counter) # Atualizar barra de progresso após cada cenário de modelo
      # Clear skip_reason for the next scenario
      if (exists("skip_reason")) rm(skip_reason)
    } # Fim do loop de cenários de modelo
  } # Fim do loop de N_train
} # Fim do loop de repetições

# Fechar barra de progresso
close(pb)
cat("\nSimulação com Teste Variável concluída.\n")

# Converter a lista de resultados para um data frame
cat("Combinando resultados das iterações...\n")
results_df <- bind_rows(results_list)


# === CALCULAR RESUMO ESTATÍSTICO ===
cat("\nCalculando resumo estatístico...\n")

# Definir colunas numéricas para as quais calcular média e desvio padrão
numeric_cols_to_summarise <- results_df %>%
  select(where(is.numeric)) %>%
  colnames() %>%
  setdiff(c("Rep", "N_train", "N_test", "Homoscedasticity_BP_p_value_train")) # Excluir colunas identificadoras/contagem e p-valor para resumo de média/SD


# Calcular o resumo
summary_results_df <- results_df %>%
  group_by(N_train, N_test, Type, Target, Model_Formula, Random_Effects_Formula) %>%
  summarise(
    Total_Iterations = n(),
    # Contar falhas onde métricas de teste na escala original ou na escala do modelo são NA
    # Note: This counts any row where key test metrics are NA, including skipped/failed fits
    Failure_Count = sum(is.na(RMSE_test_orig_mean) | is.na(R2_test_Model_Scale)),
    # Calcular média e desvio padrão para colunas numéricas (ignorando NAs = falhas)
    across(any_of(numeric_cols_to_summarise),
           list(Mean = ~mean(., na.rm = TRUE), SD = ~sd(., na.rm = TRUE))),
    # Resumir p-valor de homocedasticidade (média e proporção significativa)
    Homoscedasticity_BP_p_value_train_Mean = mean(Homoscedasticity_BP_p_value_train, na.rm = TRUE),
    Homoscedasticity_BP_prop_sig_train = mean(Homoscedasticity_BP_p_value_train < 0.05, na.rm = TRUE), # Proportion where p < 0.05
    .groups = 'drop'
  ) %>%
  arrange(N_train, Type, Target, Model_Formula, Random_Effects_Formula)


# === SALVAR RESULTADOS DETALHADOS (Iteração por Iteração) ===
cat("\nSalvando resultados detalhados (por iteração)...\n")

# Lidar com colunas de lista para saída em Excel/CSV convertendo para strings
cols_to_convert_list <- c("RE_Cov", "RE_Variances", "Fixed_Effects_Coefficients")
for (col in cols_to_convert_list) {
  if (col %in% colnames(results_df)) {
    results_df[[col]] <- lapply(results_df[[col]], function(x) {
      if (is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || (is.atomic(x) && all(is.na(x)))))) {
        return("NA")
      } else if (is.data.frame(x)) {
        # Serializar dataframe de coeficientes
        terms_str <- if("term" %in% colnames(x)) x$term else rep("NA", nrow(x))
        estimates_str <- if("estimate" %in% colnames(x)) round(x$estimate, 4) else rep(NA, nrow(x))
        pvalues_str <- if("p.value" %in% colnames(x)) round(x$p.value, 4) else rep(NA, nrow(x))
        paste(paste(terms_str, estimates_str, pvalues_str, sep = ":"), collapse = " | ")
      } else if (is.list(x)) {
        if (length(x) == 0 || all(is.na(unlist(x)))) { return("NA") } else { return(paste(unlist(x), collapse = ", ")) }
      } else { return(as.character(x)) }
    })
    results_df[[col]] <- as.character(results_df[[col]])
  }
}

save_excel_detailed <- tryCatch({ writexl::write_xlsx(as.data.frame(results_df), path = output_file_detailed_excel) ; TRUE }, error = function(e) { cat(paste0("\n		 ERRO ao salvar resultados detalhados (Excel): ", e$message, "\n")) ; FALSE })
if(save_excel_detailed) { cat(paste0("Resultados detalhados (Excel) salvos em ", output_file_detailed_excel, "\n")) }

save_csv_detailed <- tryCatch({ readr::write_csv(as.data.frame(results_df), file = output_file_detailed_csv) ; TRUE }, error = function(e) { cat(paste0("\n		 ERRO ao salvar resultados detalhados (CSV): ", e$message, "\n")) ; FALSE })
if(save_csv_detailed) { cat(paste0("Resultados detalhados (CSV) salvos em ", output_file_detailed_csv, "\n")) }


# === SALVAR RESUMO ESTATÍSTICO ===
cat("\nSalvando resumo estatístico...\n")

save_excel_summary <- tryCatch({ writexl::write_xlsx(as.data.frame(summary_results_df), path = output_file_summary_excel) ; TRUE }, error = function(e) { cat(paste0("\n		 ERRO ao salvar resumo estatístico (Excel): ", e$message, "\n")) ; FALSE })
if(save_excel_summary) { cat(paste0("Resumo estatístico (Excel) salvo em ", output_file_summary_excel, "\n")) }

save_csv_summary <- tryCatch({ readr::write_csv(as.data.frame(summary_results_df), file = output_file_summary_csv) ; TRUE }, error = function(e) { cat(paste0("\n		 ERRO ao salvar resumo estatístico (CSV): ", e$message, "\n")) ; FALSE })
if(save_csv_summary) { cat(paste0("Resumo estatístico (CSV) salvo em ", output_file_summary_csv, "\n")) }


# Re-enable warnings
options(warn = 0)

getwd()
cat("\nScript 2 (Teste Variável) concluído.\n")