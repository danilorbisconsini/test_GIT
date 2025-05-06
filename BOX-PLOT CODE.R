# ==============================================================================
# Script para gerar apenas BoxPlot1 e BoxPlot4 com thresholds exatos de IRI
# ==============================================================================

# —————————————————————————————————————————————
# 1) INSTALAR E CARREGAR PACOTES
# —————————————————————————————————————————————
pkgs <- c(
  "readxl", "dplyr", "ggplot2", "tcltk",
  "writexl", "svglite", "purrr", "tidyr"
)
for (p in pkgs) {
  if (!requireNamespace(p, quietly=TRUE)) install.packages(p)
}
lapply(pkgs, library, character.only=TRUE)
options(warn = -1)

# —————————————————————————————————————————————
# 2) CONFIGURAÇÕES DO USUÁRIO
# —————————————————————————————————————————————
# Velocidades e veículos para o BoxPlot4
speeds_to_include_plot4   <- c(90, 100, 110, 120)  # ajuste conforme desejado
vehicles_to_include_plot4 <- c("BMW 530i","NISSAN SENTRA","VW PASSAT","RENAULT LOGAN",
  "FORD FIESTA","CLIO 2","SMART FORTWO","SMART FORFOUR",
  "TOYOTA SIENTA","Renault Kangoo","Audi Q2","DACIA 1300")

# Salvar os plots e dados?
SAVE_PLOTS_TO_FILE <- TRUE

# Seleciona pasta de saída
output_dir <- tcltk::tk_choose.dir(caption = "Selecione a pasta de saída")
if (is.na(output_dir) || output_dir == "") {
  stop("Pasta de saída não selecionada. Abortando.")
}

# —————————————————————————————————————————————
# 3) CARREGAR E PREPARAR DADOS
# —————————————————————————————————————————————
input_file  <- file.choose()
df_original <- readxl::read_excel(input_file)

df <- df_original %>%
  select(Vehicle, IRI, RMSAS, V) %>%
  filter(
    !is.na(Vehicle),
    !is.na(IRI),
    !is.na(RMSAS),
    !is.na(V)
  ) %>%
  mutate(
    Vehicle = as.character(Vehicle),
    V       = as.numeric(V)
  )

# —————————————————————————————————————————————
# 4) BOXPLOT 1: RMSAS por bin de percentil de IRI
# —————————————————————————————————————————————
# calcula percentis
iri_perc    <- quantile(df$IRI, probs=seq(0.1,0.9,by=0.1), na.rm=TRUE)
names(iri_perc) <- paste0(seq(10,90,by=10), "%")

# define bins e labels
breaks_iri <- c(min(df$IRI, na.rm=TRUE), iri_perc, Inf)
labels_iri <- c(
  "0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
  "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"
)

df <- df %>%
  mutate(
    IRI_Bin = cut(
      IRI, breaks=breaks_iri, labels=labels_iri,
      include.lowest=TRUE, right=TRUE
    )
  )
df$IRI_Bin <- factor(df$IRI_Bin, levels=labels_iri)

# prepara labels customizados com unidade
custom_labels <- sapply(seq_along(labels_iri), function(i) {
  if (i == 1) {
    paste0(
      labels_iri[i],
      "\n(IRI < ", sprintf("%.2f", iri_perc["10%"]), " m/km)"
    )
  } else {
    lower <- paste0((i-1)*10, "%")
    paste0(
      labels_iri[i],
      "\n(IRI ≥ ", sprintf("%.2f", iri_perc[lower]), " m/km)"
    )
  }
})
names(custom_labels) <- labels_iri

# gera BoxPlot1
plot1 <- ggplot(df, aes(x=IRI_Bin, y=RMSAS)) +
  stat_boxplot(geom="errorbar", width=0.3) +
  geom_boxplot() +
  labs(
    title = "RMSAS por Percentil de IRI (Todos os Dados)",
    x     = "IRI Percentil Bin (m/km)",
    y     = "RMSAS (m/s²)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_x_discrete(labels = custom_labels)

# —————————————————————————————————————————————
# 5) BOXPLOT 4: 1 RMSAS aleatório por veículo para cada threshold de IRI
# —————————————————————————————————————————————
# (a) determina valores exatos de IRI para cada percentil
quant_vals <- as.numeric(iri_perc)        # os nove thresholds nominais
unique_iri <- sort(unique(df$IRI))
iri_vals   <- sapply(quant_vals, function(q) {
  unique_iri[which.min(abs(unique_iri - q))]
})
thresholds_df <- data.frame(
  Percentile = names(iri_perc),
  IRI_Value  = iri_vals,
  stringsAsFactors = FALSE
)

# (b) para cada threshold, filtra e amostra um RMSAS por veículo
df4 <- thresholds_df %>%
  mutate(
    data = purrr::map2(Percentile, IRI_Value, ~{
      sub <- df %>%
        filter(
          IRI     == .y,
          V       %in% speeds_to_include_plot4,
          Vehicle %in% vehicles_to_include_plot4
        )
      if (nrow(sub) == 0) return(NULL)
      sub %>%
        group_by(Vehicle) %>%
        slice_sample(n=1) %>%
        ungroup() %>%
        mutate(
          Percentile   = .x,
          IRI_Threshold = .y
        )
    })
  ) %>%
  select(data) %>%
  tidyr::unnest(cols = data)

# gera BoxPlot4
plot4 <- ggplot(df4, aes(x=Percentile, y=RMSAS)) +
  stat_boxplot(geom="errorbar", width=0.3) +
  geom_boxplot() +
  labs(
    title    = "RMSAS por IRI Threshold (1 obs/Veículo)",
    subtitle = paste0(
      "Speeds: ", paste(speeds_to_include_plot4, collapse=", "), " km/h; ",
      "Vehicles: ", paste(vehicles_to_include_plot4, collapse=", ")
    ),
    x        = "Percentil de IRI (valor m/km)",
    y        = "RMSAS (m/s²)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_x_discrete(labels = function(p) {
    idx <- match(p, thresholds_df$Percentile)
    val <- thresholds_df$IRI_Value[idx]
    paste0(p, "\n(", sprintf("%.2f", val), " m/km)")
  })

# —————————————————————————————————————————————
# 6) SALVAR OU EXIBIR
# —————————————————————————————————————————————
if (SAVE_PLOTS_TO_FILE) {
  ggplot2::ggsave(
    file.path(output_dir, "boxplot1_RMSAS_vs_IRI_All.svg"),
    plot1, width=10, height=7, device="svg"
  )
  ggplot2::ggsave(
    file.path(output_dir, "boxplot4_RMSAS_vs_IRI_RandomPerVehicle.svg"),
    plot4, width=10, height=7, device="svg"
  )
  writexl::write_xlsx(
    df4,
    path = file.path(output_dir, "boxplot4_data.xlsx")
  )
  message("BoxPlot1 e BoxPlot4 salvos em SVG e dados de Plot4 em Excel em:\n", output_dir)
} else {
  print(plot1)
  readline(prompt = "Pressione Enter para ver o BoxPlot4...")
  print(plot4)
}

options(warn = 0)
message("Fim do script.")