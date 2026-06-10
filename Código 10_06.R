# ==============================================================================
# SPECT CORONARIAS - ANÁLISIS DE SUPERVIVENCIA Y OUTCOMES CARDIOVASCULARES
# ==============================================================================

# ==============================================================================
# 1. LIBRERÍAS
# ==============================================================================

library(openxlsx)
library(dplyr)
library(broom)
library(readxl)
library(lubridate)
library(survival)
library(gtsummary)
library(flextable)
library(nortest)
library(survminer)
library(ggplot2)
library(epitools)
library(purrr)
library(tidyr)
library(gt)
library(adjustedCurves)
library(stringr)
library(forcats)
library(scales)
library(pammtools)
library(ggsci)
library(forestmodel)

# ==============================================================================
# 2. DIRECTORIO DE TRABAJO Y CARPETA DE SALIDA
# ==============================================================================

setwd("C:/Users/osiel/OneDrive/Escritorio/R Michelle_15 Mar")
if (!dir.exists("Figuras")) dir.create("Figuras")

# ==============================================================================
# 3. IMPORTACIÓN Y CRITERIOS DE EXCLUSIÓN
# ==============================================================================

spect <- read_excel("spectf_coronarias.xlsx")

spectf <- spect %>%
  filter(INFARTO_PREV_SPECT <= 1 | is.na(INFARTO_PREV_SPECT)) %>%   # Excluir >2 infartos previos
  filter(coronarias_no_especificadas != 1 | is.na(coronarias_no_especificadas)) %>%  # Arterias no especificadas
  filter(duplicados != 1 | is.na(duplicados)) %>%                    # Duplicados
  filter(sin_seguimiento != 1 | is.na(sin_seguimiento))              # Sin seguimiento

# ==============================================================================
# 4. SELECCIÓN DE VARIABLES
# ==============================================================================

spectf <- spectf %>%
  select(
    PACIENTE, Edad, SEXO, FECHA_ESTUDIO, SISTOLICA, DIASTOLICA, PROTOCOLO,
    HAS, Obesidad, Tabaquismo, Diabetes, Ang_Cro_Est, Dislipidemia,
    Enfcarprev_DIC, DIL_CAV, ENF_ART_PER, ENF_VALVULAR, BLOQ_RAMA_IZQ,
    ARRITMIA, FIBRI_AURI, INS_CARD, ANEURISMA, ENF_TRIVAS, EPOC, MIOCARD,
    TAQUI_VENTR, TROM_VENO_PROF, EVC, MIOCARD_DILATADA, OTRA_ECV,
    CARD_ISQ_PREVIA, ESTUDIO_CARD_ISQ_PREVIA, PAD_ACT_SIMPLIFICADO,
    ANGINA, ASINTOMATICOS, DISNEA, DET_CLAS_FX, PALPITACIONES, SINCOPE,
    Motivoestudio, RES_ESTUDIO, NORMAL, INFARTO, ISQUEMIA, INF_ISQ, Anormal,
    ESTUDIO_CISQ, ENF_CAR_DIC_2, NUM_INT, FECHA_INTERVENCION, HOSPITALIZACION,
    TIPO_HOSPITALIZACIÓN, HOSPI_CVD_ADICIONAL, FECHA_HOSP,
    FECHA_HOSPI_CVD_ADICIONAL, Tipo_Hospitalizacion_cvd, infarto_outcome,
    fecha_infarto, INFARTO_PREV_SPECT, MUERTE, FECHA_MUERTE, muerte_cvd,
    FECHA_ULTIMA_CONSULTA, vasos_isquemia, vasos_Infarto, nueva_fecha_intervencion
  )

# ==============================================================================
# 5. CONSTRUCCIÓN DE VARIABLES DE HOSPITALIZACIÓN (CVD y no CVD)
# ==============================================================================

# Recodificar tipo de hospitalización: 0 = sin hospitalización, 1 = CVD, 2 = no CVD
spectf <- spectf %>%
  mutate(TIPO_HOSPITALIZACIÓN = case_when(
    TIPO_HOSPITALIZACIÓN == 0 ~ 2,
    is.na(TIPO_HOSPITALIZACIÓN) ~ 0,
    TRUE ~ TIPO_HOSPITALIZACIÓN
  )) %>%
  mutate(
    HOSPITALIZACION_CVD = case_when(
      TIPO_HOSPITALIZACIÓN == 1 ~ FECHA_HOSP,
      TIPO_HOSPITALIZACIÓN == 2 ~ FECHA_HOSPI_CVD_ADICIONAL,
      TRUE ~ NA_POSIXct_
    ),
    HOSPITALIZACION_NO_CVD = case_when(
      TIPO_HOSPITALIZACIÓN == 2 ~ FECHA_HOSP,
      TRUE ~ NA_POSIXct_
    )
  ) %>%
  relocate(HOSPITALIZACION_CVD, .after = FECHA_HOSPI_CVD_ADICIONAL) %>%
  relocate(HOSPITALIZACION_NO_CVD, .after = HOSPITALIZACION_CVD)

# ==============================================================================
# 6. RECODIFICACIÓN: NA → 0, VARIABLES POLITÓMICAS → DICOTÓMICAS
# ==============================================================================

spectf <- spectf %>%
  mutate(
    SISTOLICA              = case_when(is.na(SISTOLICA) ~ 0, TRUE ~ SISTOLICA),
    DIASTOLICA             = case_when(is.na(DIASTOLICA) ~ 0, TRUE ~ DIASTOLICA),
    PROTOCOLO              = case_when(PROTOCOLO %in% c(12, 24) ~ 1, is.na(PROTOCOLO) ~ 0, TRUE ~ PROTOCOLO),
    HAS                    = case_when(HAS %in% c(2, 3, 4) ~ 1, is.na(HAS) ~ 0, TRUE ~ HAS),
    Obesidad               = case_when(is.na(Obesidad) ~ 0, TRUE ~ Obesidad),
    Tabaquismo             = case_when(is.na(Tabaquismo) ~ 0, TRUE ~ Tabaquismo),
    Diabetes               = case_when(is.na(Diabetes) ~ 0, TRUE ~ Diabetes),
    Ang_Cro_Est            = case_when(is.na(Ang_Cro_Est) ~ 0, TRUE ~ Ang_Cro_Est),
    Dislipidemia           = case_when(is.na(Dislipidemia) ~ 0, TRUE ~ Dislipidemia),
    DIL_CAV                = case_when(is.na(DIL_CAV) ~ 0, TRUE ~ DIL_CAV),
    Enfcarprev_DIC         = case_when(is.na(Enfcarprev_DIC) ~ 0, TRUE ~ Enfcarprev_DIC),
    ENF_ART_PER            = case_when(is.na(ENF_ART_PER) ~ 0, TRUE ~ ENF_ART_PER),
    ENF_VALVULAR           = case_when(is.na(ENF_VALVULAR) ~ 0, TRUE ~ ENF_VALVULAR),
    BLOQ_RAMA_IZQ          = case_when(is.na(BLOQ_RAMA_IZQ) ~ 0, TRUE ~ BLOQ_RAMA_IZQ),
    ARRITMIA               = case_when(is.na(ARRITMIA) ~ 0, TRUE ~ ARRITMIA),
    FIBRI_AURI             = case_when(is.na(FIBRI_AURI) ~ 0, TRUE ~ FIBRI_AURI),
    INS_CARD               = case_when(is.na(INS_CARD) ~ 0, TRUE ~ INS_CARD),
    ANEURISMA              = case_when(is.na(ANEURISMA) ~ 0, TRUE ~ ANEURISMA),
    ENF_TRIVAS             = case_when(is.na(ENF_TRIVAS) ~ 0, TRUE ~ ENF_TRIVAS),
    EPOC                   = case_when(is.na(EPOC) ~ 0, TRUE ~ EPOC),
    MIOCARD                = case_when(is.na(MIOCARD) ~ 0, TRUE ~ MIOCARD),
    TAQUI_VENTR            = case_when(is.na(TAQUI_VENTR) ~ 0, TRUE ~ TAQUI_VENTR),
    TROM_VENO_PROF         = case_when(is.na(TROM_VENO_PROF) ~ 0, TRUE ~ TROM_VENO_PROF),
    EVC                    = case_when(is.na(EVC) ~ 0, TRUE ~ EVC),
    MIOCARD_DILATADA       = case_when(is.na(MIOCARD_DILATADA) ~ 0, TRUE ~ MIOCARD_DILATADA),
    OTRA_ECV               = case_when(is.na(OTRA_ECV) ~ 0, TRUE ~ OTRA_ECV),
    CARD_ISQ_PREVIA        = case_when(is.na(CARD_ISQ_PREVIA) ~ 0, TRUE ~ CARD_ISQ_PREVIA),
    ESTUDIO_CARD_ISQ_PREVIA = case_when(is.na(ESTUDIO_CARD_ISQ_PREVIA) ~ 0, TRUE ~ ESTUDIO_CARD_ISQ_PREVIA),
    PAD_ACT_SIMPLIFICADO   = case_when(PAD_ACT_SIMPLIFICADO == 7 ~ 6, is.na(PAD_ACT_SIMPLIFICADO) ~ 0, TRUE ~ PAD_ACT_SIMPLIFICADO),
    ANGINA                 = case_when(is.na(ANGINA) ~ 0, TRUE ~ ANGINA),
    ASINTOMATICOS          = case_when(is.na(ASINTOMATICOS) ~ 0, TRUE ~ ASINTOMATICOS),
    DISNEA                 = case_when(is.na(DISNEA) ~ 0, TRUE ~ DISNEA),
    DET_CLAS_FX            = case_when(is.na(DET_CLAS_FX) ~ 0, TRUE ~ DET_CLAS_FX),
    PALPITACIONES          = case_when(is.na(PALPITACIONES) ~ 0, TRUE ~ PALPITACIONES),
    SINCOPE                = case_when(is.na(SINCOPE) ~ 0, TRUE ~ SINCOPE),
    Motivoestudio          = case_when(is.na(Motivoestudio) ~ 0, TRUE ~ Motivoestudio),
    RES_ESTUDIO            = case_when(is.na(RES_ESTUDIO) ~ 0, TRUE ~ RES_ESTUDIO),
    NORMAL                 = case_when(is.na(NORMAL) ~ 0, TRUE ~ NORMAL),
    INFARTO                = case_when(is.na(INFARTO) ~ 0, TRUE ~ INFARTO),
    ISQUEMIA               = case_when(is.na(ISQUEMIA) ~ 0, TRUE ~ ISQUEMIA),
    INF_ISQ                = case_when(is.na(INF_ISQ) ~ 0, TRUE ~ INF_ISQ),
    Anormal                = case_when(is.na(Anormal) ~ 0, TRUE ~ Anormal),
    ESTUDIO_CISQ           = case_when(is.na(ESTUDIO_CISQ) ~ 0, TRUE ~ ESTUDIO_CISQ),
    ENF_CAR_DIC_2          = case_when(is.na(ENF_CAR_DIC_2) ~ 0, TRUE ~ ENF_CAR_DIC_2),
    NUM_INT                = case_when(is.na(NUM_INT) ~ 0, TRUE ~ NUM_INT),
    HOSPITALIZACION        = case_when(is.na(HOSPITALIZACION) ~ 0, TRUE ~ HOSPITALIZACION),
    TIPO_HOSPITALIZACIÓN   = case_when(is.na(TIPO_HOSPITALIZACIÓN) ~ 0, TRUE ~ TIPO_HOSPITALIZACIÓN),
    HOSPI_CVD_ADICIONAL    = case_when(is.na(HOSPI_CVD_ADICIONAL) ~ 0, TRUE ~ HOSPI_CVD_ADICIONAL),
    infarto_outcome        = case_when(is.na(infarto_outcome) ~ 0, TRUE ~ infarto_outcome),
    INFARTO_PREV_SPECT     = case_when(is.na(INFARTO_PREV_SPECT) ~ 0, TRUE ~ INFARTO_PREV_SPECT),
    MUERTE                 = case_when(is.na(MUERTE) ~ 0, TRUE ~ MUERTE),
    muerte_cvd             = case_when(muerte_cvd %in% c(2, 3) ~ 0, is.na(muerte_cvd) ~ 0, TRUE ~ muerte_cvd),
    vasos_isquemia         = case_when(is.na(vasos_isquemia) ~ 0, TRUE ~ vasos_isquemia),
    vasos_Infarto          = case_when(is.na(vasos_Infarto) ~ 0, TRUE ~ vasos_Infarto)
  )

# ==============================================================================
# 7. VARIABLES DE TIEMPO Y EVENTOS (OUTCOMES)
# ==============================================================================

# --- 7.1 Intervención (revascularización) ---
spectf <- spectf %>%
  mutate(
    estadoint = case_when(
      is.na(nueva_fecha_intervencion) ~ 0,
      !is.na(nueva_fecha_intervencion) ~ 1,
      TRUE ~ NA_real_
    ),
    nueva_fecha_intervencion = case_when(
      estadoint == 0 ~ FECHA_ULTIMA_CONSULTA,
      estadoint == 1 ~ nueva_fecha_intervencion,
      TRUE ~ NA_POSIXct_
    )
  ) %>%
  relocate(estadoint, .before = HOSPITALIZACION) %>%
  mutate(dintspect = as.numeric(nueva_fecha_intervencion - FECHA_ESTUDIO, units = "days")) %>%
  relocate(dintspect, .after = nueva_fecha_intervencion)

# --- 7.2 Infarto no fatal ---
spectf <- spectf %>%
  mutate(
    estadoinfarto = case_when(
      is.na(fecha_infarto) ~ 0,
      !is.na(fecha_infarto) ~ 1,
      TRUE ~ NA_real_
    ),
    fecha_infarto = case_when(
      estadoinfarto == 0 ~ FECHA_ULTIMA_CONSULTA,
      estadoinfarto == 1 ~ fecha_infarto,
      TRUE ~ NA_POSIXct_
    )
  ) %>%
  relocate(estadoinfarto, .before = INFARTO_PREV_SPECT) %>%
  mutate(dinfarto_spect = as.numeric(fecha_infarto - FECHA_ESTUDIO, units = "days")) %>%
  relocate(dinfarto_spect, .after = fecha_infarto)

# --- 7.3 Muerte cardiovascular ---
# Estado: 0 = vivo/censurado, 1 = muerte CVD, 2 = muerte no CVD (censurado)
spectf <- spectf %>%
  mutate(
    estado_muerte = case_when(
      is.na(FECHA_MUERTE) ~ 0,
      !is.na(FECHA_MUERTE) & muerte_cvd == 0 ~ 2,
      !is.na(FECHA_MUERTE) ~ 1,
      TRUE ~ NA_real_
    ),
    FECHA_MUERTE = case_when(
      estado_muerte == 0 ~ FECHA_ULTIMA_CONSULTA,
      estado_muerte == 1 ~ FECHA_MUERTE,
      estado_muerte == 2 ~ FECHA_ULTIMA_CONSULTA,
      TRUE ~ NA_POSIXct_
    )
  ) %>%
  relocate(estado_muerte, .before = FECHA_ULTIMA_CONSULTA) %>%
  mutate(dMUERTE_spect = as.numeric(FECHA_MUERTE - FECHA_ESTUDIO, units = "days")) %>%
  relocate(dMUERTE_spect, .after = FECHA_MUERTE)

# --- 7.4 Hospitalización no CVD ---
spectf <- spectf %>%
  mutate(
    Estado_H_N_CVD = case_when(
      is.na(HOSPITALIZACION_NO_CVD) ~ 0,
      !is.na(HOSPITALIZACION_NO_CVD) ~ 1,
      TRUE ~ NA_real_
    ),
    HOSPITALIZACION_NO_CVD = case_when(
      Estado_H_N_CVD == 0 ~ FECHA_ULTIMA_CONSULTA,
      Estado_H_N_CVD == 1 ~ HOSPITALIZACION_NO_CVD,
      TRUE ~ NA_POSIXct_
    )
  ) %>%
  relocate(Estado_H_N_CVD, .before = infarto_outcome) %>%
  mutate(dHNCVD_spect = as.numeric(HOSPITALIZACION_NO_CVD - FECHA_ESTUDIO, units = "days")) %>%
  relocate(dHNCVD_spect, .after = HOSPITALIZACION_NO_CVD)

# --- 7.5 Hospitalización CVD (angina inestable o insuficiencia cardíaca) ---
# Tipos 1 y 4 = CVD;
spectf <- spectf %>%
  mutate(
    Estado_HCVD = case_when(
      is.na(HOSPITALIZACION_CVD) ~ 0,
      !is.na(HOSPITALIZACION_CVD) & Tipo_Hospitalizacion_cvd %in% c(2, 3, 5, 6, 7, 8) ~ 0,
      !is.na(HOSPITALIZACION_CVD) & Tipo_Hospitalizacion_cvd %in% c(1, 4) ~ 1,
      TRUE ~ NA_real_
    ),
    HOSPITALIZACION_CVD = case_when(
      Estado_HCVD == 0 ~ FECHA_ULTIMA_CONSULTA,
      Estado_HCVD == 1 ~ HOSPITALIZACION_CVD,
      TRUE ~ NA_POSIXct_
    )
  ) %>%
  relocate(Estado_HCVD, .before = HOSPITALIZACION_NO_CVD) %>%
  mutate(dHCVD_spect = as.numeric(HOSPITALIZACION_CVD - FECHA_ESTUDIO, units = "days")) %>%
  relocate(dHCVD_spect, .after = HOSPITALIZACION_CVD)

# --- 7.6 Hard MACE (compuesto: intervención + infarto + muerte CVD) ---
# Se toma el primer evento ocurrido; si el estado es 0, se registra como no evento
spectf <- spectf %>%
  rowwise() %>%
  mutate(
    IRM = min(c_across(c(dintspect, dinfarto_spect, dMUERTE_spect)), na.rm = TRUE),
    estadoIRM = case_when(
      IRM == dintspect     ~ 1,
      IRM == dinfarto_spect ~ 2,
      IRM == dMUERTE_spect  ~ 3,
      TRUE ~ NA_real_
    ),
    estadoIRM = case_when(
      estadoIRM == 1 & estadoint == 0      ~ 0,
      estadoIRM == 2 & estadoinfarto == 0  ~ 0,
      estadoIRM == 3 & estado_muerte == 0  ~ 0,
      TRUE ~ estadoIRM
    )
  ) %>%
  ungroup()

# --- 7.7 MACE ampliado (incluye hospitalización CVD) ---
spectf <- spectf %>%
  rowwise() %>%
  mutate(
    IHRM = min(c_across(c(dintspect, dHCVD_spect, dinfarto_spect, dMUERTE_spect)), na.rm = TRUE),
    estadoIHRM = case_when(
      IHRM == dintspect      ~ 1,
      IHRM == dHCVD_spect    ~ 2,
      IHRM == dinfarto_spect ~ 3,
      IHRM == dMUERTE_spect  ~ 4,
      TRUE ~ NA_real_
    ),
    estadoIHRM = case_when(
      estadoIHRM == 1 & estadoint == 0     ~ 0,
      estadoIHRM == 2 & Estado_HCVD == 0  ~ 0,
      estadoIHRM == 3 & estadoinfarto == 0 ~ 0,
      estadoIHRM == 4 & estado_muerte == 0 ~ 0,
      TRUE ~ estadoIHRM
    )
  ) %>%
  ungroup()

# ==============================================================================
# 8. VARIABLES DE EXPOSICIÓN (RESULTADO DEL SPECT)
# ==============================================================================

# --- 8.1 Resultado global del SPECT (4 categorías) ---
spectf <- spectf %>%
  mutate(
    resultado_vasos = case_when(
      vasos_isquemia == 0 & vasos_Infarto == 0 ~ 0,   # Normal
      vasos_isquemia != 0 & vasos_Infarto == 0 ~ 1,   # Solo isquemia
      vasos_isquemia == 0 & vasos_Infarto != 0 ~ 2,   # Solo infarto
      vasos_isquemia != 0 & vasos_Infarto != 0 ~ 3,   # Ambos
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(resultado_vasos, .after = vasos_Infarto)

spectf$resultado_vasos <- factor(
  spectf$resultado_vasos,
  levels = c(0, 1, 2, 3),
  labels = c("Normal", "Ischemia", "Infarction", "Ischemia + Infarction")
)

# --- 8.2 Extensión por regiones y arterias ---
spectf <- spectf %>%
  mutate(
    regiones_isquemia = case_when(
      vasos_isquemia == 0      ~ 0,
      vasos_isquemia %in% 1:3 ~ 1,
      vasos_isquemia %in% 4:6 ~ 2,
      vasos_isquemia == 7     ~ 3,
      TRUE ~ NA_real_
    ),
    regiones_infarto = case_when(
      vasos_Infarto == 0      ~ 0,
      vasos_Infarto %in% 1:3 ~ 1,
      vasos_Infarto %in% 4:6 ~ 2,
      vasos_Infarto == 7     ~ 3,
      TRUE ~ NA_real_
    ),
    arterias_isc = case_when(
      vasos_isquemia == 0      ~ 0,
      vasos_isquemia == 1     ~ 1,
      vasos_isquemia == 2     ~ 2,
      vasos_isquemia == 3     ~ 3,
      vasos_isquemia %in% 4:6 ~ 4,
      vasos_isquemia == 7     ~ 5,
      TRUE ~ NA_real_
    ),
    arterias_inf = case_when(
      vasos_Infarto == 0      ~ 0,
      vasos_Infarto == 1     ~ 1,
      vasos_Infarto == 2     ~ 2,
      vasos_Infarto == 3     ~ 3,
      vasos_Infarto %in% 4:6 ~ 4,
      vasos_Infarto == 7     ~ 5,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    regiones_isquemia = factor(regiones_isquemia),
    regiones_infarto  = factor(regiones_infarto),
    arterias_isc      = factor(arterias_isc),
    arterias_inf      = factor(arterias_inf)
  )

# ==============================================================================
# 9. VARIABLES DICOTÓMICAS PARA SUPERVIVENCIA, OTRAS COMORBILIDADES Y SEGUIMIENTO
# ==============================================================================

# Variables dicotómicas de eventos
spectf <- spectf %>%
  mutate(
    event_any            = ifelse(estadoIHRM != 0, 1, 0),
    hard_MACE_dicotomico = ifelse(estadoIRM != 0, 1, 0),
    estado_muerte        = ifelse(estado_muerte != 1, 0, 1),
    estadoint            = ifelse(estadoint != 1, 0, 1),
    estadoinfarto        = ifelse(estadoinfarto != 1, 0, 1),
    Estado_HCVD          = ifelse(Estado_HCVD != 1, 0, 1)
  )

# Variable "otras comorbilidades"
spectf <- spectf %>%
  mutate(totras_comorbilidades = if_else(
    DIL_CAV == 1 | ENF_ART_PER == 1 | ENF_VALVULAR == 1 | BLOQ_RAMA_IZQ == 1 |
      ARRITMIA == 1 | FIBRI_AURI == 1 | INS_CARD == 1 | ANEURISMA == 1 |
      ENF_TRIVAS == 1 | EPOC == 1 | MIOCARD == 1 | TAQUI_VENTR == 1 |
      TROM_VENO_PROF == 1 | EVC == 1 | MIOCARD_DILATADA == 1 | OTRA_ECV == 1,
    1, 0
  )) %>%
  relocate(totras_comorbilidades, .after = Dislipidemia)

# Tiempo total de seguimiento
spectf <- spectf %>%
  mutate(
    total_follow_up_days  = as.numeric(FECHA_ULTIMA_CONSULTA - FECHA_ESTUDIO, units = "days"),
    total_follow_up_years = total_follow_up_days / 365.25
  )

total_median <- median(spectf$total_follow_up_years, na.rm = TRUE)
total_q1     <- quantile(spectf$total_follow_up_years, 0.25, na.rm = TRUE)
total_q3     <- quantile(spectf$total_follow_up_years, 0.75, na.rm = TRUE)

cat(sprintf("\nTiempo de seguimiento — Mediana (RIC): %.1f (%.1f–%.1f) años\n",
            total_median, total_q1, total_q3))
cat(sprintf("Máximo seguimiento: %.1f años\n",
            max(spectf$total_follow_up_years, na.rm = TRUE)))

# ==============================================================================
# 10. TABLA 1: CARACTERÍSTICAS BASALES
# ==============================================================================

spectable1 <- spectf %>%
  mutate(
    SISTOLICA  = ifelse(SISTOLICA == 0, NA, SISTOLICA),
    DIASTOLICA = ifelse(DIASTOLICA == 0, NA, DIASTOLICA),
    edad_cat = factor(
      case_when(Edad < 40 ~ 1, Edad <= 65 ~ 2, TRUE ~ 3),
      levels = 1:3,
      labels = c("< 40 years", "40–65 years", "> 65 years")
    ),
    SEXO = factor(SEXO, levels = c(0, 1), labels = c("Women", "Men")),
    INFARTO_PREV_SPECT = factor(
      INFARTO_PREV_SPECT, levels = c(0, 1),
      labels = c("No previous infarction", "Previous infarction")
    ),
    PROTOCOLO = factor(
      PROTOCOLO, levels = c(1, 2),
      labels = c("99m Tc MIBI-Gated", "201-Thallium-Gated")
    ),
    PAD_ACT_SIMPLIFICADO = factor(
      PAD_ACT_SIMPLIFICADO, levels = 1:6,
      labels = c("Angina", "Asymptomatic", "Dyspnea",
                 "Impairment of NYHA functional class", "Palpitations", "Syncope")
    ),
    estadoIRM = factor(
      estadoIRM, levels = 1:3,
      labels = c("Intervention", "Myocardial Infarction", "CVD Death")
    )
  )

gtsummary::theme_gtsummary_compact()

tab1 <- spectable1 %>%
  select(
    resultado_vasos, Edad, edad_cat, SEXO, SISTOLICA, DIASTOLICA,
    CARD_ISQ_PREVIA, Diabetes, HAS, Dislipidemia, Tabaquismo, Obesidad,
    Ang_Cro_Est, totras_comorbilidades, PROTOCOLO, INFARTO_PREV_SPECT,
    PAD_ACT_SIMPLIFICADO, hard_MACE_dicotomico, estadoIRM
  ) %>%
  tbl_summary(
    by = "resultado_vasos",
    statistic = list(
      Edad ~ "{median} ({p25}-{p75})",
      SISTOLICA ~ "{median} ({p25}-{p75})",
      DIASTOLICA ~ "{median} ({p25}-{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      Edad                  = "Age (years)",
      edad_cat              = "Age group",
      SEXO                  = "Sex",
      PROTOCOLO             = "Type of protocol",
      SISTOLICA             = "Systolic blood pressure (mmHg)",
      DIASTOLICA            = "Diastolic blood pressure (mmHg)",
      HAS                   = "Systemic hypertension",
      Obesidad              = "Obesity",
      Tabaquismo            = "Active smokers",
      Diabetes              = "Type 2 diabetes",
      Ang_Cro_Est           = "Chronic stable angina",
      Dislipidemia          = "Dyslipidemia",
      totras_comorbilidades = "Other comorbidities",
      CARD_ISQ_PREVIA       = "Previous ischemic cardiomyopathy",
      PAD_ACT_SIMPLIFICADO  = "Reason for referral",
      INFARTO_PREV_SPECT    = "Previous infarction",
      estadoIRM             = "Hard MACE (first event)",
      hard_MACE_dicotomico  = "Hard MACE (any)"
    ),
    missing = "no"
  ) %>%
  add_overall() %>%
  add_p(
    test      = list(c("PAD_ACT_SIMPLIFICADO", "edad_cat") ~ "fisher.test"),
    test.args = list(c("PAD_ACT_SIMPLIFICADO", "edad_cat") ~ list(simulate.p.value = TRUE))
  ) %>%
  bold_p() %>%
  bold_labels() %>%
  modify_caption("**Table 1: Descriptive characteristics of the study population**") %>%
  modify_footnote(
    all_stat_cols() ~ "**Continuous variables: Median (IQR: Q1–Q3); Categorical variables: n (%)**"
  )

print(tab1)
tab1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "Figuras/Table 1 - Descriptive characteristics.docx")

# ==============================================================================
# 11. MODELOS DE REGRESIÓN DE COX
# ==============================================================================

# Hard MACE (outcome primario)
m1 <- coxph(Surv(IRM, hard_MACE_dicotomico) ~ resultado_vasos, data = spectf)
m2 <- coxph(Surv(IRM, hard_MACE_dicotomico) ~ Edad + SEXO + ENF_TRIVAS + Diabetes + HAS + Tabaquismo + Dislipidemia + resultado_vasos, data = spectf)

# Muerte CVD
m3 <- coxph(Surv(dMUERTE_spect, estado_muerte) ~ resultado_vasos, data = spectf)
m4 <- coxph(Surv(dMUERTE_spect, estado_muerte) ~ Edad + SEXO + ENF_TRIVAS + Diabetes + HAS + Tabaquismo + Dislipidemia + resultado_vasos, data = spectf)

# Intervención (revascularización)
m5 <- coxph(Surv(dintspect, estadoint) ~ resultado_vasos, data = spectf)
m6 <- coxph(Surv(dintspect, estadoint) ~ Edad + SEXO + ENF_TRIVAS + Diabetes + HAS + Tabaquismo + Dislipidemia + resultado_vasos, data = spectf)

# Infarto no fatal
m7 <- coxph(Surv(dinfarto_spect, estadoinfarto) ~ resultado_vasos, data = spectf)
m8 <- coxph(Surv(dinfarto_spect, estadoinfarto) ~ Edad + SEXO + ENF_TRIVAS + Diabetes + HAS + Tabaquismo + Dislipidemia + resultado_vasos, data = spectf)

# Hospitalización CVD
m9  <- coxph(Surv(dHCVD_spect, Estado_HCVD) ~ resultado_vasos, data = spectf)
m10 <- coxph(Surv(dHCVD_spect, Estado_HCVD) ~ Edad + SEXO + ENF_TRIVAS + Diabetes + HAS + Tabaquismo + Dislipidemia + resultado_vasos, data = spectf)

# MACE ampliado (outcomes secundarios)
m11 <- coxph(Surv(IHRM, event_any) ~ resultado_vasos, data = spectf)
m12 <- coxph(Surv(IHRM, event_any) ~ Edad + SEXO + ENF_TRIVAS + Diabetes + HAS + Tabaquismo + Dislipidemia + resultado_vasos, data = spectf)

# MACE ampliado por número de arterias
m13 <- coxph(Surv(IHRM, event_any) ~ arterias_isc + arterias_inf, data = spectf)
m14 <- coxph(Surv(IHRM, event_any) ~ Edad + SEXO + ENF_TRIVAS + Diabetes + HAS + Tabaquismo + Dislipidemia + arterias_isc + arterias_inf, data = spectf)

# Muerte CVD por número de arterias
m15 <- coxph(Surv(dMUERTE_spect, estado_muerte) ~ arterias_isc + arterias_inf, data = spectf)
m16 <- coxph(Surv(dMUERTE_spect, estado_muerte) ~ Edad + SEXO + ENF_TRIVAS + Diabetes + HAS + Tabaquismo + Dislipidemia + arterias_isc + arterias_inf, data = spectf)

# Intervención por número de arterias
m17 <- coxph(Surv(dintspect, estadoint) ~ arterias_isc + arterias_inf, data = spectf)
m18 <- coxph(Surv(dintspect, estadoint) ~ Edad + SEXO + ENF_TRIVAS + Diabetes + HAS + Tabaquismo + Dislipidemia + arterias_isc + arterias_inf, data = spectf)

# Infarto no fatal por número de arterias
m19 <- coxph(Surv(dinfarto_spect, estadoinfarto) ~ arterias_isc + arterias_inf, data = spectf)
m20 <- coxph(Surv(dinfarto_spect, estadoinfarto) ~ Edad + SEXO + ENF_TRIVAS + Diabetes + HAS + Tabaquismo + Dislipidemia + arterias_isc + arterias_inf, data = spectf)

# ==============================================================================
# 12. TASAS DE INCIDENCIA ESTANDARIZADAS POR EDAD Y SEXO
# ==============================================================================

age_breaks <- c(15, 45, 55, 65, 75, 85, Inf)
age_labels  <- c("15-44", "45-54", "55-64", "65-74", "75-84", "85+")

spectf <- spectf %>%
  mutate(
    age_group = cut(Edad, breaks = age_breaks, right = FALSE, labels = age_labels),
    sex       = factor(ifelse(SEXO == 0, "Female", "Male"), levels = c("Female", "Male")),
    pt        = IRM / 365.25
  )

# Población estándar interna (distribución de edad y sexo de toda la muestra)
std_pop_df <- spectf %>%
  count(age_group, sex, name = "std_pop") %>%
  tidyr::complete(
    age_group = factor(age_labels, levels = age_labels),
    sex       = factor(c("Female", "Male"), levels = c("Female", "Male")),
    fill      = list(std_pop = 0)
  ) %>%
  arrange(sex, age_group)

.get_elem <- function(x, ...) {
  nms <- names(x)
  for (nm in c(...)) if (!is.null(nms) && nm %in% nms) return(unname(x[[nm]]))
  NA_real_
}

calculate_std_rate <- function(data, outcome_var, exposure = "resultado_vasos") {
  ag <- data %>%
    dplyr::group_by(.data[[exposure]], age_group, sex) %>%
    dplyr::summarise(
      events = sum(.data[[outcome_var]] == 1, na.rm = TRUE),
      pt     = sum(pt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      !!rlang::sym(exposure),
      age_group = factor(age_labels, levels = age_labels),
      sex       = factor(c("Female", "Male"), levels = c("Female", "Male")),
      fill      = list(events = 0, pt = 0)
    ) %>%
    dplyr::left_join(std_pop_df, by = c("age_group", "sex")) %>%
    dplyr::arrange(.data[[exposure]], sex, age_group)
  
  common_cells <- ag %>%
    dplyr::group_by(age_group, sex) %>%
    dplyr::summarise(keep = all(pt > 0), .groups = "drop")
  
  ag <- ag %>%
    dplyr::left_join(common_cells, by = c("age_group", "sex")) %>%
    dplyr::filter(keep) %>%
    dplyr::select(-keep)
  
  if (nrow(ag) == 0) stop("No hay estratos edad×sexo comunes con tiempo en riesgo positivo.")
  
  ag %>%
    dplyr::group_by(.data[[exposure]]) %>%
    dplyr::summarise(
      events_total = sum(events),
      pt_total     = sum(pt),
      res = list(epitools::ageadjust.direct(
        count    = events,
        pop      = pt,
        stdpop   = std_pop,
        conf.level = 0.95
      )),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      adj_rate_per_1000py = purrr::map_dbl(res, ~ .get_elem(.x, "adj.rate", "adjrate") * 1000),
      lower_ci            = purrr::map_dbl(res, ~ .get_elem(.x, "lower", "lci") * 1000),
      upper_ci            = purrr::map_dbl(res, ~ .get_elem(.x, "upper", "uci") * 1000)
    ) %>%
    dplyr::transmute(
      exposure_level      = .data[[exposure]],
      events              = events_total,
      person_time_years   = pt_total,
      adj_rate_per_1000py,
      lower_ci,
      upper_ci
    )
}

# Tasas estandarizadas por outcome
std_rate_hardMACE     <- calculate_std_rate(spectf, "hard_MACE_dicotomico")
std_rate_CVD_death    <- calculate_std_rate(spectf, "estado_muerte")
std_rate_intervention <- calculate_std_rate(spectf, "estadoint")
std_rate_infarct      <- calculate_std_rate(spectf, "estadoinfarto")

# Tasa global
spectf_overall <- spectf %>% mutate(Overall = "Total population")
overall_rate   <- calculate_std_rate(spectf_overall, "hard_MACE_dicotomico", exposure = "Overall")

cat(sprintf("\nTasa global Hard MACE (estandarizada): %.1f por 1,000 personas-año (IC 95%%: %.1f–%.1f)\n",
            overall_rate$adj_rate_per_1000py, overall_rate$lower_ci, overall_rate$upper_ci))

# ==============================================================================
# 13. TABLA 2: TASAS ESTANDARIZADAS Y MODELOS DE COX
# ==============================================================================

# HR de modelos Cox
extract_hr <- function(model, exposure_levels) {
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    dplyr::filter(grepl("resultado_vasos", term)) %>%
    mutate(
      Exposure = gsub("resultado_vasos", "", term),
      HR_CI    = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
      p_value  = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ) %>%
    select(Exposure, HR_CI, p_value) %>%
    bind_rows(tibble(Exposure = exposure_levels[1], HR_CI = "Reference", p_value = "Reference"), .)
}

outcome_map <- list(
  "MACE"                           = list(model_unadj = m1, model_adj = m2, std = std_rate_hardMACE),
  "CVD deaths"                     = list(model_unadj = m3, model_adj = m4, std = std_rate_CVD_death),
  "Revascularization"              = list(model_unadj = m5, model_adj = m6, std = std_rate_intervention),
  "Non-fatal Myocardial Infarction" = list(model_unadj = m7, model_adj = m8, std = std_rate_infarct)
)

table_data <- purrr::imap_dfr(outcome_map, function(x, outcome_name) {
  std <- x$std %>%
    mutate(
      Outcome   = outcome_name,
      Exposure  = as.character(exposure_level),
      incidence = sprintf("%.1f (%.1f–%.1f)", adj_rate_per_1000py, lower_ci, upper_ci)
    ) %>%
    select(Outcome, Exposure, events, incidence)
  
  exposure_levels <- unique(std$Exposure)
  
  std %>%
    left_join(extract_hr(x$model_unadj, exposure_levels), by = "Exposure") %>%
    rename(`Unadjusted HR (95% C.I.)` = HR_CI, `p-value_unadj` = p_value) %>%
    left_join(extract_hr(x$model_adj, exposure_levels), by = "Exposure") %>%
    rename(`Adjusted HR (95% C.I.)` = HR_CI, `p-value_adj` = p_value)
})

desired_outcome_order <- c("MACE", "CVD deaths", "Revascularization", "Non-fatal Myocardial Infarction")
exposure_order        <- c("Normal", "Ischemia", "Infarction", "Ischemia + Infarction")

final_table <- table_data %>%
  rename(
    `Number of Events` = events,
    `Age- and sex-standardized incidence rates (95% C.I.)` = incidence,
    `p-value`          = `p-value_unadj`,
    `p-value (adj)`    = `p-value_adj`
  ) %>%
  mutate(
    Exposure = factor(Exposure, levels = exposure_order),
    Outcome  = factor(Outcome,  levels = desired_outcome_order)
  ) %>%
  arrange(Outcome, Exposure)

gt_table <- final_table %>%
  gt(groupname_col = "Outcome") %>%
  tab_header(title = "Age- and Sex-standardized Incidence Rates and Cox Proportional Hazards Models") %>%
  cols_label(
    Exposure                                               = "Exposure",
    `Number of Events`                                     = "Number of Events",
    `Age- and sex-standardized incidence rates (95% C.I.)` = "Age- and sex-standardized incidence rates⁽ᵃ⁾ (95% C.I.)",
    `Unadjusted HR (95% C.I.)`                            = "Unadjusted HR (95% C.I.)",
    `p-value`                                              = "p-value",
    `Adjusted HR (95% C.I.)`                              = "Adjusted HR⁽ᵇ⁾ (95% C.I.)",
    `p-value (adj)`                                        = "p-value"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything()))

print(gt_table)
gtsave(gt_table, "Figuras/Table 2 - Cox models and standardized rates.rtf")

# ==============================================================================
# 14. CURVAS DE KAPLAN-MEIER CON PESOS IPW
# ==============================================================================

spectf <- spectf %>%
  mutate(IRM_years = IRM / 365.25)

grps <- levels(spectf$resultado_vasos)
pal  <- ggsci::pal_nejm("default")(length(grps))

treatment_model <- nnet::multinom(
  resultado_vasos ~ Edad + SEXO + ENF_TRIVAS + Diabetes + HAS + Tabaquismo + Dislipidemia,
  data = spectf
)

s_iptw <- adjustedsurv(
  data             = spectf,
  variable         = "resultado_vasos",
  ev_time          = "IRM_years",
  event            = "hard_MACE_dicotomico",
  method           = "iptw_km",
  treatment_model  = treatment_model,
  weight_method    = "glm",
  conf_int         = TRUE,
  stabilize        = TRUE
)

fig2 <- plot(
  s_iptw,
  risk_table                 = TRUE,
  risk_table_stratify_color  = TRUE,
  risk_table_stratify        = TRUE,
  risk_table_digits          = 0,
  x_n_breaks                 = 8,
  risk_table_title_size      = 10,
  median_surv_lines          = TRUE,
  gg_theme                   = theme_bw(base_size = 14),
  risk_table_theme           = theme_pubclean(base_size = 11),
  legend.position            = "top",
  xlab                       = "Time in Years",
  ylim                       = c(0, 1),
  conf_int                   = TRUE,
  conf_int_alpha             = 0.25,
  custom_colors              = pal,
  risk_table_custom_colors   = pal
) +
  labs(title = "Kaplan–Meier Curves (IPTW-adjusted)") +
  theme(
    plot.title   = element_text(hjust = 0, size = 16, face = "bold"),
    legend.text  = element_text(size = 8),
    legend.title = element_text(size = 11),
    axis.text    = element_text(size = 11),
    axis.title   = element_text(size = 12)
  )

print(fig2)

# ==============================================================================
# 15. FOREST PLOT (MODELO AJUSTADO M2)
# ==============================================================================

panels <- list(
  list(
    width   = 0.30,
    hjust   = 0,
    heading = "Level\nHR (95% CI)\np",
    display = ~ paste0(
      str_wrap(level, 18), "\n",
      ifelse(reference, "Reference",
             sprintf("%.2f (%.2f–%.2f)\np=%s",
                     exp(estimate), exp(conf.low), exp(conf.high),
                     ifelse(is.na(p.value), "",
                            ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)))))
    )
  ),
  list(width = 0.07, display = ~n,        hjust = 0.5, heading = "N"),
  list(width = 0.08, display = ~n_events, hjust = 0.5, heading = "Events"),
  list(width = 0.05, item = "vline"),
  list(width = 0.50, item = "forest", heading = "Hazard Ratio",
       line_x = 0, linetype = "dashed", point_size = 3)
)

fig3 <- forest_model(
  m2,
  covariates          = "resultado_vasos",
  exponentiate        = TRUE,
  factor_separate_line = TRUE,
  panels              = panels,
  theme               = theme_forest(),
  limits              = c(log(0.5), log(20)),
  breaks              = c(log(0.5), log(1), log(2), log(5), log(10), log(15), log(20)),
  recalculate_width   = FALSE
) +
  labs(title = "Forest Plot of Multivariable Cox Regression Model for MACE") +
  theme(
    axis.text.x  = element_text(size = 11),
    plot.title   = element_text(hjust = 0, size = 14, face = "bold"),
    plot.margin  = margin(15, 20, 15, 20),
    text         = element_text(size = 10)
  )

print(fig3)

# ==============================================================================
# 16. FIGURA COMBINADA: KM + FOREST PLOT
# ==============================================================================

fig5 <- ggpubr::ggarrange(
  fig2, fig3,
  ncol    = 2, nrow = 1,
  labels  = LETTERS[1:2],
  align   = "h",
  widths  = c(1.2, 1)
)

print(fig5)

ggsave("Figuras/Figure 4. KM and Forest plot.svg",
       fig5, bg = "white", width = 80, height = 40, units = "cm", dpi = 450, device = "svg")
ggsave("Figuras/Figure 4. KM and Forest plot.png",
       fig5, bg = "white", width = 80, height = 40, units = "cm", dpi = 450)

# ==============================================================================
# 17. GRÁFICO DE BARRAS: TASAS ESTANDARIZADAS (HARD MACE)
# ==============================================================================

fig6 <- ggplot(std_rate_hardMACE,
               aes(x = exposure_level, y = adj_rate_per_1000py, fill = exposure_level)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.5, show.legend = FALSE) +
  scale_fill_jama() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.15, linewidth = 0.7) +
  geom_text(
    aes(y = upper_ci,
        label = sprintf("%.1f\n(%.1f–%.1f)", adj_rate_per_1000py, lower_ci, upper_ci)),
    nudge_y = 0.06 * max(std_rate_hardMACE$upper_ci),
    vjust = 0, size = 4, color = "black"
  ) +
  scale_x_discrete(limits = exposure_order) +
  scale_y_continuous(
    limits = c(0, max(std_rate_hardMACE$upper_ci) * 1.15),
    expand = expansion(mult = c(0, 0.05))
  ) +
  annotate("text", x = 2.5, y = max(std_rate_hardMACE$upper_ci) * 1.10,
           label = sprintf("Overall Rate: %.1f (95%% CI: %.1f–%.1f)",
                           overall_rate$adj_rate_per_1000py,
                           overall_rate$lower_ci, overall_rate$upper_ci),
           fontface = 2, size = 5) +
  labs(
    title = "Age- and Sex-standardized Incidence Rates",
    x     = NULL,
    y     = "Events per 1,000 Person-Years (age & sex standardized)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.title.x       = element_text(margin = margin(t = 10)),
    axis.title.y       = element_text(margin = margin(r = 10))
  )

ggsave("Figuras/Figure 6 - Age- and Sex-standardized Incidence Rates.svg",
       fig6, bg = "white", width = 40, height = 20, units = "cm", dpi = 450, device = "svg")

# ==============================================================================
# 18. TABLA DE DIAGNÓSTICO DE MODELOS COX (HARD MACE)
# ==============================================================================

extract_model_diagnostics <- function(model, model_name, exposure_name) {
  data <- eval(model$call$data)
  
  # Likelihood Ratio Test vs. modelo nulo
  null_model <- coxph(update(formula(model), . ~ 1), data = data)
  lr_stat    <- 2 * (logLik(model) - logLik(null_model))
  lr_df      <- length(coef(model)) - length(coef(null_model))
  lr_p       <- 1 - pchisq(lr_stat, df = lr_df)
  lr_text    <- sprintf("%.2f (p=%s)", lr_stat,
                        ifelse(lr_p < 0.001, "<0.001", sprintf("%.3f", lr_p)))
  
  # C-statistic (índice de concordancia de Harrell)
  conc       <- summary(model)$concordance
  c_text     <- sprintf("%.2f (%.2f–%.2f)",
                        conc[1], conc[1] - 1.96 * conc[2], conc[1] + 1.96 * conc[2])
  
  # BIC
  bic_text <- sprintf("%.2f", BIC(model))
  
  # Test de residuales de Schoenfeld (supuesto de riesgos proporcionales)
  sch       <- cox.zph(model)$table["GLOBAL", ]
  sch_text  <- sprintf("%.2f (p=%s)", sch["chisq"],
                       ifelse(sch["p"] < 0.001, "<0.001", sprintf("%.3f", sch["p"])))
  
  data.frame(
    Outcome          = model_name,
    Exposure         = exposure_name,
    Likelihood_Test  = lr_text,
    C_Statistic      = c_text,
    BIC              = bic_text,
    Schoenfeld_Test  = sch_text,
    stringsAsFactors = FALSE
  )
}

diagnostic_table <- bind_rows(
  extract_model_diagnostics(m1, "Hard MACE", "SPECT result (unadjusted)"),
  extract_model_diagnostics(m2, "Hard MACE", "SPECT result (adjusted)")
)

gt_diagnostics <- diagnostic_table %>%
  gt() %>%
  tab_header(
    title    = md("**Supplementary Table: Model diagnostics — Cox proportional hazards models (primary outcome)**"),
    subtitle = "Likelihood ratio test, C-statistic, BIC, and Schoenfeld residuals for Hard MACE"
  ) %>%
  cols_label(
    Outcome         = "Outcome",
    Exposure        = "Model specification",
    Likelihood_Test = "Likelihood Ratio Test (χ², p-value)¹",
    C_Statistic     = "C-statistic (95% CI)²",
    BIC             = "BIC³",
    Schoenfeld_Test = "Schoenfeld residuals χ² (p-value)⁴"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left",   columns = c(Outcome, Exposure)) %>%
  tab_footnote("Likelihood ratio test compares the full model to a null (intercept-only) model.",
               locations = cells_column_labels(Likelihood_Test)) %>%
  tab_footnote("Harrell's concordance index; values > 0.7 indicate good discrimination.",
               locations = cells_column_labels(C_Statistic)) %>%
  tab_footnote("Bayesian Information Criterion; lower values indicate better fit.",
               locations = cells_column_labels(BIC)) %>%
  tab_footnote("p > 0.05 suggests no violation of the proportional hazards assumption.",
               locations = cells_column_labels(Schoenfeld_Test)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything()))

print(gt_diagnostics)
gtsave(gt_diagnostics, "Figuras/Supplementary_Table_Schoenfeld_HardMACE.rtf")

# ==============================================================================
# 19. INCIDENCIA ACUMULADA (CURVAS IPTW)
# ==============================================================================

adj_df <- s_iptw$adj

adj_df <- adj_df %>%
  mutate(
    cuminc       = 1 - surv,
    cuminc_lower = 1 - ci_upper,
    cuminc_upper = 1 - ci_lower
  )

grupos_presentes <- unique(adj_df$group)
pal_ci           <- setNames(ggsci::pal_nejm("default")(length(grupos_presentes)), grupos_presentes)

fig_cuminc <- ggplot(adj_df, aes(x = time, y = cuminc, color = group, fill = group)) +
  geom_step(linewidth = 1.2) +
  geom_ribbon(aes(ymin = cuminc_lower, ymax = cuminc_upper), alpha = 0.2, linetype = 0) +
  scale_color_manual(values = pal_ci, name = "SPECT result") +
  scale_fill_manual(values  = pal_ci, name = "SPECT result") +
  scale_x_continuous(name = "Time in Years",
                     breaks = seq(0, max(adj_df$time, na.rm = TRUE), by = 1)) +
  scale_y_continuous(name   = "Cumulative Incidence (Probability of MACE)",
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title      = element_text(hjust = 0, size = 16, face = "bold"),
    legend.text     = element_text(size = 9),
    axis.title      = element_text(size = 12),
    axis.text       = element_text(size = 10)
  ) +
  labs(title = "Cumulative Incidence of Hard MACE (IPTW-adjusted)")

print(fig_cuminc)
ggsave("Figuras/Figure_Cumulative_Incidence_IPTW.png",
       fig_cuminc, width = 30, height = 20, units = "cm", dpi = 450)
ggsave("Figuras/Figure_Cumulative_Incidence_IPTW.svg",
       fig_cuminc, width = 30, height = 20, units = "cm", dpi = 450)

# Tabla de incidencias a 1, 3 y 5 años
tiempos_interes  <- c(1, 3, 5)
incidencia_tabla <- purrr::map_dfr(tiempos_interes, function(t) {
  purrr::map_dfr(grupos_presentes, function(g) {
    df_g <- adj_df[adj_df$group == g, ]
    idx  <- which.min(abs(df_g$time - t))
    data.frame(
      Time   = t,
      Group  = g,
      CumInc = df_g$cuminc[idx],
      Lower  = df_g$cuminc_lower[idx],
      Upper  = df_g$cuminc_upper[idx]
    )
  })
}) %>%
  arrange(Time, factor(Group, levels = exposure_order)) %>%
  mutate(Report = sprintf("%.1f%% (%.1f%%–%.1f%%)", CumInc * 100, Lower * 100, Upper * 100))

cat("\nIncidencia acumulada de Hard MACE a 1, 3 y 5 años (IPTW):\n")
print(incidencia_tabla[, c("Time", "Group", "Report")])

# ==============================================================================
# 20. TABLA DE INCIDENCIA ACUMULADA A 5 AÑOS + HR
# ==============================================================================

inc_5a <- incidencia_tabla %>%
  filter(Time == 5) %>%
  mutate(
    CumInc_CI = sprintf("%.1f%% (%.1f%%–%.1f%%)", CumInc * 100, Lower * 100, Upper * 100),
    Group     = factor(Group, levels = exposure_order)
  )

hr_unadj <- broom::tidy(m1, exponentiate = TRUE, conf.int = TRUE)
hr_adj   <- broom::tidy(m2, exponentiate = TRUE, conf.int = TRUE)

format_hr <- function(tidy_df) {
  rows <- tidy_df %>% filter(grepl("resultado_vasos", term))
  data.frame(
    Group   = factor(gsub("resultado_vasos", "", rows$term), levels = exposure_order),
    HR_text = sprintf("%.2f (%.2f–%.2f)", rows$estimate, rows$conf.low, rows$conf.high),
    p_text  = ifelse(rows$p.value < 0.001, "<0.001", sprintf("%.3f", rows$p.value))
  )
}

hr_table <- bind_rows(
  data.frame(Group = factor("Normal", levels = exposure_order),
             HR_unadj = "Reference", p_unadj = "",
             HR_adj   = "Reference", p_adj   = ""),
  format_hr(hr_unadj) %>% rename(HR_unadj = HR_text, p_unadj = p_text) %>%
    left_join(format_hr(hr_adj) %>% rename(HR_adj = HR_text, p_adj = p_text), by = "Group")
)

event_counts <- spectf %>%
  group_by(resultado_vasos) %>%
  summarise(N_events = sum(hard_MACE_dicotomico, na.rm = TRUE)) %>%
  rename(Group = resultado_vasos) %>%
  mutate(Group = factor(as.character(Group), levels = exposure_order))

tabla_final <- inc_5a %>%
  left_join(event_counts, by = "Group") %>%
  left_join(hr_table,     by = "Group") %>%
  arrange(Group) %>%
  select(Group, N_events, CumInc_CI, HR_unadj, p_unadj, HR_adj, p_adj)

tab_incidencia <- tabla_final %>%
  gt() %>%
  tab_header(
    title    = md("**Cumulative Incidence of Hard MACE at 5 Years and Cox Regression Models**"),
    subtitle = "IPTW-adjusted Kaplan–Meier estimates and hazard ratios by SPECT result"
  ) %>%
  cols_label(
    Group      = "SPECT result",
    N_events   = "Number of Events",
    CumInc_CI  = "Cumulative incidence at 5 years (95% CI)",
    HR_unadj   = "Unadjusted HR (95% CI)",
    p_unadj    = "p-value",
    HR_adj     = "Adjusted HR⁽ᵃ⁾ (95% CI)",
    p_adj      = "p-value"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left",   columns = Group) %>%
  tab_footnote("Adjusted for age, sex, peripheral artery disease, diabetes, hypertension, smoking, and dyslipidemia.",
               locations = cells_column_labels(HR_adj)) %>%
  tab_footnote("Cumulative incidence derived from IPTW-adjusted Kaplan–Meier curves (1 − survival).",
               locations = cells_column_labels(CumInc_CI)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything()))

print(tab_incidencia)

# ==============================================================================
# 21. TABLAS SUPLEMENTARIAS
# ==============================================================================

# --- Supplementary Table 1: Sensibilidad — umbral de revascularización tardía ---
sup_tab1_data <- data.frame(
  Threshold = rep(c("≥60 days", "≥90 days", "≥180 days", "≥365 days"), each = 4),
  Exposure  = rep(c("Normal", "Ischemia", "Infarction", "Both"), times = 4),
  Events    = c(7,16,12,46, 7,14,11,42, 6,10,9,27, 4,8,8,17),
  Incidence = c(
    "10.1 (4.0–22.1)", "43.8 (24.6–73.1)", "27.0 (13.5–51.5)", "84.2 (60.1–120.9)",
    "10.1 (4.0–22.1)", "36.9 (19.8–64)",   "24.9 (12.0–49.0)", "76.3 (53.5–111.7)",
    "9.0 (3.2–20.6)",  "25.8 (12.1–49.8)", "21.1 (9.2–44.4)",  "49.2 (31.3–80.5)",
    "6.0 (1.6–16.5)",  "21.0 (8.8–43.6)",  "19.0 (7.8–41.9)",  "31.5 (17.5–59.7)"
  ),
  HR_unadj = c(
    "Reference", "4.66 (1.92–11.32)", "3.29 (1.30–8.36)",  "9.31 (4.20–20.61)",
    "Reference", "3.95 (1.60–9.80)",  "2.99 (1.16–7.73)",  "8.27 (3.72–18.41)",
    "Reference", "3.09 (1.12–8.51)",  "2.80 (1.00–7.86)",  "5.78 (2.39–14.00)",
    "Reference", "3.55 (1.07–11.81)", "3.75 (1.13–12.46)", "5.18 (1.74–15.40)"
  ),
  p_unadj = c(
    "", "<0.001", "0.012", "<0.001",
    "", "0.003",  "0.023", "<0.001",
    "", "0.029",  "0.051", "<0.001",
    "", "0.038",  "0.031", "0.003"
  ),
  HR_adj = c(
    "Reference", "4.32 (1.77–10.55)", "2.76 (1.07–7.13)",  "7.81 (3.46–17.65)",
    "Reference", "3.56 (1.43–8.87)",  "2.36 (0.90–6.19)",  "6.55 (2.88–14.89)",
    "Reference", "2.69 (0.97–7.45)",  "2.07 (0.72–5.97)",  "4.26 (1.70–10.66)",
    "Reference", "3.15 (0.94–10.56)", "3.04 (0.89–10.34)", "4.13 (1.34–12.73)"
  ),
  p_adj = c(
    "", "0.001", "0.036", "<0.001",
    "", "0.006", "0.081", "<0.001",
    "", "0.057", "0.176", "0.002",
    "", "0.063", "0.075", "0.014"
  )
)

sup_tab1_gt <- sup_tab1_data %>%
  gt(groupname_col = "Threshold") %>%
  tab_header(
    title    = md("**Supplementary Table 1:** Cox models evaluating the effect of SPECT results on late revascularization by definition threshold."),
    subtitle = "Sensitivity Analysis: Late Revascularization by Definition Threshold"
  ) %>%
  cols_label(
    Exposure  = "Exposure",
    Events    = "Number of Events",
    Incidence = "Age- and sex-standardized incidence rates⁽ᵃ⁾ (95% C.I.)",
    HR_unadj  = "Unadjusted HR (95% C.I.)",
    p_unadj   = "p-value",
    HR_adj    = "Adjusted HR⁽ᵇ⁾ (95% C.I.)",
    p_adj     = "p-value"
  ) %>%
  tab_footnote("Age- and sex-standardized incidence rates expressed per 1,000 person-years.",
               locations = cells_column_labels(Incidence)) %>%
  tab_footnote("Adjusted for age, sex, trivascular disease, diabetes, hypertension, smoking, and dyslipidemia.",
               locations = cells_column_labels(HR_adj)) %>%
  cols_align(align = "center", columns = everything())

gtsave(sup_tab1_gt, "Figuras/Supplementary_Table_1.rtf")


# --- Supplementary Table 2: MACE ampliado ---
sup_tab2_data <- data.frame(
  Outcome   = rep(c("MACE", "CVD hospitalization"), each = 4),
  Exposure  = rep(c("Normal", "Ischemia", "Infarction", "Both"), times = 2),
  Events    = c(10, 20, 18, 52, 2, 3, 0, 4),
  Incidence = c(
    "15.4 (7.2–29.3)", "54.4 (32.7–86.2)", "43.9 (25.2–73.5)", "92.3 (67.4–129.4)",
    "4.0 (0.5–14.6)",  "8.7 (1.7–27.2)",   "0",                "5.9 (1.6–27.8)"
  ),
  HR_unadj = c(
    "Reference", "4.16 (1.95–8.89)", "3.53 (1.63–7.66)", "7.36 (3.74–14.47)",
    "Reference", "2.92 (0.49–17.57)", "---",              "2.43 (0.44–13.28)"
  ),
  p_unadj = c("", "<0.001", "0.001", "<0.001", "", "0.241", "---", "0.305"),
  HR_adj   = c(
    "Reference", "3.84 (1.79–8.26)", "2.95 (1.34–6.49)", "6.19 (3.09–12.40)",
    "Reference", "2.24 (0.36–13.98)", "---",              "1.79 (0.29–11.21)"
  ),
  p_adj = c("", "<0.001", "0.007", "<0.001", "", "0.387", "---", "0.534")
)

sup_tab2_gt <- sup_tab2_data %>%
  gt(groupname_col = "Outcome") %>%
  tab_header(
    title    = md("**Supplementary Table 2:** Cox models evaluating the effect of SPECT results on MACE including CVD hospitalization."),
    subtitle = "Age- and Sex-standardized Incidence Rates and Cox Proportional Hazards Models"
  ) %>%
  cols_label(
    Exposure  = "Exposure",
    Events    = "Number of Events",
    Incidence = "Age- and sex-standardized incidence rates⁽ᵃ⁾ (95% C.I.)",
    HR_unadj  = "Unadjusted HR (95% C.I.)",
    p_unadj   = "p-value",
    HR_adj    = "Adjusted HR⁽ᵇ⁾ (95% C.I.)",
    p_adj     = "p-value"
  ) %>%
  tab_footnote("Age- and sex-standardized incidence rates expressed per 1,000 person-years.",
               locations = cells_column_labels(Incidence)) %>%
  tab_footnote("Adjusted for age, sex, trivascular disease, diabetes, hypertension, smoking, and dyslipidemia.",
               locations = cells_column_labels(HR_adj)) %>%
  cols_align(align = "center", columns = everything())

gtsave(sup_tab2_gt, "Figuras/Supplementary_Table_2.rtf")

# --- Supplementary Table 3: Win Ratio — análisis de sensibilidad por jerarquía ---
sup_tab3_data <- data.frame(
  Model     = 1:6,
  Hierarchy = c(
    "CVD > MI > Revasc (Primary)", "CVD > Revasc > MI",
    "MI > CVD > Revasc",           "MI > Revasc > CVD",
    "Revasc > CVD > MI",           "Revasc > MI > CVD"
  ),
  Win_Ratio = c(4.62, 4.57, 4.66, 4.77, 4.67, 4.70),
  WR_lower  = c(2.91, 2.87, 2.94, 2.99, 2.93, 2.95),
  WR_upper  = c(7.34, 7.27, 7.41, 7.58, 7.44, 7.49),
  Net_Benefit = c(0.086, 0.086, 0.087, 0.088, 0.086, 0.087),
  NB_lower    = c(0.055, 0.055, 0.056, 0.056, 0.055, 0.056),
  NB_upper    = c(0.117, 0.117, 0.117, 0.118, 0.117, 0.117),
  Win_Odds  = c(1.189, 1.188, 1.190, 1.192, 1.190, 1.190),
  WO_lower  = c(1.117, 1.116, 1.118, 1.120, 1.118, 1.118),
  WO_upper  = c(1.265, 1.264, 1.266, 1.268, 1.266, 1.266)
) %>%
  mutate(
    WR_CI = sprintf("%.2f (%.2f–%.2f)", Win_Ratio, WR_lower, WR_upper),
    NB_CI = sprintf("%.3f (%.3f–%.3f)", Net_Benefit, NB_lower, NB_upper),
    WO_CI = sprintf("%.3f (%.3f–%.3f)", Win_Odds, WO_lower, WO_upper)
  )

sup_tab3_gt <- sup_tab3_data %>%
  select(Model, Hierarchy, WR_CI, NB_CI, WO_CI) %>%
  gt() %>%
  tab_header(
    title    = md("**Supplementary Table 3:** Sensitivity Analysis — Win Ratio, Net Benefit, and Win Odds by composite endpoint hierarchy."),
    subtitle = "All models adjusted; results stable across hierarchies"
  ) %>%
  cols_label(
    Model     = "Model",
    Hierarchy = "Hierarchy (priority order)",
    WR_CI     = "Win Ratio (95% CI)",
    NB_CI     = "Net Benefit (95% CI)",
    WO_CI     = "Win Odds (95% CI)"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left",   columns = Hierarchy) %>%
  tab_footnote(
    "Win ratio estimates (4.6–4.8), net benefit, and win odds were consistent across all hierarchies (all p < 0.001), demonstrating robustness to endpoint prioritization.",
    locations = cells_body(columns = WR_CI, rows = 1)
  ) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything()))

print(sup_tab3_gt)

gtsave(sup_tab3_gt, "Figuras/Supplementary_Table_3.rtf")

# ==============================================================================
# 22. EXPORTAR TABLAS PRINCIPALES
# ==============================================================================

tab1 %>%
  gtsummary::as_flex_table() %>%
  flextable::save_as_docx(path = "Figuras/Tabla_1.docx")

tab_incidencia %>%
  gtsave(filename = "Figuras/Tabla_Incidencia_Acumulada.rtf")

gt_diagnostics %>%
  gtsave(filename = "Figuras/Supplementary_Table_Schoenfeld_HardMACE.rtf")


cat("\nTodas las tablas exportadas a la carpeta 'Figuras'.\n")