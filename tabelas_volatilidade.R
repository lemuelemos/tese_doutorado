# Renda Fixa --------------------------------------------------------------

diferenca_vol_rf_30_dias |> 
  filter(lubridate::month(DT_COMPTC) == 6) |> 
  summarise(Quantidade = n(),
            `30 Dias Antes` = median(`30 Dias Antes`,na.rm = T),
            `30 Dias Depois` =  median(`30 Dias Depois`,na.rm = T),
            .by = c("POLIT_INVEST","EXISTE_TAXA_PERFM")) |>
  arrange(POLIT_INVEST) |> 
  mutate(`Diferença` = `30 Dias Antes` - `30 Dias Depois`)




diferenca_vol_rf_30_dias |> 
  filter(lubridate::month(DT_COMPTC) == 12) |> 
  summarise(Quantidade = n(),
            `30 Dias Antes` = median(`30 Dias Antes`,na.rm = T),
            `30 Dias Depois` =  median(`30 Dias Depois`,na.rm = T),
            .by = c("POLIT_INVEST","EXISTE_TAXA_PERFM")) |> 
  arrange(POLIT_INVEST) |> 
  mutate(`Diferença` = `30 Dias Antes` - `30 Dias Depois`)

#####################################

bind_rows(diferenca_rf_teste_30dias_ambos,
          diferenca_rf_teste_30dias_cp,
          diferenca_rf_teste_30dias_sp) |> 
  mutate(Mês = factor(Mês,levels = c("janeiro",
                                     "fevereiro",
                                     "março",
                                     "abril",
                                     "maio",
                                     "junho",
                                     "julho",
                                     "agosto",
                                     "setembro",
                                     "outubro",
                                     "novembro",
                                     "dezembro"))) |> 
  arrange(Mês) |> 
  kbl(caption = "Diferença de Volatilidade - Renda Fixa 30 dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T,
      linesep = linesep(c(3,3,3,3,3,3,3,3,3,3,3,3))) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))


bind_rows(diferenca_rf_teste_60dias_ambos,
          diferenca_rf_teste_60dias_cp,
          diferenca_rf_teste_60dias_sp) |> 
  mutate(Mês = factor(Mês,levels = c("janeiro",
                                     "fevereiro",
                                     "março",
                                     "abril",
                                     "maio",
                                     "junho",
                                     "julho",
                                     "agosto",
                                     "setembro",
                                     "outubro",
                                     "novembro",
                                     "dezembro"))) |> 
  arrange(Mês) |> 
  kbl(caption = "Diferença de Volatilidade - Renda Fixa 60 dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T,
      linesep = linesep(c(3,3,3,3,3,3,3,3,3,3,3,3))) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))



lapply(plots_rf_30_quartil,\(x){
  extract_stats(x[[1]])$subtitle_data  |> 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = str_extract(x$patches$annotation$title,"(?<=: ).*"),
           `Quartil` = x[[1]]$labels$title) %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) -> quartil1
  
  extract_stats(x[[2]])$subtitle_data  |> 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = str_extract(x$patches$annotation$title,"(?<=: ).*"),
           `Quartil` = x[[2]]$labels$title) %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) -> quartil2
  
  bind_rows(quartil1,
            quartil2)
}) |> 
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) |>
  relocate(Mês:`Tamanho da Diferença`) |> 
  mutate(Mês = factor(Mês,levels = c("janeiro",
                                     "fevereiro",
                                     "março",
                                     "abril",
                                     "maio",
                                     "junho",
                                     "julho",
                                     "agosto",
                                     "setembro",
                                     "outubro",
                                     "novembro",
                                     "dezembro"))) |> 
  arrange(Mês) |> 
  kbl(caption = "Diferença de Volatilidade - Renda Fixa 30 dias - Quartis",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T,
      linesep = linesep(c(2,2,2,2,2,2,2,2,2,2,2,2))) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))


lapply(plots_rf_60_quartil,\(x){
  extract_stats(x[[1]])$subtitle_data  |> 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = str_extract(x$patches$annotation$title,"(?<=: ).*"),
           `Quartil` = x[[1]]$labels$title) %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) -> quartil1
  
  extract_stats(x[[2]])$subtitle_data  |> 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = str_extract(x$patches$annotation$title,"(?<=: ).*"),
           `Quartil` = x[[2]]$labels$title) %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) -> quartil2
  
  bind_rows(quartil1,
            quartil2)
}) |> 
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) |>
  relocate(Mês:`Tamanho da Diferença`) |> 
  mutate(Mês = factor(Mês,levels = c("janeiro",
                                     "fevereiro",
                                     "março",
                                     "abril",
                                     "maio",
                                     "junho",
                                     "julho",
                                     "agosto",
                                     "setembro",
                                     "outubro",
                                     "novembro",
                                     "dezembro"))) |> 
  arrange(Mês) |> 
  kbl(caption = "Diferença de Volatilidade - Renda Fixa 60 dias - Quartis",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T,
      linesep = linesep(c(2,2,2,2,2,2,2,2,2,2,2,2))) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))

# Multimercado ------------------------------------------------------------

bind_rows(diferenca_mm_teste_30dias_ambos,
          diferenca_mm_teste_30dias_cp,
          diferenca_mm_teste_30dias_sp) |> 
  mutate(Mês = factor(Mês,levels = c("janeiro",
                                     "fevereiro",
                                     "março",
                                     "abril",
                                     "maio",
                                     "junho",
                                     "julho",
                                     "agosto",
                                     "setembro",
                                     "outubro",
                                     "novembro",
                                     "dezembro"))) |> 
  arrange(Mês) |> 
  kbl(caption = "Diferença de Volatilidade - Multimercados 30 dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T,
      linesep = linesep(c(3,3,3,3,3,3,3,3,3,3,3,3))) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))



bind_rows(diferenca_mm_teste_60dias_ambos,
          diferenca_mm_teste_60dias_cp,
          diferenca_mm_teste_60dias_sp) |> 
  mutate(Mês = factor(Mês,levels = c("janeiro",
                                     "fevereiro",
                                     "março",
                                     "abril",
                                     "maio",
                                     "junho",
                                     "julho",
                                     "agosto",
                                     "setembro",
                                     "outubro",
                                     "novembro",
                                     "dezembro"))) |> 
  arrange(Mês) |> 
  kbl(caption = "Diferença de Volatilidade - Multimercados 60 dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T,
      linesep = linesep(c(3,3,3,3,3,3,3,3,3,3,3,3))) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))


lapply(plots_mm_30_quartil,\(x){
  extract_stats(x[[1]])$subtitle_data  |> 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = str_extract(x$patches$annotation$title,"(?<=: ).*"),
           `Quartil` = x[[1]]$labels$title) %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) -> quartil1
  
  extract_stats(x[[2]])$subtitle_data  |> 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = str_extract(x$patches$annotation$title,"(?<=: ).*"),
           `Quartil` = x[[2]]$labels$title) %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) -> quartil2
  
  bind_rows(quartil1,
            quartil2)
}) |> 
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) |>
  relocate(Mês:`Tamanho da Diferença`) |> 
  mutate(Mês = factor(Mês,levels = c("janeiro",
                                     "fevereiro",
                                     "março",
                                     "abril",
                                     "maio",
                                     "junho",
                                     "julho",
                                     "agosto",
                                     "setembro",
                                     "outubro",
                                     "novembro",
                                     "dezembro"))) |> 
  arrange(Mês) |> 
  kbl(caption = "Diferença de Volatilidade - Multimercado 30 dias - Quartis",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T,
      linesep = linesep(c(2,2,2,2,2,2,2,2,2,2,2,2))) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))


lapply(plots_mm_60_quartil,\(x){
  extract_stats(x[[1]])$subtitle_data  |> 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = str_extract(x$patches$annotation$title,"(?<=: ).*"),
           `Quartil` = x[[1]]$labels$title) %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) -> quartil1
  
  extract_stats(x[[2]])$subtitle_data  |> 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = str_extract(x$patches$annotation$title,"(?<=: ).*"),
           `Quartil` = x[[2]]$labels$title) %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) -> quartil2
  
  bind_rows(quartil1,
            quartil2)
}) |> 
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) |>
  relocate(Mês:`Tamanho da Diferença`) |> 
  mutate(Mês = factor(Mês,levels = c("janeiro",
                                     "fevereiro",
                                     "março",
                                     "abril",
                                     "maio",
                                     "junho",
                                     "julho",
                                     "agosto",
                                     "setembro",
                                     "outubro",
                                     "novembro",
                                     "dezembro"))) |> 
  arrange(Mês) |> 
  kbl(caption = "Diferença de Volatilidade - Multimercado 60 dias - Quartis",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T,
      linesep = linesep(c(2,2,2,2,2,2,2,2,2,2,2,2))) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))

# Ações -------------------------------------------------------------------

bind_rows(diferenca_mm_teste_30dias_ambos,
          diferenca_mm_teste_30dias_cp,
          diferenca_mm_teste_30dias_sp) |> 
  mutate(Mês = factor(Mês,levels = c("janeiro",
                                     "fevereiro",
                                     "março",
                                     "abril",
                                     "maio",
                                     "junho",
                                     "julho",
                                     "agosto",
                                     "setembro",
                                     "outubro",
                                     "novembro",
                                     "dezembro"))) |> 
  arrange(Mês) |> 
  kbl(caption = "Diferença de Volatilidade - Ações 30 dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T,
      linesep = linesep(c(3,3,3,3,3,3,3,3,3,3,3,3))) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))



bind_rows(diferenca_mm_teste_60dias_ambos,
          diferenca_mm_teste_60dias_cp,
          diferenca_mm_teste_60dias_sp) |> 
  mutate(Mês = factor(Mês,levels = c("janeiro",
                                     "fevereiro",
                                     "março",
                                     "abril",
                                     "maio",
                                     "junho",
                                     "julho",
                                     "agosto",
                                     "setembro",
                                     "outubro",
                                     "novembro",
                                     "dezembro"))) |> 
  arrange(Mês) |> 
  kbl(caption = "Diferença de Volatilidade - Ações 60 dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T,
      linesep = linesep(c(3,3,3,3,3,3,3,3,3,3,3,3))) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))


lapply(plots_acoes_30_quartil,\(x){
  extract_stats(x[[1]])$subtitle_data  |> 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = str_extract(x$patches$annotation$title,"(?<=: ).*"),
           `Quartil` = x[[1]]$labels$title) %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) -> quartil1
  
  extract_stats(x[[2]])$subtitle_data  |> 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = str_extract(x$patches$annotation$title,"(?<=: ).*"),
           `Quartil` = x[[2]]$labels$title) %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) -> quartil2
  
  bind_rows(quartil1,
            quartil2)
}) |> 
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) |>
  relocate(Mês:`Tamanho da Diferença`) |> 
  mutate(Mês = factor(Mês,levels = c("janeiro",
                                     "fevereiro",
                                     "março",
                                     "abril",
                                     "maio",
                                     "junho",
                                     "julho",
                                     "agosto",
                                     "setembro",
                                     "outubro",
                                     "novembro",
                                     "dezembro"))) |> 
  arrange(Mês) |> 
  kbl(caption = "Diferença de Volatilidade - Ações 30 dias - Quartis",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T,
      linesep = linesep(c(2,2,2,2,2,2,2,2,2,2,2,2))) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))

lapply(plots_acoes_60_quartil,\(x){
  extract_stats(x[[1]])$subtitle_data  |> 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = str_extract(x$patches$annotation$title,"(?<=: ).*"),
           `Quartil` = x[[1]]$labels$title) %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) -> quartil1
  
  extract_stats(x[[2]])$subtitle_data  |> 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = str_extract(x$patches$annotation$title,"(?<=: ).*"),
           `Quartil` = x[[2]]$labels$title) %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) -> quartil2
  
  bind_rows(quartil1,
            quartil2)
}) |> 
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) |>
  relocate(Mês:`Tamanho da Diferença`) |> 
  mutate(Mês = factor(Mês,levels = c("janeiro",
                                     "fevereiro",
                                     "março",
                                     "abril",
                                     "maio",
                                     "junho",
                                     "julho",
                                     "agosto",
                                     "setembro",
                                     "outubro",
                                     "novembro",
                                     "dezembro"))) |> 
  arrange(Mês) |> 
  kbl(caption = "Diferença de Volatilidade - Ações 60 dias - Quartis",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T,
      linesep = linesep(c(2,2,2,2,2,2,2,2,2,2,2,2))) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))



bind_rows(diferenca_vol_rf_30_dias,
          diferenca_vol_multimercado_30_dias,
          diferenca_vol_acoes_30_dias) |>
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         CALC_TAXA_PERFM) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  distinct_all() |> 
  count(POLIT_INVEST,
        PUBLICO_ALVO,
        EXISTE_TAXA_PERFM,
        CALC_TAXA_PERFM) |> 
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não"),
         CALC_TAXA_PERFM = case_when(is.na(CALC_TAXA_PERFM) & EXISTE_TAXA_PERFM == "Sim" ~ "NÃO INFORMA",
                                     EXISTE_TAXA_PERFM == "Não" ~ "--",
                                     .default = CALC_TAXA_PERFM)) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM,
         `Método de Cálculo` = CALC_TAXA_PERFM,
         `Público Alvo` = PUBLICO_ALVO,
         Quantidade = n) |> 
  kbl(caption = "Fundos de Investimentos - Distribuição quanto a política, público alvo,taxa e método do calculo da performace",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down")) |> 
  footnote(general = "Dados do portal de dados aberto da CVM. Elaboração Própria.",
           general_title = "Fonte:",
           footnote_as_chunk = T)



bind_rows(diferenca_vol_rf_30_dias,
          diferenca_vol_multimercado_30_dias,
          diferenca_vol_acoes_30_dias) |>
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         CALC_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM,
           CALC_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`,
         CALC_TAXA_PERFM = case_when(is.na(CALC_TAXA_PERFM) ~ "NÃO SE APLICA",
                                     T ~ CALC_TAXA_PERFM)) |> 
  left_join(
    bind_rows(diferenca_vol_rf_30_dias,
              diferenca_vol_multimercado_30_dias,
              diferenca_vol_acoes_30_dias) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM,CALC_TAXA_PERFM) |> 
      summarise(Quantidade = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM",
                        "CALC_TAXA_PERFM")) |> 
      mutate(CALC_TAXA_PERFM = case_when(is.na(CALC_TAXA_PERFM) ~ "NÃO SE APLICA",
                                         T ~ CALC_TAXA_PERFM))
  ) |> 
  relocate(CALC_TAXA_PERFM:Quantidade,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM,
         `Método de Cálculo` = CALC_TAXA_PERFM) |>  
  kbl(caption = "Diferença Volatilidade - Renda Fixa - 30 Dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))


tibble(`p-valor` = c("p < 0,0001","0,001 <= p < 0,01","0,01 <= p < 0,01","0,05 <= p < 0,1","p>=0,1"),
       `Conclusão` = c("Muito forte evidencia contra a hipótese nula em favor da alternativa",
                       "Forte evidencia contra a hipótese nula em favor da alternativa",
                       "Evidência moderada contra a hipótese nula em favor da alternativa",
                       "Baixa evidência contra a hipótese nula em favor da alternativa",
                       "Falta de evidência contra a hipótese nula: consistente com a hipótese nula")) |> 
  kbl(caption = "Tabela referênca de significância para o teste de Wilcoxon Rank Sum Test",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))







