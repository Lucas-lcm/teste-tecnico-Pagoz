#Ambiente ----

caminho <- "F:/PAGOZ_Teste_Lucas-Cardoso/Resolucao R"

setwd(caminho)

library(data.table)
library(readxl)
library(dplyr)
library(stringr)
library(RSQLite)
library(plotly)

#Dados auxilaires ----

leitor <- data.table::fread(
  file = paste0(getwd(), "/Tabelas/leitor.csv"), sep = ",", encoding = "UTF-8"
) %>% 
  dplyr::rename("descricao_leitor" = "descricao")

head(leitor)

captura <- data.table::fread(
  file = paste0(getwd(), "/Tabelas/captura.csv"), sep = ",", encoding = "UTF-8"
) %>% 
  dplyr::rename("descricao_captura" = "descricao")

head(captura)

pagamento <- data.table::fread(
  file = paste0(getwd(), "/Tabelas/pagamento.csv"), encoding = "Latin-1"
) %>% 
  dplyr::rename("descricao_pagamento" = "descricao")

head(pagamento)

status_pagamento <- data.table::fread(
  file = paste0(getwd(), "/Tabelas/status_pagamento.csv"), encoding = "Latin-1"
) %>% 
  dplyr::rename("descricao_status_pagamento" = "descricao")

head(status_pagamento)

canal_entrada <- readxl::read_xls(
  path = paste0(getwd(), "/Tabelas/canal_entrada.xls")
) %>% 
  dplyr::rename("descricao_canal_entrada" = "descricao")

head(canal_entrada)

evento <- readxl::read_xlsx(
  path = paste0(getwd(), "/Tabelas/evento.xlsx")
) %>% 
  dplyr::rename("descricao_evento" = "descricao")

head(evento)

#Dado principal ----

transacoes <- data.table::fread(
  file = paste0(getwd(), "/Tabelas/transacoes.csv"), encoding = "Latin-1"
)

#Join entre as tabelas ----

transacoes <- transacoes %>% 
  dplyr::left_join(leitor, by = c("leitor" = "# leitor")) %>%  
  dplyr::left_join(captura, by = c("meio_captura" = "# meio_captura")) %>% 
  dplyr::left_join(pagamento, by = c("meio_pagamento" = "# meio_pagamento")) %>% 
  dplyr::left_join(status_pagamento, by = c("status_pagamento" = "# status_pagamento")) %>% 
  dplyr::left_join(canal_entrada, by = c("canal_entrada" = "# canal_entrada")) %>%
  dplyr::left_join(evento, by = c("tipo_evento" = "# tipo_evento"))

colnames(transacoes)

#Resolucoes ----

# Qual estabelecimento movimentou o cógido api CAOD9ND23H4B7ZCMGWXAN47Z8KEWAF5W?

questao_um <- transacoes %>% 
  subset(`# movimento_api_codigo` == "CAOD9ND23H4B7ZCMGWXAN47Z8KEWAF5W") %>% 
  dplyr::select(`# movimento_api_codigo`, estabelecimento) %>% 
  print()

# Quantas vendas em cartão de crédito foram realizadas entre a data 2022-01-18 e 2023-02-01?

questao_um_ <- transacoes %>% 
  dplyr::mutate(
    data_venda_ajuste_num = as.numeric( str_remove_all( data_venda_ajuste, "-" ) )
    ) %>% 
  subset(
    data_venda_ajuste_num > 20220117 & data_venda_ajuste_num < 20230131 & descricao_pagamento == "Cartão de Crédito"
  ) %>% 
  dplyr::count() %>% print()

# Quais são os clientes com maior valor de faturamento? (Identificados na coluna "estabelecimento")

questao_dois <- transacoes %>% 
  dplyr::group_by(estabelecimento) %>% 
  dplyr::mutate(faturamento_por_estabelecimento = sum(valor_total_transacao)) %>% 
  dplyr::select(estabelecimento, faturamento_por_estabelecimento) %>%
  dplyr::arrange(desc(faturamento_por_estabelecimento)) %>% 
  head() %>% print()

# Quantas máquinas cada cliente possui?

questao_tres <- transacoes %>% 
  dplyr::select(estabelecimento, descricao_leitor) %>% 
  unique() %>% 
  dplyr::group_by(estabelecimento) %>% 
  dplyr::mutate(
    n_maquininha = length(estabelecimento)
  ) %>% print()

questao_tres_ <- questao_tres %>% 
  dplyr::select(estabelecimento, n_maquininha) %>% 
  unique() %>% print()

questao_tres_ <- questao_tres_ %>% 
  dplyr::arrange(desc(n_maquininha)) %>% 
  head() %>% print()

# Qual o valor total em "eventos" negativos?

questao_quatro <- transacoes %>% 
  subset(sinal == "-") %>% 
  dplyr::select(estabelecimento, sinal, valor_total_transacao) %>% 
  dplyr::mutate(
    valor_total_eventos_negativos = sum( valor_total_transacao )
    ) %>% 
  print()

# Qual a porcentagem de vendas com "Leitor de chip e senha" em relação a "Venda pela Maquininha"? 
##Em número de transações e valor de faturamento total.

questao_cinco <- transacoes %>% 
  subset(
    descricao_canal_entrada == "Venda com leitor de chip e senha" |
    descricao_canal_entrada == "Venda pela Maquininha"
    ) %>% 
  dplyr::group_by(canal_entrada) %>% 
  dplyr::mutate(
    n_transacoes = length( descricao_canal_entrada ),
    sum_faturamento = sum( valor_total_transacao )
  ) %>% 
  dplyr::select(descricao_canal_entrada, n_transacoes, sum_faturamento) %>% 
  unique() %>% dplyr::ungroup() %>% print()

questao_cinco_ <- questao_cinco %>% 
  dplyr::mutate(
    percent_num_transacoes = n_transacoes[descricao_canal_entrada == "Venda com leitor de chip e senha"]/n_transacoes[descricao_canal_entrada == "Venda pela Maquininha"]*100,
    percent_total_vendas = sum_faturamento[descricao_canal_entrada == "Venda com leitor de chip e senha"]/sum_faturamento[descricao_canal_entrada == "Venda pela Maquininha"]*100
  ) %>% 
  dplyr::select(percent_num_transacoes, percent_total_vendas) %>% 
  unique() %>% print()

# Precisamos de um gráfico que demonstre o faturamento total por parcelas.

questao_seis <- transacoes %>% 
  dplyr::select(quantidade_parcela, valor_total_transacao) %>% 
  dplyr::group_by(quantidade_parcela) %>% 
  dplyr::mutate(
    valor_por_parcela = sum(valor_total_transacao)
  ) %>% 
  dplyr::select(quantidade_parcela, valor_por_parcela) %>% 
  dplyr::ungroup() %>% unique() %>% 
  dplyr::arrange(quantidade_parcela) %>% 
  print()

plot_faturameto_parcela <- questao_seis %>% 
  plotly::plot_ly(
    x = ~quantidade_parcela,
    y = ~valor_por_parcela,
    type = "scatter",
    mode = "lines+markers"
  )

plot_faturameto_parcela

# Gráfico com a razão de transações débito, crédito e parcelado.

questao_sete <- transacoes %>% 
  dplyr::mutate(
    transacoes_debito = nrow(transacoes[meio_pagamento == 8 & quantidade_parcela  < 2]),
    transacoes_credito = nrow(transacoes[meio_pagamento == 3 & quantidade_parcela  < 2]),
    transacoes_parcelado = nrow(transacoes[quantidade_parcela  > 1]),
    total_transacoes = nrow(transacoes)
  ) %>% 
  select(transacoes_debito, transacoes_credito, transacoes_parcelado, total_transacoes) %>% 
  unique() %>% print()

questao_sete_ <- data.frame(
  tipo_transacao = c(
    "debito", 
    "credito", 
    "parcelado"
  ),
  razao = c(
    questao_sete$transacoes_debito/questao_sete$total_transacoes, 
    questao_sete$transacoes_credito/questao_sete$total_transacoes, 
    questao_sete$transacoes_parcelado/questao_sete$total_transacoes
  )
) %>% print()

plot_razao_tipo_transacao <- questao_sete_ %>% 
  plotly::plot_ly(
    labels = ~tipo_transacao,
    values= ~razao,
    type = "pie"
  )

plot_razao_tipo_transacao
