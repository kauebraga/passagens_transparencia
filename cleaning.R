library(portransp)
library(data.table)
library(dplyr)
library(lubridate)

potr_download(opendata = "viagens", reference = 2019, destfile = ("."))


viagens_teste <- fread("viagens_2015/2015_Trecho.csv", nrows = 1000)

viagens <- fread("viagens_2015/2015_Trecho.csv",
                 select = c("Identificador do processo de viagem ", 
                            "Origem - País",
                            "Origem - Cidade", 
                            "Destino - País",
                            "Destino - Cidade",
                            "Meio de transporte",
                            "Origem - Data"))


colnames(viagens) <- c("id", "origem_pais", "origem_cidade", 
                       "destino_pais", "destino_cidade", "meio_transporte",
                       "origem_data")


# para 2019
viagens_china <- viagens[destino_pais == "China" | origem_pais == "China"]
# para 2015
viagens_china <- viagens[destino_pais == "Coréia do Sul" | origem_pais == "Coréia do Sul"]


# para 2019
viagens_wuhan <- viagens_china[destino_cidade == "Wuhan" | origem_cidade == "Wuhan"]
# para 2015
viagens_wuhan <- viagens_china[destino_cidade == "Mungyeong" | origem_cidade == "Mungyeong"]
viagens_wuhan <- viagens_china[destino_cidade == "Incheon" | origem_cidade == "Incheon"]


# format date
viagens_wuhan <- viagens_wuhan[, origem_data := as.POSIXct(origem_data, format = "%d/%m/%Y")]
viagens_wuhan <- viagens_wuhan[, mes := month(origem_data)]
viagens_wuhan <- viagens_wuhan[mes %in% c(9, 10, 11)]

ids_wuhan <- unique(viagens_wuhan$id)

# viagens que passagram por wuihan
viagens_wuhan_v2 <- viagens[id %in% ids_wuhan]




# PAGAMENTOS ----------------------------------------------------------------------------------
pagamentos_teste <- fread("viagens_2019/2019_Pagamento.csv", nrows = 1000)



pagamento <- fread("viagens_2015/2015_Pagamento.csv", 
                   select = c("Identificador do processo de viagem", 
                              "Tipo de pagamento",
                              "Valor",
                              "Nome do órgão superior"))

colnames(pagamento) <- c("id", "tipo_pagamento", "valor", "orgao")

# FILTRAR PAGAMENTOS DE VIAGENS DE WUHAN
pagamentos_wuhan <- pagamento[id %in% ids_wuhan]
pagamentos_wuhan[, valor := gsub(",", ".", valor)]
pagamentos_wuhan[, valor := as.numeric(valor)]


pagamento_sum <- pagamentos_wuhan %>% group_by(tipo_pagamento) %>% summarise(sum = sum(valor))
pagamentos_id_sum <- pagamentos_wuhan %>% group_by(id, orgao) %>% summarise(sum = sum(valor))
pagamentos_wuhan %>% count(orgao)


a <- fread("viagens_2019/2019_Passagem.csv", nrow = 100)
colnames(b)


# PEGAR SO EXERCITO ---------------------------------------------------------------------------

viagens_exercito <- left_join(viagens_wuhan_v2, pagamentos_id_sum, by = "id")

teste <- viagens_exercito %>% group_by(id, orgao) %>% summarise(valor1 = first(sum))

teste_fim <- teste %>% group_by(orgao) %>% summarise(valor2 = sum(valor1))
