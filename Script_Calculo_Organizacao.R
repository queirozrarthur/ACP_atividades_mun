## Preparação dos Dados

# Definir o diretório
setwd('C:\\Users\\queir\\OneDrive\\Documents\\Mestrado\\Segundo Período\\MARU\\Trabalho Final\\ACP_atividades_mun')

# Carregar os pacotes
library(openxlsx)
library(reshape2)
library(dplyr)
library(readstata13)
library(foreign)
library(haven)
library(tidyr)
library(tidyverse)
library(EconGeo)

# Carregar a base de Empregos de 2018
data <- read.csv2('Emp_2018.csv', check.names = F) %>% 
  rename(municipio = Município) %>% 
  melt(id.vars=c("municipio")) %>% 
  rename(cnae_id = variable, emp = value) %>% 
  mutate(year = 2018)

TempoRef <- 2018 # Nesse caso, consideraremos apenas um ano: 2018.

# Calcular os indicadores de complexidade:

# RCA
dados_emp_a <- subset(data[data$year == TempoRef,], select = c(1,2,3))
mat_emp_a <- get.matrix(dados_emp_a, sparse = F)
VCR_binary_a <- RCA(mat_emp_a, binary = T)
VCR_a <- RCA(mat_emp_a, binary = F)

# ECI
autovetorK <- KCI(VCR_binary_a, RCA=F) 
eci <- (autovetorK - mean(autovetorK)) / (sd(autovetorK)) 
names(eci) <- rownames(VCR_binary_a) 

# PCI
autovetorQ <- TCI(VCR_binary_a, RCA=F) 
pci <- (autovetorQ - mean(autovetorQ)) / (sd(autovetorQ))
names(pci) <- rownames(t(VCR_binary_a))

# Ocorrência
coocorrencia <- co.occurrence(t(VCR_binary_a), diagonal=F) 
coocorrencia[lower.tri(coocorrencia, diag=F)] <- t(coocorrencia)[lower.tri(t(coocorrencia), diag=F)]

# Proximidade
proximidade <- relatedness(coocorrencia)/100 

# Densidade 
densidade <- relatedness.density(VCR_binary_a, relatedness(coocorrencia))/100 

# IGO
rel <- (1-VCR_binary_a) %*% relatedness(coocorrencia) 
reltot <- colSums(relatedness(coocorrencia)) 
reldens <- t(rel) / reltot 
reldens <- t(reldens) * 100 
distancia <- round(reldens, digits = 3)/100 

rel <- t(((1-VCR_binary_a) %*% proximidade)) * pci 
reltot <- colSums(proximidade)
reldens <- rel / reltot 
relnew <- (1-t(distancia)) * pci 
gain <- reldens - relnew 
gain <- t(gain)

# Juntar todos os valores em uma tabela
TabEmp <- merge(rownames(mat_emp_a),colnames(mat_emp_a), all.x=T,all.y=T)
TabEmp <- cbind(TabEmp,as.vector(mat_emp_a)) # Coluna do Emprego
TabVCR <- merge(rownames(VCR_a),colnames(VCR_a), all.x=T,all.y=T)
TabVCR <- cbind(TabVCR, as.vector(VCR_a)) # Coluna da RCA
TabDens <- merge(rownames(densidade),colnames(densidade), all.x=T,all.y=T)
TabDens <- cbind(TabDens, as.vector(densidade)) # Coluna da Densidade
TabIGO <- merge(rownames(gain),colnames(gain), all.x=T,all.y=T) 
TabIGO <- cbind(TabIGO, as.vector(gain)) # Coluna do IGO
TabAll <- cbind(names(eci),eci)
colnames(TabAll) <- c("ibge_id", "eci")
TabAll <- as.data.frame(TabAll)
TabAll$eci <- as.numeric(as.vector(TabAll$eci)) # Incluir ECI
TabAll$ibge_id <- as.character(TabAll$ibge_id) 
TabPCI <- cbind(names(pci),pci)
TabAll <- merge(TabAll, TabPCI, all.x=T,all.y=T) # Incluir PCI
colnames(TabAll) <- c("ibge_id", "eci", "cnae_id","pci")
TabAll$pci <- as.numeric(as.vector(TabAll$pci))
TabAll$cnae_id <- as.character(TabAll$cnae_id)
TabAll <- merge(TabAll, TabEmp, by.x = c(1,3), by.y=c(1,2)) # Juntar as colunas de Emprego
TabAll <- merge(TabAll, TabVCR, by.x = c(1,2), by.y=c(1,2)) # Juntar as colunas de RCA
TabAll <- merge(TabAll, TabDens, by.x = c(1,2), by.y=c(1,2)) # Juntar as colunas de Density
TabAll <- merge(TabAll, TabIGO, by.x = c(1,2), by.y=c(1,2)) # Juntar as colunas de IGO
colnames(TabAll) <- c("ibge_id", "cnae_id","eci", "pci","num_emp","vcr","density", "opp_gain")
TabAll$year <- TempoRef # Incluir coluna para o ano

# Carregar as bases com nome das atividades e dos municípios

# Atividades
tab_cnae <- read.csv2("cnae_id.csv", check.names = F)
tab_cnae <- tab_cnae %>% 
  mutate(cnae_id = as.character(cnae_id))

#Municípios
tab_mun <- read.xlsx("mun_id.xlsx")
tab_mun <- tab_mun %>% 
  mutate(ibge_id = as.character(ibge_id))

# Incluir os nome na base com os indicadores de complexidade
base_trabalho <- TabAll %>% 
  mutate(cnae_id = as.character(cnae_id),
         ibge_id = as.character(ibge_id)) %>% 
  left_join(tab_cnae, by = c('cnae_id')) %>% 
  left_join(tab_mun, by = c('ibge_id')) %>% 
  select(c(uf_id, uf_name, mun_id, ibge_id, mun_name, cnae_id, cnae_name,
           year, num_emp, everything())) %>% 
  mutate(uf_id = as.character(uf_id),
         mun_id = as.character(mun_id))

# Tendo calculado os indicadores de complexidade, o próximos passo é
# calcular indicadores de desenvolvimento (PHDI), de desigualdade (PGI),
# e de emissão de poluentes (PEII) para cada uma das atividades. Para calcular
# tais indicadores, é preciso antes ter a matriz S, definida como a matriz
# de participação das atividades em cada um dos municípios.

# Matriz S
part_s <- TabAll %>% 
  select(-c(eci, pci, vcr, density, opp_gain)) %>% 
  group_by(ibge_id) %>% 
  mutate(emp_mun = sum(num_emp)) %>% 
  ungroup() %>% 
  mutate(part = num_emp/emp_mun) %>% 
  select(-c(num_emp, year, emp_mun)) %>% 
  spread(cnae_id, part) %>% 
  arrange(ibge_id) %>% # Ordenar as linhas
  remove_rownames() %>% 
  column_to_rownames(var = 'ibge_id') %>% 
  select(colnames(VCR_binary_a)) # Ordenar as colunas

matrix_s <- as.matrix(part_s) 

# Pela Matriz S, conseguimos o valor de Np
N_p <- VCR_binary_a * matrix_s 

# Temos, portanto, o denominador dos índices
denominat <- colSums(N_p)

# Product Human Development Index (PHDI)
tab_idh <- read.xlsx('IDHM_2010.xlsx')  
idh_c <- tab_idh %>% 
  select(c(ibge_id, idhm)) %>%
  arrange(ibge_id) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'ibge_id') %>% 
  as.matrix()

numerat <- t(N_p) %*% idh_c
idh_index <- numerat / denominat

idh_index_df <- idh_index %>%
  as.data.frame() %>% 
  rename(idh_index = idhm) %>%
  rownames_to_column(var='cnae_id') %>% 
  left_join(tab_cnae, by = c('cnae_id')) %>% 
  select(c(cnae_id, cnae_name, idh_index)) %>% 
  rename(PHDI = idh_index)

# Product Emission Intensity Index (PEII)
tab_emission <- read.xlsx('Emission_PerCapita.xlsx')
emission_c <- tab_emission %>% 
  filter(year==2018) %>% 
  select(c(ibge_id, emission_pc)) %>%
  mutate(emission_pc = as.numeric(emission_pc)) %>% 
  arrange(ibge_id) %>%
  remove_rownames() %>% 
  column_to_rownames(var = 'ibge_id') %>% 
  as.matrix()

numerat <- t(N_p) %*% emission_c  
emission_index <- numerat / denominat

emission_index_df <- emission_index %>%
  as.data.frame() %>% 
  rename(emission_index = emission_pc) %>%
  rownames_to_column(var='cnae_id') %>% 
  left_join(tab_cnae, by = c('cnae_id')) %>% 
  select(c(cnae_id, cnae_name, emission_index)) %>% 
  rename(PEII = emission_index)

# Product Gini Index (PGI)
tab_gini <- read.csv2('Gini_2018.csv', check.names = F,
                      encoding = 'UTF-8')
gini_c <- tab_gini %>% 
  select(c(ibge_id, gini)) %>%
  arrange(ibge_id) %>%
  remove_rownames() %>% 
  column_to_rownames(var = 'ibge_id') %>% 
  as.matrix()

numerat <- t(N_p) %*% gini_c  
gini_index <- numerat / denominat

gini_index_df <- gini_index %>%
  as.data.frame() %>% 
  rename(gini_index = gini) %>%
  rownames_to_column(var='cnae_id') %>% 
  left_join(tab_cnae, by = c('cnae_id')) %>% 
  select(c(cnae_id, cnae_name, gini_index)) %>% 
  rename(PGI = gini_index)

# Product Complexity Index
# Aqui vamos somente separar o PCI
pci_df <- base_trabalho %>% 
  select(c(cnae_id, cnae_name, pci)) %>% 
  unique() %>% 
  rename(PCI = pci)

# Juntar todas as bases
tab_final <- idh_index_df %>% 
  left_join(emission_index_df, by = c('cnae_id', 'cnae_name')) %>% 
  left_join(gini_index_df, by = c('cnae_id', 'cnae_name')) %>% 
  left_join(pci_df, by = c('cnae_id', 'cnae_name')) 

write.xlsx(tab_final, 'Products_index.xlsx')

# Realizar o mesmo processo para obter a base sob a ótica dos municípios

# Economic Complex Index 
eci_df <- base_trabalho %>% 
  select(mun_id, ibge_id, mun_name, eci) %>%
  mutate(mun_id = as.character(mun_id),
         ibge_id = as.character(ibge_id)) %>% 
  unique() 

# Human Development Index
tab_idh <- tab_idh %>% 
  mutate(mun_id = as.character(mun_id),
         ibge_id = as.character(ibge_id)) %>% 
  select(ibge_id, idhm)

# Emission Per Capita
tab_emission <- tab_emission %>% 
  mutate(ibge_id = as.character(ibge_id)) %>% 
  filter(year==2018) %>% 
  select(ibge_id, emission_pc)

# Gini Index
tab_gini <- tab_gini %>% 
  mutate(ibge_id = as.character(ibge_id)) %>% 
  select(ibge_id, gini)

# Juntar as bases
tab_mun <- eci_df %>% 
  left_join(tab_idh, by = c('ibge_id')) %>%  
  left_join(tab_emission, by = c('ibge_id')) %>% 
  left_join(tab_gini, by = c('ibge_id')) %>% 
  select(mun_id, ibge_id, mun_name, eci, idhm, emission_pc, gini) %>% 
  unique() %>% 
  rename(ECI = eci,
         IDH = idhm,
         E_pc = emission_pc,
         GINI = gini)

write.xlsx(tab_mun, 'Muns_index.xlsx')



