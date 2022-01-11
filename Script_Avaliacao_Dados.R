# Avaliação dos Dados

# Definir o diretório
setwd('C:\\Users\\queir\\OneDrive\\Documents\\Mestrado\\Segundo Período\\MARU\\Trabalho Final\\ACP_atividades_mun')

# Carregar pacotes
library(openxlsx)
library(reshape2)
library(dplyr)
library(readstata13)
library(foreign)
library(haven)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

# Aqui plotaremos alguns gráficos para avaliar a correlação (e não a causalidade)
# entre os indicadores de complexidade e os demais. Faremos para os produtos
# (atividades CNAE) e para os municípios.

# Para as atividades:

# Carregar a base
tab_products <- read.xlsx('Products_index.xlsx')

# Gráfico PCI x PHDI  
tiff('PCIxPHDI.png', units="in", width=8, height=6, res=300)
ggplot(tab_products, aes(x=PHDI, y=PCI)) +
  geom_point(color='black')+
  geom_smooth(method=lm, color='red', formula = y ~ x) +
  xlab('Product Human Development Index (PHDI)') +
  ylab('Product Complexity Index (PCI)') +
  ggtitle('PCI x PHDI') +
  theme_classic() +
  theme(
    axis.title.x = element_text(color='black', size=12, vjust=-3),
    axis.title.y = element_text(color='black', size=12, vjust=3),
    plot.title = element_text(color='black', size=12, 
                              face='bold', hjust = 0.5,vjust=4),
    plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  stat_cor(aes(label = paste(..rr.label..)),
           size = 3.5, label.x = 0.74, label.y = -3.4) +
  stat_cor(aes(label = paste(..p.label..)),
           size = 3.5, label.x = 0.74, label.y = -3.8) 
dev.off()

# Gráfico PCI x PGI
tiff('PCIxPGI.png', units="in", width=8, height=6, res=300)
ggplot(tab_products, aes(x=PGI, y=PCI)) +
  geom_point(color='black')+
  geom_smooth(method=lm, color='red', formula = y ~ x) +
  xlab('Product Gini Index (PGI)') +
  ylab('Product Complexity Index (PCI)') +
  ggtitle('PCI x PGI') +
  theme_classic() +
  theme(
    axis.title.x = element_text(color='black', size=12, vjust=-3),
    axis.title.y = element_text(color='black', size=12, vjust=3),
    plot.title = element_text(color='black', size=12, 
                              face='bold', hjust = 0.5,vjust=4),
    plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  stat_cor(aes(label = paste(..rr.label..)),
           size = 3.5, label.x = 0.4, label.y = -3.4) +
  stat_cor(aes(label = paste(..p.label..)),
           size = 3.5, label.x = 0.4, label.y = -3.8) 
dev.off()

# Gráfico PCI x PEII
tiff('PCIxPEII.png', units="in", width=8, height=6, res=300)
ggplot(tab_products, aes(x=PEII, y=PCI)) +
  geom_point(color='black')+
  geom_smooth(method=lm, color='red', formula = y ~ x) +
  xlab('Product Emission Intensity Index (PEII)') +
  ylab('Product Complexity Index (PCI)') +
  ggtitle('PCI x PEII') +
  theme_classic() +
  theme(
    axis.title.x = element_text(color='black', size=12, vjust=-3),
    axis.title.y = element_text(color='black', size=12, vjust=3),
    plot.title = element_text(color='black', size=12, 
                              face='bold', hjust = 0.5,vjust=4),
    plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  stat_cor(aes(label = paste(..rr.label..)),
           size = 3.5, label.x = 4, label.y = -3.4) +
  stat_cor(aes(label = paste(..p.label..)),
           size = 3.5, label.x = 4, label.y = -3.8) 
dev.off()

# Para os Municípios:

# Carregar a base:
tab_mun <- read.xlsx('Muns_index.xlsx')

# Gráfico ECI x HDI
tiff('ECIxHDI.png', units="in", width=8, height=6, res=300)
ggplot(tab_mun, aes(x=IDH, y=ECI)) +
  geom_point(color='gray')+
  geom_smooth(method=lm, color='black', formula = y ~ x) +
  xlab('Human Development Index (HDI)') +
  ylab('Economic Complexity Index (ECI)') +
  ggtitle('ECI x HDI') +
  theme_classic() +
  theme(
    axis.title.x = element_text(color='black', size=12, vjust=-3),
    axis.title.y = element_text(color='black', size=12, vjust=3),
    plot.title = element_text(color='black', size=12, 
                              face='bold', hjust = 0.5,vjust=4),
    plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  stat_cor(aes(label = paste(..rr.label..)),
           size = 3.5, label.x = 0.8, label.y = -2) +
  stat_cor(aes(label = paste(..p.label..)),
           size = 3.5, label.x = 0.8, label.y = -2.2) 
dev.off()

# Gráfico ECI x Ln(Emission)
Emission_ln <- tab_mun %>% 
  mutate(ln_emission = log(E_pc)) 

tiff('ECIxEmission.png', units="in", width=8, height=6, res=300)
ggplot(Emission_ln, aes(x=ln_emission, y=ECI)) +
  geom_point(color='gray')+
  geom_smooth(method=lm, color='black', formula = y ~ x) +
  xlab('Ln of Emission (CO2e)') +
  ylab('Economic Complexity Index (ECI)') +
  ggtitle('ECI x Ln(Emission)') +
  theme_classic() +
  theme(
    axis.title.x = element_text(color='black', size=12, vjust=-3),
    axis.title.y = element_text(color='black', size=12, vjust=3),
    plot.title = element_text(color='black', size=12, 
                              face='bold', hjust = 0.5,vjust=4),
    plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  stat_cor(aes(label = paste(..rr.label..)),
           size = 3.5, label.x = 6, label.y = -2) +
  stat_cor(aes(label = paste(..p.label..)),
           size = 3.5, label.x = 6, label.y = -2.2) 
dev.off()

# Gráfico ECI x Gini
tiff('ECIxGINI.png', units="in", width=8, height=6, res=300)
ggplot(tab_mun, aes(x=GINI, y=ECI)) +
  geom_point(color='gray')+
  geom_smooth(method=lm, color='black', formula = y ~ x) +
  xlab('Gini Index') +
  ylab('Economic Complexity Index (ECI)') +
  ggtitle('ECI x Gini') +
  theme_classic() +
  theme(
    axis.title.x = element_text(color='black', size=12, vjust=-3),
    axis.title.y = element_text(color='black', size=12, vjust=3),
    plot.title = element_text(color='black', size=12, 
                              face='bold', hjust = 0.5,vjust=4),
    plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  stat_cor(aes(label = paste(..rr.label..)),
           size = 3.5, label.x = 0.25, label.y = -2) +
  stat_cor(aes(label = paste(..p.label..)),
           size = 3.5, label.x = 0.25, label.y = -2.3) 
dev.off()
