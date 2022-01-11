## Execução da Análise de Componentes Principais

# Definir o diretório
setwd('C:\\Users\\queir\\OneDrive\\Documents\\Mestrado\\Segundo Período\\MARU\\Trabalho Final\\ACP_atividades_mun')

# Carregar os pacotes
library(openxlsx)
library(haven)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(remotes)
library(FactoMineR)
library(factoextra)
library(ggbiplot)
library(conflicted)
library("corrplot")

# Retirar a duplicidade de algumas funções
conflict_prefer('rename', 'dplyr')
conflict_prefer('mutate', 'dplyr')
conflict_prefer('filter', 'dplyr')

# Carregar a base de municípios
tab_mun <- read.xlsx('Muns_index.xlsx')

# Carregar a base de produtos
tab_product <- read.xlsx('Products_index.xlsx')

# Faremos a análise de componentes principais para os produtos e municípios.

## Produtos:

# Preparar a base
data_product <- tab_product %>%
  remove_rownames() %>% 
  column_to_rownames(var = 'cnae_id') %>%
  select(-c(cnae_name))
summary(data_product)

# Gráfico total das correlações 
tiff('Plot_products.png', units="in", width=8, height=6, res=300)
plot(data_product)
dev.off()

# ACP
acp_product <- princomp(data_product, cor=T)
summary(acp_product)
acp_product$loadings

# Adicionar uma subclassificação para melhorar a visualização do gráfico a seguir:
subclas <- read.xlsx('subclas_cnae.xlsx')
tab_product <- tab_product %>% 
  left_join(subclas, by = c('cnae_id'))

# Gráfico da ACP
tiff('PCA_products.png', units="in", width=8, height=6, res=300)
ggbiplot(acp_product) +
  geom_text(label = rownames(data_product), size = 2,
            hjust = 0, nudge_x = 0.07) +
  geom_point(aes(colour=tab_product$sector), size = 2.3) +
  theme_bw() +
  xlab('Componente 1 (57,0%)') +
  ylab('Componente 2 (21,3%)') +
  ggtitle('PCA - Products') +
  theme(
    axis.title.x = element_text(color='black', size=11, vjust=-3),
    axis.title.y = element_text(color='black', size=11, vjust=3),
    plot.title = element_text(color='black', size=12, 
                              face='bold', hjust = 0.5,vjust=4),
    plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  guides(color=guide_legend("Setores:")) +
  expand_limits(y = c(-2.5, 2.5), x = c(-2.5,2.5)) +
  geom_hline(yintercept=0, linetype="dashed", colour = "black") +
  geom_vline(xintercept=0, linetype="dashed", colour = "black") +
  scale_colour_manual(values = c("#800000",
                                 "#B22222", "#E9967A",
                                 "#D3D3D3"))
dev.off()

# Gráfico dos Autovalores
tiff('Eigenvalue_products.png', units="in", width=8, height=6, res=300)
fviz_eig(acp_product, choice = 'eigenvalue', addlabels = T, linecolor = '#A52A2A',
         main = 'PCA - Products', geom = 'line', hjust = -0.1) +
  geom_hline(yintercept=1, linetype="dashed", colour = "black")  
dev.off()

# Gráfico da variância explicada
tiff('Var_explained_products.png', units="in", width=8, height=6, res=300)
fviz_eig(acp_product, choice = 'variance', addlabels = T, linecolor = 'black', barfill = '#A52A2A',
         barcolor='white', main = 'PCA - Products', geom = 'bar', hjust = 0.5)  
dev.off()

# Gráfico dos autovetores
var_product <- get_pca_var(acp_product)
tiff('CORR_products.png', units="in", width=8, height=6, res=300)
corrplot(var_product$cos2, is.corr = FALSE, method = "circle", tl.col = "black",
         col=colorRampPalette(c("white", "red", "dark red"))(200))
dev.off()

# Da mesma forma, faremos para os municípios.

# Municípios:

# Incluir informações das capitais para melhorar a visualização do gráfico
capitais <- read.xlsx('capitals.xlsx')  
capitais <- capitais %>% 
  mutate(ibge_id = as.character(ibge_id))

tab_mun <- tab_mun %>% 
  left_join(capitais, by = c('ibge_id')) %>%
  select(-c(mun_id.y, mun_name.y)) %>% 
  rename(mun_id = mun_id.x,
         mun_name = mun_name.x)

# Preparar a base
data_mun <- tab_mun %>%
  remove_rownames() %>% 
  column_to_rownames(var = 'ibge_id') %>% 
  select(ECI, IDH, E_pc, GINI)

# ACP
acp_mun <- princomp(data_mun, cor=T)
summary(acp_mun)
acp_mun$loadings
  
# Gráfico da ACP
tiff('PCA_mun.png', units="in", width=8, height=6, res=300)
ggbiplot(acp_mun, alpha = 0) +
  geom_text(label = tab_mun$uf_name, size = 2,
            hjust = 0, nudge_x = 0) +
    theme_bw() +
  xlab('Componente 1 (47,4%)') +
  ylab('Componente 2 (25,1%)') +
  ggtitle('PCA - Mun') +
  theme(
    axis.title.x = element_text(color='black', size=11, vjust=-3),
    axis.title.y = element_text(color='black', size=11, vjust=3),
    plot.title = element_text(color='black', size=12, 
                              face='bold', hjust = 0.5,vjust=4),
    plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  guides(color=guide_legend("Regiões:")) +
  geom_hline(yintercept=0, linetype="dashed", colour = "black") +
  geom_vline(xintercept=0, linetype="dashed", colour = "black") +
  xlim(-1.5, 2) + 
  ylim(-0.5, 1.75) 
dev.off() 

# Gráfico dos autovalores
tiff('Eigenvalue_mun.png', units="in", width=8, height=6, res=300)
fviz_eig(acp_mun, choice = 'eigenvalue', addlabels = T, linecolor = 'dark gray',
         main = 'PCA - Mun', geom = 'line', hjust = -0.1) +
  geom_hline(yintercept=1, linetype="dashed", colour = "black")  
dev.off()

# Gráfico da variância explicada
tiff('Var_explain_mun.png', units="in", width=8, height=6, res=300)
fviz_eig(acp_mun, choice = 'variance', addlabels = T, linecolor = 'black', barfill = 'dark gray',
         barcolor='white', main = 'PCA - Products', geom = 'bar', hjust = 0.5)  
dev.off()

# Gráficos dos autovetores
var_mun <- get_pca_var(acp_mun)
tiff('CORR_mun.png', units="in", width=8, height=6, res=300)
corrplot(var_mun$cos2, is.corr = FALSE, method = "circle", tl.col = "black",
         col=colorRampPalette(c("white", "gray", "dark gray", "black"))(200))
dev.off()

