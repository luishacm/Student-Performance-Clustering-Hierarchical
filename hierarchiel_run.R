# Carregamento dos pacotes
pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas e 'fviz_nbclust' para o método do cotovelo
             "ade4", #função 'ade4' para matriz de distâncias em var. binárias
             "readxl",
             "fpc",
             "writexl") 

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Carregando a base de dados
Cluster_MT <- read_excel("Cluster_PT.xlsx") # Substitua pelo caminho completo do arquivo

# Removendo as variáveis "Código do Aluno"
Cluster_MT <- Cluster_MT %>%
  select(-c("Código do Aluno"))

# Calculando a matriz de distância
matriz_D <- Cluster_MT %>% 
  dist.binary(method = 2)

# Definição do esquema hierárquico de aglomeração (Até chegar a 1 só cluster)
cluster_hier <- agnes(x = matriz_D, method = "complete")

# Determinação do número de clusters usando o método do cotovelo
max_clusters <- 40

# Defina o comprimento de wss_values para o número máximo de clusters
wss_values <- numeric(max_clusters)

# Use um loop for para calcular a WSS para cada número de clusters
for (i in 1:max_clusters) {
  kmeans_result <- kmeans(Cluster_MT, centers = i, nstart = 10)
  wss_values[i] <- kmeans_result$tot.withinss
}

#Plotando o gráfico do método do cotovelo
plot(1:max_clusters, wss_values, type = "b", pch = 19, frame = FALSE, 
     xlab = "Número de Clusters",
     ylab = "Soma dos Quadrados Intra-cluster (WSS)",
     main = "Método do Cotovelo para Determinação do Número de Clusters")

k <- 20

# Executando K-means com N clusters
kmeans_result <- kmeans(matriz_D, centers = k)

# Visualização dos clusters usando fviz_cluster
fviz_cluster(kmeans_result, data = matriz_D)

# Calcular os centróides dos clusters
centroids <- kmeans_result$centers

# Calcular a matriz de ligação (linkage matrix) para o dendrograma
hclust_object <- hclust(as.dist(matriz_D), method = "complete")

# Plotar o dendrograma com visualização melhorada
pdf("dendrograma.pdf", width = 100, height = 8)  # Salvar o gráfico em um arquivo PDF
plot(hclust_object, hang = -1, cex = 0.8, main = "Dendrograma de Clustering Hierárquico", xlab = "Indivíduos", labels = FALSE)
rect.hclust(hclust_object, k = k, border = "red")  # Adicionar retângulos destacando os clusters
dev.off()

# Visualização do dendrograma melhorado
plot(hclust_object, hang = -1, cex = 0.8, main = "Dendrograma de Clustering Hierárquico", xlab = "Indivíduos", labels = FALSE)
rect.hclust(hclust_object, k = k, border = "red")  # Adicionar retângulos destacando os clusters

#Salvando os arquivos
Cluster_MT <- read_excel("Cluster_PT.xlsx")
Cluster_MT$cluster <- kmeans_result$cluster
nome_arquivo <- "Cluster_PT_com_clusters.xlsx"
write_xlsx(Cluster_MT, path = nome_arquivo)