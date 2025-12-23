# ======================================================================= #
# ------------------------- CARREGANDO PACOTES -------------------------- #
# ======================================================================= #

library(sf)         # manipulação de dados espaciais (shapefiles, CRS etc.)
library(readxl)     # importação de arquivos do Excel
library(dplyr)      # Manipulação de dados (tibbles, data.frames, filter, mutate, join etc.)
library(tmap)       # Para mapas estáticos e interativos
library(spdep)      # Análise espacial: vizinhança, Moran, LISA
library(purrr)      # Funções funcionais (map, map_dfr etc.)
library(ggplot2)    # visualização de dados (mapas, gráficos)

# ======================================================================= #
# ----------------------- IMPORTAÇÃO SHAPEFILE -------------------------- #
# ======================================================================= #

# Importa o shapefile de Glasgow
Glasgow = read_sf(dsn = "Glasgow.shp")
Glasgow  # Exibe o shapefile com geometria + atributos

# Visualização inicial simples
ggplot(data = Glasgow) +
  geom_sf(fill = "white") +    # Preenche os bairros de branco
  theme_light()                # Define tema leve para visualização

# ======================================================================= #
# ---------------------- IMPORTAÇÃO BASE DE DADOS ---------------------- #
# ======================================================================= #

# Lê o arquivo XLSX com dados de preços de imóveis em Glasgow
base = read_excel(path = "base preco propriedade.xlsx")

# Visualiza a base carregada
base

# ======================================================================= #
# ------------------ JUNÇÃO SHAPEFILE + BASE DE DADOS ------------------ #
# ======================================================================= #

# Fazendo junção dos shapefiles com os dados
# 'IZ' deve ser a coluna comum entre shapefile e base de dados
glasgow_dados = left_join(x = Glasgow,
                          y = base,
                          by = "IZ")

glasgow_dados # Exibe o resultado

# ======================================================================= #
# -------------------------- QUESTÃO 1 ---------------------------------- #
# ======================================================================= #

#Definindo o tipo do mapa como estatico 
tmap_mode("plot")

# Mapa coroplético com intervalos de mesma amplitude
tm_shape(shp = glasgow_dados) +
  tm_polygons(
    fill = "preco",  # Variável para preenchimento
    fill.scale = tm_scale_intervals(style = "equal", n = 5, values = "matplotlib.reds"), # 5 classes iguais
    fill.legend = tm_legend(title = "Preço mediano dos imóveis")
  ) +
  tm_layout(frame = FALSE)   # Remove moldura
  
# ======================================================================= #
# -------------------------- QUESTÃO 2 ---------------------------------- #
# ======================================================================= #

#Definindo o tipo do mapa como interativo 
tmap_mode("view") 

tm_shape(shp = glasgow_dados) + 
  tm_polygons( # Adiciona polígonos preenchidos 
    fill = "preco", # Preenche polígonos pela variável 'preco' 
    fill.scale = tm_scale_intervals( style = "quantile", # define agrupamento com faixas baseada nos quantis 
                                     n = 8, # número de classes 
                                     values = "matplotlib.blues" # paleta de cores 
    ), 
    fill.legend = tm_legend(title = "Preço mediano dos imóveis"), # Título da legenda 
    lwd = 0.25, # Espessura da borda dos polígonos 
    fill_alpha = 0.5)


# ======================================================================= #
# -------------------------- QUESTÃO 3 ---------------------------------- #
# ======================================================================= #

# Criando matriz de vizinhança Queen (contiguidade por vértice ou lado)
W.queen <- poly2nb(pl = glasgow_dados, 
                   queen = TRUE)

# Calculando centroides de cada zona intermediária
centroids <- st_centroid(x = glasgow_dados)

# Criando linhas entre zonas intermediárias vizinhas
nb_lines_queen <- map_dfr(1:length(W.queen), function(i) {
  if (length(W.queen[[i]]) > 0) {
    map_dfr(W.queen[[i]], function(j) {
      coords_i <- st_coordinates(centroids[i, ])
      coords_j <- st_coordinates(centroids[j, ])
      st_sf(geometry = st_sfc(st_linestring(rbind(coords_i, coords_j)), crs = st_crs(glasgow_dados)))
    })
  }
})

# Visualizando vizinhança Queen
ggplot() +
  geom_sf(data = glasgow_dados, fill = "white", color = "gray50") +   # Mapa base
  geom_sf(data = nb_lines_queen, color = "red", size = 0.4, alpha = 0.6) +  # Linhas de vizinhança
  geom_sf(data = centroids, color = "blue", size = 1) +          # Centroides
  theme_minimal() +
  labs(title = "Mapa de Vizinhança (Queen)", subtitle = "Conexões entre zonas intermediárias")

# Criando objeto de pesos espaciais padronizado por linha
recWQW <- nb2listw(neighbours = W.queen, style = "W", zero.policy = TRUE)
recWQW$weights  # Verificando pesos


# ======================================================================= #
# -------------------------- QUESTÃO 4 ---------------------------------- #
# ======================================================================= #

# Índice global de Moran (autocorrelação espacial)
# H0: Independência ou aleatoriedade espacial (I = 0);
# H1: Correlação espacial direta (I > 0).

moran.test(x = glasgow_dados$preco, listw = recWQW)

# Conclusão: Como p-valor = 2.2e-16 < 0,05 (nível de significância de 5%), 
# rejeitamos H0, logo a variável preco apresenta correlação espacial direta

# Criando variáveis padronizadas para Moran scatterplot
glasgow_dados <- glasgow_dados |>
  mutate(
    preco_std = scale(preco)[,1],                    # Padroniza variável preco
    lag_preco_std = lag.listw(recWQW, preco_std)    # Calcula lag espacial
  )

# Plot Moran Scatterplot em ggplot2
ggplot(data = glasgow_dados, 
       mapping = aes(x = preco_std, y = lag_preco_std)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +           # Pontos
  geom_smooth(method = "lm", se = FALSE, color = "red") +       # Linha de regressão
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +  
  geom_text(aes(label = name), check_overlap = TRUE, nudge_y = 0.05, size = 3) + # Labels
  theme_minimal() +
  labs(title = "Moran Scatterplot - Preço mediano dos imóveis",
       x = "Preço padronizado",
       y = "Lag espacial padronizado")

# ======================================================================= #
# -------------------------- QUESTÃO 5 ---------------------------------- #
# ======================================================================= #

# Calculando Local Moran (LISA)
local_moran <- localmoran(x = glasgow_dados$preco, recWQW)

# Adicionando resultados ao shapefile
glasgow_dados <- glasgow_dados |>
  mutate(
    Ii = local_moran[, "Ii"],             # Estatística local
    Pr.Ii = local_moran[, "Pr(z != E(Ii))"]  # p-valor
  )

# Criando clusters LISA
media_total <- mean(x = glasgow_dados$preco, na.rm = TRUE)
preco_std <- scale(x = glasgow_dados$preco)[,1]
lag_preco_std <- scale(x = lag.listw(recWQW, glasgow_dados$preco))[,1]

glasgow_dados$LISA_cluster <- "Não significativo"          # Valor padrão

# H0: Independência ou aleatoriedade espacial entre localização e sua vizinhança (Ii = 0);
# H1: Correlação espacial direta - localização com valores altos 
# rodeada por uma vizinhança de valores altos (Ii > 0).
glasgow_dados$LISA_cluster[preco_std > 0 & lag_preco_std > 0 & glasgow_dados$Pr.Ii <= 0.05] <- "Alto-Alto"

# H0: Independência ou aleatoriedade espacial entre localização e sua vizinhança (Ii = 0);
# H1: Correlação espacial direta - localização com valores baixos 
# rodeada por uma vizinhança de valores baixos (Ii > 0).
glasgow_dados$LISA_cluster[preco_std < 0 & lag_preco_std < 0 & glasgow_dados$Pr.Ii <= 0.05] <- "Baixo-Baixo"

# H0: Independência ou aleatoriedade espacial entre localização e sua vizinhança (Ii = 0);
# H1: Correlação espacial inversa - localização com valores altos 
# rodeada por uma vizinhança de valores baixos (Ii < 0).
glasgow_dados$LISA_cluster[preco_std > 0 & lag_preco_std < 0 & glasgow_dados$Pr.Ii <= 0.05] <- "Alto-Baixo"

# H0: Independência ou aleatoriedade espacial entre localização e sua vizinhança (Ii = 0);
# H1: Correlação espacial inversa - localização com valores baixos 
# rodeada por uma vizinhança de valores altos (Ii < 0).
glasgow_dados$LISA_cluster[preco_std < 0 & lag_preco_std > 0 & glasgow_dados$Pr.Ii <= 0.05] <- "Baixo-Alto"

# Mapa de clusters LISA
ggplot(data = glasgow_dados) +
  geom_sf(mapping = aes(fill = LISA_cluster), color = "gray50") +
  scale_fill_manual(values = c(
    "Alto-Alto" = "red",
    "Baixo-Baixo" = "blue",
    "Alto-Baixo" = "orange",
    "Baixo-Alto" = "lightblue",
    "Não significativo" = "gray80"
  )) +
  theme_minimal() +
  labs(title = "Mapa LISA Local - Preço mediano dos imóveis", fill = "Cluster")

