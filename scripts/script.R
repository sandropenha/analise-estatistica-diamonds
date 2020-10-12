#### SOBRE ####
# Análise estatística e exploratoria do dataset "diamonds" disponibilizado pela biblioteca do ggplot2.
# Dentre os objetivos deste projeto, estão a prática de técnicas de análise exploratória e modelagem estatistica.
# O dataset possui variáveis categoricas e numéricas que explicam o preço de um diamante.

#### LIBRARYS ####
pacman::p_load(tidyverse,knitr,qcc,corrplot,lvplot,e1071)


#### TRADUZINDO O DATASET ####
# Para facilitar a compreenssão  das variáveis, traduziremos o dataset para o português.
df <- diamonds %>% 
  select_all() %>% 
  transmute(peso = carat,
            qualidade = cut,
            cor = color,
            claridade = clarity,
            comprimento = x,
            largura = y,
            profundidade = depth,
            tabela = table,
            preco = price);df

#### DICIONARIO DE DADOS ####
# peso: peso do diamante
# qualidade: qualidade do corte (fair,good,very good,premium,ideal)
# cor: cor do diamante, de D (best) a J (worst)
# claridade: medidade quao claro o diamante é, (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))
# comprimento: comprimento em mm (0–10.74)
# largura: largura em mm (0–58.9)
# profundidade: profundidade em mm (0–31.8)
# tabela: largura do topo do diamante relativo ao ponto mais largo (43–95)
# preco: preço em dolares  (\$326–\$18,823)

#### ANÁLISE EXPLORATÓRIA ####

## Número de diamantes por qualidade
df_fi <- df %>% 
  select(qualidade) %>% 
  group_by(qualidade) %>% 
  summarise(n = n()) %>% 
  mutate(fi = round(n/sum(n)*100));kable(df_fi)
p1 <- ggplot(df_fi,aes(x = qualidade))+
  geom_bar(aes(y = n),fill = "steelblue", alpha = 0.8, stat = "identity")+
  theme_minimal()+
  labs(title = "Número de diamantes por qualidade", x = "Qualidade",y="");p1

# 40% dos diamantes presentes no dataset sao classificados como o tipo "Ideal". Em valores absolutos, isso nos diz que
#  há mais de 20.000 registros para esse tipo de diamantes presentes no dataset. 
#  Diamantes, do tipo "Fair" são os que possuem menor representatividade no dataset.

## Número de diamantes por faixa de peso em kilogramas
p2 <- ggplot(df, aes(x = peso))+
  geom_histogram(bins = 30, fill = "steelblue", color = "white" ,alpha = 0.8)+
  theme_minimal()+
  labs(title = "Número de diamantes por faixa de peso", x = "Kilogramas", y="");p2
# Grande parte dos diamantes possuem entre aproximadamente 0,25 e 1 kilogramas.

## Numero de diamantes por qualidade e peso
p3 <- ggplot(df,aes(x = peso,color=qualidade))+
  geom_freqpoly(size = 2)+
  theme_minimal()+
  labs(title = "Número de diamantes por faixa de peso e qualidade", x = "Kilogramas",y="");p3

# Já nesse plot, podemos confirmar o que já haviamos observado anteriormente. Diamantes do tipo "Ideal" são os mais presentes no dataset.
# Também é possível ver aqui a relação do tipo de diamantes por qualidade e sua distribuição de contagem por Kilogramas. Em sua grande maioria,
# diamantes do tipo Ideal, estão concetrados na faixa de aproximadamente 0,25 kilogramas.


## Boxplot de diamantes por peso em kilogramas
p4 <- ggplot(df,aes(x = qualidade, y = peso, fill = qualidade))+
  geom_boxplot(color = "grey")+
  theme_minimal()+
  labs(title="Boxplot por qualidade dos diamantes", x = "Qualidade",y="Kilogramas");p4

# Há alguns insights interessantes que podemos tirar analisando o box plot por peso e qualidade dos diamantes:
# Diamantes do tipo "Fair" posssui intervalo interqualico menor que os demais tipos de diamantes. Sendo o diamante do tipo 'Premium', o que possui
# maior intervalo interquatilico do conjunto de dados. Essa media é importante pois aqui conseguimos avaliar a variabilidade dos dados sem sofrer
# influencia dos outliers.
# Podemos medir a variabilidade também atravé da amplitude (valor máximo - valor minimo), a diferença é que aqui estamos considerando valores outliers.
# Por essa visão, 'Fair' é o tipo que mais apresenta variabilidade, é importante notar também a concentração de outliers na faixa acima de 4
# Kilogramas, sendo o unico tipo de diamante com essa característica.
# Também podemos notar que em todos os casos, temos a média maior que a mediana, 
# pois o "bigode" possui inclinação para a parte superior, ou seja ambas as amostras podem ser ditas como assimétricas. Podemos confirmar tais
# observações calculando o coeficiente de Assimetria (skewness)

## Calculando o coeficiente de Assimetria
# Se  menor que -1 ou maior que 1, a distribuição é altamente distorcida.
# Se  entre -1 e -0,5 ou entre 0,5 e 1, a distribuição é enviesada moderadamente.
# Se  entre -0,5 e 0,5, a distribuição é aproximadamente simétrica.
premium <- df %>% filter(qualidade == "Premium")
ideal <- df %>% filter(qualidade == "Ideal")
verygood <- df %>% filter(qualidade == "Very Good")
good <- df %>% filter(qualidade == "Good")
fair <- df %>% filter(qualidade == "Fair")
skewness(premium$peso) # 0.8615343
skewness(ideal$peso) # 1.339344
skewness(verygood$peso) # 0.9936572
skewness(good$peso) # 1.028119
skewness(fair$peso) # 1.683545
# Pode-se concluir que as amostras são de fato assimétricas.

# Vamos avaliar também o boxplot, utilizando o preço por qualidade de diamantes:
p5 <- ggplot(df,aes(x = qualidade, y = preco, fill = qualidade))+
  geom_boxplot(color = "grey")+
  theme_minimal()+
  labs(title="Boxplot por qualidade dos diamantes", x = "Qualidade",y="Preço");p5
median(fair$preco)
median(ideal$preco)
median(premium$preco)
median(verygood$preco)
median(good$preco)

# Um insight importante que podemo tirar dessa análise é que, diamantes do tipo "Ideal", que é a melhor qualidade, tem uma mediana de preço menor 
# que os demais tipos de diamantes.
# E curiosamente, diamantes do tipo "Fair" em geral são mais caros.
# Para essas distribuições, também temos dados assimétricos.
# Quando falamos em intervalo interquatil, ou seja, a variabilidade dos dados desconsiderandos os valores outliers, diamantes do tipo 'Fair',
# também possuem aqui, menor variabilidade.


# Uma outra forma de ver valores outliers é através de coordenadas cartesianas. Usaremos como exemplo a variável largura.
p6 <- ggplot(df)+
  geom_histogram(aes(x = largura), binwidth = 0.5, fill = "steelblue", color = "white")+
  coord_cartesian(ylim = c(0,50))+
  ggtitle("Análise de largura dos diamantes - Identificando outliers")+
  theme_minimal();p6

# Há valores outliers na faixa intermediária de 30 e na faixa superior de 50 e também ha valores na faixa de 0.

## Analisando os outliers mais a fundo
df_out <- df %>% 
  filter(largura == 0 | largura > 20) %>% 
  arrange(largura);kable(df_out)

# Podemos então concluir que esses valores encontrados são outliers pois possuem erros de input, tal afirmação é sustentada por:
# 1 - Não é possível haver diamantes com 0cm de largura (curiosamente, quando possuem 0cm de largura também possuem 0cm de comprimento no dataset).
# 2 - É muito improvavel que exista diamantes com ~30cm e ~60cm, mas ainda que existam, eles custariam milhares de dolares, o que podemos
# ver que não acontece.

# Como já verificamos que os valores encontrados tratam-se de valores que tiveram inputs errados, vamos substituir esses valores por valores
# faltantes (NA's)

df2 <- df %>% 
  mutate(largura = ifelse(largura < 3 | largura > 20, NA, largura))
kable(head(df2));kable(tail(df2))


## Verificando valores outliers com correlação comprimento x largura
# Uma outra forma de identificar valores outliers é correlacionar uma variável independente a uma variável dependente.
# Isso também é muito útil para entendermos como se relacionam as variáveis do dataset.
# no caso, vamos correlacionar o comprimento com a largura, pois levando-se em consideração de que estamos falando de diamantes,
# o normal é que a medida que seu comprimento aumente, sua largura também deve aumentar.

p7 <- ggplot(df2, aes(x = comprimento, y = largura))+
  geom_point()+
  theme_minimal()+
  ggtitle("Correlação - Comprimento x Largura");p7
# O Gráfico de correlação nos mostra que esse padrão é verdadeiro, contudo, é possível identifcar um valor outlier na faixa de 0cm 
# de comprimento. Vamos avalia-lo em seguida, e se for o caso, aplicar a mesma técnica de inputar valores NA para onde existam erros nos dados.

df_com_out <- df2 %>% 
  filter(comprimento <3);kable(df_com_out)
# Por padrão o ggplot desconsiderou os valores NA do grafico anterior, nos mostrando apenas um valor outlier na faixa de 0 comprimento, contudo,
# vemos aqui que existem outros valores onde comprimento é igual a 0.

## Aplicando input de NA nos dados de comprimento < 3 e plot de correlação
df3 <- df2 %>%
  mutate(comprimento = ifelse(comprimento < 3, NA, comprimento));kable(head(df3));kable(tail(df3))

p8 <- ggplot(df3, aes(x = comprimento, y = largura))+
  geom_point()+
  theme_minimal()+
  ggtitle("Correlação - Comprimento x Largura");p8

# Podemos ainda medir o coeficiente de correlação de Pearson entre essas variáveis
 df4<- df3 %>% drop_na();cor(df4$comprimento,df4$largura)
# Comprimento x largura possuem forte correlação positiva.

# Podemos avaliar ainda, quais variáveis são mais determinantes para a precificação dos diamantes.
# Faremos isso utilizando uma matriz de correlação com o corrplot
num_vars <- sapply(df4, is.numeric)
cor <- cor(df4[num_vars])
corrplot(cor,method = "number")

# Aqui, podemos ver que, o peso, comprimento e largura são as variáveis mais determinantes para o preço de um diamante, enquanto as demais variáveis
# são pouco representativas.

# Também podemos analisar o que determina a qualidade de um diamante quando este é Ideal, Premium, Good e etc.
# Isso também nos auxiliará a entender o motivo pelo qual diamantes do tipo "Fair" (pior qualidade) tem mediana de preços mais elevados que
# diamantes do tipo "Ideal" (melhor qualidade)
# Para isso, vamos padronizar nossos dados, e transformar a variavel "qualidade" em numérica e em seguida fazer o estudo de correlação.

qualidade <- unique(df4$qualidade);qualidade
cor <- unique(df4$cor);cor
claridade <- unique(df$claridade);claridade

## Definição das variáveis categóricas:
# Qualidade:
# 1: Fair
# 2: Good:
# 3: Very Good
# 4: Premium
# 5: Ideal

df_ <- df4 %>% 
  mutate(qualidade = ifelse(qualidade == "Fair", 1,
                            ifelse(qualidade == "Good",2,
                                   ifelse(qualidade == "Very Good",3,
                                          ifelse(qualidade == "Premium",4,5)))))

num_vars <- sapply(df_,is.numeric)
cor <- cor(df_[num_vars])
corrplot(cor,method = "number")

# A variáve qualidade, não  é peça determinante na composição do preço do diamante, mas sim seu comprimento, largura e  principlamente seu peso.
# Muito provalvelmente, diamantes de qualidade inferior costumam pesar mais, logo tem preço maior. Vamos verificar esse hipotese estudando o peso
# dos diamantes novamente:

p4 <- ggplot(df,aes(x = qualidade, y = peso, fill = qualidade))+
  geom_boxplot(color = "grey")+
  theme_minimal()+
  labs(title="Boxplot por qualidade dos diamantes", x = "Qualidade",y="Kilogramas");p4

# Revistando esse plot feito anteriormente acerca dos kilogramas dos diamantes por qualidade, podemos atestar aqui que, diamantes de qualidade
# inferior (Fair) possuem mais kilogramas em comparação com diamantes de qualidade superior (Ideal), ou seja, diamantes "Fair" são mais caros
# pois possuem mais kilogramas, pois conforme vimos no plot de correlação, Peso é a variável que mais está correlacionada ao Preço.

plot <- ggplot(df_, aes(x = peso, y = preco))+
  geom_point(color = "steelblue", alpha = 0.3)+
  theme_minimal()+
  ggtitle("Correlação entre peso e preço do diamante");plot


corrplot(cor,method = "number")

# É possível ver também que há correlação negativa moderada entre qualidade e tabela (largura do topo do diamante em relação ao ponto mais largo),
# ou seja, quanto menor a largura do topo em relação ao ponto mais largo do diamante, sua qualidade tende a aumentar.

p9 <- ggplot(df_,aes(x = qualidade,y=tabela))+
  geom_point()+
  theme_minimal()+
  ggtitle("Correlação - Qualidade x Tabela");p9


# Plotando principais correlacoes
p10 <- ggplot(df_,aes(x=comprimento,y=preco))+
  geom_point(color = "lightblue",alpha = 0.5)+
  theme_minimal()+
  ggtitle("Comprimento x Preco")

p11 <- ggplot(df_,aes(x=largura,y=preco))+
  geom_point(color = "green",alpha = 0.1)+
  theme_minimal()+
  ggtitle("Largura x Preco")

p12 <-  ggplot(df_,aes(x=peso,y=preco))+
  geom_point(color = "red",alpha = 0.1)+
  theme_minimal()+
  ggtitle("Peso x Preco")

p13 <-  ggplot(df_,aes(x=tabela,y=preco))+
  geom_point(color = "grey",alpha = 0.3)+
  theme_minimal()+
  ggtitle("Tabela x Preco")


gridExtra::grid.arrange(p10,p11,p12,p13,nrow = 2)

# Também é possível ver que, dentre as medidas do diamante, tabela (largura do topo do diamante em relação ao ponto mais largo) 
# é a unica na qual não possui relação significativa com o preço aplicado ao diamante, embora possua correlação negativa moderada com a qualidade
# do diamante.


#### CONCLUSÃO DA ANÁLISE EXPLORATÓRIA ####
# Diamantes do tipo "Fair" são mais pesados quando falamos em kilogramas;
# Diamantes do tipo "Ideal são mais leves quando falamos em kilogramas;
# O pesos, assim como as dimensoes (exceto tabela) são às variáveis mais determinantes no preço dos diamantes;
# Diamantes do tipo "Fair" são mais caros pois possuem mais kilogramas;
# A variável tabela é moderadamente significante na composição da qualidade de um diamante.