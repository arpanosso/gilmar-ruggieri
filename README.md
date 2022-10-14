
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Análise de volatilização de amônia por excretas animal

## Carregando Pacotes

``` r
library(tidyverse)
library(agricolae)
library(lmerTest)
library(emmeans)
library(readxl)
library(skimr)
library(MASS)
library(lme4)
```

## Entrada de dados

``` r
dados <- read_xlsx("data/DADOS GILMAR VOL NH3.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate_at(vars(estacao, especie, excreta, rp), as_factor)
glimpse(dados)
#> Rows: 90
#> Columns: 11
#> $ tratamento             <chr> "Urina de Caprino", "Urina de Caprino", "Urina ~
#> $ estacao                <fct> VERÃO, VERÃO, VERÃO, VERÃO, VERÃO, VERÃO, VERÃO~
#> $ especie                <fct> CAPRINO, CAPRINO, CAPRINO, CAPRINO, CAPRINO, EQ~
#> $ excreta                <fct> URINA, URINA, URINA, URINA, URINA, URINA, URINA~
#> $ rp                     <fct> 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1,~
#> $ tr                     <dbl> 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 6, 6, 6, 6, 6, 1,~
#> $ especie_2              <dbl> 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1,~
#> $ excreta_2              <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2,~
#> $ estacao_2              <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,~
#> $ bloco                  <dbl> 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1,~
#> $ percent_n_volatilizado <dbl> 13.31456414, 14.72593502, 11.09723583, 6.643591~
```

## Resumo simples dos dados

``` r
skim(dados)
```

|                                                  |       |
|:-------------------------------------------------|:------|
| Name                                             | dados |
| Number of rows                                   | 90    |
| Number of columns                                | 11    |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |       |
| Column type frequency:                           |       |
| character                                        | 1     |
| factor                                           | 4     |
| numeric                                          | 6     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |       |
| Group variables                                  | None  |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| tratamento    |         0 |             1 |  14 |  16 |     0 |        6 |          0 |

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts                 |
|:--------------|----------:|--------------:|:--------|---------:|:---------------------------|
| estacao       |         0 |             1 | FALSE   |        3 | VER: 30, INV: 30, PRI: 30  |
| especie       |         0 |             1 | FALSE   |        3 | CAP: 30, EQU: 30, OVI: 30  |
| excreta       |         0 |             1 | FALSE   |        2 | URI: 45, FEZ: 45           |
| rp            |         0 |             1 | FALSE   |        5 | 1: 18, 2: 18, 3: 18, 4: 18 |

**Variable type: numeric**

| skim_variable           | n_missing | complete_rate | mean |    sd |   p0 |  p25 |  p50 |   p75 |  p100 | hist  |
|:------------------------|----------:|--------------:|-----:|------:|-----:|-----:|-----:|------:|------:|:------|
| tr                      |         0 |             1 | 3.50 |  1.72 | 1.00 | 2.00 | 3.50 |  5.00 |  6.00 | ▇▃▃▃▃ |
| especie_2               |         0 |             1 | 2.00 |  0.82 | 1.00 | 1.00 | 2.00 |  3.00 |  3.00 | ▇▁▇▁▇ |
| excreta_2               |         0 |             1 | 1.50 |  0.50 | 1.00 | 1.00 | 1.50 |  2.00 |  2.00 | ▇▁▁▁▇ |
| estacao_2               |         0 |             1 | 2.00 |  0.82 | 1.00 | 1.00 | 2.00 |  3.00 |  3.00 | ▇▁▇▁▇ |
| bloco                   |         0 |             1 | 3.00 |  1.42 | 1.00 | 2.00 | 3.00 |  4.00 |  5.00 | ▇▇▇▇▇ |
| percent_n\_volatilizado |         0 |             1 | 7.55 | 10.03 | 0.01 | 0.66 | 3.24 | 10.44 | 46.59 | ▇▁▁▁▁ |

## Explorando a variável resposta

``` r
dados %>% 
  ggplot(aes(x=percent_n_volatilizado, y = ..density..)) +
  geom_histogram(bins = 10, color="black", fill="aquamarine4")+
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Testando os pressupostos

``` r
dados <- dados %>% 
  mutate(
    trat = interaction(estacao, especie, excreta,sep="-"))
```

## Anova Preliminar

``` r
mod0 <- aov(percent_n_volatilizado ~ trat, data =dados)
summary(mod0)
#>             Df Sum Sq Mean Sq F value Pr(>F)    
#> trat        17   8190   481.8   44.98 <2e-16 ***
#> Residuals   72    771    10.7                   
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

dados %>% 
  mutate(
    pred = predict(mod0),
    rs = rstudent(mod0)
  ) %>% 
  ggplot(aes(x=rs, y= ..density..)) +
  geom_histogram(bins = 10, color="black", fill="lightgray")+
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> teste de
normalidade de **Shapiro-Wilk**

``` r
mod0 %>% rstudent() %>% shapiro.test()
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  .
#> W = 0.83627, p-value = 1.427e-08
```

``` r
dados %>% 
  ggplot(aes(x=trat, y= percent_n_volatilizado,fill=trat)) +
  geom_boxplot()+
  theme_minimal() +
  coord_flip() +
  theme(legend.position = "none") +
  scale_fill_viridis_d()
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

> Existe uma discrepância nos valores de `percent_n_volatilizado` entre
> urina e fezes, levando a heterocedasticidade.

### Possiveis Soluções popostas:

-   1)  Aplicar uma transformação nos dados para trazer todos a uma
        mesma escala.

``` r
BoxCox <- (boxcox(mod0))
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> Encontrando
o valor de
![\lambda](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda "\lambda").

``` r
lambda <- BoxCox %>% 
  data.frame() %>% 
  arrange(desc(y)) %>% 
  slice(n=1) %>% 
  pull(x)
lambda
#> [1] 0.2222222
```

como
![\lambda \neq 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda%20%5Cneq%200 "\lambda \neq 0")
e
![\lambda \neq 1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda%20%5Cneq%201 "\lambda \neq 1")
usamos a transformação de dados:

![y = \frac{y^{\lambda}-1}{\lambda}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y%20%3D%20%5Cfrac%7By%5E%7B%5Clambda%7D-1%7D%7B%5Clambda%7D "y = \frac{y^{\lambda}-1}{\lambda}")

``` r
dados <- dados %>% 
  mutate(
    percent_n_volatilizado_t = (percent_n_volatilizado^lambda-1)/(lambda)
    )
```

Vamos refazer as análises, portanto.

``` r
dados %>% 
  ggplot(aes(x=percent_n_volatilizado_t, y = ..density..)) +
  geom_histogram(bins = 10, color="black", fill="aquamarine1")+
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
mod1 <- aov(percent_n_volatilizado_t ~ trat, data =dados)
dados %>% 
  mutate(
    pred_t = predict(mod1),
    rs_t = rstudent(mod1)
  ) %>% 
  ggplot(aes(x=rs_t, y= ..density..)) +
  geom_histogram(bins = 10, color="black", fill="lightgray")+
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

teste de normalidade de **Shapiro-Wilk**

``` r
mod1 %>% rstudent() %>% shapiro.test()
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  .
#> W = 0.98417, p-value = 0.3456
```

``` r
dados %>% 
  ggplot(aes(x=trat, y= percent_n_volatilizado_t,fill=trat)) +
  geom_boxplot()+
  theme_minimal() +
  coord_flip() +
  theme(legend.position = "none") +
  scale_fill_viridis_d()
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Análise para FEZES - Variável original

``` r
mod <- lmer(percent_n_volatilizado ~ especie*estacao + (1|rp),
            data=dados %>% 
              filter(excreta == "FEZES"))
anova(mod)
#> Type III Analysis of Variance Table with Satterthwaite's method
#>                 Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
#> especie         50.279  25.139     2    32  71.215 1.646e-12 ***
#> estacao         98.805  49.402     2    32 139.946 < 2.2e-16 ***
#> especie:estacao 92.187  23.047     4    32  65.286 6.201e-15 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
post_hoc <- emmeans(mod, ~ especie*estacao)
pairs(post_hoc, adjust="tukey")
#>  contrast                             estimate    SE df t.ratio p.value
#>  CAPRINO VERÃO - EQUINO VERÃO          -0.2691 0.376 32  -0.716  0.9982
#>  CAPRINO VERÃO - OVINO VERÃO            0.0786 0.376 32   0.209  1.0000
#>  CAPRINO VERÃO - CAPRINO INVERNO       -1.1336 0.376 32  -3.017  0.0991
#>  CAPRINO VERÃO - EQUINO INVERNO        -0.6242 0.376 32  -1.661  0.7644
#>  CAPRINO VERÃO - OVINO INVERNO         -1.2620 0.376 32  -3.358  0.0460
#>  CAPRINO VERÃO - CAPRINO PRIMAVERA     -2.5430 0.376 32  -6.767  <.0001
#>  CAPRINO VERÃO - EQUINO PRIMAVERA      -0.4361 0.376 32  -1.161  0.9591
#>  CAPRINO VERÃO - OVINO PRIMAVERA       -7.7321 0.376 32 -20.577  <.0001
#>  EQUINO VERÃO - OVINO VERÃO             0.3477 0.376 32   0.925  0.9897
#>  EQUINO VERÃO - CAPRINO INVERNO        -0.8644 0.376 32  -2.300  0.3713
#>  EQUINO VERÃO - EQUINO INVERNO         -0.3551 0.376 32  -0.945  0.9882
#>  EQUINO VERÃO - OVINO INVERNO          -0.9928 0.376 32  -2.642  0.2093
#>  EQUINO VERÃO - CAPRINO PRIMAVERA      -2.2738 0.376 32  -6.051  <.0001
#>  EQUINO VERÃO - EQUINO PRIMAVERA       -0.1670 0.376 32  -0.444  0.9999
#>  EQUINO VERÃO - OVINO PRIMAVERA        -7.4630 0.376 32 -19.860  <.0001
#>  OVINO VERÃO - CAPRINO INVERNO         -1.2122 0.376 32  -3.226  0.0624
#>  OVINO VERÃO - EQUINO INVERNO          -0.7028 0.376 32  -1.870  0.6375
#>  OVINO VERÃO - OVINO INVERNO           -1.3405 0.376 32  -3.567  0.0278
#>  OVINO VERÃO - CAPRINO PRIMAVERA       -2.6216 0.376 32  -6.977  <.0001
#>  OVINO VERÃO - EQUINO PRIMAVERA        -0.5147 0.376 32  -1.370  0.9008
#>  OVINO VERÃO - OVINO PRIMAVERA         -7.8107 0.376 32 -20.786  <.0001
#>  CAPRINO INVERNO - EQUINO INVERNO       0.5094 0.376 32   1.356  0.9059
#>  CAPRINO INVERNO - OVINO INVERNO       -0.1284 0.376 32  -0.342  1.0000
#>  CAPRINO INVERNO - CAPRINO PRIMAVERA   -1.4094 0.376 32  -3.751  0.0176
#>  CAPRINO INVERNO - EQUINO PRIMAVERA     0.6974 0.376 32   1.856  0.6466
#>  CAPRINO INVERNO - OVINO PRIMAVERA     -6.5985 0.376 32 -17.560  <.0001
#>  EQUINO INVERNO - OVINO INVERNO        -0.6377 0.376 32  -1.697  0.7438
#>  EQUINO INVERNO - CAPRINO PRIMAVERA    -1.9188 0.376 32  -5.106  0.0004
#>  EQUINO INVERNO - EQUINO PRIMAVERA      0.1881 0.376 32   0.501  0.9999
#>  EQUINO INVERNO - OVINO PRIMAVERA      -7.1079 0.376 32 -18.916  <.0001
#>  OVINO INVERNO - CAPRINO PRIMAVERA     -1.2810 0.376 32  -3.409  0.0408
#>  OVINO INVERNO - EQUINO PRIMAVERA       0.8258 0.376 32   2.198  0.4309
#>  OVINO INVERNO - OVINO PRIMAVERA       -6.4702 0.376 32 -17.218  <.0001
#>  CAPRINO PRIMAVERA - EQUINO PRIMAVERA   2.1069 0.376 32   5.607  0.0001
#>  CAPRINO PRIMAVERA - OVINO PRIMAVERA   -5.1891 0.376 32 -13.809  <.0001
#>  EQUINO PRIMAVERA - OVINO PRIMAVERA    -7.2960 0.376 32 -19.416  <.0001
#> 
#> Degrees-of-freedom method: kenward-roger 
#> P value adjustment: tukey method for comparing a family of 9 estimates
```

``` r
dados %>% 
  filter(excreta == "FEZES") %>% 
  group_by(especie, estacao) %>% 
  summarise(
    media = mean(percent_n_volatilizado)
  ) %>% ungroup() %>% 
  ggplot(
    aes(x=estacao, y=media, fill=especie)
  ) +
  geom_col(position="dodge") +
  theme_minimal() +
  scale_fill_viridis_d()
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## Análise para URINA

``` r
mod <- lmer(percent_n_volatilizado ~ especie*estacao + (1|rp),
            data=dados %>% 
              filter(excreta == "URINA"))
post_hoc <- emmeans(mod, ~ especie*estacao)
pairs(post_hoc, adjust="tukey")
#>  contrast                             estimate  SE df t.ratio p.value
#>  CAPRINO VERÃO - EQUINO VERÃO             7.56 2.9 32   2.607  0.2232
#>  CAPRINO VERÃO - OVINO VERÃO              8.76 2.9 32   3.018  0.0987
#>  CAPRINO VERÃO - CAPRINO INVERNO         -6.00 2.9 32  -2.067  0.5113
#>  CAPRINO VERÃO - EQUINO INVERNO          -2.40 2.9 32  -0.828  0.9950
#>  CAPRINO VERÃO - OVINO INVERNO            3.38 2.9 32   1.164  0.9584
#>  CAPRINO VERÃO - CAPRINO PRIMAVERA      -23.74 2.9 32  -8.182  <.0001
#>  CAPRINO VERÃO - EQUINO PRIMAVERA       -15.56 2.9 32  -5.365  0.0002
#>  CAPRINO VERÃO - OVINO PRIMAVERA          5.69 2.9 32   1.962  0.5785
#>  EQUINO VERÃO - OVINO VERÃO               1.19 2.9 32   0.411  1.0000
#>  EQUINO VERÃO - CAPRINO INVERNO         -13.56 2.9 32  -4.674  0.0015
#>  EQUINO VERÃO - EQUINO INVERNO           -9.97 2.9 32  -3.435  0.0383
#>  EQUINO VERÃO - OVINO INVERNO            -4.19 2.9 32  -1.443  0.8724
#>  EQUINO VERÃO - CAPRINO PRIMAVERA       -31.30 2.9 32 -10.789  <.0001
#>  EQUINO VERÃO - EQUINO PRIMAVERA        -23.13 2.9 32  -7.971  <.0001
#>  EQUINO VERÃO - OVINO PRIMAVERA          -1.87 2.9 32  -0.644  0.9991
#>  OVINO VERÃO - CAPRINO INVERNO          -14.76 2.9 32  -5.086  0.0005
#>  OVINO VERÃO - EQUINO INVERNO           -11.16 2.9 32  -3.846  0.0138
#>  OVINO VERÃO - OVINO INVERNO             -5.38 2.9 32  -1.854  0.6477
#>  OVINO VERÃO - CAPRINO PRIMAVERA        -32.50 2.9 32 -11.201  <.0001
#>  OVINO VERÃO - EQUINO PRIMAVERA         -24.32 2.9 32  -8.383  <.0001
#>  OVINO VERÃO - OVINO PRIMAVERA           -3.06 2.9 32  -1.056  0.9765
#>  CAPRINO INVERNO - EQUINO INVERNO         3.60 2.9 32   1.240  0.9411
#>  CAPRINO INVERNO - OVINO INVERNO          9.38 2.9 32   3.231  0.0616
#>  CAPRINO INVERNO - CAPRINO PRIMAVERA    -17.74 2.9 32  -6.115  <.0001
#>  CAPRINO INVERNO - EQUINO PRIMAVERA      -9.57 2.9 32  -3.297  0.0530
#>  CAPRINO INVERNO - OVINO PRIMAVERA       11.69 2.9 32   4.030  0.0086
#>  EQUINO INVERNO - OVINO INVERNO           5.78 2.9 32   1.992  0.5595
#>  EQUINO INVERNO - CAPRINO PRIMAVERA     -21.34 2.9 32  -7.354  <.0001
#>  EQUINO INVERNO - EQUINO PRIMAVERA      -13.16 2.9 32  -4.537  0.0022
#>  EQUINO INVERNO - OVINO PRIMAVERA         8.10 2.9 32   2.790  0.1578
#>  OVINO INVERNO - CAPRINO PRIMAVERA      -27.12 2.9 32  -9.346  <.0001
#>  OVINO INVERNO - EQUINO PRIMAVERA       -18.94 2.9 32  -6.529  <.0001
#>  OVINO INVERNO - OVINO PRIMAVERA          2.32 2.9 32   0.798  0.9961
#>  CAPRINO PRIMAVERA - EQUINO PRIMAVERA     8.18 2.9 32   2.818  0.1495
#>  CAPRINO PRIMAVERA - OVINO PRIMAVERA     29.43 2.9 32  10.145  <.0001
#>  EQUINO PRIMAVERA - OVINO PRIMAVERA      21.26 2.9 32   7.327  <.0001
#> 
#> Degrees-of-freedom method: kenward-roger 
#> P value adjustment: tukey method for comparing a family of 9 estimates
```

``` r
dados %>% 
  filter(excreta == "URINA") %>% 
  group_by(especie, estacao) %>% 
  summarise(
    media = mean(percent_n_volatilizado)
  ) %>% ungroup() %>% 
  ggplot(
    aes(x=estacao, y=media, fill=especie)
  ) +
  geom_col(position="dodge") +
  theme_minimal() +
  scale_fill_viridis_d()
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

## Análise de variância PRIMAVERA - variável transformada

``` r
mod4 <- lm(percent_n_volatilizado_t ~ excreta * especie,
  data= dados %>% filter(estacao == "PRIMAVERA")
)
anova(mod4)
#> Analysis of Variance Table
#> 
#> Response: percent_n_volatilizado_t
#>                 Df Sum Sq Mean Sq F value    Pr(>F)    
#> excreta          1 65.617  65.617 288.939 6.923e-15 ***
#> especie          2  7.427   3.714  16.353 3.304e-05 ***
#> excreta:especie  2 51.741  25.870 113.918 5.612e-13 ***
#> Residuals       24  5.450   0.227                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Comparação de médias

``` r
post_hoc_ExEs <- emmeans(mod4, ~ excreta*especie)
pairs(post_hoc_ExEs, adjust="tukey")
#>  contrast                      estimate    SE df t.ratio p.value
#>  URINA CAPRINO - FEZES CAPRINO    4.282 0.301 24  14.207  <.0001
#>  URINA CAPRINO - URINA EQUINO     0.611 0.301 24   2.027  0.3572
#>  URINA CAPRINO - FEZES EQUINO     5.912 0.301 24  19.616  <.0001
#>  URINA CAPRINO - URINA OVINO      3.471 0.301 24  11.518  <.0001
#>  URINA CAPRINO - FEZES OVINO      2.762 0.301 24   9.163  <.0001
#>  FEZES CAPRINO - URINA EQUINO    -3.671 0.301 24 -12.181  <.0001
#>  FEZES CAPRINO - FEZES EQUINO     1.630 0.301 24   5.409  0.0002
#>  FEZES CAPRINO - URINA OVINO     -0.811 0.301 24  -2.689  0.1144
#>  FEZES CAPRINO - FEZES OVINO     -1.520 0.301 24  -5.044  0.0005
#>  URINA EQUINO - FEZES EQUINO      5.301 0.301 24  17.589  <.0001
#>  URINA EQUINO - URINA OVINO       2.861 0.301 24   9.491  <.0001
#>  URINA EQUINO - FEZES OVINO       2.151 0.301 24   7.136  <.0001
#>  FEZES EQUINO - URINA OVINO      -2.441 0.301 24  -8.098  <.0001
#>  FEZES EQUINO - FEZES OVINO      -3.150 0.301 24 -10.453  <.0001
#>  URINA OVINO - FEZES OVINO       -0.710 0.301 24  -2.355  0.2119
#> 
#> P value adjustment: tukey method for comparing a family of 6 estimates
```

``` r
dados %>% filter(estacao == "PRIMAVERA") %>% 
  group_by(excreta, especie) %>% 
  summarise(
    y= mean(percent_n_volatilizado),
    y_t= mean(percent_n_volatilizado_t)
  ) %>% ungroup() %>% 
  ggplot(
    aes(x=excreta, y=y, fill=especie)
  ) +
  geom_col(position="dodge") +
  theme_minimal() +
  scale_fill_viridis_d()
```

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

## Análise de variância VERÃO - variável transformada

``` r
mod5 <- lm(percent_n_volatilizado_t ~ excreta * especie,
  data= dados %>% filter(estacao == "VERÃO")
)
anova(mod5)
#> Analysis of Variance Table
#> 
#> Response: percent_n_volatilizado_t
#>                 Df Sum Sq Mean Sq F value    Pr(>F)    
#> excreta          1 81.789  81.789 183.163 9.969e-13 ***
#> especie          2 11.190   5.595  12.529 0.0001879 ***
#> excreta:especie  2  9.278   4.639  10.389 0.0005620 ***
#> Residuals       24 10.717   0.447                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Comparação de médias

``` r
post_hoc_ExEs <- emmeans(mod5, ~ excreta*especie)
pairs(post_hoc_ExEs, adjust="tukey")
#>  contrast                      estimate    SE df t.ratio p.value
#>  URINA CAPRINO - FEZES CAPRINO    4.826 0.423 24  11.419  <.0001
#>  URINA CAPRINO - URINA EQUINO     1.878 0.423 24   4.444  0.0021
#>  URINA CAPRINO - FEZES EQUINO     4.080 0.423 24   9.654  <.0001
#>  URINA CAPRINO - URINA OVINO      2.455 0.423 24   5.810  0.0001
#>  URINA CAPRINO - FEZES OVINO      5.335 0.423 24  12.623  <.0001
#>  FEZES CAPRINO - URINA EQUINO    -2.948 0.423 24  -6.975  <.0001
#>  FEZES CAPRINO - FEZES EQUINO    -0.746 0.423 24  -1.765  0.5053
#>  FEZES CAPRINO - URINA OVINO     -2.370 0.423 24  -5.609  0.0001
#>  FEZES CAPRINO - FEZES OVINO      0.509 0.423 24   1.204  0.8304
#>  URINA EQUINO - FEZES EQUINO      2.202 0.423 24   5.210  0.0003
#>  URINA EQUINO - URINA OVINO       0.577 0.423 24   1.366  0.7460
#>  URINA EQUINO - FEZES OVINO       3.457 0.423 24   8.179  <.0001
#>  FEZES EQUINO - URINA OVINO      -1.624 0.423 24  -3.844  0.0090
#>  FEZES EQUINO - FEZES OVINO       1.255 0.423 24   2.969  0.0649
#>  URINA OVINO - FEZES OVINO        2.879 0.423 24   6.813  <.0001
#> 
#> P value adjustment: tukey method for comparing a family of 6 estimates
```

``` r
dados %>% filter(estacao == "VERÃO") %>% 
  group_by(excreta, especie) %>% 
  summarise(
    y= mean(percent_n_volatilizado),
    y_t= mean(percent_n_volatilizado_t)
  ) %>% ungroup() %>% 
  ggplot(
    aes(x=excreta, y=y, fill=especie)
  ) +
  geom_col(position="dodge") +
  theme_minimal() +
  scale_fill_viridis_d()
```

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

## Análise de variância INVERNO - variável transformada

``` r
mod6 <- lm(percent_n_volatilizado_t ~ excreta * especie,
  data= dados %>% filter(estacao == "INVERNO")
)
anova(mod6)
#> Analysis of Variance Table
#> 
#> Response: percent_n_volatilizado_t
#>                 Df Sum Sq Mean Sq  F value    Pr(>F)    
#> excreta          1 79.865  79.865 265.6308 1.758e-14 ***
#> especie          2  2.465   1.233   4.0997  0.029401 *  
#> excreta:especie  2  3.658   1.829   6.0835  0.007291 ** 
#> Residuals       24  7.216   0.301                       
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Comparação de médias

``` r
post_hoc_ExEs <- emmeans(mod6, ~ excreta*especie)
pairs(post_hoc_ExEs, adjust="tukey")
#>  contrast                      estimate    SE df t.ratio p.value
#>  URINA CAPRINO - FEZES CAPRINO   3.7830 0.347 24  10.909  <.0001
#>  URINA CAPRINO - URINA EQUINO    0.4810 0.347 24   1.387  0.7341
#>  URINA CAPRINO - FEZES EQUINO    4.2117 0.347 24  12.145  <.0001
#>  URINA CAPRINO - URINA OVINO     1.4442 0.347 24   4.164  0.0042
#>  URINA CAPRINO - FEZES OVINO     3.7202 0.347 24  10.727  <.0001
#>  FEZES CAPRINO - URINA EQUINO   -3.3020 0.347 24  -9.522  <.0001
#>  FEZES CAPRINO - FEZES EQUINO    0.4287 0.347 24   1.236  0.8150
#>  FEZES CAPRINO - URINA OVINO    -2.3388 0.347 24  -6.744  <.0001
#>  FEZES CAPRINO - FEZES OVINO    -0.0628 0.347 24  -0.181  1.0000
#>  URINA EQUINO - FEZES EQUINO     3.7307 0.347 24  10.758  <.0001
#>  URINA EQUINO - URINA OVINO      0.9632 0.347 24   2.777  0.0962
#>  URINA EQUINO - FEZES OVINO      3.2392 0.347 24   9.340  <.0001
#>  FEZES EQUINO - URINA OVINO     -2.7675 0.347 24  -7.980  <.0001
#>  FEZES EQUINO - FEZES OVINO     -0.4915 0.347 24  -1.417  0.7167
#>  URINA OVINO - FEZES OVINO       2.2760 0.347 24   6.563  <.0001
#> 
#> P value adjustment: tukey method for comparing a family of 6 estimates
```

``` r
dados %>% filter(estacao == "INVERNO") %>% 
  group_by(excreta, especie) %>% 
  summarise(
    y= mean(percent_n_volatilizado),
    y_t= mean(percent_n_volatilizado_t)
  ) %>% ungroup() %>% 
  ggplot(
    aes(x=excreta, y=y, fill=especie)
  ) +
  geom_col(position="dodge") +
  theme_minimal() +
  scale_fill_viridis_d()
```

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->
