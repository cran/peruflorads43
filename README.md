
<!-- README.md is generated from README.Rmd. Please edit that file -->

# peruflorads43

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/peruflorads43)](https://CRAN.R-project.org/package=peruflorads43)
[![R-CMD-check](https://github.com/PaulESantos/peruflorads43/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PaulESantos/peruflorads43/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/PaulESantos/peruflorads43/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PaulESantos/peruflorads43?branch=master)
<!-- badges: end -->

El objetivo de `peruflorads43` es brindar a los usuarios una forma
cómoda de acceder y analizar información sobre las especies de plantas
incluidas en la “Clasificación Oficial de Especies Amenazadas de Flora
Silvestre en el Perú”. Este paquete incluye funciones para buscar
coincidencias parciales de los nombres de las especies.

### Instalación

Para instalar la versión en desarrollo de `peruflorads43` desde GitHub,
se puede utilizar el siguiente comando :

``` r
pak::pak("PaulEsantos/peruflorads43")
```

### Comentarios

1.- En relación a la información contenida en el listado de especies, se
ha identificado el registro duplicado de la especie *Bishopanthus
soliceps*, la cual se encuentra en peligro crítico.

2.- Asimismo, se han detectado errores en la escritura de los nombres de
las algunas especies, se corrigieron estos errores para asegurar la
precisión de la información en incorporada en `peruflorads43`.

    #> # A tibble: 20 × 2
    #>    listed_name              correct_name            
    #>    <chr>                    <chr>                   
    #>  1 Ascidiogine wurdackii    Ascidiogyne wurdackii   
    #>  2 Proboscidea altheaefolia Proboscidea althaeifolia
    #>  3 Jaltomata mioneii        Jaltomata mionei        
    #>  4 Larnax macrocalix        Larnax macrocalyx       
    #>  5 Larnax sagastegui        Larnax sagasteguii      
    #>  6 Solanum chuquidenum      Solanum chiquidenum     
    #>  7 Mutisia wurdacki         Mutisia wurdackii       
    #>  8 Peltogyne altisima       Peltogyne altissima     
    #>  9 Solanum huancabambese    Solanum huancabambense  
    #> 10 Salvia opossitiflora     Salvia oppositiflora    
    #> 11 Ligeophila spp.          Ligeophila              
    #> 12 Masdevallia zebraceae    Masdevallia zebracea    
    #> 13 Telipogon alegria        Telipogon alegriae      
    #> 14 Aspasia psitticina       Aspasia psittacina      
    #> 15 Gongora quinquinervis    Gongora quinquenervis   
    #> 16 Maxillaria rotumdilabia  Maxillaria rotundilabia 
    #> 17 Mormodes revolutum       Mormodes revoluta       
    #> 18 Mormodes rolfeanum       Mormodes rolfeana       
    #> 19 Trichopilia fragans      Trichopilia fragrans    
    #> 20 Trichopilia juninense    Trichopilia juninensis

3.- La lista de especies se ha reducido debido a la actualización de la
información taxonómica. Se han identificado especies que fueron
registradas con nombres diferentes en el momento de la elaboración del
listado, y en la actualidad se ha unificado el nombre utilizado para
referirse a ellas. Por lo tanto, se ha producido una reducción en el
número de especies listadas, pero se ha mejorado la precisión y
consistencia de la información en `peruflorads43`.

    #> # A tibble: 10 × 2
    #>    species_name                      accepted_name       
    #>    <chr>                             <chr>               
    #>  1 Ceroxylon crispum                 Ceroxylon vogelianum
    #>  2 Ceroxylum verriculosum            Ceroxylon vogelianum
    #>  3 Comparettia coccinea              Comparettia coccinea
    #>  4 Comparettia peruviana             Comparettia coccinea
    #>  5 Geonoma undata                    Geonoma undata      
    #>  6 Geonoma weberbaueri               Geonoma undata      
    #>  7 Lycaste ciliata                   Lycaste ciliata     
    #>  8 Lycaste fimbriata                 Lycaste ciliata     
    #>  9 Mila caespitosa subsp. caespitosa Mila caespitosa     
    #> 10 Mila caespitosa subsp. densiseta  Mila caespitosa

4.- Para las especies listadas bajo la categoría de forma:

- Haageocereus acranthus subsp. olowinskianus forma clavispinus (Rauh &
  Backeberg) Ostolaza
- Haageocereus acranthus subsp. olowinskianus forma repandus (Rauh &
  Backeberg) Ostolaza  
- Haageocereus acranthus subsp. olowinskianus forma rubriflorior (Rauh &
  Backeberg) Ostolaza

Estos registros se unifican bajo *Haageocereus acranthus subsp.
olowinskianus*.

- *Haageocereus pseudomelanostele subsp. setosus forma longicomus*
  (Akers) Ostolaza pasa a ser sinónimo de la especie *Haageocereus
  multangularis*.

5.- La información taxonómica de las siguientes especies se conserva
respecto del listado original, debido a que no se cuenta con suficiente
información para la determinación des estado de identificación.

- *Corryocactus quadrangularis*
- *Epidendrum pardothyrsus*
- *Myrosmodes paludosum*
- *Prosthechea cyperifolia*
- *Stanhopea haselowiana*

6.- Se tiene incluido en el listado un el genero *Ligeophila*.

### Como usar `peruflorads43`

``` r
library(peruflorads43)
#> This is peruflorads043_2006_ag 0.1.1
```

- `search_ds043` esta fucnion puede ser empleada con un vector o
  data.frame:

``` r
# vector
splist <- c("Cleistocactus clavispinus",
              "Welfia alfredi",
              "Matucana hayneii")
search_ds043(splist = splist)
#> [1] "Present"        ""               "P_updated_name"

# data.frame

df <- data.frame(splist = splist)
df
#>                      splist
#> 1 Cleistocactus clavispinus
#> 2            Welfia alfredi
#> 3          Matucana hayneii

df$ds_043_2006 <- search_ds043(df$splist)
df
#>                      splist    ds_043_2006
#> 1 Cleistocactus clavispinus        Present
#> 2            Welfia alfredi               
#> 3          Matucana hayneii P_updated_name
# tidyverse - tibble
df <- tibble::tibble(splist = splist)
df |> 
  dplyr::mutate(ds_043_2006 = search_ds043(splist))
#> # A tibble: 3 × 2
#>   splist                    ds_043_2006     
#>   <chr>                     <chr>           
#> 1 Cleistocactus clavispinus "Present"       
#> 2 Welfia alfredi            ""              
#> 3 Matucana hayneii          "P_updated_name"
```

- `category_ds043_2006()` permite la identificación de la categoría en
  la cual esta incorporada la especie.

``` r
category_ds043_2006(splist = splist)
#>              name_submitted           category             accepted_name
#> 1 Cleistocactus clavispinus En peligro critico Cleistocactus clavispinus
#> 2            Welfia alfredi               <NA>                      <NA>
#> 3          Matucana hayneii         Vulnerable           Matucana haynei
#>   accepted_family
#> 1       Cactaceae
#> 2            <NA>
#> 3       Cactaceae
```
