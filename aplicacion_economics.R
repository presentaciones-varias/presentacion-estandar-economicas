### Capacitacion: Aplicacion del estandar de calidad para la evaluación de estimaciones en encuestas económicas ###

### instalar paquete calidad ###
## desde cran:
install.packages('calidad')

## desde github:
# install.packages('devtools')
# devtools::install_github("inesscc/calidad")


# paquetes ----------------------------------------------------------------

library(calidad)    # ultima version: v0.8.0 sessionInfo()
library(survey)     # para crear diseño de muestra
library(dplyr)      # procesamiento de datos


# revision datos ----------------------------------------------------------

# usaremos la base ELE7 disponible en el paquete

str(ELE7)

# diseño de muestra -------------------------------------------------------

options(survey.lonely.psu = 'remove')    # indicamos tratamiento upm

## diseño de muestra con factor de expansión transversal
dc_ele <- svydesign(ids = ~rol_ficticio,
                    weights = ~fe_transversal,
                    strata = ~estrato,
                    fpc = ~pob,                 # correccion por poblacion finita
                    data = ELE7)
dc_ele


# estimacion  -------------------------------------------------------------

# Para estimar la productividad salarial (razón entre el Valor agregado 2022 y Remuneración total bruta del personal contratado)
# debemos utilizar la funcion create_prop(), en donde le indicaremos el numerador y denominador:

## sin dominios
create_prop(var= 'VA_2022f',
            denominator = 'REMP_TOTAL',
            design = dc_ele)


# segun codigo de tamaño y codido de actividad
prod_salarial <- create_prop('VA_2022f',
                             denominator = 'REMP_TOTAL',
                             domains = 'cod_tamano+ cod_actividad',
                             design = dc_ele)

prod_salarial


# evaluacion --------------------------------------------------------------

# Recordemos que una de las principales nuevas caracteristicas del estandar de economicas es que posee una etapa de recuperacion muestral
# Por lo que necesitamos el tamaño de muestra objetivo de nuestros datos, para ello contamos con el dataframe ELE7_n_obj que nos indica el tamaño de muestra objetivo desagrado por tamaño y actividad economica.

str(ELE7_n_obj)   # revision data


assess(prod_salarial,                # indicamos estimacion con funcion create
       scheme = 'chile_economics',   # indicamos criterio
       domain_info = T,              # ¿pertenece a dominio planificado?
       table_n_obj = ELE7_n_obj)






# Oops! notemos que las clases de los dominios de nuestra tabla ELE7_n_obj no coincide con prod_salarial y tenemos diferentes codificaciones en la variable cod_actividad.

str(ELE7_n_obj)
str(prod_salarial)

# modifiquemos la tabla ELE7_n_obj:
ELE7_n_obj2 <- ELE7_n_obj %>%
  mutate(cod_actividad = cod_actividad_letra,
         cod_tamano = as.character(cod_tamano))


## evaluación indicando tabla df_n_obj modificada
evaluacion_estandar <- assess(prod_salarial,
                              scheme = 'chile_economics',
                              domain_info = T,
                              table_n_obj = ELE7_n_obj2,     # tabla con tamaños objetivos corregidos
                              ratio_between_0_1 = FALSE)



# revisión del estado final para n<30:
evaluacion_estandar %>%
  filter(n<30)

# Como todos los registros no cumplen con la tasa de cumplimiento, tendremos que la estimacion sera no fiable.



# Modifiquemos un tamaño objetivo para ver el cambio de evaluacion:

ELE7_n_obj3 <- ELE7_n_obj2 %>%
  mutate(n_obj = ifelse(cod_tamano == '2' & cod_actividad== 'B', 20, n_obj))


evaluacion_modificada <- assess(prod_salarial,
                                scheme = 'chile_economics',
                                domain_info = T,
                                table_n_obj = ELE7_n_obj3,
                                ratio_between_0_1 = FALSE)

evaluacion_modificada %>%
  filter(n<30)


# ¿Que ocurre si por algún motivo el paquete no acepta correctamente mi dataframe con n_obj? ----
# El paquete calidad es capaz de identificar la columna n_obj en la tabla generada por las funciones create_*
# Por lo que podemos unir esta columna directamente a la tabla:

# tabla prod_salarial le agregamos los tamaños objetivos:
prod_salarial2 <- prod_salarial %>%
  left_join(ELE7_n_obj2 %>% select(-cod_actividad_letra),
            by = c('cod_tamano', 'cod_actividad'))

# verificamos que no se hayan generado NAs:
is.na(prod_salarial2$n_obj) %>% sum


# realizamos la evaluación:
evaluacion_estandar2 <- assess(prod_salarial2,
                               scheme = 'chile_economics',
                               domain_info = T,
                               ratio_between_0_1 = FALSE)



# ¿Que pasa si no cuento con los tamaños de muestra objetivo? -----
# El paquete asignará como no fiables a las estimaciones que tengan un tamaño de muestra objetivo menor a 30

evaluacion_sin_nobj <- assess(prod_salarial, scheme = 'chile_economics', domain_info = T, ratio_between_0_1 = FALSE)

## revisión del estado final para n<30
evaluacion_sin_nobj %>%
  filter(n<30)


# evaluacion del tabulado -------------------------------------------------

# Otro parametro disponible en la funcion assess es "publish", el cual nos ayudara indicando si el tabulado es publicable o no

evaluacion_estandar_publish <- assess(prod_salarial,
                                      scheme = 'chile_economics',
                                      domain_info = TRUE,
                                      table_n_obj = ELE7_n_obj2,     # tabla con tamaños objetivos corregidos
                                      ratio_between_0_1 = FALSE,
                                      publish = TRUE)


evaluacion_estandar_publish %>% head()


## comparemos el resultado
evaluacion_estandar_publish$label %>% table() %>% prop.table()



