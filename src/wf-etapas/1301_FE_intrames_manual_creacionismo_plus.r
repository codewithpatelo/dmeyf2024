#!/usr/bin/env Rscript
cat( "ETAPA  z1301_FE_intrames_manual.r  INIT\n")

# Workflow  Feature Engineering intrames manual artesanal

# inputs
#  * dataset
# output  
#   un dataset algo mas grande:
#     misma cantidad de registros
#     nuevos atributos construidos en forma artesanal y con mucho sufrimiento
#     generados en codigo R,  especificos para este dataset y clase

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE)  # garbage collection

require("data.table", quietly=TRUE)
require("yaml", quietly=TRUE)


#cargo la libreria
# args <- c( "~/labo2024ba" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )
#------------------------------------------------------------------------------

atributos_presentes <- function( patributos )
{
  atributos <- unique( patributos )
  comun <- intersect( atributos, colnames(dataset) )

  return(  length( atributos ) == length( comun ) )
}
#------------------------------------------------------------------------------
# Esta es la parte que los alumnos deben desplegar todo su ingenio
# Agregar aqui sus PROPIAS VARIABLES manuales

AgregarVariables_IntraMes <- function(dataset) {
  cat( "inicio AgregarVariables_IntraMes()\n")
  gc(verbose= FALSE)
  # INICIO de la seccion donde se deben hacer cambios con variables nuevas

  # el mes 1,2, ..12
  if( atributos_presentes( c("foto_mes") ))
    dataset[, kmes := foto_mes %% 100]

  # creo un ctr_quarter que tenga en cuenta cuando
  # los clientes hace 3 menos meses que estan
  # ya que seria injusto considerar las transacciones medidas en menor tiempo
  if( atributos_presentes( c("ctrx_quarter") ))
    dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter) ]

  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]

  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]

  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[
      cliente_antiguedad == 3,
      ctrx_quarter_normalizado := ctrx_quarter * 1.2
    ]

  # variable extraida de una tesis de maestria de Irlanda
  if( atributos_presentes( c("mpayroll", "cliente_edad") ))
    dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

  # se crean los nuevos campos para MasterCard  y Visa,
  #  teniendo en cuenta los NA's
  # varias formas de combinar Visa_status y Master_status
  if( atributos_presentes( c("Master_status", "Visa_status") ))
  {
    dataset[, vm_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
    dataset[, vm_status02 := Master_status + Visa_status]

    dataset[, vm_status03 := pmax(
      ifelse(is.na(Master_status), 10, Master_status),
      ifelse(is.na(Visa_status), 10, Visa_status)
    )]

    dataset[, vm_status04 := ifelse(is.na(Master_status), 10, Master_status)
      + ifelse(is.na(Visa_status), 10, Visa_status)]

    dataset[, vm_status05 := ifelse(is.na(Master_status), 10, Master_status)
      + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]

    dataset[, vm_status06 := ifelse(is.na(Visa_status),
      ifelse(is.na(Master_status), 10, Master_status),
      Visa_status
    )]

    dataset[, mv_status07 := ifelse(is.na(Master_status),
      ifelse(is.na(Visa_status), 10, Visa_status),
      Master_status
    )]
  }


  # combino MasterCard y Visa
  if( atributos_presentes( c("Master_mfinanciacion_limite", "Visa_mfinanciacion_limite") ))
    dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

  if( atributos_presentes( c("Master_Fvencimiento", "Visa_Fvencimiento") ))
    dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]

  if( atributos_presentes( c("Master_Finiciomora", "Visa_Finiciomora") ))
    dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]

  if( atributos_presentes( c("Master_msaldototal", "Visa_msaldototal") ))
    dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]

  if( atributos_presentes( c("Master_msaldopesos", "Visa_msaldopesos") ))
    dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_msaldodolares", "Visa_msaldodolares") ))
    dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mconsumospesos", "Visa_mconsumospesos") ))
    dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mconsumosdolares", "Visa_mconsumosdolares") ))
    dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mlimitecompra", "Visa_mlimitecompra") ))
    dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]

  if( atributos_presentes( c("Master_madelantopesos", "Visa_madelantopesos") ))
    dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_madelantodolares", "Visa_madelantodolares") ))
    dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]

  if( atributos_presentes( c("Master_fultimo_cierre", "Visa_fultimo_cierre") ))
    dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]

  if( atributos_presentes( c("Master_mpagado", "Visa_mpagado") ))
    dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mpagospesos", "Visa_mpagospesos") ))
    dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mpagosdolares", "Visa_mpagosdolares") ))
    dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]

  if( atributos_presentes( c("Master_fechaalta", "Visa_fechaalta") ))
    dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]

  if( atributos_presentes( c("Master_mconsumototal", "Visa_mconsumototal") ))
    dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]

  if( atributos_presentes( c("Master_cconsumos", "Visa_cconsumos") ))
    dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_cadelantosefectivo", "Visa_cadelantosefectivo") ))
    dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mpagominimo", "Visa_mpagominimo") ))
    dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

  # a partir de aqui juego con la suma de Mastercard y Visa
  if( atributos_presentes( c("Master_mlimitecompra", "vm_mlimitecompra") ))
    dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]

  if( atributos_presentes( c("Visa_mlimitecompra", "vm_mlimitecompra") ))
    dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]

  if( atributos_presentes( c("vm_msaldototal", "vm_mlimitecompra") ))
    dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]

  if( atributos_presentes( c("vm_msaldopesos", "vm_mlimitecompra") ))
    dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]

  if( atributos_presentes( c("vm_msaldopesos", "vm_msaldototal") ))
    dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]

  if( atributos_presentes( c("vm_msaldodolares", "vm_mlimitecompra") ))
    dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]

  if( atributos_presentes( c("vm_msaldodolares", "vm_msaldototal") ))
    dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]

  if( atributos_presentes( c("vm_mconsumospesos", "vm_mlimitecompra") ))
    dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mconsumosdolares", "vm_mlimitecompra") ))
    dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]

  if( atributos_presentes( c("vm_madelantopesos", "vm_mlimitecompra") ))
    dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]

  if( atributos_presentes( c("vm_madelantodolares", "vm_mlimitecompra") ))
    dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mpagado", "vm_mlimitecompra") ))
    dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mpagospesos", "vm_mlimitecompra") ))
    dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mpagosdolares", "vm_mlimitecompra") ))
    dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mconsumototal", "vm_mlimitecompra") ))
    dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mpagominimo", "vm_mlimitecompra") ))
    dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]

  # Aqui debe usted agregar sus propias nuevas variables
  # Crear la variable mpasivos_margen_lag1 como un lag de orden 1 de mpasivos_margen
  dataset[, mpasivos_margen_lag1 := shift(mpasivos_margen, n = 1, type = "lag")]

  
  # Supervariable Creacionista de Generación 1
   if( atributos_presentes( c("ctrx_quarter_normalizado", "ctarjeta_visa_transacciones") ))
    dataset[, fem_iter_1_var_625 := rowSums(cbind(ctrx_quarter_normalizado, ctarjeta_visa_transacciones), na.rm = TRUE)]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "cpayroll_trx") ))
    dataset[, fem_iter_1_var_614 := rowSums(cbind(ctrx_quarter_normalizado, cpayroll_trx), na.rm = TRUE)]

   if( atributos_presentes( c("vm_status01", "cpayroll_trx") ))
    dataset[, fem_iter_1_var_644 := vm_status01 / cpayroll_trx]

  if( atributos_presentes( c("cpayroll_trx", "ctarjeta_visa_transacciones") ))
    dataset[, fem_iter_1_var_69 := cpayroll_trx * ctarjeta_visa_transacciones]

  if( atributos_presentes( c("cpayroll_trx", "ctarjeta_visa_transacciones") ))
    dataset[, fem_iter_1_var_285 := cpayroll_trx / ctarjeta_visa_transacciones]

  if( atributos_presentes( c("cpayroll_trx", "vmr_mpagominimo") ))
    dataset[, fem_iter_1_var_288 := cpayroll_trx / vmr_mpagominimo]

  if( atributos_presentes( c("cpayroll_trx", "vmr_mpagominimo") ))
    dataset[, fem_iter_1_var_15 := cpayroll_trx / vmr_mpagominimo]

  if( atributos_presentes( c("vm_status01", "tcallcenter") ))
    dataset[, fem_iter_1_var_796 := rowSums(cbind(vm_status01, tcallcenter), na.rm = TRUE)]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "tcallcenter") ))
    dataset[, fem_iter_1_var_227 := ctrx_quarter_normalizado / tcallcenter]

  if( atributos_presentes( c("vm_status01", "tcallcenter") ))
   dataset[, fem_iter_1_var_796 := rowSums(cbind(vm_status01, ctarjeta_visa_transacciones), na.rm = TRUE)]
  
  if( atributos_presentes( c("ctrx_quarter_normalizado", "cliente_edad") ))
    dataset[, fem_iter_1_var_20 := ctrx_quarter_normalizado * cliente_edad]

  if( atributos_presentes( c("ctarjeta_visa_transacciones", "tcallcenter") ))
    dataset[, fem_iter_1_var_227 := ctarjeta_visa_transacciones / tcallcenter]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "vmr_mpagominimo") ))
    dataset[, fem_iter_1_var_796 := rowSums(cbind(ctrx_quarter_normalizado, vmr_mpagominimo), na.rm = TRUE)]

  if( atributos_presentes( c("cpayroll_trx", "vm_status01") ))
    dataset[, fem_iter_1_var_227 := cpayroll_trx / vm_status01]

  if( atributos_presentes( c("tcallcenter", "vmr_mpagominimo") ))
    dataset[, fem_iter_1_var_548 := tcallcenter / vmr_mpagominimo]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "vmr_mpagominimo") ))
    dataset[, fem_iter_1_var_18 := ctrx_quarter_normalizado / vmr_mpagominimo]

  if( atributos_presentes( c("vm_status01", "tcallcenter") ))
    dataset[, fem_iter_1_var_487 := vm_status01 / tcallcenter] 

  if( atributos_presentes( c("cpayroll_trx", "vmr_mpagominimo") ))
    dataset[, fem_iter_1_var_796 := rowSums(cbind(cpayroll_trx, vmr_mpagominimo), na.rm = TRUE)]


  # Supervariables Creacionistas de Ordenes Superiores (GEN > 1)

  if( atributos_presentes( c("ctrx_quarter_normalizado", "ctarjeta_visa_transacciones", "mpayroll_sobre_edad") ))
    dataset[, fem_iter_2_var_617 := rowSums(cbind(ctrx_quarter_normalizado, ctarjeta_visa_transacciones, mpayroll_sobre_edad), na.rm = TRUE)]

  if( atributos_presentes( c("ctrx_quarter", "ctarjeta_visa_transacciones", "cpayroll_trx", "vmr_mpagominimo") ))
    dataset[, fem_iter_2_var_667 := rowSums(cbind(ctrx_quarter, ctarjeta_visa_transacciones, cpayroll_trx / vmr_mpagominimo), na.rm = TRUE)]

    
  dataset[, fem_iter_5_var_622 := (
  ((mcaja_ahorro + mprestamos_personales) + (mtarjeta_visa_consumo + mpasivos_margen_lag1)) * 
  ((ctrx_quarter + mpayroll) + (mtarjeta_master_consumo)) +
  ((mpayroll + mtarjeta_visa_consumo) + (mprestamos_personales + mcuentas_saldo) +
   ((ctrx_quarter + mpayroll) * (mtarjeta_visa_consumo + mpasivos_margen)))
) + (
  ((mpayroll + mtarjeta_visa_consumo) + (mcaja_ahorro + mprestamos_personales) +
   ((mtarjeta_visa_consumo + mpasivos_margen_lag1) + (mcuenta_debitos_automaticos))) +
  ((mpayroll + mtarjeta_visa_consumo) + (mprestamos_personales + mcuentas_saldo) +
   ((ctrx_quarter + mpayroll) * (mtarjeta_visa_consumo + mpasivos_margen)))
)]


  dataset[, cantidad_na := rowSums(is.na(.SD))]

  # Crear la nueva columna 'cantidad_productos' contando cuántas de las variables específicas NO son NA por cada fila.
  cols_to_count <- c("cproductos", "ccuenta_corriente", "ccaja_ahorro", "ctarjeta_debito", "ctarjeta_visa",
                   "ctarjeta_master", "cprestamos_personales", "cprestamos_prendarios", "cprestamos_hipotecarios",
                   "cplazo_fijo", "cinversion1", "cinversion2", "cseguro_vida", "cseguro_auto",
                   "cseguro_vivienda", "cseguro_accidentes_personales", "ccaja_seguridad", "Master_cconsumos")

  # Utilizar rowSums para contar las columnas que no son NA
  dataset[, cantidad_productos := rowSums(!is.na(.SD)), .SDcols = cols_to_count]

  # Crear la nueva columna 'cantidad_consumos_compras' contando cuántas de las variables específicas NO son NA por cada fila.
  cols_consumos_compras <- c("ctarjeta_debito_transacciones", "ctarjeta_visa_transacciones", "ctarjeta_master_transacciones",
                           "ccuenta_debitos_automaticos", "ctarjeta_visa_debitos_automaticos",
                           "ctarjeta_master_debitos_automaticos", "cpagodeservicios", "cpagomiscuentas",
                           "Visa_cconsumos", "Visa_cadelantosefectivo")

  # Utilizar rowSums para contar las columnas que no son NA
  dataset[, cantidad_consumos_compras := rowSums(!is.na(.SD)), .SDcols = cols_consumos_compras]

  # Crear la nueva columna 'total_transacciones_voluntarias' sumando todas las transacciones voluntarias de cada fila
  cols_transacciones_voluntarias <- c("cpayroll_trx", "cpayroll2_trx", "cforex", "cforex_buy", "cforex_sell",
                                    "ctransferencias_recibidas", "ctransferencias_emitidas", "cextraccion_autoservicio",
                                    "ccheques_depositados", "ccheques_emitidos", "ccheques_depositados_rechazados",
                                    "ccheques_emitidos_rechazados", "ccallcenter_transacciones", "chomebanking_transacciones",
                                    "ccajas_transacciones", "ccajas_consultas", "ccajas_depositos",
                                    "ccajas_extracciones", "ccajas_otras", "catm_trx", "catm_trx_other",
                                    "ctrx_quarter", "Visa_madelantopesos", "Visa_madelantodolares")

  # Utilizar rowSums para sumar las columnas que no son NA
  dataset[, total_transacciones_voluntarias := rowSums(.SD, na.rm = TRUE), .SDcols = cols_transacciones_voluntarias]


  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )

  infinitos_qty <- sum(unlist(infinitos))
  if (infinitos_qty > 0) {
    cat(
      "ATENCION, hay", infinitos_qty,
      "valores infinitos en tu dataset. Seran pasados a NA\n"
    )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )

  nans_qty <- sum(unlist(nans))
  if (nans_qty > 0) {
    cat(
      "ATENCION, hay", nans_qty,
      "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
    )

    cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

  cat( "fin AgregarVariables_IntraMes()\n")
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
cat( "ETAPA  z1301_FE_intrames_manual.r  START\n")
action_inicializar() 


# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

GrabarOutput()

# Agrego las variables manuales
cat( "variables intra mest\n")
AgregarVariables_IntraMes(dataset)

#------------------------------------------------------------------------------
# grabo el dataset
cat( "grabado del dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)
cat( "Finalizado grabado del dataset\n" )


# copia la metadata sin modificar
cat( "grabado de metadata\n")
write_yaml( envg$PARAM$dataset_metadata, 
  file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
  file = "dataset.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
cat( "Fin del programa\n")

envg$OUTPUT$dataset$ncol <- ncol(dataset)
envg$OUTPUT$dataset$nrow <- nrow(dataset)
envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("dataset.csv.gz","dataset_metadata.yml")) 
cat( "ETAPA  z1301_FE_intrames_manual.r  END\n")
