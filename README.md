# Competencia 02

## Instrucciones

(Semilla: 945799) -- Dataset con clase ternaria creado a partir de clase_ternaria2.py
Correr código 990_workflow_competencia2_final.r (VM 256 GB RAM 24 vCPU)


# MODELO 2 - SEMI-CONSERVADOR C/ CANARITOS
  DT_incorporar_dataset()
  #DC_eliminar_bajas1()
  CA_catastrophe_base(metodo="MachineLearning")
  FEintra_manual_creacionismo()  # Este código agrega unas pocas de las mejores variables creadas en las iteraciones del experimento creacionista
  DR_drifting_base(metodo="rank_cero_fijo")
  FEhist_base() # Lags/Deltas 1 y 2 | Tendencias1
  ultimo <- FErf_attributes_base() # 25 arbolitos
  CN_canaritos_asesinos_base(ratio=1, desvio=0)

  ts8 <- TS_strategy_est8() # Todos meses por default de 990 pero sacando Marzo y Abril

  # la Bayesian Optimization con el semillerio dentro
  ht <- HT_tuning_semillerio(
    semillerio = 70, # semillerio dentro de la Bayesian Optim
    bo_iteraciones = 40  
  )

  fm <- FM_final_models_lightgbm_semillerio( 
    c(ht, ts8), # los inputs
    ranks = c(1), # 1 = el mejor de la bayesian optimization
    semillerio = 70,   # cantidad de semillas finales
    repeticiones_exp = 1  # cantidad de repeticiones del semillerio
  )

  SC_scoring_semillerio( c(fm, ts8) )
  KA_evaluate_kaggle_semillerio()

### Entrega seleccionada: 
* KA-0002_01_051_r1_10500.csv  
  

## Como entregas probé también:

# MODELO 1 - Robatuti
(Este es igual al modelo 2, pero sacaba bajas y asignaba pesos estacionales según el mejor modelo del experimento estacional). No pareció dar buenos resultados pero voy a estar seguro luego de la competencia2. Por cuestiones de tiempo usaré la competencia 2 como experimento para saber que modelo es mejor. Y en base a eso decidiré cuál podré usar en la competencia3.

## Cosas que quedaron en el tintero o a medio camino:

(1) Agregar más variables creacionistas en la ingenieria de atributos manual intrames
(2) Probar xGBoost
(3) Probar pesos obtenidos por BO
(4) Agregar atributos artesanales de lógica booleana o de triple valor (como neutrosofica) para indicar imputaciones de valores 
