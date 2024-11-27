import polars as pl

# Cargar el dataset
dataset = pl.read_csv("/kaggle/input/competencia-3/competencia_03_crudo.csv", infer_schema_length=50000)
#%% Pivoteo

# Pivoteo con las fechas para comparar los meses. Tomo el valor de active_quarter para simplificar los reemplazos
tabla = dataset[['numero_de_cliente', 'foto_mes', 'active_quarter']].pivot(on='foto_mes', index='numero_de_cliente', values='active_quarter')

# Reemplazar valores nulos por False y valores enteros por True
tabla_bool = tabla.with_columns([
    pl.col(column).is_not_null().alias(column)
    for column in tabla.columns[1:]
])

#%% Función de evaluación

# Con esto evalúo cada uno de los primeros 4 meses en función a los dos siguientes
# Para el caso de clientes intermitentes se los considera como que se fueron en el mes en cuestión
# y se evalúa nuevamente a partir que se reincorporan.

def evaluar_condiciones(tabla: pl.DataFrame, col1: str, col2: str, col3: str) -> pl.Expr:
    evaluacion = (
        pl.when(pl.col(col1) == False)
        .then(None)
        .when(
            (pl.col(col1) == True) & (pl.col(col2) == True) & (pl.col(col3) == True)
        )
        .then(pl.lit("CONTINUA"))
        .when(
            (pl.col(col1) == True) & (pl.col(col2) == True) & (pl.col(col3) == False)
        )
        .then(pl.lit("BAJA+2"))
        .when(
            (pl.col(col1) == True) & (pl.col(col2) == False)
        )
        .then(pl.lit("BAJA+1"))
        .otherwise(None)
    )

    return evaluacion

#%% Genero los resultados para los 4 primeros meses

lista_meses = ['202101', '202102', '202103', '202104', '202105', '202106']

for i in range(4):
    tabla_bool = tabla_bool.with_columns(evaluar_condiciones(tabla_bool, lista_meses[i], lista_meses[i+1], lista_meses[i+2]).alias(f"evaluacion_{lista_meses[i]}"))

#%% Unpivoteo y hago join

clase_cliente = tabla_bool.unpivot(index='numero_de_cliente')
clase_cliente = clase_cliente.rename({
    "variable": "foto_mes",
    'value': 'clase_ternaria'
})
# Descarto columnas auxiliares
clase_cliente = clase_cliente.filter(pl.col("foto_mes").str.starts_with("e"))
# Paso en limpio las fechas
clase_cliente = clase_cliente.with_columns(pl.col("foto_mes").str.slice(-6).cast(pl.Int64).alias("foto_mes"))

#%% Realizar la unión usando 'numero_de_cliente' y 'foto_mes' como claves
resultado = dataset.join(
    clase_cliente,
    on=["numero_de_cliente", "foto_mes"],
    how="left"
)

# Guardar resultados
resultado.write_csv("competencia_03.csv.gz", compression="gzip", separator=",")  
resultado.write_parquet("competencia_03.parquet")
