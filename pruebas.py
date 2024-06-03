# Crear el modelo de CoxTimeVarying
ctv = CoxTimeVaryingFitter()

# Ajustar el modelo para las transiciones de cada estado
for transition in [(1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (1, 6), (2, 6), (3, 6), (4, 6)]:
    start_state, end_state = transition
    df_transition = df[(df['estado_actual'] == start_state) & (df['estado_siguiente'] == end_state)]
    if not df_transition.empty:
        print(f"Ajustando el modelo para la transición de {start_state} a {end_state}")
        ctv.fit(df_transition, id_col='ID', event_col='estado_siguiente', start_col='start', stop_col='stop', show_progress=True)
        # Guardar el modelo ajustado para esta transición si es necesario
        # ctv.save_model(f'model_transition_{start_state}_to_{end_state}.pkl')

        # Mostrar el resumen del modelo
        print(ctv.summary)

        # Graficar la importancia de las variables para esta transición
        plt.figure(figsize=(10, 6))
        ctv.plot()
        plt.title(f'Importancia de las Variables para la Transición {start_state} a {end_state}')
        plt.xlabel('Coeficiente')
        plt.ylabel('Variable')
        plt.grid(True)
        plt.savefig(f'/mnt/data/importancia_variables_transition_{start_state}_to_{end_state}.png')  # Guardar la gráfica
        plt.show()

print('fin2')