# Información sobre Variables Modificables

Se encuentran en Variables.f90:

- **T** : controla el valor de la temperatura en la simulación.
- **L** : determina la dimensión o el tamaño del sistema simulado.
- **num_MCS**: número total de Monte Carlo steps.
- **num_meas**: número de Monte Carlo steps entre guardados de observables.

Estos valores se pueden modificar de la siguiente forma:

1. **Desde el código:**
   - Encuentra la definición de las variables `T` y `L` en el módulo `Variables`
2. **Para obtener los resultados:**
   - Pega la línea de compilación:
     ```
     gfortran -O3 -march=native -funroll-loops -ffast-math Variables.f90 r1279.f90 spin.f90 Measureament_Observable.f90 lattice.f90 table_data.f90 ran2.f MAIN.f90 -o ising_opt
       
     ```
   - Una vez finalizada la compilación, ejecuta el archivo de salida (por ejemplo, `ising_opt`) con:
     ```
     ./ising_opt
     ```
   - Los resultados aparecerán según lo definido en el programa (en la terminal, archivos de salida, etc.).
