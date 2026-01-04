# Información sobre Variables Modificables

En este programa puedes cambiar dos variables principales:

- **T** (Temperatura): controla el valor de la temperatura en la simulación.
- **L** (Tamaño de la red): determina la dimensión o el tamaño del sistema simulado.

Estos valores se pueden modificar de la siguiente forma:

1. **Desde el código:**
   - Encuentra la definición de las variables `T` y `L` en el módulo `Variables`
2. **Para obtener los resultados:**
   - Ejecuta el archivo `Makefiles` con el siguiente comando en la terminal:
     ```
     make -f Makefiles
     ```
   - Alternativamente:
     ```
     gfortran Variables.f90 r1279.f90 spin.f90 Measureament_Observable.f90 lattice.f90 table_data.f90 ran2.f MAIN.f90 -o <ejecutable>       
     ```
   - Una vez finalizada la compilación, ejecuta el archivo de salida (por ejemplo, `P1_MC.out`) con:
     ```
     ./P1_MC.out
     ```
   - Los resultados aparecerán según lo definido en el programa (en la terminal, archivos de salida, etc.).
