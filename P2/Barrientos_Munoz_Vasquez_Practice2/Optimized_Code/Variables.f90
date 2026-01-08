MODULE VARIABLES
IMPLICIT NONE

  DOUBLE PRECISION :: temp = 2.0
  INTEGER :: L = 4
  INTEGER :: num_MCS = 1000000     ! MCS : N (Numero de spines en el sistema) intentos de cambiar el spin
  INTEGER :: num_mes = 1            ! Semilla del generador de numeros random y cada cuantas MCS se calculan los observables

END MODULE VARIABLES
