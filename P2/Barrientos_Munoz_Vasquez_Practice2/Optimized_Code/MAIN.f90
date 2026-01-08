PROGRAM P1_MC
        USE TABLE_METROPOLIS
        USE SPIN
        USE VARIABLES
        USE MEASUREMENT_OBSERVABLE
        USE INITIALIZE_LATTICE

IMPLICIT NONE

  DOUBLE PRECISION :: E
  INTEGER :: M, N, i, x, S_i, Delta_M
  INTEGER, ALLOCATABLE :: nbr_array(:,:), s_array(:), s_possible_array(:)
  DOUBLE PRECISION, ALLOCATABLE :: table(:)
  INTEGER :: idx
  CHARACTER(len=100) :: fname_out
  
DOUBLE PRECISION :: t_start, t_end, t_elapsed, fps

  N = L**2
  E = 0
  M = 0
  write(fname_out,'("OBS_L",I0,"_T",F0.2,"_MCS",I0, "_mes",I0, ".dat")') L, temp, num_MCS, num_mes

  call setr1279(num_mes)                                                   ! Generador numeros random de 1-0
  
  allocate(nbr_array(4, N), s_array(N), s_possible_array(N), table(9))


  CALL Specify_Neighbors(L, nbr_array)                                     ! Crear array(4:Num_part) con los vecinos de cada spin
  
  CALL Spin_Initialization(s_array, N)                                     ! Inicializar cada spin aleatoriamente
  
  CALL Write_Table(table)                                                  ! Generacion de tabla para el alagoritmo de Metropolis

  CALL Calc_E_M(s_array, nbr_array, L, E, M)                               ! Calcular observables (Energia total y magnetizacion)

  open (unit=10, file=trim(fname_out), status="replace")                   ! Indicar fichero en el que se escribiran los datos de salida

  call cpu_time(t_start)
  do x = 1, num_MCS                                                        ! Loop para los num_MCS indicados
      do i = 1, N                                                          ! Loop para el numero de spines en el sistema

          CALL Monte_Carlo_Update(nbr_array, s_array, N, table, E, M)      ! Aplicar el algoritmo Metropolis para intentar cambiar el spin

      end do
      if (mod(x, num_mes) == 0) then                                       ! Escribir en el fichero de salida cada num_mes MCS
          CALL Write_Output(E, M)
      end if
  end do

   !         call cpu_time(t_end)
  ! t_elapsed = t_end - t_start
  ! fps = dble(num_MCS) * dble(N) / t_elapsed         ! Necesario para calcular intentos de cambiar de spin por segundo
  ! print *, "CPU time (s) =", t_elapsed
  ! print *, "MC speed =", fps, " flips/s"
 !  print *, L, temp, num_MCS, num_mes

  print *, "MC Simulation completed"


END PROGRAM P1_MC

  SUBROUTINE Write_Output(E,M)
  IMPLICIT NONE

    DOUBLE PRECISION, INTENT(in) :: E
    INTEGER, INTENT(in) :: M

    write(10, *) E, M
 
 END SUBROUTINE Write_Output
