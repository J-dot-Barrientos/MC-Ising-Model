PROGRAM P1_MC
        USE table_mod
        USE spin
        USE VARIABLES
        USE MEASUREMENT_OBSERVABLE
        USE lattice

IMPLICIT NONE

  DOUBLE PRECISION :: E
  INTEGER :: M, N, i, x, S_i
  INTEGER, ALLOCATABLE :: nbr_array(:,:), s_array(:), s_possible_array(:)
  DOUBLE PRECISION, ALLOCATABLE :: table(:)
  INTEGER :: Delta_E, idx, meas_step
  REAL :: r1279
  ! DOUBLE PRECISION :: t_start, t_end, t_elapsed, fps
  ! INTEGER(8) :: total_flips

  N = L**2

  ! total_flips = 1_8 * num_MCS * N

  call setr1279(num_mes)
  
  allocate(nbr_array(4, N), s_array(N), s_possible_array(N), table(9))

  CALL neighbors(L, nbr_array)
  
  CALL spin_initialization(s_array, N)
  
  CALL write_table(table)

  open (unit=10, file="Energy4.dat", status="replace")

  ! call cpu_time(t_start)

  do x = 1, num_MCS
      do i = 1, N
          CALL spin_change(s_array, N, s_possible_array, S_i)
          Delta_E = -2 * s_possible_array(S_i) * sum(s_array(nbr_array(:, S_i)))
          if (Delta_E < 0) then
              s_array(S_i) = s_possible_array(S_i)
              else
                idx = (Delta_E + 2*4)/2 + 1
                if (r1279() < table(idx)) then
                    s_array(S_i) = s_possible_array(S_i)
                end if
          end if
      end do
          
      if (mod(x, num_mes) == 0) then
          CALL CALC_E_M(s_array, nbr_array, L, E, M)
          write(10, *) E, M
          print *, x
      end if
  end do

  ! call cpu_time(t_end)
  ! t_elapsed = t_end - t_start
  ! fps = dble(num_MCS) * dble(N) / t_elapsed

  CALL CALC_E_M(s_array, nbr_array, L, E, M)
  CALL Output(E,N)
  print *, "MC Simulation completed"
  ! print *, "CPU time (s) =", t_elapsed
  ! print *, "Attempted flips =", total_flips
  ! print *, "MC speed =", fps, " flips/s"

END PROGRAM P1_MC

  SUBROUTINE Output(E,N)
  IMPLICIT NONE

    DOUBLE PRECISION, INTENT(in) :: E
    INTEGER, INTENT(in) :: N
    print'(A, F10.3)', "Energy = ", E/N

  END SUBROUTINE Output
