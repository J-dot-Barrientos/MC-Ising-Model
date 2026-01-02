PROGRAM P1_MC
        USE table_mod
        USE spin
        USE VARIABLES
        USE MEASUREMENT_OBSERVABLE
        USE lattice

IMPLICIT NONE

  REAL*16 :: E
  INTEGER :: M, N, MCS, x, S_i
  INTEGER, ALLOCATABLE :: nbr_array(:,:), s_array(:), s_possible_array(:)
  REAL*16, ALLOCATABLE :: table(:)
  INTEGER :: Delta_E, idx
  REAL*4 :: r1279
  N = L**2
        
  call setr1279(1234)
  allocate(nbr_array(4, N), s_array(N), s_possible_array(N), table(9))

  CALL neighbors(L, nbr_array)
  
  CALL spin_initialization(s_array, N)
  
  CALL write_table(table)

  open (unit=10, file="Energy.dat", status="replace") 
  do x = 1, num_MCS
        CALL spin_change(s_array, N, s_possible_array, S_i)
        Delta_E = 2 * s_possible_array(S_i) * sum(s_array(nbr_array(:, S_i)))
	if (Delta_E < 0) then
    		s_array(S_i) = s_possible_array(S_i)
	else
    		idx = (Delta_E + 2*4)/2 + 1
	        if (r1279() < table(idx)) then
	            s_array(S_i) = s_possible_array(S_i)
		end if
	end if
        if (mod(x, 10000) == 0) then
                CALL CALC_E_M(s_array, nbr_array, L, E, M)
                write(10, *) E
                print *, x
          end if
  end do

  CALL CALC_E_M(s_array, nbr_array, L, E, M)
  CALL Output(E,N)

END PROGRAM P1_MC

  SUBROUTINE Output(E,N)
  IMPLICIT NONE

    REAL*16, INTENT(in) :: E
    INTEGER, INTENT(in) :: N
    print'(A, F10.3)', "Energy = ", E/N

  END SUBROUTINE Output

