MODULE MEASUREMENT_OBSERVABLE

	! USE READ_SPIN
	USE spin
IMPLICIT NONE

CONTAINS

  SUBROUTINE CALC_E_M(s_array, nbr_array, L, E, M)
    
    IMPLICIT NONE
    INTEGER, INTENT(in) :: nbr_array(:,:), s_array(:)
    INTEGER, INTENT(in) :: L

    INTEGER, INTENT(out) :: M
    DOUBLE PRECISION, INTENT(out) :: E
 
    INTEGER :: num_part   
    INTEGER :: i,z,j, dmn
    !INTEGER, ALLOCATABLE :: s_array(:)

    num_part = L**2
    dmn = 2

    !ALLOCATE(s_array(num_part))

    ! CALL Read_S(s_array, Num_part)
    
    E = 0
    z = dmn*2

    do i=1,num_part
      do j=1,z
        E = E - 0.5_16*dble(s_array(i)) * dble(s_array(nbr_array(j, i)))
      end do
    end do

    M = CALC_M(s_array, num_part)
     
  END SUBROUTINE CALC_E_M

  INTEGER FUNCTION CALC_M(s_array, num_part)

    INTEGER, INTENT(in) :: s_array(:)
    INTEGER, INTENT(in) :: num_part

    INTEGER :: M
    INTEGER :: i

    M = 0

    do i=1,num_part
      M = M + s_array(i)
    end do

    CALC_M = M

  END FUNCTION CALC_M

END MODULE MEASUREMENT_OBSERVABLE
