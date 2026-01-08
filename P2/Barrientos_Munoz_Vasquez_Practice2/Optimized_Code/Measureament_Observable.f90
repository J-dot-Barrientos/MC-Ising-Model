MODULE MEASUREMENT_OBSERVABLE
IMPLICIT NONE

CONTAINS

  SUBROUTINE Calc_E_M(s_array, nbr_array, L, E, M)
    
    IMPLICIT NONE
    INTEGER, INTENT(in) :: nbr_array(:,:), s_array(:)
    INTEGER, INTENT(in) :: L

    INTEGER, INTENT(out) :: M
    DOUBLE PRECISION, INTENT(out) :: E
 
    INTEGER :: num_part   
    INTEGER :: i,z,j, dmn

    num_part = L**2                                   ! Calculo numero de spins
    dmn = 2                                           ! Calculo numero de dimensiones del sistema
    
    E = 0
    z = dmn*2


    ! Calculo de la Energia del sistema, depende de todos los spines y sus vecinos

    do i=1,num_part
      do j=1,z
        E = E - 0.5_16*dble(s_array(i)) * dble(s_array(nbr_array(j, i)))
      end do
    end do

    ! Calcular Magnetizacion

    M = CALC_M(s_array, num_part)
    
  
  END SUBROUTINE Calc_E_M

! ------------------------------------------------------

  INTEGER FUNCTION Calc_M(s_array, num_part)

    INTEGER, INTENT(in) :: s_array(:)
    INTEGER, INTENT(in) :: num_part

    INTEGER :: M
    INTEGER :: i

    M = 0


    ! Calculo de Magentizacion como la suma de todos los spines del sistema

    do i=1,num_part
      M = M + s_array(i)
    end do

    CALC_M = M


  END FUNCTION Calc_M

END MODULE MEASUREMENT_OBSERVABLE
