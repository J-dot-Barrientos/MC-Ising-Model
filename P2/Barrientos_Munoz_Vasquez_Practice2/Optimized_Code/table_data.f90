module TABLE_METROPOLIS
        use Measurement_Observable
        use Variables

        contains
        subroutine Write_Table(table) 
                DOUBLE PRECISION, intent(out) :: table(:)
                integer :: n, dmn, z, idx
                DOUBLE PRECISION :: beta

                beta = 1.0d0 / temp                
                dmn = 2.0d0
                z = dmn*2


                ! Llenar una tabla con todas las posibles diferencia de energia al cambiar un spin (depende del spin de los vecinos), se usara en la actualizacion de Metropolis	

                do n = -2*z, 2*z, 2
                        idx = (n + 2*z) / 2 + 1        
                        table(idx) = exp(-beta * n)
                end do


        end subroutine Write_Table
end module TABLE_METROPOLIS
