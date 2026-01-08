module table_mod
        use Measurement_Observable
        use Variables

        contains
        subroutine write_table(table) 
                DOUBLE PRECISION, intent(out) :: table(:)
                integer :: n, dmn, z, idx
                DOUBLE PRECISION :: beta

                beta = 1.0d0 / temp                
                dmn = 2.0d0
                z = dmn*2
                do n = -2*z, 2*z, 2
                        idx = (n + 2*z) / 2 + 1
                        table(idx) = exp(-beta * n)
                end do
        end subroutine write_table
end module table_mod
