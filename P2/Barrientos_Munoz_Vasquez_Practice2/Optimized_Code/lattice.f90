module INITIALIZE_LATTICE
    implicit none
contains

    subroutine Specify_Neighbors(L, nbrs)
        implicit none
        integer, intent(in) :: L
        integer, intent(out) :: nbrs(:,:)   ! dimension (4, Num_part), contiene los vecinos de cada particula
        integer, allocatable :: in(:,:)     ! dimension (2, L), tabla de consulta para la periocidad
        integer :: x, y, i

        allocate(in(2,L))


        ! Condiciones periodicas 'toroidal' 

        do i = 1, L
            in(1,i) = i - 1             ! izq
            in(2,i) = i + 1             ! der
        end do
        in(1,1) = L                     ! arriba
        in(2,L) = 1                     ! abajo

        ! Llenar la array de los vecinos

        i = 0
        do y = 1, L
            do x = 1, L
                i = i + 1
                nbrs(1,i) = in(2,x) + (y-1)*L     ! der
                nbrs(2,i) = in(1,x) + (y-1)*L     ! izq
                nbrs(3,i) = x + (in(2,y)-1)*L     ! abajo
                nbrs(4,i) = x + (in(1,y)-1)*L     ! arriba
            end do
        end do


        deallocate(in)
    end subroutine Specify_Neighbors

end module INITIALIZE_LATTICE

