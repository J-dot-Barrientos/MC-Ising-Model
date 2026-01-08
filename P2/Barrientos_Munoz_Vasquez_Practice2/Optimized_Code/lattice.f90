module lattice
    implicit none
contains

    subroutine neighbors(L, nbrs)
        implicit none
        integer, intent(in) :: L
        integer, intent(out) :: nbrs(:,:)   ! dimension (4, L*L)
        integer, allocatable :: in(:,:)
        integer :: x, y, i

        ! Allocate periodic lookup table
        allocate(in(2,L))

        ! Left (1) and right (2) indices
        do i = 1, L
            in(1,i) = i - 1
            in(2,i) = i + 1
        end do
        in(1,1) = L
        in(2,L) = 1

        ! Fill neighbor mapping
        i = 0
        do y = 1, L
            do x = 1, L
                i = i + 1
                nbrs(1,i) = in(2,x) + (y-1)*L     ! right
                nbrs(2,i) = in(1,x) + (y-1)*L     ! left
                nbrs(3,i) = x + (in(2,y)-1)*L     ! down
                nbrs(4,i) = x + (in(1,y)-1)*L     ! up
            end do
        end do

        deallocate(in)
    end subroutine neighbors

end module lattice

