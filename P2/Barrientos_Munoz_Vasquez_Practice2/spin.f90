module spin
        contains
        subroutine spin_initialization(s_array, Num_part)
                integer, intent(in) :: Num_part
                integer, intent(out) :: s_array(:)
                integer :: i
                do i = 1, Num_part
                        s_array(i) = 2 * mod(int(2*r1279()),2)-1
                end do
        end subroutine spin_initialization

        subroutine spin_change(s_array, Num_part, s_possible_array, S_i)
                integer, intent(in) :: s_array(:)
                integer, intent(out) :: s_possible_array(:)
                INTEGER, intent(out) :: S_i
               
                S_i = mod(int(Num_part*r1279()), Num_part) + 1
                s_possible_array(:) = s_array(:)
                s_possible_array(S_i) = 2 * mod(int(2*r1279()),2)-1
        end subroutine spin_change
end module spin

