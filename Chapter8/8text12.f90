! Page 141 8-12
! 8text12.f90
! The latest version (2024/12/31)

recursive subroutine permutation(array_work, array_store_each_digit, size_array, position_digit)

        implicit none
        integer :: i
        integer :: num_tmp, index_max
        integer, intent(in) :: size_array, position_digit
        integer :: buf_store_original_array(1:size_array)
        integer, intent(inout) :: array_work(1:size_array), array_store_each_digit(1:size_array)

        if (position_digit == size_array + 1) then
                call output()
        else
                index_max = size_array - position_digit + 1
                do i = 1, index_max
                        array_store_each_digit(position_digit) = array_work(i)
                        if (position_digit < size_array) then
                                buf_store_original_array(1:index_max) = array_work(1:index_max)
                                num_tmp = array_work(index_max)
                                array_work(index_max) = array_work(i)
                                array_work(i) = num_tmp
                        end if

                        call permutation(array_work, array_store_each_digit, size_array, position_digit + 1)
 
                        if (position_digit < size_array) then
                                array_work(1:index_max) = buf_store_original_array(1:index_max)
                        end if
                end do
        end if

contains
        subroutine output()
        
                implicit none
                character(len = 80) :: frmt
                integer :: denominator, numerator, solution
                integer, save :: counter = 0

                numerator = 0
                do i = 1, 5
                        numerator = numerator + array_store_each_digit(i) * 10 ** (5 - i)
                end do

                denominator = 0
                do i = 6, 9
                        denominator = denominator + array_store_each_digit(i) * 10 ** (9 - i)
                end do

                solution = numerator / denominator
                if (mod(numerator, denominator) == 0) then
                        counter = counter + 1
                        frmt = "(I3, ':', 1X, I5, 1X, '/', 1X, I4, 1X, '=', 1X, I3)"
                        write(*, frmt)counter, numerator, denominator, solution
                end if
        end subroutine output

end subroutine permutation

program practice

        implicit none
        integer :: i
        integer, parameter :: size_array = 9
        integer, allocatable :: array_work(:), array_store_each_digit(:)

        allocate(array_work(1:size_array), array_store_each_digit(1:size_array))
        do i = 1, size_array
                array_work(i) = i
                array_store_each_digit(i) = 0
        end do

        call permutation(array_work, array_store_each_digit, size(array_work), 1)

end program practice
