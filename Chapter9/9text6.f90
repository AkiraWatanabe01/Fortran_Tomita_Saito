! Page 172 9-6
! 9text6.f90
! The latest version 2025/1/15

module matrix_operator

        implicit none
        interface operator(.comm.)
                module procedure commutative_matrix
        end interface

contains
  
        function commutative_matrix(x, y) result(z)
 
                implicit none
		integer :: num_row_matrix, num_column_matrix
                integer, intent(in) :: x(1:, 1:), y(1:, 1:)
                integer, allocatable :: z(:, :)

		num_row_matrix = size(x, 1)
		num_column_matrix = size(x, 2)
		allocate(z(1:num_row_matrix, 1:num_column_matrix))
  
		z(:, :) = matmul(x, y) - matmul(y, x)

        end function commutative_matrix

end module matrix_operator

program practice

        use matrix_operator, only : operator(.comm.)
        implicit none
	integer :: i
        integer :: a(1:2, 1:2), b(1:2, 1:2), matrix_commutative(1:2, 1:2)

	! Example
	a(1, 1:2) = (/7, 4/)
	a(2, 1:2) = (/6, 5/)

	b(1, 1:2) = (/9, 2/)
	b(2, 1:2) = (/3, 8/)

	write(*,*)"Matrix a:"
	do i = 1, 2
		write(*,*)a(i, 1:2)
	end do

	write(*,*)"Matrix b:"
	do i = 1, 2
		write(*,*)b(i, 1:2)
	end do

        matrix_commutative = a .comm. b

	write(*,*)"a .comm. b:"
	do i = 1, 2
		write(*,*)matrix_commutative(i, 1:2)
	end do

end program practice
