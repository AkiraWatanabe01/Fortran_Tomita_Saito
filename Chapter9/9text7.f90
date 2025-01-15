! Page 172 9-7
! 9text7.f90
! The latest version 2025/1/14

module matrix_calculator

contains
	function Tp(matrix) result (transposed_matrix)

		implicit none
		integer :: i
		integer :: num_row_transposed_matrix, num_column_transposed_matrix
		integer, intent(in) :: matrix(1:, 1:)
		integer, allocatable :: transposed_matrix(:, :)

		num_column_transposed_matrix = size(matrix, 1)
		num_row_transposed_matrix = size(matrix, 2)
		allocate(transposed_matrix(1:num_row_transposed_matrix, 1:num_column_transposed_matrix))

		do i = 1, num_row_transposed_matrix
			transposed_matrix(i, 1:num_column_transposed_matrix) = matrix(1:num_column_transposed_matrix, i)
		end do

	end function Tp
end module matrix_calculator

program practice

	use matrix_calculator, only : Tp
	implicit none
	integer :: i
	integer :: num_row_transposed_array, num_column_transposed_array
	integer, dimension(1:3, 1:4) :: a
	integer, allocatable :: transposed_array(:, :), buf_result_embedded_function(:, :)

	num_column_transposed_array = size(a, 1)
	num_row_transposed_array = size(a, 2)
	allocate(transposed_array(1:num_row_transposed_array, 1:num_column_transposed_array))
	allocate(buf_result_embedded_function(1:num_row_transposed_array, 1:num_column_transposed_array))

	! Example
	write(*,*)"Example:"
	a(1, 1:4) = (/1, 2, 3, 4/)
	a(2, 1:4) = (/5, 6, 7, 8/)
	a(3, 1:4) = (/9, 10, 11, 12/)

	do i = 1, 3
		write(*,*)a(i, 1:4)
	end do

	write(*,*)"Module function:"
	transposed_array = Tp(a)
	do i = 1, num_row_transposed_array
		write(*,*)transposed_array(i, 1:num_column_transposed_array)
	end do

	write(*,*)"Embedded function:"
	buf_result_embedded_function = transpose(a)
	do i = 1, num_row_transposed_array
		write(*,*)buf_result_embedded_function(i, 1:num_column_transposed_array)
	end do

end program practice
