! Page 85 6-3
! 6text3.f990
! The latest version (2024/10/7)

program practice
implicit none

	integer::i, j
	integer::position_row, position_col, max_component
	integer::size_row, size_col
	integer,allocatable::dst_matrix(:,:)
 
	! Formation of the matrix
 	write(*,*)"What's the size of your matrix ?"
 
	write(*,*)"Input the size of the row (> 0)"
	read(*,*)size_row
	if (size_row < 1) stop "The size should be 1 or greater."

	write(*,*)"Input the size of the column (> 0)"
	read(*,*)size_col
	if (size_col < 1) stop "The size should be 1 or greater."

	allocate (dst_matrix(1:size_row, 1:size_col))

	! Store the components
 	write(*,*)"Input its components"
	do i = 1, size_row
 		read(*,*)(dst_matrix(i, j), j = 1, size_col)
	end do 

	! Display the matrix
	do i = 1, size_row
 		write(*,*)(dst_matrix(i, j), j = 1, size_col)
	end do 

	! Sorting
 	max_component = dst_matrix(1, 1)
 	do j = 1, size_col
		do i = 1, size_row
			if (max_component <= dst_matrix(i, j)) then
				max_component = dst_matrix(i, j)
				position_row = i
				position_col = j
			end if
		end do
	end do

	! Display the result
	write(*,*)"The maximum value is ", max_component
	write(*,*)"Position, Row: ", position_row, ", Column: ", position_col

end program practice
