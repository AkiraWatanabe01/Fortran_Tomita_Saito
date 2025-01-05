! Page 85 6-2(2)
! 6text2-2.f90
! The latest version (2025/1/5)

program practice

	implicit none
	integer :: i, j, k, l, p, q
	integer, dimension(1:9, 1:9) :: original_matrix
	integer, allocatable :: final_matrix(:, :)

	! Original matrix
 	original_matrix(1, 1:9) = (/1, 2, 3, 4, 5, 6, 7, 8, 9/)
 	original_matrix(2, 1:9) = (/2, 2, 3, 4, 5, 6, 7, 8, 9/)
 	original_matrix(3, 1:9) = (/3, 3, 3, 4, 5, 6, 7, 8, 9/)
 	original_matrix(4, 1:9) = (/4, 4, 4, 4, 5, 6, 7, 8, 9/)
 	original_matrix(5, 1:9) = (/5, 5, 5, 5, 5, 6, 7, 8, 9/)
 	original_matrix(6, 1:9) = (/6, 6, 6, 6, 6, 6, 7, 8, 9/)
 	original_matrix(7, 1:9) = (/7, 7, 7, 7, 7, 7, 7, 8, 9/)
 	original_matrix(8, 1:9) = (/8, 8, 8, 8, 8, 8, 8, 8, 9/)
 	original_matrix(9, 1:9) = (/9, 9, 9, 9, 9, 9, 9, 9, 9/)

	do p = 1, 9
		write(*,*)(original_matrix(p,q), q = 1, 9)
	end do

	write(*,*)"Input the range of the row (from 1 to 9)"
	write(*,*)"(Row) From :"
 	read(*,*)i
	if ((i < 1) .or. (9 < i)) stop "Input an integer from 1 to 9"

	do 
		write(*,*)"(Row) To :"
 		read(*,*)j
		if ((i <= j) .and. (j <= 9)) exit
		write(*, '(A, I1, A)')"Input an integer (", i, " to 9)"
	end do

	write(*,*)"Input the range of the column (from 1 to 9)"
	write(*,*)"(Column) From :"
 	read(*,*)k
	if ((k < 1) .or. (9 < k)) stop "Input an integer from 1 to 9"

	do 
		write(*,*)"(Column) To :"
 		read(*,*)l
		if ((k <= l) .and. (l <= 9)) exit
		write(*, '(A, I1, A)')"Input an integer (", k, " to 9)"
	end do

	! Formation of the final matrix
	allocate(final_matrix(1:(j - i + 1), 1:(l - k + 1)))
 	final_matrix(:, :) = original_matrix(i:j, k:l)

	! Display the result
	write(*,*)"The result is"
	do p = 1, (j - i + 1)
		write(*,*)(final_matrix(p, q), q = 1, (l - k + 1))
	end do

end program practice
