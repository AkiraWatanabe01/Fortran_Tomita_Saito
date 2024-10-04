! Page 85 6-2(1)
! 6text2-1.f90
! The latest version (2024/10/4)

program practice
implicit none

	integer::i, j, p, q
	integer, dimension(1:9, 1:9)::original_matrix     ! original matrix
	integer, dimension(1:8, 1:8)::b, c, d, e          ! partial matrix 
	integer, dimension(1:8, 1:8)::final_matrix        ! final matrix

	! Original matrix        
	original_matrix(1, 1:9)=(/1, 2, 3, 4, 5, 6, 7, 8, 9/)
	original_matrix(2, 1:9)=(/2, 2, 3, 4, 5, 6, 7, 8, 9/)
	original_matrix(3, 1:9)=(/3, 3, 3, 4, 5, 6, 7, 8, 9/)
	original_matrix(4, 1:9)=(/4, 4, 4, 4, 5, 6, 7, 8, 9/)
	original_matrix(5, 1:9)=(/5, 5, 5, 5, 5, 6, 7, 8, 9/)
	original_matrix(6, 1:9)=(/6, 6, 6, 6, 6, 6, 7, 8, 9/)
	original_matrix(7, 1:9)=(/7, 7, 7, 7, 7, 7, 7, 8, 9/)
 	original_matrix(8, 1:9)=(/8, 8, 8, 8, 8, 8, 8, 8, 9/)
	original_matrix(9, 1:9)=(/9, 9, 9, 9, 9, 9, 9, 9, 9/)

	b(:, :) = 0
	c(:, :) = 0
	d(:, :) = 0
	e(:, :) = 0

	do p = 1, 9
		write(*,*)(original_matrix(p, q), q = 1, 9)
	end do

	i = 10
	do while ((i < 1).or.(i > 9))
		write(*,*)"Input i :"
		read(*,*)i
	end do

 	j = 10
	do while ((j < 1).or.(j > 9))
		write(*,*)"Input j :"
		read(*,*)j
	end do

	if ((i == 1).and.(j == 1)) then
		final_matrix(1:8, 1:8) = original_matrix(2:9, 2:9)
	else if ((i == 1).and.(j == 9)) then
		final_matrix(1:8, 1:8) = original_matrix(2:9, 1:8)
	else if ((i == 9).and.(j == 1)) then
		final_matrix(1:8, 1:8) = original_matrix(1:8, 2:9)
	else if ((i == 9).and.(j == 9)) then
		final_matrix(1:8, 1:8) = original_matrix(1:8, 1:8)
 	else
		b(1:(i - 1), 1:(j - 1)) = original_matrix(1:(i - 1), 1:(j - 1))
		c(i:8, 1:(j - 1)) = original_matrix((i + 1):9, 1:(j - 1))
		d(1:(i - 1), j:8) = original_matrix(1:(i - 1), (j + 1):9)
		e(i:8, j:8) = original_matrix((i + 1):9, (j + 1):9)
		final_matrix(:, :) = b(:, :) + c(:, :) + d(:, :) + e(:, :)
	end if

	! Display the result
	write(*,*)'The result is'
	do p = 1, 8
		write(*,*)(final_matrix(p, q), q = 1, 8)
	end do

end program practice
