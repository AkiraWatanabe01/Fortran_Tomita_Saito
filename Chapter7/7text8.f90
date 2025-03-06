! Page 106 7-8
! 7text8.f90
! The latest version (2025/3/6)

program practice

	implicit none
	character(len = 1) :: alphabet
	character(len = 7) :: buf_alphabet(1:182)
	integer :: i
	integer :: row_top, row_bottom
	integer, parameter :: height_alphabet = 7

	do 
		write(*, '(A)', advance = 'no')"Input a lower case alphabetical character :"
		read(*,*)alphabet
		if ((ichar('a') <= ichar(alphabet)) .and. (ichar(alphabet) <= ichar('z'))) exit
	end do

	row_top = height_alphabet * (ichar(alphabet) - ichar('a')) + 1
	row_bottom = row_top + height_alphabet - 1

	! Reading an external file
	open(8, file = '7text8_alphabet.txt')
	do i = 1, 182
		read(8, '(A)')buf_alphabet(i)
	end do
	close(8)

	! Display the result
	do i = row_top, row_bottom
		write(*, '(A)')buf_alphabet(i)
	end do

end program practice
