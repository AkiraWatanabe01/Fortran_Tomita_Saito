! Page28 3-7
! 3text7.f90 
! The latest version (2024/9/17)

program practice
implicit none

	integer::i, n

	write(*,*)"Input an integer:"
	read(*,*)i
	write(*,*)"Input the number of bits:"
	read(*,*)n

	if (n > 32) then
		write(*,*)i
	else if (n <= 0) then
		write(*,*)0
	else
		write(*,*)ibits(i, 0, n)
	end if

end program practice
