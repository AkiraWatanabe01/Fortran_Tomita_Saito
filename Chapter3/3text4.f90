! p28 3-4
! 3text4.f90
! The latest version (2024/9/21)

program practice
implicit none

	integer::n, remainder

	write(*,*)'Input integer n: '
	read(*,*)n
 
	remainder = mod(n, 7)
 
	! SELECT CASE statement
	select case (remainder)
	case (0)
		write(*,*)'Sunday'
	case (1)
		write(*,*)'Monday'
	case (2)
		write(*,*)'Tuesday'
	case (3)
		write(*,*)'Wednesday'
	case (4)
		write(*,*)'Thursday'
	case (5)
		write(*,*)'Friday'
	case (6)
		write(*,*)'Saturday'
	end select

end program practice
