! p42 4-4
! 4text4.f90
! The latest version (2024/9/22)

program practice
implicit none
 
	real::x, y        
	integer::z

	write(*,*)'Input real number x.'
	read(*,*)x

	z = int(x)
	y = x - z

	if (y == 0e0) then
		write(*,*)'f = c =', z
	else
		if (x >= 0e0) then
			write(*,*)'f =', z, ', c =', z + 1
		else
			write(*,*)'f =', z - 1, ', c =', z
		end if
	end if

end program practice
