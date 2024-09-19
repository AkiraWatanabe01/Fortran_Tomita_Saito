! p28 3-3
! 3text3.f90
! The latest version (2024/9/20)

program practice
implicit none

	real::a, b, c, x1, x2
 	real::disciminant

	write(*,*)'Input coefficients.'
	write(*,*)'a = ?'
	read(*,*)a
	write(*,*)'b = ?'
	read(*,*)b
	write(*,*)'c = ?'
	read(*,*)c

	if (a == 0) then
		if (b /= 0) then
			x1 = -c / b
			write(*,*)x1
		else
			if (c==0) then
   				write(*,*)'Solution indetermine'
			else
				write(*,*)'No solution'
    			end if 
		end if
	else
 		discriminant = b * b - 4e0 * a * c
		if (discriminant > 0) then
			x1=(-b + sqrt(discriminant)) / (2e0 * a)   
			x2=(-b - sqrt(discriminant)) / (2e0 * a)   
			write(*,*)x1
			write(*,*)x2
		else if (discriminant == 0) then
        		x1 = -b / (2e0 * a)
			write(*,*)x1
   		else
			write(*,*)'Complex roots'
		end if
	end if

end program practice
