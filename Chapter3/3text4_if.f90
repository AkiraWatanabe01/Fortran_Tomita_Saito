! p28 3-4
! 3text4_if.f90
! The latest version (2024/12/22)

program practice

	implicit none
	integer :: n, remainder

	write(*,*)"Input integer n: "
	read(*,*)n
 
	remainder = mod(n, 7)
 
	! IF statement (SELECT CASE statement is better in this exercise!) 
	if (remainder == 0) then          
		write(*,*)"Sunday"
	else if (remainder == 1) then 
		write(*,*)"Monday"
	else if (remainder == 2) then 
		write(*,*)"Tuesday"
	else if (remainder == 3) then 
		write(*,*)"Wednesday"
	else if (remainder == 4) then 
		write(*,*)"Thursday"
	else if (remainder == 5) then 
		write(*,*)"Friday"
	else  
		write(*,*)"Saturday"
	end if

end program practice
