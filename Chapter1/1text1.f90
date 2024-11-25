! page8 1-1
! 1text1.f90
! The latest version (2024/11/25)

program practice

	implicit none
	! Specify variables which you'll use.
	integer :: n
	real :: x
 
	! Use ' ' or " " when you write a sentence or some words
	write(*,*)"Input positive real number x."   
	! Don't use ' ' nor " " when you read or write variables.
	read(*,*)x

	write(*,*)"Input positive integer n." 
	read(*,*)n

	! ** means raising a number to a power.
	! The answer will be written on the display.
	write(*,*)x ** (1.0 / n)   

end program practice
