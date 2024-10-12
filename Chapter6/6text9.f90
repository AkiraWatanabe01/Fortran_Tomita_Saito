! Page 86  6-9
! 6text9.f90 
! The latest version (2024/10/12)

program practice
implicit none

	integer::i
	integer::num, quotient, index = 0
	integer, allocatable::exponent(:), factor(:)
	character(len = 1), allocatable::operator(:)

	write(*,*)"Input a positive integer:"
	read(*,*)num
	if (num < 1) stop "The number should be positive."
	allocate(exponent(1:num), factor(1:num))
 	exponent(:) = 0
	quotient = num

	do i = 2, (num - 1)
		if (mod(quotient, i) == 0) then
			index = index + 1
			factor(index) = i
			do
				if (mod(quotient, i) /= 0) exit
				quotient = quotient / i
				exponent(index) = exponent(index) + 1
			end do
		end if
	end do

	allocate(operator(1:index))
	operator(1) = ' '
	operator(2:index) = '*'

 	if (exponent(1) == 0) then       
		! num is a prime number   
		write(*,*)num, "=", num
	else
		! num is a composite number
		write(*,*)num, "=", (operator(i), factor(i), "**", exponent(i), i = 1, index)
	end if

end program practice
