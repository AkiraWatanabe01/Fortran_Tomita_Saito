! Page 106 7-10
! 7text10.f90
! The latest version (2024/10/2)

program practice
implicit none

	integer::j
	integer::num, composite
	integer::index = 0
	integer::factor(1:9), exponent(1:9)
	integer::digit_factor(1:9), digit_exponent(1:9)
	character(len = 80)::form
	character(len = 2)::char_num(1:10)=(/"1 ", "2 ", "3 ", "4 ", "5 ", &
					     "6 ", "7 ", "8 ", "9 ", "10"/)

	! Input an integer
	write(*,'(A)',advance='no')'Input an integer (2 - 2147483647):'
	read(*,*)num
	if ((num < 2).or.(2147483647 < num)) stop "The integer should be from 2 to 2147483647"

	! Factorization
	composite = num
	do j = 2, num
		if (mod(composite, j) == 0) then
			index = index + 1
			factor(index) = j
			exponent(index) = 0
			digit_exponent(index) = 1
			do
				if (mod(composite, j) /= 0) exit
				digit_factor(index) = aint(log10(real(j))) + 1
				composite = composite / j
				exponent(index) = exponent(index) + 1
			end do
			if (exponent(index) >= 10) then
				digit_exponent(index) = 2
			end if
		end if
	end do

	! Display exponents 
	do j = 1, index
		if (j == 1) then
			write(*,'(A13)', advance='no')"             "
		end if

		form = "( 1X," // char_num(digit_factor(j)) // "X, I1, 3X)"
		if (digit_exponent(j) > 1) then
			form = "( 1X," // char_num(digit_factor(j)) // "X, I2, 2X)"
		end if
		write(*, form, advance='no')exponent(j)
	end do
	write(*,*)""

	! Display factors 
	write(*, '(1X, I10, A2)', advance='no')num, "= "
	do j = 1, index
		if (j > 1) then
			write(*, '(3X, A1)', advance='no')"*"
		end if
		form = "( 1X, I" // char_num(digit_factor(j)) // ")"
		write(*, form, advance='no')factor(j)
	end do 
	write(*,*)""

end program practice
