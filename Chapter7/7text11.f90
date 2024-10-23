! Page 106 7-11
! 7text11.f90
! The latest version (2024/10/22)

program practice
implicit none

	character(len = 20)::math_expression
	character(len = 20)::buf_work
	integer::i
	integer::offset = 1
	integer::buf_num, length_math_expression
	integer::buf_array_first(1:20), buf_array_second(1:20)
	integer::num_first = 0, num_second = 0, result
	integer::digit, pos_operator

	! Input
	write(*,*)"Input an arithmetic operation"
	write(*,*)"Usage:"
	write(*,*)"Integer (+, -, *, or /) Integer ="
	read(*, '(A)')math_expression(:)

	! Put characters on the left side
	do i = 1, len(math_expression)
		if (math_expression(i:i) == ' ') cycle
		buf_work(offset:offset) = math_expression(i:i)
		offset = offset + 1
	end do
	length_math_expression = offset - 1

	! Parse
	if (ichar(buf_work(length_math_expression:length_math_expression)) /= ichar('=')) STOP "Invalid input!" 
	buf_num = ichar(buf_work(1:1)) - ichar('0')
	if ((buf_num < 0).or.(9 < buf_num)) STOP "Invalid input (first integer)!"

	write(*, '(A)', advance = 'no')buf_work(:)

	! Parse the first number
	do i = 1, (length_math_expression - 1)
		buf_num = ichar(buf_work(i:i)) - ichar('0')
		if ((buf_num < 0).or.(9 < buf_num)) exit 
		buf_array_first(i) = buf_num
	end do

	pos_operator = i

	! Parse the second number
	offset = 1
	do i = (pos_operator + 1), (length_math_expression - 1)
		buf_num = ichar(buf_work(i:i)) - ichar('0')
		if ((buf_num < 0).or.(9 < buf_num)) STOP "Invalid input (integer)!"
		buf_array_second(offset) = buf_num
		offset = offset + 1
	end do

	! First number
	digit = pos_operator - 1
	do i = 1, digit
		num_first = num_first + buf_array_first(i) * 10 ** (digit - i)
	end do

	! Second number
	digit = length_math_expression - pos_operator - 1
	do i = 1, digit
		num_second = num_second + buf_array_second(i) * 10 ** (digit - i)
	end do

	! Calculation
	select case (ichar(buf_work(pos_operator:pos_operator)))
	case (ichar('+'))
		result = num_first + num_second 
	case (ichar('-'))
		result = num_first - num_second 
	case (ichar('*'))
		result = num_first * num_second 
	case (ichar('/'))
		result = num_first / num_second 
	case default
		STOP "Invalid input (operator)!"
	end select

	write(*,*)result

end program practice
