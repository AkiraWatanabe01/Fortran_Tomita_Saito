! Page 106 7-12
! 7text12.f90
! The latest version (2025/3/6)

program practice

	implicit none
	character(len = 40) :: buf_num_a, buf_num_b          ! buffer for num_a and num_b
	character(len = 40) :: cha_num_a, cha_num_b          ! aligned num_a and num_b
	character(len = 41) :: buf_sum_ab                    ! buffer for the sum of num_a and num_b
	character(len = 41) :: cha_sum_ab                    ! aligned sum
	character(len = 80) :: buf_product_ab                ! buffer for the product of num_a and num_b
	character(len = 80) :: cha_product_ab                ! aligned product
	character(len = 80) :: form                          ! format for the mathematical expression
	character(len = 2) :: char_num(1:40)=(/"1 ", "2 ", "3 ", "4 ", "5 ", "6 ", "7 ", "8 ", "9 ", "10", &
					       "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", &
					       "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", &
					       "31", "32", "33", "34", "35", "36", "37", "38", "39", "40"/)
	integer :: i, j, k
	integer :: length_num_a, length_num_b                ! length of num_a and num_b
	integer :: index_digit                               ! index
	integer :: buf_length, buf_num                       ! buffer
	integer :: sum_num                                   ! the sum of numbers
	integer :: increase_digit                            ! increase in digit for summation
	integer :: increase_digit_1, increase_digit_2        ! increase in digit for multiplication
	integer :: sum_each_digit, product_num_plus
	integer :: sum_ab                                    ! the sum of num_a and num_b
	integer :: product_ab = 0                            ! the product of num_a and num_b
	integer, dimension(1:40, 1:41) :: product_digit
	integer, dimension(1:40) :: num_a = 0, num_b = 0

	! Input 2 numbers
	write(*,*)"Input the number of A."
	read(*, '(A)')buf_num_a(:)

	write(*,*)"Input the number of B."
	read(*, '(A)')buf_num_b(:)

	! Put characters on the left side
	cha_num_a = adjustl(buf_num_a)
	length_num_a = len_trim(cha_num_a)
	cha_num_b = adjustl(buf_num_b)
	length_num_b = len_trim(cha_num_b)

	! Parse
	do i = length_num_a, 1, -1
		buf_num = ichar(cha_num_a(i:i))
		if ((buf_num < ichar('0')) .or. (ichar('9') < buf_num)) STOP "Invalid input"
		num_a(length_num_a - i + 1) = buf_num - ichar('0')
	end do
	do i = length_num_b, 1, -1
		buf_num = ichar(cha_num_b(i:i))
		if ((buf_num < ichar('0')) .or. (ichar('9') < buf_num)) STOP "Invalid input"
		num_b(length_num_b - i + 1) = buf_num - ichar('0')
	end do

	! Summation
	increase_digit = 0
	do i = 1, 40
		sum_num = num_a(i) + num_b(i) + increase_digit
		sum_ab = mod(sum_num, 10) + ichar('0')
		buf_sum_ab(i:i) = char(sum_ab)
		increase_digit = sum_num / 10
	end do
	sum_ab = increase_digit + ichar('0')
	buf_sum_ab(41:41) = char(sum_ab)

	! Reverse
	do i = 41, 2, -1
		if (buf_sum_ab(i:i) /= '0') exit
	end do
	index_digit = i

	do i = 1, index_digit
		cha_sum_ab(i:i) = buf_sum_ab((index_digit - i + 1):(index_digit - i + 1))
	end do

	! Display the summation
	write(*,*)"Summation:"
	form = "(1X," // char_num(length_num_a) // "I1, 1X, '+', 1X," // char_num(length_num_b) // "I1, 1X, '=')"
	write(*, form)num_a(length_num_a:1:-1), num_b(length_num_b:1:-1)
	write(*, '(1X, A)')cha_sum_ab(1:index_digit)

	! Multiplication
	do i = 1, 40
		increase_digit = 0
		do j = 1, 40
			product_num_plus = num_a(j) * num_b(i) + increase_digit
			product_digit(i, j) = mod(product_num_plus, 10)
			increase_digit = product_num_plus / 10
		end do
		product_digit(i, 41) = increase_digit
	end do

	increase_digit_1 = 0
	increase_digit_2 = 0
	do k = 1, 80
		sum_each_digit = 0
		do i = 1, 40
			do j = 1, 41
				if ((i + j - 1) > k) exit 
				if ((i + j - 1) == k) then
					sum_each_digit = sum_each_digit + product_digit(i, j)
				end if
			end do
		end do
		sum_each_digit = sum_each_digit + increase_digit_1 + increase_digit_2 * 10
		increase_digit_2 = sum_each_digit / 100
		increase_digit_1 = (sum_each_digit - increase_digit_2 * 100 ) / 10
		product_ab = mod(sum_each_digit, 10) + ichar('0')
		buf_product_ab(k:k) = char(product_ab)
	end do

	! Reverse
	do i = 80, 2, -1
		if (ichar(buf_product_ab(i:i)) /= ichar('0')) exit
	end do
	index_digit = i

	do i = 1, index_digit
		cha_product_ab(i:i) = buf_product_ab((index_digit - i + 1):(index_digit - i + 1))
	end do

	! Display the product
	write(*,*)"Multiplication:"
	form = "(1X," // char_num(length_num_a) // "I1, 1X, '*', 1X," // char_num(length_num_b) // "I1, 1X, '=')"
	write(*, form)num_a(length_num_a:1:-1), num_b(length_num_b:1:-1)
	write(*, '(1X, A)')cha_product_ab(1:index_digit)

end program practice
