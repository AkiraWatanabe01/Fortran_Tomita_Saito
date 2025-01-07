! Page 140 8-1
! 8text1.f90
! The latest version  (2025/1/7)

subroutine binary_expression(num_decimal, num_binary)

	implicit none
	character(len = 32), intent(out) :: num_binary
	integer, intent(in) :: num_decimal
	integer :: i
	integer :: buf_num, diff_num
	integer, parameter :: num_limit_lower = 2 ** 30 * (-2)   ! -2^31

	if (num_decimal == num_limit_lower) then
		num_binary(32:32) = '1'
		do i = 1, 31
			num_binary(i:i) = '0'
		end do

		return
	end if

	buf_num = abs(num_decimal)
	num_binary(32:32) = '0'
	do i = 31, 1, -1
		num_binary(i:i) = '0'
		diff_num = buf_num - 2 ** (i - 1)
		if (diff_num >= 0) then
			num_binary(i:i) = '1'
			buf_num = diff_num
		end if
	end do

	if (num_decimal >= 0) then
		return
	end if

	call make_binary_negative(num_binary)

contains
	subroutine make_binary_negative(data_binary)
	
		implicit none
		character(len = 32), intent(inout) :: data_binary
	
		do i = 1, 32
			if (data_binary(i:i) == '0') then
				data_binary(i:i) = '1'
			else
				data_binary(i:i) = '0'
			end if
		end do

		do i = 1, 32
			if (data_binary(i:i) == '0') then
				data_binary(i:i) = '1'
				exit
			end if
			
			data_binary(i:i) = '0'
		end do

	end subroutine make_binary_negative

end subroutine binary_expression

program practice

	implicit none
	character(len = 32) :: number_bin
	integer :: i
	integer :: number_dec

	write(*, '(A)', advance='no')"Input an integer: "
	read(*,*)number_dec

	call binary_expression(number_dec, number_bin)
	write(*, '(32A1)')(number_bin(i:i), i = 32, 1, -1)

end program practice
