! Page 140  8-5
! 8text5.f90
! The latest version  (2024/11/25)
integer function centi_second(second) result (c_second)

	implicit none
	character(len = 10), intent(in) :: second  
	integer :: buf_num, total_time 
	
	buf_num = (ichar(second(1:1)) - ichar('0')) * 10 + (ichar(second(2:2)) - ichar('0')) 
	total_time = buf_num * 60 * 60

	buf_num = (ichar(second(3:3)) - ichar('0')) * 10 + (ichar(second(4:4)) - ichar('0')) 
	total_time = total_time + buf_num * 60
	
	buf_num = (ichar(second(5:5)) - ichar('0')) * 10 + (ichar(second(6:6)) - ichar('0')) 
	total_time = total_time + buf_num

	total_time = total_time * 100

	buf_num = (ichar(second(8:8)) - ichar('0')) * 10 + (ichar(second(9:9)) - ichar('0')) 
	c_second = total_time + buf_num

end function centi_second

program practice

	implicit none
	character(len = 10) :: t
	integer :: rslt
	integer, external :: centi_second

	call date_and_time(time = t)
	rslt = centi_second(t)

	write(*,*)rslt

end program practice
