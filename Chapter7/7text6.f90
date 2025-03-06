! Page 105 7-6  
! 7text6.f90
! The latest version(2025/3/6)

program main

	implicit none
	character(len = 100) :: fmt
	character(len = 2) :: interval_odd(1:6) = (/'34', '28', '22', '16', '10', ' 4'/)
	character(len = 2) :: interval_even(1:6) = (/'31', '25', '19', '13', ' 7', ' 1'/)
	character(len = 2) :: repeat_odd(1:6) = (/' 1', ' 3', ' 5', ' 7', ' 9', '11'/)
	character(len = 2) :: repeat_even(1:6) = (/' 2', ' 4', ' 6', ' 8', '10', '12'/)
	integer :: i
	integer :: index
	integer, parameter :: num_max = 12
	integer, allocatable :: c(:)

	allocate(c(0:num_max))
	c(:) = 0
	c(1) = 1
  
	do i = 1, num_max
		c(1:i) = c(1:i) + c(0:(i - 1))
 
		if (mod(i, 2) == 1) then
			index = (i + 1) / 2
			fmt = "(I2,A1," // interval_odd(index) // "X," // repeat_odd(index) // "I6)"    
		else
			index = i / 2
			fmt = "(I2,A1," // interval_even(index) // "X," // repeat_even(index) // "I6)"
		end if

		! Desplay the result
		print fmt, i, ':' , c(1:i)                                  ! print '(I2,A1,_X,_I6)',i,':',c(1:i)
	end do

end program main
