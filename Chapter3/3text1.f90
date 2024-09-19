! p28 3-1
! 3text1.f90
! The latest version (2024/9/20)

program practice
implicit none
 
	integer::n, calling_charge

	write(*,*)'How long did you talk on the phone?'
	read(*,*)n
	if (n < 0) stop 'Input > 0'

	! if () then ... else if () then ... else ... end if
	if ((0 <= n).and.(n < 180)) then
 		calling_charge = 10
	else
 		calling_charge = 11 + (n - 180) / 6
	end if

	write(*,*)'Your telephone rate is', calling_charge, 'yen'

end program practice
