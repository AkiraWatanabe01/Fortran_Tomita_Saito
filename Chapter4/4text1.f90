! p42 4-1
! 4text1.f90
! The latest version (2024/9/22)

program practice
implicit none

	integer::t, h, m
	real::s

	write(*,*)'Input integer t (< 86400000).'
	read(*,*)t                                   ! ex) t = 80020531
	if (t >=86400000) stop 't should be < 86400000.'
 
	h = t / 3600000                                  ! h = 80020531 / 3600000 = 22 (because h is an integer)                             
	m = (t - h * 3600000) / 60000                        ! m = 820531 / 60000 = 13 (because m is an integer)                           
	s = (t - h * 3600000 - m * 60000) * 1.0e-3                ! s = 40531 * 1.0e-3 = 40.531

	if (h > 11) then                        
		write(*,*)'P.M.', (h - 12), ':' , m, ':', s         ! 22 - 12 = 10
	else
 		write(*,*)'A.M.', h, ':', m, ':', s 
	end if

end program practice
