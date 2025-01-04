! Page56 5-4 
! 5text4.f90
! The latest version (2025/1/4)

program practice

	implicit none
	integer :: j, k, n, m

 	! Upper part
	! n=1        * 
	! n=2       ***
        ! n=3      ***** 
        ! n=4     *******
        ! n=5    ********* 
        ! n=6   ***********
        ! n=7  *************
	do n = 1, 7
		write(*,*)(" ", j = 1, 7 - n), ("*", k = 1, 2 * n - 1)         
	end do                                 

	! Lower part
 	! m=1   ***********
	! m=2    *********
        ! m=3     *******
        ! m=4      *****  
        ! m=5       ***
        ! m=6        *
	do m = 1, 6                                     
		write(*,*)(" ", j = 1, m), ("*", k = 1, -2 * m + 13)         
	end do

end program practice                     
