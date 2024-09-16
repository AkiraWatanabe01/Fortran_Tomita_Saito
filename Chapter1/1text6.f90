! page8 1-6
! 1text6.f90
! The latest version (2024/9/16) 

program practice
implicit none

	integer::p, q, r, s, n

	write(*,*)'Input the value of p (0 <= p < 16).'
	read(*,*)p

	write(*,*)'Input the value of q (0 <= q < 16).'
	read(*,*)q

	write(*,*)'Input the value of r (0 <= r < 16).'
	read(*,*)r

	write(*,*)'Input the value of s (0 <= s < 16).'
	read(*,*)s

	! Calculation
	n = p * 16 ** 3 + q * 16 ** 2 + r * 16 + s   

	write(*,*)n

end program practice
