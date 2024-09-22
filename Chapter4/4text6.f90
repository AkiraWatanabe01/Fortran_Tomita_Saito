! p42 4-6
! 4text6.f90
! The latest version (2024/9/22)

program practice
implicit none
 
	real::x1, x2, y1, y2  
	complex::z1, z2
	logical::h

	write(*,*)'Input a complex number z1 (= x1 + i * y1).'
	write(*,*)'x1 = ?'
	read(*,*)x1
	write(*,*)'y1 = ?'
	read(*,*)y1

	write(*,*)'Input a complex number z2 (= x2 + i * y2).'
	write(*,*)'x2 = ?'
	read(*,*)x2
	write(*,*)'y2 = ?'
	read(*,*)y2
 
	z1 = cmplx(x1, y1)
	z2 = cmplx(x2, y2)
	h = (abs(z1 + z2) <= (abs(z1) + abs(z2)))

	write(*,*)'|z1 + z2| <= |z1| + |z2|? (T:True, F:False)'
	write(*,*)h
 	write(*,*)'|z1 + z2| = ', abs(z1 + z2), '|z1| + |z2| = ', (abs(z1) + abs(z2))

end program practice
