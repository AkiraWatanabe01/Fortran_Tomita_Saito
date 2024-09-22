! p42 4-5
! 4text5.f90
! The latest version (2024/9/22)

program practice
implicit none
 
	real::x, y, r_square
 	real::num1_real_part, num1_imaginary_part
 	real::num2_real_part, num2_imaginary_part
	complex::z

	write(*,*)'Input two real numbers x and y.'
	write(*,*)'x = ?'
	read(*,*)x
	write(*,*)'y = ?'
	read(*,*)y
 
	z = cmplx(x, y)
	r_square = x ** 2 + y ** 2

	num1_real_part = x ** 2 - y ** 2
	num1_imaginary_part = 2 * x * y
	num2_real_part = x / r_square
	num2_imaginary_part = - y / r_square

	write(*,*)'z^2 = ', z ** 2
	write(*,*)'1/z = ', 1 / z
	write(*,*)'(x^2-y^2, 2xy) = (', num1_real_part, ',', num1_imaginary_part, ')'
	write(*,*)'(x/r^2, -y/r^2) = (', num2_real_part, ',', num2_imaginary_part, ')'

end program practice
