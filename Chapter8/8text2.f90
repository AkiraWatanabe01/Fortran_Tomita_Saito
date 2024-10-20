! Page 140 8-2
! 8text2.f90
! The latest version (2024/10/20)

! Subroutine
subroutine cross_product(x, y, r)
implicit none

	real, dimension(1:3), intent(in)::x, y
	real, dimension(1:3), intent(out)::z

	z(1) = x(2) * y(3) - x(3) * y(2)
	z(2) = x(3) * y(1) - x(1) * y(3)
	z(3) = x(1) * y(2) - x(2) * y(1)

end subroutine cross_product

! Function
function func_cross(x,y) result (z)
implicit none

	real, dimension(1:3), intent(in)::x, y
	real, dimension(1:3)::z                     ! 'intent' is not necessary.

	z(1) = x(2) * y(3) - x(3) * y(2)
	z(2) = x(3) * y(1) - x(1) * y(3)
	z(3) = x(1) * y(2) - x(2) * y(1)

end function func_cross

! Main
program practice
implicit none

	real, dimension(1:3)::a, b, sub_cross

	interface                                  ! To use the array
 		function func_cross(x, y) result(z)
			real, dimension(1:3),intent(in)::x, y
			real, dimension(1:3)::z
		end function
	end interface

	write(*, '(A)', advance='no')"input a :"
	read(*,*)a
	write(*, '(A)', advance='no')"input b :"
	read(*,*)b

	! Function
	write(*,*)"Function: The cross product of a and b is ", func_cross(a, b)

	! subroutine
	call cross_product(a, b, sub_cross)               
	write(*,*)"Subroutine: The cross product of a and b is ", sub_cross

end program practice
