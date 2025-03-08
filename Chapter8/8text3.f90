! Page 140 8-3
! 8text3.f90
! The latest version (2025/3/8)

! Subroutine for Runge Kutta method
subroutine rungekutta (x, y, dy)

        implicit none
        real(8) :: d1, d2, d3, d4
        real(8) :: f
        real(8), parameter :: h = 1.0d-3
        real(8), intent(in) :: x, y
        real(8), intent(out) :: dy

        d1 = f(x, y)
        d2 = f(x + h / 2.0d0, y + h * d1 / 2.0d0)
        d3 = f(x + h / 2.0d0, y + h * d2 / 2.0d0)
        d4 = f(x + h, y + h * d3)

        dy = h * (d1 + 2.0d0 * d2 + 2.0d0 * d3 + d4) / 6.0d0

end subroutine rungekutta

! Function  ! f(x,y)=-x/y
function f(x, y) result (z)

        implicit none
        real(8), intent(in) :: x, y
        real(8) :: z

        z = - x / y

end function f

! Main 
program practice

        implicit none
        integer :: i
        real(8) :: x, y, dy
        real(8), parameter :: dx = 1.0d-3

        write(*, '(A)', advance = 'no')"input x :"
        read(*,*)x
        write(*, '(A)', advance = 'no')"input y :"
        read(*,*)y

        write(*,'(F10.6,1X,F10.6)')x, y

        do i = 1, 1000
                call rungekutta(x, y, dy)
                x = x + dx
                y = y + dy
                write(*, '(F10.6, 1X, F10.6)')x, y
        end do

end program practice
