! Page 140 8-4
! 8text4.f90
! The latest version (2024/11/3)

recursive subroutine bisection(val_left, val_right, rslt, num)
implicit none

        real(8)::val_mid
        real(8), intent(in)::num
        real(8), intent(inout)::val_left, val_right
        real(8), intent(out)::rslt
        real(8), external::f

        val_mid = (val_left + val_right) / 2d0

        if (abs(f(val_mid, num)) < 1d-8) then
                ! To stop this subroutine 
                rslt = val_mid
                return
        end if

        if (f(val_left, num) * f(val_mid, num) < 0) then
                val_right = val_mid
        else if (f(val_mid, num) * f(val_right, num) < 0) then
                val_left = val_mid
        end if

        call bisection(val_left, val_right, rslt, num)

end subroutine bisection

function f(x, num) result (y)
implicit none

        real(8),intent(in)::x, num
        real(8)::y

        y = x ** 2d0 - num

end function f

program practice
implicit none
 
        real(8)::num
        real(8)::x0, init_val_left, init_val_right
        real(8)::product_num
        real(8), external::f

        write(*, '(A)', advance = 'no')"Calculate the square root of "
        read(*,*)num
        if (num <= 0d0) stop "Input a positive number!" 

        write(*, '(A)', advance = 'no')"Input two initial values (left) :"
        read(*,*)init_val_left
        write(*, '(A)', advance = 'no')"Input two initial values (right) :"
        read(*,*)init_val_right

        product_num = f(init_val_left, num) * f(init_val_right, num)
        if (product_num > 0d0) stop 'One of them should be negative.'

        call bisection(init_val_left, init_val_right, x0, num)
        write(*, '(A, F12.8)')"The answer is ", x0

end program practice
