! Page 172 9-2
! 9text2.f90
! The latest version 2025/1/2

module user_defined_function

        implicit none
        interface division_allow_zero
                module procedure real_division_allow_zero, integer_division_allow_zero
        end interface

contains

        ! Case of real number
        function real_division_allow_zero(a, b) result (quotient)

                implicit none
                real, intent(in) :: a, b
                real :: quotient

                if (b < 1e-30) then
                        quotient = 0.0e0
                        return
                end if
                quotient = a / b

        end function real_division_allow_zero

        ! Case of integer
        function integer_division_allow_zero(a, b) result (quotient)

                implicit none
                integer, intent(in) :: a
                real :: quotient
                real, intent(in) :: b

                if (b < 1e-30) then
                        quotient = 0.0e0
                        return
                end if
                quotient = a / b

        end function integer_division_allow_zero

end module user_defined_function

program practice

        use user_defined_function, only : division_allow_zero
        implicit none
        real :: rslt

        rslt = division_allow_zero(1, 3.125e0)
        write(*, '(F10.3)') rslt
        rslt = division_allow_zero(1, 1e-31)
        write(*, '(F10.3)') rslt
        rslt = division_allow_zero(1e0, 6.25e0)
        write(*, '(F10.3)') rslt
        rslt = division_allow_zero(1e0, 0e0)
        write(*, '(F10.3)') rslt

end program practice
