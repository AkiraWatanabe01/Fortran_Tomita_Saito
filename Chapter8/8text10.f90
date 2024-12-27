! Page 141
! 8text8.f90
! The latest version (2024/12/28)

subroutine arithmetric_doctor(num_question)

        implicit none
        integer :: i
        integer :: answer
        integer, intent(in) :: num_question
        integer, dimension(1:3) :: num_rnd_int
        real, dimension(1:2) :: num_rnd 
        real :: num_rnd_operation
        character(len = 1) :: operation
        character(len = 7), allocatable :: recorder_grade(:)

        allocate(recorder_grade(num_question))
        call random_seed

        do i = 1, num_question
                call random_number(num_rnd)
                num_rnd_int(1:2) = nint(num_rnd(1:2) * 99)
                num_rnd_int(3) = num_rnd_int(1) + num_rnd_int(2)
                operation = '+'
                call random_number(num_rnd_operation)
                if (num_rnd_operation < 0.5) then
                        num_rnd_int(3) = num_rnd_int(1) * num_rnd_int(2)
                        operation = '*'
                end if

                write(*, '(A10, 1X, I4, A1)', advance = 'no')"(Question.", i, ")"
                write(*, '(I3, 1X, A1, 1X, I3, 1X, A1, 1X)', advance = 'no')num_rnd_int(1), operation, num_rnd_int(2), "="
                read(*,*)answer
                
                call check_answer(num_rnd_int(3), answer, recorder_grade(i))
        end do

        write(*,*)"Your result"
        do i = 1, num_question
                write(*, '(A10, 1X, I4, A1, 1X, A7)')"(Question.", i, ")", recorder_grade(i)
        end do

end subroutine arithmetric_doctor

subroutine check_answer(num_1, num_2, rslt)

        implicit none
        integer, intent(in) :: num_1, num_2
        character(len = 7), intent(out) :: rslt

        rslt = "Wrong  "
        if (num_1 == num_2) then
                rslt = "Correct"
        end if

end subroutine check_answer

program practice

        implicit none
        integer, parameter :: num_question = 10 

        call arithmetric_doctor(num_question)

end program practice
