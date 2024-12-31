! Page 141
! 8text11.f90
! The latest version (2024/12/28)

subroutine generate_sequence(num_sequence, size_num_sequence)

        implicit none
        integer :: i, j
        integer :: n
        integer, parameter :: num_max = 1, num_min = -1
        integer, intent(in) :: size_num_sequence
        real :: num_rnd, mean
        real, allocatable :: num_rnd_uniform(:)
        real, intent(out) :: num_sequence(1:size_num_sequence)

        write(*, '(A)', advance = 'no')"Input the size of one block: "
        read(*,*)n
        allocate(num_rnd_uniform(1:n))

        call random_seed

        do i = 1, size_num_sequence
                do j = 1, n
                        call random_number(num_rnd)
                        num_rnd_uniform(j) = (num_max - num_min) * num_rnd + num_min
                end do
                mean = sum(num_rnd_uniform) / size(num_rnd_uniform)
                num_sequence(i) = sqrt(3e0 * n) * mean
        end do

end subroutine generate_sequence

subroutine draw_histgram(num_array, size_num_array)

        implicit none
        integer :: i, j
        integer :: flag
        integer, allocatable :: frequency_distribution(:)
        integer, intent(in) :: size_num_array
        real :: x, range_lower, range_larger
        real, parameter :: dx = 2e-1
        real, intent(in) :: num_array(1:size_num_array)

        allocate(frequency_distribution(-16:16))
        frequency_distribution(:) = 0

        do i = 1, size_num_array
                flag = 0
                do j = -16, 16
                        if (flag == 1) exit
                        x = j * dx
                        range_lower = x - 0.5e0 * dx
                        range_larger = x + 0.5e0 * dx
                        if ((num_array(i) < range_lower) .or. (range_larger <= num_array(i))) cycle
                        frequency_distribution(j) = frequency_distribution(j) + 1
                        flag = 1
                end do
        end do

        do i = -16, 16
                x = i * dx
                frequency_distribution(i) = frequency_distribution(i) / 25
                write(*, '(1X, F4.1, 1X, "|", 50A1)')x, ("*", j = 1, frequency_distribution(i))
        end do
        
end subroutine draw_histgram

program practice

        implicit none
        real :: num_sequence(1:10000)

        call generate_sequence(num_sequence, size(num_sequence))
        call draw_histgram(num_sequence, size(num_sequence))

end program practice
