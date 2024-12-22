! Page 140
! 8text8.f90
! The latest version (2024/12/22)

program practice

        implicit none
        integer :: i
        integer :: counter = 0
        integer :: seedsize
        integer, parameter :: num_trial = 1000000
        integer, allocatable :: seed_x(:), seed_y(:), seed_z(:)
        real(8) :: sum_square, volume
        real(8), dimension(1:num_trial) :: x, y, z

        call random_seed(size = seedsize)
        allocate(seed_x(seedsize))
        allocate(seed_y(seedsize))
        allocate(seed_z(seedsize))

        seed_x(:) = 1
        seed_y(:) = 2
        seed_z(:) = 3

        ! Monte Carlo method
        call random_seed(put = seed_x)
        do i = 1, num_trial
                call random_number(x(i))
        end do

        call random_seed(put = seed_y)
        do i = 1, num_trial
                call random_number(y(i))
        end do

        call random_seed(put = seed_z)
        do i = 1, num_trial
                call random_number(z(i))
        end do

        do i = 1, num_trial
                sum_square = x(i) ** 2 + y(i) ** 2 + z(i) ** 2
                if (sum_square <= 1.0d0) then
                        counter = counter + 1
                end if
        end do

        write(*, '(A30, 1X, F10.8)')"4 / 3 * pi * radius ** 3 :", 4d0 / 3d0 * 3.14159265d0 * 1
        volume = 8.0d0 * dble(counter) / dble(num_trial)
        write(*, '(A30, 1X, F10.8)')"Monte Carlo method       :", volume

end program practice
