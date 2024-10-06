! Page 86 6-8
! 6text8.f90
! The latest version (2024/10/6)

program practice
implicit none

        integer::i, j, k
        integer::sum_num                                   ! the sum of numbers
        integer::increase_digit                            ! increase in digit for summation
        integer::increase_digit_1, increase_digit_2        ! increase in digit for multiplication
        integer::sum_each_digit, product_num_plus
        integer,dimension(1:20)::num_a, num_b
        integer,dimension(1:21)::sum_ab                    ! the sum of num_a and num_b
        integer,dimension(1:40)::product_ab = 0            ! the product of num_a and num_b
        integer,dimension(1:20, 1:21)::product_digit

        ! Input 2 numbers
        write(*,*)"Input the number of A."
        write(*, '(A20)')"********************"
        read(*, '(20I1)')(num_a(i), i = 20, 1, -1)

        write(*,*)"Input the number of B."
        write(*, '(A20)')"********************"
        read(*, '(20I1)')(num_b(i), i = 20, 1, -1)

        ! Summation
        increase_digit = 0
        do i = 1, 20
                sum_num = num_a(i) + num_b(i) + increase_digit
                sum_ab(i) = mod(sum_num, 10)
                increase_digit = sum_num / 10
        end do

        ! Display the summation
        write(*, '(20I1, 1X, "+", 1X, 20I1, 1X, "=")')num_a(20:1:-1), num_b(20:1:-1)
        if (increase_digit >= 1) then
                sum_ab(21) = increase_digit
                write(*, '(21I1)')sum_ab(21:1:-1)
        else
                write(*, '(1X, 20I1)')sum_ab(20:1:-1)
        end if

        ! Multiplication
        do i = 1, 20
                increase_digit = 0
                do j = 1, 20
                        product_num_plus = num_a(j) * num_b(i) + increase_digit
                        product_digit(i, j) = mod(product_num_plus, 10)
                        increase_digit = product_num_plus / 10
                end do
                product_digit(i, 21) = increase_digit
        end do

        increase_digit_1 = 0
        increase_digit_2 = 0
        do k = 1, 40
                sum_each_digit = 0
                do i = 1, 20
                        do j = 1, 21
                                if ((i + j - 1) > k) exit 
                                if ((i + j - 1) == k) then
                                        sum_each_digit = sum_each_digit + product_digit(i, j)
                                end if
                        end do
                end do
                sum_each_digit = sum_each_digit + increase_digit_1 + increase_digit_2 * 10
                increase_digit_2 = sum_each_digit / 100
                increase_digit_1 = (sum_each_digit - increase_digit_2 * 100 ) / 10
                product_ab(k) = mod(sum_each_digit, 10)
        end do

        ! Display the product
        write(*, '(20I1, 1X, "*", 1X, 20I1, 1X, "=")')num_a(20:1:-1), num_b(20:1:-1)
        if (product_ab(40) >= 1) then
                write(*, '(40I1)')product_ab(40:1:-1)
        else
                write(*, '(1X, 39I1)')product_ab(39:1:-1)
        end if

end program practice
