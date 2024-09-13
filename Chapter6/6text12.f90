! Page86 6-12
! 6text12.f90 
! The latest version (2024/9/13)

program practice
implicit none

	integer::i, j
	integer::rule, determinant
	integer::left, right                  ! indices for the periodic boundary condition
	integer::num_bit = 75                 ! number of bits
	integer::flag(0:7)                    ! flag for the rule (0 or 1)
	integer, allocatable::b(:), temp(:)
	real::num
	character(len = 1)::symbol(0:1) = (/' ', '*'/)

	allocate(b(1:num_bit), temp(1:num_bit))

	! cellular automaton rule
	write(*,*)"Input the rule number (0..255):"
	read(*,*)rule
	if ((rule < 0) .OR. (255 < rule)) stop "Invalid value"

	do i = 7, 0, -1
		flag(i) = rule / (2 ** i)
		rule = rule - flag(i) * (2 ** i)
	end do

	! intial value
	call random_seed
	do i = 1, num_bit
		call random_number(num)
		b(i) = mod(int(num * 10000), 2)
	end do
	write(*,'(i3, ":", 75a1)')0, symbol(b(:))

	! calculation
	do i = 1, 100
		do j = 1, num_bit
			left = j - 1
			right = j + 1

			! periodic boundary condition
			if (j == 1) then
				left = num_bit
			end if 
			if (j == num_bit) then
				right = 1
			end if
			determinant = b(left)* (2 ** 2) + b(j) * 2 + b(right)

			select case (determinant)
				case (7)
					temp(j) = flag(7)
				case (6)
					temp(j) = flag(6)
				case (5)
					temp(j) = flag(5)
				case (4)
					temp(j) = flag(4)
				case (3)
					temp(j) = flag(3)
				case (2)
					temp(j) = flag(2)
				case (1)
					temp(j) = flag(1)
				case (0)
					temp(j) = flag(0)
			end select
		end do
		write(*,'(i3, ":", 75a1)')i, symbol(temp(:))

		! update
		b(:) = temp(:)
	end do

end program practice
