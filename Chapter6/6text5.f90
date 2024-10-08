! Page 85 6-5
! 6text5.f90
! The latest version (2024/10/8)

program practice
implicit none

	integer::i
	integer, dimension(1:3)::array_1, array_2, array_3
	integer, dimension(1:3)::array_max, array_min

	write(*,*)"Input the first array:"
	read(*,*)(array_1(i), i = 1, 3)
	write(*,*)"Input the second array:"
	read(*,*)(array_2(i), i = 1, 3)
	write(*,*)"Input the second array:"
	read(*,*)(array_3(i), i = 1, 3)

	! array_max
	array_max(1) = array_1(1)
	array_max(2) = array_1(2)
	array_max(3) = array_1(3)

	do i = 1, 3
		if (array_max(i) < array_2(i)) then
			array_max(i) = array_2(i)
		end if

		if (array_max(i) < array_3(i)) then
			array_max(i) = array_3(i)
		end if
	end do

	! array_min
	array_min(1) = array_1(1)
	array_min(2) = array_1(2)
	array_min(3) = array_1(3)

	do i = 1, 3
		if (array_min(i) > array_2(i)) then
			array_min(i) = array_2(i)
		end if

		if (array_min(i)>array_3(i)) then
			array_min(i)=array_3(i)
		end if
	end do

	! Display the result
	write(*,*)"The maximum array is : ", array_max(:)
	write(*,*)"The minimum array is : ", array_min(:)

end program practice
