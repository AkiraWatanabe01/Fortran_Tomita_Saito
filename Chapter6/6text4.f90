! Page 85 6-4
! 6text4.f90
! The latest version (2025/1/5)

program practice

	implicit none
	integer :: i
	integer :: money
	integer, dimension(1:10) :: paper = 0
	integer, dimension(1:10) :: currency = (/ 10000, 5000, 2000, 1000, 500, 100, 50, 10, 5, 1/)

	write(*,*)"Input money :"
	read(*,*)money
	if (money < 0) stop "It should be non-negative."

	do i = 1, 10
		do while (money >= currency(i))
			paper(i) = paper(i) + 1
			money = money - currency(i)
		end do
		write(*, '(I5, A5, 1X, I4)')currency(i), " yen:", paper(i)
	end do

end program practice
