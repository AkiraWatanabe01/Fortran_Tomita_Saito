! Page56 5-1
! 5text1.f90
! The latest version (2025/1/4)

program practice

	implicit none
	integer :: budget, price      
	integer :: total_price = 0	! At first, set "total_price = 0" to use "do while" command.

	! Set budget
	write(*,*)"Input your budget."
	read(*,*)budget

	! Repeat reading prices
	do while (total_price <= budget)
		! If "total_price" is smaller than "budget", "do while" continues.
		write(*,*)"Input the price."
		read(*,*)price
		total_price = total_price + price
	end do

	! Display the total price
	write(*,*)"The total price : ", total_price

end program practice
