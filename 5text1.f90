! Page56 5-1
!
! The latest version (2016/9/18)

program practice
implicit none

 integer::budget,price=0,p  ! At first, set "price=0" to use "do while" command.
 
 write(*,*)"Input your budget."  ! Set budget.
 read(*,*)budget

 do while (price<=budget)
    ! If "price(the initial value is 0)" is smaller than "budget","do while" continues.

   write(*,*)"Input the price."
   read(*,*)p
   price=price+p   ! price(new value)=price(previous value)+p(new input)
 end do

 write(*,*)price    ! Display the sum.

end program practice
