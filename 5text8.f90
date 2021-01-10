! Page56 5-8
! The sum of divisors 
! The latest version (2016/10/21)

program practice
implicit none
 
 integer::i,j,num,sum

 do j=1,1000
   write(*,*)"Input a number or 0 to stop."
   read(*,*)num

   if (num<=0) exit

   sum=1+num

     do i=2,num-1
        if (mod(num,i)/=0) cycle
        sum=sum+i
     end do

   write(*,*)'The sum of divisors of',num,'is',sum

 end do

end program practice
