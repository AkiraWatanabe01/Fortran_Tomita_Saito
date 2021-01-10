! Page57 5-7
!
! The latest version (2016/10/21)

program practice
implicit none
 
 integer::num,i,rem,j,h

 write(*,*)'Input a positive integer.'
 read(*,*)num

 do i=2,num-1
   rem=mod(num,i)
   if (rem/=0) cycle
   h=0                         ! h : How many times can i divide num ?  

   do while (mod(num,i)==0)
      num=num/i
      h=h+1
   end do

   write(*,*)(i,j=1,h)
 end do

end program practice
