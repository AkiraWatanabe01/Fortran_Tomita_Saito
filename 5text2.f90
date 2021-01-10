! Page56 5-2
!
! The latest version (2016/9/18)

program practice
implicit none

 integer::l=1,m=1,n,r,k  ! l=x_n-2,m=x_n-1,and k=x_n
 real(8)::ratio   ! The ratio of x_n and x_n-1
 
 do n=2,45
   k=m+l
   ratio=real(k)/real(m)
   l=m                      !  1 + 1 = 2
   m=k                      !      1 + 2 = 3 
                            !          2 + 3 = 5 
                            !        :
                            !        :
                            ! This is Fibonacci's sequence
    
   write(*,'(I2,1X,I10,1X,F18.16)')n,m,ratio
   ! 2 integers, 1 blank, 10 integers, 1 blank, and 18 real numbers(16th decimal place)

 end do

end program practice
