! page8 1-2
!
! The latest version (2016/10/4)

program practice
implicit none

 real::C,F               

 write(*,*)'Input Celsius degree.' 
                 
 read(*,*)C     

 F=1.8e0*C+32.0e0 
    ! Calculation  
    ! Just 1.8 is OK, but 1.8e0 is better than 1.8. The same is true for 32.0e0.

 write(*,*)'Fahrenheit :',F     

end program practice
