! page8 1-3
!
! The latest version (2016/10/5)

program practice
implicit none

 real::w,h,BMI               

 write(*,*)'Input your weight[kg].'                  
 read(*,*)w     

 write(*,*)'Input your height[m].'                  
 read(*,*)h     

 BMI=w/(h*h)
    ! Calculation  
    ! BMI=w/h**2.0e0 is also OK.

 write(*,*)'Your BMI is',BMI     

end program practice
