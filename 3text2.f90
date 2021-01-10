! p28 3-2
!
! The latest version (2016/10/10)

program practice
implicit none

 real::h,w,BMI

 write(*,*)'What is your height [m]?'
 read(*,*)h
 
 write(*,*)'What is your weight [kg]?'
 read(*,*)w

 BMI=w/(h*h)

  if (BMI<=18.5e0) then
     write(*,*)'Your BMI is',BMI
     write(*,*)'So, you are thin'
  else if ((BMI>18.5e0).and.(BMI<25e0)) then 
     write(*,*)'Your BMI is',BMI
     write(*,*)'So, you are normal'
  else  
     write(*,*)'Your BMI is',BMI
     write(*,*)'So, you are overweight'   
  end if




end program practice
