! page8 1-1
!
! The latest version (2024/9/10)

program practice
implicit none

 real::x           ! Specify variables which you'll use.
 integer::n
 
 ! Use ' ' or " " when you write a sentence or some words
 write(*,*)'Input positive real number x.'   
 ! Don't use ' ' nor " " when you read or write variables.
 read(*,*)x

 write(*,*)"Input positive integer n." 
 read(*,*)n

 ! ** means raising a number to a power.
 ! The answer will be written on the display.
 write(*,*)x**(1.0/n)   

end program practice
