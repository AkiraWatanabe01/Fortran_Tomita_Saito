! page8 1-1
!
! The latest version (2016/10/3)

program practice
implicit none

 real::x           ! Specify variables which you'll use.
 integer::n       

 write(*,*)'Input positive real number x.' 
                   ! Use ' ' or " " when you write a sentence or some words.  
 read(*,*)x        ! Don't use ' ' nor " " when you read or write variables.

 write(*,*)"Input positive integer n." 
 read(*,*)n

 write(*,*)x**(1.0/n)   ! ** means involution.
                        ! The answer will be written on the display.

end program practice
