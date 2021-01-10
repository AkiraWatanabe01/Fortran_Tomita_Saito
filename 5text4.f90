! Page56 5-4 
! Histogram
! The latest version ( 2016/10/21)

program practice
implicit none

 integer::j,k,n,m
 
 do n=1,7 
    write(*,*)(" ",j=1,7-n),("*",k=1,2*n-1)         ! n=1        * 
 end do                                             ! n=2       ***
                                                    ! n=3      ***** 
                                                    ! n=4     *******
                                                    ! n=5    ********* 
                                                    ! n=6   ***********
                                                    ! n=7  *************

 do m=1,6                                           
    write(*,*)(" ",j=1,m),("*",k=1,-2*m+13)         ! m=1   ***********
 end do                                             ! m=2    *********
                                                    ! m=3     *******
                                                    ! m=4      *****  
                                                    ! m=5       ***
                                                    ! m=6        *


end program practice                     
