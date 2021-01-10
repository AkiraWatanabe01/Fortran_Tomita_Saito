! p28 3-3
!
! The latest version (2016/10/10)

program practice
implicit none

 real::a,b,c,x1,x2,x3,x4

 write(*,*)'Input coefficients.'
 write(*,*)'a = ?'
 read(*,*)a
 write(*,*)'b = ?'
 read(*,*)b
 write(*,*)'c = ?'
 read(*,*)c
 
  if (a==0) then          ! Start of 1st if
     if (b/=0) then       ! Start of 2nd if 
        x1=-c/b
        write(*,*)x1

     else                 
        if (c==0) then                         ! Start of 3rd if 
           write(*,*)'Solution indetermine'
        else
           write(*,*)'No solution'
        end if                                 ! End of 3rd if 

     end if               ! End of 2nd if 

  else   
     if (b*b>4.0e0*a*c) then                   ! Start of 4th if (Discriminant > 0)  
        x2=(-b+sqrt(b*b-4.0e0*a*c))/(2.0e0*a)   
        x3=(-b-sqrt(b*b-4.0e0*a*c))/(2.0e0*a)   
        write(*,*)x2
        write(*,*)x3

     else if (b*b==4.0e0*a*c) then             ! (Discriminant = 0)  
        x4=-b/(2.0e0*a)
        write(*,*)x4

     else                                      ! (Discriminant < 0)
        write(*,*)'Complex roots'

     end if                                    ! End of 4th if
 
  end if                  ! End of 1st if 


end program practice
