! Page58 5-9
! Approximation 
! The latest version (2016/11/11)

program practice
implicit none
 
 integer::i,j,h
 real::x,f,x1,x2,x3,point1,point2
 real,parameter::dx=0.1e0

! ----------------------Drawing a graph----------------------------
 do i=-30,20
    x=i*dx
    f=x**3+x**2-5.0e0*x-1.0e0
    h=int(f)+6                                                ! '+6' is added to read the graph  
    write(*,'(1x,f5.1,1x,"|",50a1,a1)')x,(" ",j=1,h-1),'*'
 end do
! -------------------End of drawing a graph------------------------

! ------------------------The Main part----------------------------
 write(*,*)'Input one initial point.'
 read(*,*)point1
 write(*,*)'Input the other initial point.'
 read(*,*)point2

 x1=point1
 x2=point2
 x3=(x1+x2)/2.0e0

! -----------------------Dividing into cases-----------------------
! --------------------------Case 1---------------------------------
 if ((x1**3+x1**2-5.0e0*x1-1.0e0>=0.0e0).and.(x2**3+x2**2-5.0e0*x2-1.0e0<=0.0e0)) then

      do while (abs(x3**3+x3**2-5.0e0*x3-1.0e0)>1.0e-4)
         if (x3**3+x3**2-5.0e0*x3-1.0e0>0.0e0) then
            x1=x3
         else
            x2=x3
         end if
            x3=(x1+x2)/2.0
      end do

      write(*,*)x3

! --------------------------Case 2---------------------------------
 else if ((x1**3+x1**2-5.0e0*x1-1.0e0<=0.0e0).and.(x2**3+x2**2-5.0e0*x2-1.0e0>=0.0e0)) then

      do while (abs(x3**3+x3**2-5.0e0*x3-1.0e0)>1.0e-4)
         if (x3**3+x3**2-5.0e0*x3-1.0e0>0.0e0) then
            x2=x3
         else
            x1=x3
         end if
            x3=(x1+x2)/2.0
      end do
      
      write(*,*)x3

! --------------------------Case 3---------------------------------
 else
     write(*,*)'Please consider initial points again.'
 
 end if

! ---------------------End of  the main part------------------------


end program practice
