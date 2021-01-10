! Page 85 6-1
!
! The latest version (2016/10/28)

program practice
implicit none

 integer::i,j,k,l,s
 integer,dimension(-100:100)::inta
 real,dimension(-100:100)::a,b,c
 real::dt=1.0e0/16.0e0

 a(:)=(/(0.0e0,i=-100,100)/)                ! The initial condition 
 a(0)=10000.0e0                             ! The initial condition

 write(*,*)'How many steps do you have ?'
 read(*,*)s

 do j=1,s
   b(:)=cshift(a(:),-1)                     ! b means a(n-1) 
   b(-100)=0                                  
   b(100)=0
   c(:)=cshift(a(:),1)                      ! c means a(n+1)
   c(-100)=0
   c(100)=0
   a(:)=(1.0e0-2.0e0*dt)*a(:)+dt*(b(:)+c(:))  ! The new a(n)

! Drawing histograms to see the process (This is an original way.)   
   do k=-100,100
      inta(k)=int(a(k))/100
      write(*,'(1x,i4,"|",100A1)')k,('*',l=1,inta(k))
   end do
! The end of drawing

 end do  


end program practice
