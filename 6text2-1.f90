! Page 85 6-2(1)
!
! The latest version (2016/10/29)

program practice
implicit none

 integer::i,j,p,q
 integer,dimension(1:9,1:9)::a
 integer,dimension(1:8,1:8)::b,c,d,e,f

! ------------n=9-------------------        
  a(1,1:9)=(/1,2,3,4,5,6,7,8,9/)
  a(2,1:9)=(/2,2,3,4,5,6,7,8,9/)
  a(3,1:9)=(/3,3,3,4,5,6,7,8,9/)
  a(4,1:9)=(/4,4,4,4,5,6,7,8,9/)
  a(5,1:9)=(/5,5,5,5,5,6,7,8,9/)
  a(6,1:9)=(/6,6,6,6,6,6,7,8,9/)
  a(7,1:9)=(/7,7,7,7,7,7,7,8,9/)
  a(8,1:9)=(/8,8,8,8,8,8,8,8,9/)
  a(9,1:9)=(/9,9,9,9,9,9,9,9,9/)
!-----------------------------------

 b(:,:)=0
 c(:,:)=0
 d(:,:)=0
 e(:,:)=0

 do p=1,9
    write(*,*)(a(p,q),q=1,9)
 end do

 i=10
 do while (i<1.or.i>9)
    write(*,'(a)',advance='no')"Input i :"
    read(*,*)i
 end do

 j=10
 do while (j<1.or.j>9)
    write(*,'(a)',advance='no')"Input j :"
    read(*,*)j
 end do

 if (i==1.and.j==1) then
    f(1:8,1:8)=a(2:9,2:9)
 else if (i==1.and.j==9) then  
    f(1:8,1:8)=a(2:9,1:8)
 else if (i==9.and.j==1) then
    f(1:8,1:8)=a(1:8,2:9)
 else if (i==9.and.j==9) then
    f(1:8,1:8)=a(1:8,1:8)
 else 
   b(1:i-1,1:j-1)=a(1:i-1,1:j-1)
   c(i:8,1:j-1)=a(i+1:9,1:j-1)
   d(1:i-1,j:8)=a(1:i-1,j+1:9)
   e(i:8,j:8)=a(i+1:9,j+1:9)
 
   f(:,:)=b(:,:)+c(:,:)+d(:,:)+e(:,:)
 end if

!---------output---------- 
 write(*,*)'The result is'
 do p=1,8
    write(*,*)(f(p,q),q=1,8)
 end do
!-------------------------

end program practice
