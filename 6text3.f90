! Page 85 6-3
!
! The latest version (2016/10/29)

program practice
implicit none

 integer::i=0,j=0,k,l,m,n,maximum,p,q
 integer,allocatable::a(:,:)
 
 write(*,*)"What's the size of this matrix (i× j)?"
 
 do while(i<=0)
    write(*,'(a)',advance='no')"Input i :"
    read(*,*)i 
 end do

 do while(j<=0)
    write(*,'(a)',advance='no')"Input j :"
    read(*,*)j
 end do

!----------Formation of the matrix----------
 allocate (a(1:i,1:j))

 do p=1,i
    write(*,*)"Input the number of row",p
    read(*,*)(a(p,q),q=1,j)
 end do 
!-------------------------------------------

!---------------Sorting---------------------
 maximum=a(1,1)
 do k=1,i
    do l=1,j
       if (maximum<=a(k,l)) then
          maximum=a(k,l)
          m=k
          n=l
       end if
    end do
 end do
!-------------------------------------------

!---------------output----------------------
 write(*,*)"The maximum value is",maximum
 write(*,*)"Its place is (",m,",",n,")"
!-------------------------------------------

end program practice
