! Page 85 6-5
!
! The latest version (2016/10/29)

program practice
implicit none

 integer::i,j,k
 integer,dimension(1:3)::a,b,c,d,e

 write(*,*)"Input a."
 read(*,*)(a(i),i=1,3)
 write(*,*)"Input b."
 read(*,*)(b(i),i=1,3)
 write(*,*)"Input c."
 read(*,*)(c(i),i=1,3)

!-------The maximum array d ----
 d(1)=a(1)
 d(2)=a(2)
 d(3)=a(3)

 do j=1,3
    if (d(j)<b(j)) then
       d(j)=b(j)
    end if

    if (d(j)<c(j)) then
       d(j)=c(j)
    end if
 end do
!-------------------------------

!-------The minimum array e ----
 e(1)=a(1)
 e(2)=a(2)
 e(3)=a(3)

 do k=1,3
    if (e(k)>b(k)) then
       e(k)=b(k)
    end if

    if (e(k)>c(k)) then
       e(k)=c(k)
    end if
 end do
!-------------------------------

!-------------output----------------------
 write(*,*)"The maximum array is :",d(:)
 write(*,*)"The minimum array is :",e(:)
!-----------------------------------------

end program practice
