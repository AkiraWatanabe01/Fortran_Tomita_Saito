! Page 86  6-8
!
! The latest version (2016/11/4)

program practice
implicit none

 integer::h,i,j,k,l,m,r,d(1:40),e(1:41),fall,gall    ! d is the product
 integer,dimension(1:20)::a,b
 integer,dimension(1:21)::c,suma                     ! c is the sum
 integer,dimension(1:39,1:39)::f
 integer,dimension(1:40,1:40)::g

 f(:,:)=0   ! 1 no kurai
 g(:,:)=0   ! kuriagari

! Input 2 numbers
 write(*,*)"Input the number of A."
 read(*,'(20I1)')(a(i),i=1,20)

 write(*,*)"Input the number of B."
 read(*,'(20I1)')(b(i),i=1,20)

! Addition
 r=0
 do j=20,1,-1
    suma(j+1)=a(j)+b(j)+r
    if (suma(j+1)>=10) then
       suma(j+1)=suma(j+1)-10
       r=1
    else
       r=0
    end if
    c(j+1)=suma(j+1)
 end do

 if (r==1) then
    c(1)=1
 else 
    c(1)=0
 end if

 do K=1,20
    do l=1,20
       f(k,l)=a(k)*b(l)-(a(k)*b(l))/10*10
       g(k,l)=(a(k)*b(l))/10
    end do
 end do

! Multiplication
 e(41)=0
 do m=40,1,-1
    fall=0
    gall=0
    h=1
    do while (m-h>=1)
      fall=f(m-h,h)+fall
      h=h+1
    end do

    h=1
    do while (m-h+1>=1)
      gall=g(m-h+1,h)+gall
      h=h+1
    end do

    e(m)=fall+gall+e(m+1)/10
    d(m)=e(m)-e(m)/10*10
  end do

 write(*,'(20I1,1X,"+",1X,20I1,1X,"=")')a(1:20),b(1:20)
 write(*,'(40I1)')c(1:21)
 write(*,'(20I1,1X,"× ",1X,20I1,1X,"=")')a(1:20),b(1:20)
 write(*,'(40I1)')d(1:40)

end program practice
