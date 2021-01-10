! Page 85 6-2(2)
!
!making 2016/10/29

program practice
implicit none

 integer::i,j,k,l,p,q
 integer,dimension(1:9,1:9)::a
 integer,allocatable::f(:,:)

!------------n=9-----------------
  a(1,1:9)=(/1,2,3,4,5,6,7,8,9/)
  a(2,1:9)=(/2,2,3,4,5,6,7,8,9/)
  a(3,1:9)=(/3,3,3,4,5,6,7,8,9/)
  a(4,1:9)=(/4,4,4,4,5,6,7,8,9/)
  a(5,1:9)=(/5,5,5,5,5,6,7,8,9/)
  a(6,1:9)=(/6,6,6,6,6,6,7,8,9/)
  a(7,1:9)=(/7,7,7,7,7,7,7,8,9/)
  a(8,1:9)=(/8,8,8,8,8,8,8,8,9/)
  a(9,1:9)=(/9,9,9,9,9,9,9,9,9/)
!--------------------------------

 do p=1,9
    write(*,*)(a(p,q),q=1,9)
 end do

 i=10
 j=10
 do while (i>j.or.i<1.or.j>9)
    write(*,'(a)',advance='no')"Input i :"
    read(*,*)i
    write(*,'(a)',advance='no')"Input j :"
    read(*,*)j
 end do

 k=10
 l=10
 do while (k>l.or.k<1.or.l>9)
    write(*,'(a)',advance='no')"Input k :"
    read(*,*)k
    write(*,'(a)',advance='no')"Input l :"
    read(*,*)l
 end do

!------Formation of a matrix--------
 allocate(f(1:j-i+1,1:l-k+1))
 f(:,:)=a(i:j,k:l)
!-----------------------------------

!------------output--------------
 write(*,*)'The result is'
 do p=1,j-i+1
    write(*,*)(f(p,q),q=1,l-k+1)
 end do
!--------------------------------

end program practice
