! Page 105  7-5
!
! The latest version (2016/11/4)

program practice
implicit none

 integer::key,j,k,l=1,m
 character(80)::sen,code
 integer,allocatable::num(:),i(:)

 write(*,*)"Input one sentence."
 read(*,'(A)')sen(:)
 k=len_trim(sen(:))

 write(*,'(A)',advance='no')"Input a key number :"
 read(*,*)key

 allocate(i(1:key),num(1:key))
 i(1:key)=1

 do j=1,key
    num(j)=j
 end do

 do m=1,key
    do while (num(m)<=k)
       code(l:l)=sen(num(m):num(m))
       num(m)=key*i(m)+m
       l=l+1
       i(m)=i(m)+1
    end do
 end do

 write(*,'(A80)')code(:)


end program practice
