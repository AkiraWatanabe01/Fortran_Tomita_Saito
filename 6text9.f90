! Page 86  6-9
! 
! The latest version (2016/11/4)

program practice
implicit none

 integer::num,a,b,i,j
 integer,allocatable::power(:),combi(:,:)

 write(*,'(a)',advance='no')"Input the number :"
 read(*,*)num

 a=num

 allocate(power(2:num),combi(2:num,1:2))
 power(:)=0

 do i=2,num
    do while (mod(a,i)==0)
       a=a/i
       power(i)=power(i)+1
    end do

    combi(i,1)=i                  ! prime factor
    combi(i,2)=power(i)           ! index
 end do

 if (sum(power(:))==0) then       
    write(*,*)num,"=",num         ! If num is a prime number   
 else
    do j=2,num                    ! If num is a composite number
       if (combi(j,2)==0) cycle
       write(*,*)combi(j,1),"**",combi(j,2)
    end do
 end if

end program practice
