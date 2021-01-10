! Page 106  7-8
!
! The latest version (2016/11/4)

program practice
implicit none

 character(1)::alp(1:7,1:182),moji
 integer::i,num

 write(*,'(a)',advance='no')"Input an alphabet :"
 read(*,*)moji

 num=ichar(moji)-96

! -------Reading an exteral file------- 
 open(8,file='alpha.txt')

    do i=1,7*num
       read(8,'(7A1)')alp(1:7,i:i)
    end do

 close(8)
!--------------------------------------

 do i=7*num-6,7*num
    write(*,'(7A1)')alp(1:7,i:i)
 end do


end program practice













