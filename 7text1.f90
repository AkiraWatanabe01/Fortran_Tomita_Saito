! Page 105  7-1
!
! The latest version  (2016/11/4)

program practice
implicit none

 character(20)::word1,smword
 integer::i
 integer,dimension(1:20)::cha                  ! The maximum length of the word is 20           
 write(*,'(a)',advance='no')"Input a word :"
 read(*,'(A20)')word1

 do i=1,20
    cha(i)=ichar(word1(i:i))                   ! Convert word1 into an integer
  
    if (cha(i)<=90.and.cha(i)>=65) then
       smword(i:i)=char(cha(i)+32)
    else
       smword(i:i)=word1(i:i)
    end if

 end do

 write(*,*)smword(1:20)


end program practice
