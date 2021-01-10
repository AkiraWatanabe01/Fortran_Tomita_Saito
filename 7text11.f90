! Page 106  7-11
!
! The latest version (2016/11/4)

program practice
implicit none
character(1)::c1,c2
integer::i1,i2,i3,i4

write(*,*)"4 arithmetic operation"
read(*,'(I4,A1,I4)')i1,c1,i2

if (ichar(c1)==43) then                                ! Addition
  i3=i1+i2
  write(*,'(2(I4,1X,A1,1X),I4)')i1,"+",i2,"=",i3

else if (ichar(c1)==45) then                           ! Subtraction 
  i3=i1-i2
  write(*,'(2(I4,1X,A1,1X),I4)')i1,"-",i2,"=",i3

else if (ichar(c1)==42) then                           ! Multiplication
  i3=i1*i2
  write(*,'(2(I4,1X,A1,1X),I4)')i1,"*",i2,"=",i3

else if (ichar(c1)==47) then                           ! Division
  i3=i1/i2
  i4=i1-(i1/i2)*i2
  write(*,'(I4,1X,A1,1X,I4,1X,A1,1X,I4,1X,A7,1X,I4)') &
        i1,"/",i2,"=",i3,"divisor",i4

else
  write(*,*)"Input +,-,*,or /."

end if


end program practice
