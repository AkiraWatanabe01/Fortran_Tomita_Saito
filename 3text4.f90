! p28 3-4
!
! The latest version (2016/10/10)

program practice
implicit none

 integer::n,div

 write(*,*)'Input integer n'
 read(*,*)n
 
 div=mod(n,7)
  
  if (div==0) then          ! if () then ... else if () then ... else ... end if 
     write(*,*)'Sunday'

  else if (div==1) then 
     write(*,*)'Monday'

  else if (div==2) then 
     write(*,*)'Tuesday'

  else if (div==3) then 
     write(*,*)'Wednesday'

  else if (div==4) then 
     write(*,*)'Thursday'

  else if (div==5) then 
     write(*,*)'Friday'

  else  
     write(*,*)'Saturday'

  end if


end program practice
