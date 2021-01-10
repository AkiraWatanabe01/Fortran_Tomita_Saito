! p28 3-1
!
! The latest version (2016/10/10)

program practice
implicit none
 
 integer::n,l,m

 write(*,*)'How long did you talk on the phone?'
 read(*,*)n
 m=n-180

! if () then ... else if () then ... else ... end if

  if ((0<=n).and.(n<180)) then
     write(*,*)'Your telephone rate is 10 yen'
  else if (mod(m,6)==0) then 
     l=11+m/6
     write(*,*)'Your telephone rate is',l,'yen'
  else if (mod(m,6)/=0) then 
     l=11+(m-mod(m,6))/6
     write(*,*)'Your telephone rate is',l,'yen'
  else
     write(*,*)'Input > 0'
  end if




end program practice
