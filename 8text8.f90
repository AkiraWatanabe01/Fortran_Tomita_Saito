! Page 140  8-8
! Monte Carlo method
! The latest version (2016/11/5)

! ---------------Subroutine------------------
subroutine ran(p)
implicit none
 
 real(8),intent(out)::p

 call random_seed
 call random_number(p)

end subroutine ran
!--------------------------------------------

! ---------------Main------------------------
program practice
implicit none

 integer::i,n=0,im=2**20
 real(8)::x,y,z,vol

 do i=1,im
    call ran(x)
    call ran(y)
    call ran(z)

    if (x**2+y**2+z**2<=1.0d0) then
       n=n+1
    end if

 end do

 vol=8.0d0*dble(n)/dble(im)

 write(*,'(F10.8)')vol


end program practice
