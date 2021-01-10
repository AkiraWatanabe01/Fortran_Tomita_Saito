! Page57 5-6
! Pythagoras number 
! The latest version ( 2016/10/21)

program practice
implicit none

 integer::x,y,z,k,m,n
 real::z0,v

 do x=1,70                         ! x should be smaller than 71 (Make sure !)  
   do y=x+1,100
      z0=sqrt(real(x)**2.0E0+real(y)**2.0E0)
      v=z0-int(z0)
      if (v==0) then
         m=x
         n=y

! ----Euclidean algorithm----
         do
            k=mod(m,n)
            if (k==0) exit
            m=n
            n=k
         end do
! ---------------------------

         if (n==1) then
            z=int(z0)

            if (z<100) then          ! 0 < z < 100     
               write(*,*)x,y,z
            end if 
                
         end if

      end if

   end do
 end do



end program practice
