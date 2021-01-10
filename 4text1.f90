! p42 4-1
!
! The latest version (2016/10/14)

program practice
implicit none

 integer::t,h,m
 real::s

 write(*,*)'Input integer t (< 86400000).'
 read(*,*)t                                   ! ex) t = 80020531
 
 h=t/3600000                                  ! h = 80020531 / 3600000 = 22 (because h is an integer)                             
 m=(t-h*3600000)/60000                        ! m = 820531 / 60000 = 13 (because m is an integer)                           
 s=(t-h*3600000-m*60000)/1.0e3                ! s = 40531 / 1.0e3 = 40.531

 if (h>11.and.h<24) then                        
    write(*,*)'P.M.',h-12,':',m,':',s         ! 22 - 12 = 10
 else if (h>=0.and.h<=11) then
    write(*,*)'A.M.',h,':',m,':',s 
 else
    write(*,*)'Input t (< 86400000).'
 end if



end program practice
