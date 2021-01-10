! Page 105  7-2
!
! The latest version (2016/11/4)

program practice
implicit none

 character(39)::iaxis,faxis,maxis,axis,line
 real::r,rerad=18.0e0/3.1415927e0
 integer::k,p,q,s,t,u

! --------------axis-----------------
 iaxis="+1-------------------------------------"
 faxis="-1-------------------------------------"
 maxis=" 0-------------------------------------"
  axis=" |                  |                  "

 do k=100,-100,-4
    if (k==100) then
       line(:)=iaxis(:)
    else if (k==0) then
       line(:)=maxis(:)
    else if (k==-100) then
       line(:)=faxis(:)
    else
       line(:)=axis(:)
    end if
! ------------------------------------

! ---------------sin------------------
    r=k*1.0e-2
    p=nint(rerad*asin(r))+21
    line(p:p)='s'
  
    if (k<=0) then
       t=nint((-1.0e0)*rerad*asin(r))+3
       line(t:t)='s'
    else 
       u=nint((-1.0e0)*rerad*asin(r))+39
       line(u:u)='s'
    end if
!-------------------------------------

!----------------cos------------------
    q=nint((-1.0e0)*rerad*acos(r))+21
    line(q:q)='c'
    s=nint(rerad*acos(r))+21
    line(s:s)='c'
!-------------------------------------

!-------------output------------------
    write(*,'(A39)')line(:)

 end do


end program practice
