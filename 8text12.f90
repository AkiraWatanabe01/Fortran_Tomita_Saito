! Page 141  8-12
! Anagram
! The latest version (2016/11/5)

! -----------------Subroutine--------------------
recursive subroutine permutation(So,Ta,Num)
implicit none

 integer::Num,i,j,r=9,Tmp,N=9,deno,nume
 integer,dimension(1:9)::SoTmp
 integer::So(:),Ta(:)
  
 if (Num==r+1) then
    call output
 else
    do i=1,N-Num+1
       Ta(Num)=So(i)
       If (Num<=r) then    
          do j=1,N-Num+1
             SoTmp(j)=So(j) 
          end do  
          Tmp=So(N-Num+1)
          So(N-Num+1)=So(i)
          So(i)=Tmp
       end if

       call permutation(So,Ta,Num+1)
 
       if (Num<=r) then
          do j=1,N-Num+1 
             So(j)=SoTmp(j) 
          end do
       end if
     end do
 end if
   
  contains
! ------------------Internal subroutine----------------------
    subroutine output
      deno=Ta(1)*10000+Ta(2)*1000+Ta(3)*100+Ta(4)*10+Ta(5)
      nume=Ta(6)*1000+Ta(7)*100+Ta(8)*10+Ta(9)
      if (mod(deno,nume)==0) then
         write(*,*)deno,"/",nume," = ",deno/nume  
      end if
    end subroutine output
! -----------------------------------------------------------

end subroutine permutation
!------------------------------------------------------------


! ------------------Main-------------------------------------
program practice
implicit none
 integer::r=9,N=9,i
 integer,allocatable::So(:),Ta(:)
 interface
   subroutine permutation(So,Ta,Num)
     integer::So(:),Ta(:)
   end subroutine permutation
 end interface

 allocate(So(1:9),Ta(1:9))
 do i=1,9
    So(i)=i 
    Ta(i)=0
 end do

 call permutation(So,Ta,1)

end program practice
