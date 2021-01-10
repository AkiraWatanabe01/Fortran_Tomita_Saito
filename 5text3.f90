! Page56 5-3
!
! The latest version (2016/10/21)

program practice
implicit none
 
 integer::score,i=1,n,max,min,base
 character(1)::ans

 write(*,*)"Input the score."
 read(*,*)base                   ! The first score  
 max=base
 min=base
 do n=1,1000                     ! In this program, the number of students is within 1001.        
   write(*,*)"Continue?"
   read(*,*)ans
   if ((ans=='N').or.(ans=='n')) exit

   i=i+1
   read(*,*)score                      

   if (score>max) then           ! Compare the score input just now with the maximum score.         
      max=score
   end if

   if (score<min) then           ! Compare the score input just now with the minimum score.      
      min=score
   end if

 end do
 
 ! The result

 write(*,*)'The number of students :',i              
 write(*,*)'The maximum score :',max
 write(*,*)'The minimum score :',min

end program practice
