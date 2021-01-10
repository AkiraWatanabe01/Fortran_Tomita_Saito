!Page 105 7-6  
!
!The latest version(2016/3/23)

program main
  
  implicit none
  integer,parameter::max=12
  integer::i,n,c(0:max)=0
  character(100)::fmt
  character(2)::f1(6)=(/'34','28','22','16','10',' 4'/),f2(6)=(/'31','25','19','13',' 7',' 1'/)   ! for interval  
  character(2)::g1(6)=(/' 1',' 3',' 5',' 7',' 9','11'/),g2(6)=(/' 2',' 4',' 6',' 8','10','12'/)   ! for repeat

  c(1)=1
  
  do i=1,max
     
     c(1:i)=c(1:i)+c(0:i-1)
 
     if (mod(i,2)==1) then
        n=(i+1)/2
        fmt="(I2,A1,"//f1(n)//"X,"//g1(n)//"I6)"    
        print fmt,i,':',c(1:i)                      ! print '(I2,A1,_X,_I6)',i,':',c(1:i)                     
  
     else
        n=i/2
        fmt="(I2,A1,"//f2(n)//"X,"//g2(n)//"I6)"
        print fmt,i,':',c(1:i)                      ! print '(I2,A1,_X,_I6)',i,':',c(1:i)
  
     end if

  end do

end program main
