! Page 85 6-4
!
! The latest version (2016/10/29)


program practice
implicit none

 integer::money,i
 integer,dimension(1:10)::paper

 write(*,'(a)',advance='no')"Input money :"
 read(*,*)money

 paper(:)=(/(0,i=1,10)/)
 do while (money>=10000)
     paper(1)=paper(1)+1
     money=money-10000
 end do

 do while (money>=5000)
     paper(2)=paper(2)+1
     money=money-5000
 end do

 do while (money>=2000)
     paper(3)=paper(3)+1
     money=money-2000
 end do

 do while (money>=1000)
     paper(4)=paper(4)+1
     money=money-1000
 end do

 do while (money>=500)
     paper(5)=paper(5)+1
     money=money-500
 end do

 do while (money>=100)
     paper(6)=paper(6)+1
     money=money-100
 end do

 do while (money>=50)
     paper(7)=paper(7)+1
     money=money-50
 end do

 do while (money>=10)
     paper(8)=paper(8)+1
     money=money-10
 end do

 do while (money>=5)
     paper(9)=paper(9)+1
     money=money-5
 end do

 do while (money>=1)
     paper(10)=paper(10)+1
     money=money-1
 end do

write(*,'(I5,A4,1X,I4)')10000,"yen:",paper(1)
write(*,'(I5,A4,1X,I4)')5000,"yen:",paper(2)
write(*,'(I5,A4,1X,I4)')2000,"yen:",paper(3)
write(*,'(I5,A4,1X,I4)')1000,"yen:",paper(4)
write(*,'(I5,A4,1X,I4)')500,"yen:",paper(5)
write(*,'(I5,A4,1X,I4)')100,"yen:",paper(6)
write(*,'(I5,A4,1X,I4)')50,"yen:",paper(7)
write(*,'(I5,A4,1X,I4)')10,"yen:",paper(8)
write(*,'(I5,A4,1X,I4)')5,"yen:",paper(9)
write(*,'(I5,A4,1X,I4)')1,"yen:",paper(10)

end program practice
