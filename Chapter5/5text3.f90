! Page56 5-3
! 5text3.f90
! The latest version (2024/9/28)

program practice
implicit none

	integer::score
 	integer::max = -1, min = 10000000    ! initial maximum and minimum values
 	integer::num = 0                     ! number of students
	character(1)::answer
 
	do   
		write(*,*)"Continue?"
		read(*,*)answer
		if ((answer == 'N').or.(answer == 'n')) exit

		num = num + 1
		write(*,*)"Input the score :"
		read(*,*)score                      

		! Compare the score input just now with the maximum score. 
		if (score > max) then        
			max = score
		end if

  		! Compare the score input just now with the minimum score.
		if (score < min) then
			min = score
		end if
	end do
 
	! The result
 	if (num == 0) stop "No students!"
	write(*,*)"The number of students : ", num              
	write(*,*)"The maximum score      : ", max
	write(*,*)"The minimum score      : ", min

end program practice
