! Page 140
! 8text7.f90
! The latest version (2024/12/22)
subroutine game_manager(num_trial)

	implicit none
	integer, intent(in) :: num_trial
	integer :: i
	integer :: result, counter = 0

	do i = 1, num_trial
		write(*, '(A7, 1X, I4, 1X, A1, 1X, I4)')"Trial :", i, "/", num_trial
		call transparency_game_number(result)
		if (result == 1) then
			counter = counter + 1
		end if
		write(*,*)""
	end do

	write(*,*)"Your grade (Number of correct answers / Number of trials) is"
	write(*, '(I4, 1X, A1, 1X, I4)')counter, "/", num_trial

end subroutine game_manager	

subroutine transparency_game_number(result)

	implicit none
	real :: num_rnd
	integer, intent(out) :: result
	integer :: num_pc, num_player
	call random_seed

	call random_number(num_rnd)
	num_pc = anint(9 * num_rnd)

	write(*, '(A)', advance='no')"I chose one number (0-9). Guess which number I chose: "
	read(*,*)num_player 

	if (num_pc == num_player) then
		result = 1
		return
	end if

	result = 0
	return

end subroutine transparency_game_number

program practice

	implicit none
	integer:: num_trial = 10

	call game_manager(num_trial)

end program practice
