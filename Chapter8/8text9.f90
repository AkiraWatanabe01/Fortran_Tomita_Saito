! Page 141 8-9
! 8text9.f90
! The latest version  (2025/1/11)

subroutine rock_paper_scissors_game()

	implicit none
	integer :: player_choice
	real :: num_random

	write(*,*)"Your choice?"
	write(*,*)"1:Rock, 2:Paper, 3:scissors"

	do while (.TRUE.)
		read(*,*)player_choice
		if ((1 <= player_choice) .and. (player_choice <= 3)) exit
		write(*,*)"Choose 1 to 3."
	end do

	call random_seed
	call random_number(num_random)

	call fair_game(num_random)
!	call cheat_game(num_random)

contains

	subroutine fair_game(num_real)

		implicit none
		integer :: remainder
		real, intent(in) :: num_real

		remainder = mod(int(10000000 * num_real), 3)

		select case (remainder)
		case (0)
			write(*,*)"You win!"
		case (1)
			write(*,*)"You lose!"
		case default
			write(*,*)"Draw!"
		end select

	end subroutine fair_game

	subroutine cheat_game(num_real)

		implicit none
		integer :: remainder
		real, intent(in) :: num_real

		remainder = mod(int(10000000 * num_real), 10)

		select case (remainder)
		case (0:2)
			write(*,*)"You win!"
		case (3:6)
			write(*,*)"You lose!"
		case default
			write(*,*)"Draw!"
		end select

	end subroutine cheat_game

end subroutine rock_paper_scissors_game

program practice

	implicit none
	call rock_paper_scissors_game()

end program practice
