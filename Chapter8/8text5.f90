! Page 140 8-5
! 8text5.f90
! The latest version  (2025/1/11)

subroutine read_file(file_name, array_word, counter_word)

	implicit none
	character(len = 20), intent(in) :: file_name
	character(len = 20), intent(inout) :: array_word(1:1000)
	integer :: i
	integer :: num_io, file_status
	integer, intent(out) :: counter_word

	counter_word = 0

	open(8, FILE = file_name, IOSTAT = file_status)
		if (file_status > 0) stop "The file cannot be opened."
		do i = 1, 1000
			read(8, '(A20)', IOSTAT = num_io)array_word(i)
			if (num_io < 0) exit
			counter_word = counter_word + 1
		end do
	close(8)

end subroutine read_file

subroutine search_array(array_word, counter_word)

	implicit none
	character(len = 20), intent(inout) :: array_word(1:1000)
	character(len = 20) :: input_word, temp_word
	integer :: i, j
	integer, intent(in) :: counter_word

	write(*,*)"Input a word"
	read(*,*)input_word

	! Search the word in the array
	do i = 1, counter_word
		if (array_word(i) == input_word) then
			return
		end if
	end do

	! Add the word to the array
	array_word(counter_word + 1) = input_word

	! Sorting
	do i = counter_word, 2, -1
		do j = 1, i
			if (array_word(j) > array_word(j + 1)) then
				temp_word = array_word(j)(:)
				array_word(j)(:) = array_word(j + 1)(:)
				array_word(j + 1)(:) = temp_word
			end if
		end do
	end do

end subroutine search_array

program practice

	implicit none
	character(len = 20) :: file_name
	character(len = 20) :: array_word(1:1000) = ' '
	integer :: i
	integer :: counter_word

	file_name = "8text5.txt"
	call read_file(file_name, array_word, counter_word)
	call search_array(array_word, counter_word)

	write(*,*)"The content of the array is :"
	do i = 1, 1000
		if (array_word(i) == ' ') exit
		write(*, '(A)')array_word(i)
	end do

end program practice
