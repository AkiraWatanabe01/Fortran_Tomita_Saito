! Page 105  7-1
! 7text1.f90
! The latest version  (2025/3/5)

program practice

	implicit none
	character(20) :: word, word_modified			! The maximum length of the word is 20
	integer :: i
	integer, dimension(1:20) :: num_array_converted_word

	write(*, '(A)', advance = 'no')"Input a word :"
	read(*, '(A20)')word

	do i = 1, 20
		! Convert a character into an integer
		num_array_converted_word(i) = ichar(word(i:i))
  
		if ((num_array_converted_word(i) <= 90) .and. (num_array_converted_word(i) >= 65)) then
			word_modified(i:i) = char(num_array_converted_word(i) + 32)
		else
			word_modified(i:i) = word(i:i)
		end if
	end do

	write(*,*)word_modified(1:20)

end program practice
