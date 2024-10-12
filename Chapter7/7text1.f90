! Page 105  7-1
! 7text1.f90
! The latest version  (2024/10/12)

program practice
implicit none

	character(20)::word, modified_word			! The maximum length of the word is 20
	integer::i
	integer, dimension(1:20)::converted_word
	write(*, '(A)', advance='no')"Input a word :"
	read(*, '(A20)')word

	do i = 1, 20
		! Convert word into an integer
		converted_word(i)=ichar(word(i:i))
  
		if ((converted_word(i) <= 90).and.(converted_word(i) >= 65)) then
			modified_word(i:i) = char(converted_word(i) + 32)
		else
			modified_word(i:i) = word(i:i)
		end if
	end do

	write(*,*)modified_word(1:20)

end program practice
