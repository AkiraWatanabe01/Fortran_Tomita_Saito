! Page 105 7-4
! 7text4.f90
! The latest version (2024/10/14)

program practice
implicit none

	character(len = 20)::word, word_modified
	integer::char_last, char_last_but_one, char_last_but_two
	integer::length_word

	do
		write(*, '(A)', advance='no')"Input a word :"
		read(*, '(A)')word
		length_word = len_trim(word)
		if (length_word > 18) stop "Too long!"
		if (length_word > 0) exit
	end do

	char_last = ichar(word(length_word:length_word))
	char_last_but_one = ichar(word((length_word - 1):(length_word - 1)))
	char_last_but_two = ichar(word((length_word - 2):(length_word - 2)))
	word_modified(:) = word(:)

	select case (char_last)
	case (ichar('s'))
		goto 100
	case (ichar('x'))
		goto 100
	case (ichar('z'))
		goto 100
	case (ichar('h'))
		if (char_last_but_one == ichar('c')) goto 100
		if (char_last_but_one == ichar('s')) goto 100
	case (ichar('o'))
		if (char_last_but_one /= ichar('a')) goto 100
		if (char_last_but_one /= ichar('i')) goto 100
		if (char_last_but_one /= ichar('u')) goto 100
		if (char_last_but_one /= ichar('e')) goto 100
		if (char_last_but_one /= ichar('o')) goto 100
	case (ichar('y'))
		if (char_last_but_one /= ichar('a')) goto 90
		if (char_last_but_one /= ichar('i')) goto 90
		if (char_last_but_one /= ichar('u')) goto 90
		if (char_last_but_one /= ichar('e')) goto 90
		if (char_last_but_one /= ichar('o')) goto 90
	case (ichar('f'))
		if (char_last_but_one == ichar('l')) goto 91
		if ((char_last_but_two == ichar('e')).and.(char_last_but_one == ichar('a'))) goto 91
		if ((char_last_but_two == ichar('o')).and.(char_last_but_one == ichar('a'))) goto 91
	case (ichar('e'))
		if (char_last_but_one == ichar('f')) then
			word_modified((length_word - 1):(length_word - 1)) = 'v'
			word_modified((length_word + 1):(length_word + 1)) = 's'
		end if
	end select

	word_modified((length_word + 1):(length_word + 1)) = 's'
	goto 200

90	word_modified(length_word:length_word) = 'i'
	goto 100

91	word_modified(length_word:length_word) = 'v'

100	word_modified((length_word + 1):(length_word + 1)) = 'e'
	word_modified((length_word + 2):(length_word + 2)) = 's'
	goto 200

200	write(*,*)"Original word        : Mutiple"
	write(*,*)word(:), " : ", word_modified(:)

end program practice
