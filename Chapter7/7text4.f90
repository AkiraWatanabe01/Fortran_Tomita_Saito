! Page 105 7-4
! 7text4.f90
! The latest version (2025/3/5)

program practice

	implicit none
	character(len = 20) :: word, word_modified
	character(len = 1) :: char_last, char_last_but_one, char_last_but_two
	integer :: length_word

	do
		write(*, '(A)', advance = 'no')"Input a word :"
		read(*, '(A)')word
		length_word = len_trim(word)
		if (length_word > 18) stop "Too long!"
		if (length_word > 0) exit
	end do

	char_last = word(length_word:length_word)
	char_last_but_one = word((length_word - 1):(length_word - 1))
	char_last_but_two = word((length_word - 2):(length_word - 2))
	word_modified(:) = word(:)

	select case (char_last)
	case ('s')
		goto 100
	case ('x')
		goto 100
	case ('z')
		goto 100
	case ('h')
		if (char_last_but_one == 'c') goto 100
		if (char_last_but_one == 's') goto 100
	case ('o')
		if (char_last_but_one /= 'a') goto 100
		if (char_last_but_one /= 'i') goto 100
		if (char_last_but_one /= 'u') goto 100
		if (char_last_but_one /= 'e') goto 100
		if (char_last_but_one /= 'o') goto 100
	case ('y')
		if (char_last_but_one /= 'a') goto 90
		if (char_last_but_one /= 'i') goto 90
		if (char_last_but_one /= 'u') goto 90
		if (char_last_but_one /= 'e') goto 90
		if (char_last_but_one /= 'o') goto 90
	case ('f')
		if (char_last_but_one == 'l') goto 91
		if ((char_last_but_two == 'e') .and. (char_last_but_one == 'a')) goto 91
		if ((char_last_but_two == 'o').and.(char_last_but_one == 'a')) goto 91
	case ('e')
		if (char_last_but_one == 'f') then
			word_modified((length_word - 1):(length_word - 1)) = 'v'
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
