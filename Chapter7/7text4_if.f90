! Page 105 7-4
! 7text4_if.f90
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

	if (char_last == ichar('s')) then
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if (char_last == ichar('x')) then
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if (char_last == ichar('z')) then
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one == ichar('c')).and.(char_last == ichar('h'))) then
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one == ichar('s')).and.(char_last == ichar('h'))) then
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one /= ichar('a')).and.(char_last == ichar('o'))) then
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one /= ichar('i')).and.(char_last == ichar('o'))) then
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one /= ichar('u')).and.(char_last == ichar('o'))) then
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one /= ichar('e')).and.(char_last == ichar('o'))) then
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one /= ichar('o')).and.(char_last == ichar('o'))) then
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one /= ichar('a')).and.(char_last == ichar('y'))) then
		word_modified(length_word:length_word) = 'i'
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one /= ichar('i')).and.(char_last == ichar('y'))) then
		word_modified(length_word:length_word) = 'i'
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one /= ichar('u')).and.(char_last == ichar('y'))) then
		word_modified(length_word:length_word) = 'i'
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one /= ichar('e')).and.(char_last == ichar('y'))) then
		word_modified(length_word:length_word) = 'i'
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one /= ichar('o')).and.(char_last == ichar('y'))) then
		word_modified(length_word:length_word) = 'i'
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one == ichar('l')).and.(char_last == ichar('f'))) then
		word_modified(length_word:length_word) = 'v'
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_two == ichar('e')).and.(char_last_but_one == ichar('a')).and.(char_last == ichar('f'))) then
		word_modified(length_word:length_word) = 'v'
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_two == ichar('o')).and.(char_last_but_one == ichar('a')).and.(char_last == ichar('f'))) then
		word_modified(length_word:length_word) = 'v'
		word_modified((length_word + 1):(length_word + 1)) = 'e'
		word_modified((length_word + 2):(length_word + 2)) = 's'
	else if ((char_last_but_one == ichar('f')).and.(char_last == ichar('e'))) then
		word_modified((length_word - 1):(length_word - 1)) = 'v'
		word_modified((length_word + 1):(length_word + 1)) = 's'
	else
		word_modified((length_word + 1):(length_word + 1))='s'
	end if

	write(*,*)"Original word        : Mutiple"
	write(*,*)word(:), " : ", word_modified(:)

end program practice
