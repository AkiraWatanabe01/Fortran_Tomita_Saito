! Page 106 7-9
! 7text9.f90
! The latest version (2025/3/6)
! Caution: The apostrophe is treated as a single quotation.

program practice

	implicit none
	character(len = 80) :: line(1:100)
	character(len = 15) :: word(1:2000) = ' '
	character(len = 15) :: word_temp
	integer :: i, j, k
	integer :: io
	integer :: num_line, num_word, length_word
	integer :: buf_num
	integer :: offset
	integer :: counter

	! Read a file
	open (8, file = "7text9.txt")
	do i = 1, 100
		read(8, '(A80)', iostat = io)line(i)(1:80)
		if (io > 0) stop "Input file error"
		if (io < 0) exit
	end do
	close(8)

	num_line = i - 1

	! Parse
	k = 1
	do i = 1, num_line
		offset = 1
		do j = 1, 80
			if (line(i)(j:j) == ' ') then
				if ((j + 1) == 80) then
					k = k + 1
					exit
				end if

				if (line(i)((j + 1):(j + 1)) /= ' ') then
					k = k + 1
					offset = 1
				end if
				cycle
			end if

			if (line(i)(j:j) == ',') then
				cycle
			end if

			if (line(i)(j:j) == '.') then
				cycle
			end if

			if (line(i)(j:j) == '?') then
				cycle
			end if

			! Convert uppercase letters to lowercase
			buf_num = ichar(line(i)(j:j))
			if ((ichar('A') <= buf_num) .and. (buf_num <= ichar('Z'))) then
				line(i)(j:j) = char(ichar(line(i)(j:j)) + ichar('a') - ichar('A'))
			end if

			word(k)(offset:offset) = line(i)(j:j) 
			offset = offset + 1
		end do
	end do

	num_word = k

	! Convert trailing hyphen and apostrophe
	do k = 1, num_word
		! Hyphen
		length_word = len_trim(word(k)(:))
		if (word(k)(length_word:length_word) == '-') then
			do j = length_word, 15
				offset = j - length_word + 1
				word(k)(j:j) = word(k + 1)(offset:offset)
			end do
			word(k + 1)(:) = ' ' 
		end if

		! Apostrophe
		buf_num = index(word(k)(:), "'")
		if (buf_num /= 0) then
			word(k)(buf_num:15) = ' '
		end if 
	end do

	! Sorting
	do k = num_word, 2, -1
		do i = 1, (k - 1)
			if (word(i) > word(i + 1)) then
				word_temp = word(i)(:)
				word(i)(:) = word(i + 1)(:)
				word(i + 1)(:) = word_temp
			end if
		end do
	end do

	! Display the result
	counter = 1
	do k = 1, num_word
		buf_num = ichar(word(k)(1:1))
		if ((buf_num < ichar('a')) .or. (ichar('z') < buf_num)) then
			cycle
		end if

		if (word(k)(:) == word(k + 1)(:)) then
			counter = counter + 1
			cycle
		end if

		if ((word(k)(1:1) == 'i') .and. (word(k)(2:2) == ' ')) then
			word(k)(1:1) = 'I'
		end if

		write(*, '(A15, 1X, I4)')word(k)(1:15), counter
		counter = 1
	end do

end program practice
