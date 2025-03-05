! Page 105 7-5
! 7text5.f90
! The latest version (2025/3/5)

program practice

	implicit none
	character(len = 80) :: sentence
	character(len = 80) :: sentence_code = ' ', sentence_decode = ' '
	integer :: k = 3
	integer :: length_sentence, num_stored_char = 0
	integer :: position_start, position, offset_start, offset
	integer :: key

	! Input the sentence
	write(*,*)"Input one sentence."
	read(*, '(A)')sentence(:)
	length_sentence = len_trim(sentence(:))

	! CODE
	position_start = 1
	position = position_start
	offset = position_start
	do while (num_stored_char < length_sentence)
		sentence_code(offset:offset) = sentence(position:position)
		num_stored_char = num_stored_char + 1
		offset = offset + 1
		position = position + k
		if (position > length_sentence) then
			position_start = position_start + 1 
			position = position_start 
		end if
	end do

	! Display the coded sentence
	write(*, '(A80)')sentence_code(:)

	! Input the key number
	do 
		write(*, '(A)', advance = 'no')"Input a key integer :"
		read(*,*)key
		if (key > 0) exit
	end do

	! DECODE
	num_stored_char = 0
	offset_start = 1
	offset = offset_start
	position = offset_start
	do while (num_stored_char < length_sentence)
		sentence_decode(offset:offset) = sentence_code(position:position)
		num_stored_char = num_stored_char + 1
		offset = offset + key
		position = position + 1
		if (offset > length_sentence) then
			offset_start = offset_start + 1 
			offset = offset_start 
		end if
	end do

	! Display the decoded sentence
	write(*, '(A80)')sentence_decode(:)

end program practice
