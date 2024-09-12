! Page86 6-11
! 6text11.f90 
! The latest version (2024/9/12)

program practice
implicit none

	integer::n
	integer::num_party			! number of parties
	integer::result(2)			! return value of maxloc function
	integer::index1, index2
	integer::i, j
	integer, allocatable::total_vote(:)	! total votes
	real, allocatable::num_ind_vote(:,:)	! number of votes for individuals
	character(len = 1)::alp_party(1:26) = (/'A','B','C','D','E','F','G', &
						'H','I','J','K','L','M','N', &
						'O','P','Q','R','S','T','U', &
						'V','W','X','Y','Z'/)

	write(*,*)"Input number of winners:"
	read(*,*)n
	write(*,*)"Input number of parties (1..26):"
	read(*,*)num_party
	if ((num_party < 1) .OR. (26 < num_party)) stop "Invalid value"

	allocate(total_vote(1:num_party))
	allocate(num_ind_vote(1:n, 1:num_party))

	write(*,*)"Input number of total votes for each party:"
	do i = 1, num_party
		read(*,*)total_vote(i)
	end do

	do i = 1, num_party
		do j = 1, n
			num_ind_vote(j, i) = real(total_vote(i)) / j 
		end do
	end do

	do i = 1, n
		result = maxloc(num_ind_vote)
		index1 = result(1)
		index2 = result(2)
		write(*,'(1x, a1,a1,i2,a7,1x,f12.3)')alp_party(index2), "-", index1, ", vote:", maxval(num_ind_vote) 
		num_ind_vote(index1, index2) = -1
	end do

end program practice
