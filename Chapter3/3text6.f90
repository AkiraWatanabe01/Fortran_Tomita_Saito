! p28 3-6
! 3text6.f90
! The latest version (2024/9/21)

program practice
implicit none
 
	integer::i1, j1, k1, i2, j2, k2, t1, t2, dif, i3, j3, k3

	write(*,*)'Input i1 hour, j1 minute, and k1 second'
	write(*,*)'i1 ?'
	read(*,*)i1
	write(*,*)'j1 ?'
	read(*,*)j1
	write(*,*)'k1 ?'
	read(*,*)k1

	write(*,*)'Input i2 hour, j2 minute, and k2 second'
	write(*,*)'i2 ?'
	read(*,*)i2
	write(*,*)'j2 ?'
	read(*,*)j2
	write(*,*)'k2 ?'
	read(*,*)k2

	! Convert hour and minute into second
	t1 = 3600 * i1 + 60 * j1 + k1
	t2 = 3600 * i2 + 60 * j2 + k2
 
 	! Calculate the difference
	dif = abs(t1 - t2)
	i3 = dif / 3600
	j3 = (dif - i3 * 3600) / 60
	k3 = dif - i3 * 3600 - j3 * 60

	write(*,*)i3, 'hour', j3, 'minute', k3, 'second'

end program practice
