! p42 4-7
! 4text7.f90
! The latest version (2024/9/22)

program practice
implicit none

	logical, parameter::a = .TRUE., b = .TRUE.

	write(*,*)"T:True, F:False"

	! (1)
	write(*,*)(.NOT.(a.AND.b)).EQV.((.NOT.a).OR.(.NOT.b))

	! (2)
	write(*,*)(.NOT.(a.OR.b)).EQV.((.NOT.a).AND.(.NOT.b))

end program practice
