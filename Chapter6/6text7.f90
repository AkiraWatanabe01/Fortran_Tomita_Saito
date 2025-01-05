! Page 86 6-7
! 6text7.f90
! The latest version (2024/10/12)
!
! gfortran -o 6text7 6text7.f90
! ./6text7 < 6text7.dat

program practice

	implicit none
	integer :: i, j
	integer :: n = 100
	integer, allocatable :: grade(:, :), square_grade(:, :), array_product(:)
	integer, allocatable :: student(:), student_total_square_grade(:), student_total_grade(:)
	real :: covariance
	real :: student_mean, student_variance, student_standard_deviation
	real, dimension(1:5) :: subject_mean, subject_variance, subject_standard_deviation
	real, dimension(1:5, 1:5) :: correlation
	real, allocatable :: student_hensachi(:)
	character(len = 14) :: subject_name(1:5) = (/"       English", "          Math", "      Japanese", "       Science", "Social-studies"/)

	allocate(grade(1:n, 1:5), square_grade(1:n, 1:5), array_product(1:n))
	allocate(student(1:n), student_total_grade(1:n), student_total_square_grade(1:n), student_hensachi(1:n))

	do i = 1, n
		read(*,*)student(i), (grade(i, j), j = 1, 5)
	end do

	! SUBJECT MEAN
	write(*,*)"Mean :"
	subject_mean(1:5) = real(sum(grade, dim = 1)) / size(grade, dim = 1)
	write(*, '(5(A14, 1X))')(subject_name(i), i = 1, 5)
	write(*, '(5(F14.6, 1X))')(subject_mean(i), i = 1, 5)

	! SUBJECT VARIANCE
	write(*,*)"Variance :"
	do i = 1, n
		do j = 1, 5
			square_grade(i, j) = grade(i, j) ** 2
		end do
	end do
	subject_variance(1:5) = real(sum(square_grade, dim = 1)) / size(grade, dim = 1) - subject_mean(1:5) ** 2 
	write(*, '(5(A14, 1X))')(subject_name(i), i = 1, 5)
	write(*, '(5(F14.6, 1X))')(subject_variance(i), i = 1, 5)

	! SUBJECT STADARD DEVIATION
	write(*,*)"Standard deviation :"
	subject_standard_deviation(1:5) = sqrt(subject_variance(1:5))
	write(*, '(5(A14, 1X))')(subject_name(i), i = 1, 5)
	write(*, '(5(F14.6, 1X))')(subject_standard_deviation(i), i = 1, 5)

	! CORRELATION COEFFICIENT
	write(*,*)"Correlation coefficient :"
	array_product(:) = 0
	do i = 1, 5
		do j = 1, 5
			array_product(:) = grade(:, i) * grade(:, j)
			covariance = real(sum(array_product)) / size(grade, dim = 1) - subject_mean(i) * subject_mean(j)
			correlation(i, j) = covariance / subject_standard_deviation(i) / subject_standard_deviation(j)
		end do
	end do

	write(*, '(15X, 5(A14, 1X))')(subject_name(i), i = 1, 5)
	do i = 1, 5
		write(*, '(A14, 1X, 5(F14.6, 1X))')subject_name(i), (correlation(i, j), j = 1, 5)
	end do

	! HENSACHI
	write(*,*)"Hensachi :"
	student_total_grade(1:n) = sum(grade, dim = 2)
	student_mean = real(sum(student_total_grade)) / size(student_total_grade)
	student_total_square_grade(1:n) = student_total_grade(1:n) ** 2
	student_variance = real(sum(student_total_square_grade)) / size(student_total_square_grade)
	student_standard_deviation = sqrt(student_variance)
	student_hensachi(1:n) = 50e0 + 10e0 * (student_total_grade(1:n) - student_mean) / student_standard_deviation

	write(*, '(A7, 7X, A11, 3X, A8)')"Student", "Total grade", "Hensachi"
	do i = 1, n 
		write(*, '(A7, 1X, I4, A1, 1X, I11, 2X, F9.6)')"Student", i, ":", student_total_grade(i), student_hensachi(i)
	end do

end program practice
