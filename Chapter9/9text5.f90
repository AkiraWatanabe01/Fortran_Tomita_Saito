! Page 172 9-5
! 9text5.f90
! The latest version 2025/1/1

module vector_operator

        implicit none
        type vector
                real :: x, y, z
        end type
        interface operator(.cross.)
                module procedure cross_product
        end interface

contains
  
        function cross_product(u, v) result(w)
 
                implicit none
                type(vector), intent(in) :: u, v
                type(vector) :: w
  
                w%x = u%y * v%z - u%z * v%y
                w%y = u%z * v%x - u%x * v%z
                w%z = u%x * v%y - u%y * v%x

        end function cross_product
end module vector_operator

program practice

        use vector_operator, only : vector, operator(.cross.)
        implicit none
        type(vector) :: vector_1, vector_2, vector_product

        write(*,*)"Input 3 components of the first vector"
        read(*,*)vector_1
        write(*,*)"Input 3 components of the second vector"
        read(*,*)vector_2

        vector_product = vector_1 .cross. vector_2

        write(*,*)"vector product :"
        write(*, '(1X, "(", F9.4, ",", F9.4, ",", F9.4, ")")')vector_product

end program practice
