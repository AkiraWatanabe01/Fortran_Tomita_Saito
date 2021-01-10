! Page 105  7-4
!
! The latest version (2016/11/4)

program practice
implicit none

 character(20)::word1,word2
 integer::k=0,l,m,n

 do while (k<=1)
    write(*,'(a)',advance='no')"Input a word :"
    read(*,'(A)')word1
    k=len_trim(word1)        ! The length of word1 
 end do

 l=ichar(word1(k:k))         ! The last character
 m=ichar(word1(k-1:k-1))     ! The last character but one
 n=ichar(word1(k-2:k-2))     ! The last character but two
 word2=word1

 if (l==115) then            ! 1)
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==120) then       ! 1)
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==122) then       ! 1)
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (m==99.and.l==104) then    ! 1)
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (m==115.and.l==104) then   ! 1)
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==111.and.m/=97) then    ! 2)
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==111.and.m/=105) then   ! 2)
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==111.and.m/=117) then   ! 2)
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==111.and.m/=101) then   ! 2)
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==111.and.m/=111) then   ! 2)
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==121.and.m/=97) then    ! 3)
    word2(k:k)='i'
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==121.and.m/=105) then   ! 3)
    word2(k:k)='i'
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==121.and.m/=117) then   ! 3)
    word2(k:k)='i'
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'          
 else if (l==121.and.m/=101) then   ! 3)
    word2(k:k)='i'
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==121.and.m/=111) then   ! 3)
    word2(k:k)='i'
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==102.and.m==97.and.n==101) then   ! 4)
    word2(k:k)='v'
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==102.and.m==97.and.n==111) then  ! 4)
    word2(k:k)='v'
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==102.and.m==108) then             ! 4)
    word2(k:k)='v'
    word2(k+1:k+1)='e'
    word2(k+2:k+2)='s'
 else if (l==101.and.m==102) then             ! 5)
    word2(k-1:k-1)='v'
    word2(k:k)='e'
    word2(k+1:k+1)='s'
 else                                         ! 6)
    word2(k+1:k+1)='s'
 end if

 write(*,'(A)')word2


end program practice
