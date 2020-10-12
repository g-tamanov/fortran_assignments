program L5T1
    integer mass(10), iflag, sum
    do i = 1, 10
        mass(i) = irand(iflag)
        sum = sum + mass(i)
    end do
    write(*, *) " сумма элементов массива = ",sum

end program L5T1

!program L5T1
!    implicit none
!    integer, allocatable :: matrix(:)
!    integer size
!    size=10
!    call random_seed(size=size)
!    write (*, *) size
!    size=10
!    allocate(matrix(size))
!    call random_seed(get=matrix)
!    write (*, *) matrix
!
!
!end program L5T1