program L5T1
    integer mass(10), sum
    sum=0
    do i = 1, size(mass,1)
        mass(i) =  anint(hav * 2) - 1
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