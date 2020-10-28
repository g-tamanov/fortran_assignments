program L5T2
        implicit none
        integer, allocatable :: matrix(:)
        integer seed,i,max,min,j
        OPEN (1, FILE = 'l5t2.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

        call random_seed(size=seed)
        allocate(matrix(seed))
        call random_seed(get=matrix)

!        do i=1 ,size(matrix,1)
!            if(matrix(i)>max) then
!                max=matrix(i)
!            end if
!            if(matrix(i)<min) then
!                min=matrix(i)
!            end if
!        end do
        min=minloc(matrix,1)
        max=maxloc(matrix,1)
        write(*,*) "array: ", matrix
        write(1,*) "array: ", matrix
        write (*, *) "разница индексов между значениями max и min: ",abs(min-max)
        write (1, *) "разница индексов между значениями max и min: ",abs(min-max)
        close(1)

end program L5T2