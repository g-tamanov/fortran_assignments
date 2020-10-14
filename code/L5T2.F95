program L5T2
        implicit none
        integer, allocatable :: matrix(:)
        integer seed,i,max,min
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
        write (*, *) "разница индексов между значениями max и min: ",abs(min-max)

end program L5T2