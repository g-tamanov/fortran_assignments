program L5T3
        implicit none
        integer, allocatable :: matrix(:)
        integer seed,i,max,min
        call random_seed(size=seed)
        allocate(matrix(seed))
        call random_seed(get=matrix)

        do i=1 ,size(matrix,1)
            if(matrix(i)>0) then
                max=max+matrix(i)
            end if
            if(matrix(i)<0) then
                min=min-matrix(i)
            end if
        end do
        write (*, *) "сумма положительных чисел: ",max
        write (*, *) "сумма отрицательных чисел: ",min

end program L5T3