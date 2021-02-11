program L5T2
        implicit none
        integer, allocatable :: matrix(:)
        integer seed,i,max,min,j
        OPEN (1, FILE = 'l5t2.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

        call random_seed(size=seed)
        allocate(matrix(seed))
        call random_seed(get=matrix)

        min=minloc(matrix,1)
        max=maxloc(matrix,1)
        write(*,*) "массив: "
        write(1,*) "массив: "
        write(*,2)(matrix(i),i=1,size(matrix))
        write(1,2)(matrix(i),i=1,size(matrix))

        write (*, 4) matrix(max),matrix(min)
        write (1, 4) matrix(max),matrix(min)

        write (*, 3) abs(min-max)
        write (1, 3) abs(min-max)

        2 format(5(2x, i15))
        3 format("разница индексов между значениями max и min: ", i4)
        4 format("значение max", i15 ," min: ", i15)
        close(1)

end program L5T2