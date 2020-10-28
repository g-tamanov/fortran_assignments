program L5T3
    implicit none
    integer, allocatable :: matrix(:)
    integer s, i, min, max
    real hav
    OPEN (1, FILE = 'l5t3.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    max=0
    min=0
    s = 10
    allocate(matrix(s))

    do i = 1, size(matrix, 1)
        call random_number(hav)
        matrix(i) = anint(hav * 200) - 100
        if(matrix(i)>0) then
            max = abs(max +matrix(i))
        end if
        if(matrix(i)<0) then
            min = abs(-min +matrix(i))
        end if
        write (*, *) "matrix(i) ", i,"=",matrix(i)
        write (1, *) "matrix(i) ", i,"=",matrix(i)

    end do
    write (*, *) "сумма положительных чисел: ", max
    write (1, *) "сумма положительных чисел: ", max
    write (*, *) "сумма отрицательных чисел: ", min
    write (1, *) "сумма отрицательных чисел: ", min
    close(1)

end program L5T3