program L5T3
    implicit none
    integer, allocatable :: matrix(:)
    integer s, i, min, max
    real hav
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
            min = abs(min +matrix(i))
        end if
        write (*, *) "i: ", matrix(i)
    end do
    write (*, *) "сумма положительных чисел: ", max
    write (*, *) "сумма отрицательных чисел: ", min

end program L5T3