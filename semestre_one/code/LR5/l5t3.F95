program L5T3
    implicit none
    integer, allocatable :: matrix(:)
    integer s, i, min, max
    real hav
    OPEN (1, FILE = 'l5t3.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    max = 0
    min = 0
    s = 10
    allocate(matrix(s))

    do i = 1, size(matrix, 1)
        call random_number(hav)
        matrix(i) = anint(hav * 200) - 100
        if(matrix(i)>0) then
            max = abs(max + matrix(i))
        end if
        if(matrix(i)<0) then
            min = abs(-min + matrix(i))
        end if
    end do

    write(*, *) "массив: "
    write(1, *) "массив: "
    write(*, 2)(matrix(i), i = 1, size(matrix))
    write(1, 2)(matrix(i), i = 1, size(matrix))

    write (*, 3)  max
    write (1, 3)  max
    write (*, 4)  min
    write (1, 4)  min

    2 format(5(2x, i15))
    3 format("сумма положительных чисел: ", i4)
    4 format("сумма отрицательных чисел: ", i4)
    close(1)

end program L5T3