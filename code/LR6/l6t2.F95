program L6T2
    integer  matrix (10, 10), min, max, pointx(10), pointy(10)
    integer iimin, ijmin, iimax, ijmax, pointi
    real x
    OPEN (1, FILE = 'l6t2.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')
    pointi = 0
    CALL RANDOM_SEED()

    do i = 1, size(matrix, 1)
        do j = 1, size(matrix, 2)
            call random_number(x)
            matrix(i, j) = anint(10 * x) - 5
            if(matrix(i, j)==0) then
                pointx(pointi) = i
                pointy(pointi) = j
                pointi = pointi + 1
            end if
        end do
    end do
    write(*, 10)  ((matrix(i, j), i = 1, 10), j = 1, 10)
    write(1, 10)  ((matrix(i, j), i = 1, 10), j = 1, 10)
    10   format(2x, 10i10)
    do i = 1, pointi
        write(*, *) "Координаты нулевых элементов массива"," x: ",pointx(i),"y:",pointy(i)
        write(1, *) "Координаты нулевых элементов массива"," x: ",pointx(i),"y:",pointy(i)
    end do
    close(1)

end program L6T2