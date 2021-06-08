program lr5t2

    integer :: i, j, k, toch
    integer, parameter :: n = 5
    real, parameter :: e = 0.001
    real(8) a(n, n), b(n), x(n), alpha(n, n), beta(n), x0(n), p(n)

    OPEN (1, fILE = 'lr5t2.txt', aCCESS = 'SEQUENTIaL', STaTUS = 'unknown')

    do i = 1, n
        do j = 1, n
            if (i==j)  then
                a(i, j) = i
            else
                a(i, j) = 0.001
            endif
        enddo
        b(i) = i
    enddo

    ! исходные данные
    write (*, 10)
    write (1, 10)
    do i = 1, n
        write (*, 11) (a(i, j), j = 1, n), b(i)
        write (1, 11) (a(i, j), j = 1, n), b(i)
    end do

    do i = 1, n
        beta(i) = b(i) / a(i, i)
        x0(i) = beta(i)
    enddo

    do i = 1, n
        do j = 1, n
            if (i==j) then
                alpha(i, j) = 0
            else
                alpha(i, j) = -a(i, j) / a(i, i)
            endif
        enddo
    enddo

    i = 1
    do while(abs(x(i) - x0(i))>e)
        do i = 1, n
            x(i) = beta(i)
            do j = 1, n
                x(i) = x(i) + alpha(i, j) * x0(j)
            enddo
            x0(i) = x(i)
        enddo
    enddo

    write(*, 12)
    write(1, 12)

    write(*, 11)  (x(k), k = 1, n)
    write(1, 11)  (x(k), k = 1, n)

    write(*, 13)
    write(1, 13)

    do k = 1, n
        do j = 1, n
            p(k) = p(k) + a(k, j) * x(j)
        enddo
    enddo

    write(*, 11)  (p(k), k = 1, n)
    write(1, 11)  (p(k), k = 1, n)

    10 format ('СЛАУ методом итерации ', /, /, 'исходная матрица a(n*n) и вектор b(n)')
    11 format (6f12.3)
    12 format (/, 'решение')
    13 format (/, 'проверка')

    close(1)
end
