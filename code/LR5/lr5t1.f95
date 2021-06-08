program lr5t1
    implicit none
    integer, parameter :: n = 5
    real(8) a(n, n), b(n), x(n)
    integer i, j

    OPEN (1, fILE = 'lr5t1.txt', aCCESS = 'SEQUENTIaL', STaTUS = 'unknown')

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

    call gauss(a, b, x, n)

    ! матрица а и ветктор б
    write (*, 12)
    write (1, 12)
    do i = 1, n
        write (*, 11)  (a(i, j), j = 1, n), b(i)
        write (1, 11)  (a(i, j), j = 1, n), b(i)
    end do
    ! результат
    write (*, 13)
    write (1, 13)
    write (*, 11) (x(i), i = 1, n)
    write (1, 11) (x(i), i = 1, n)
    10 format (' Решение СЛАУ Гаусом ', /, /, ' Исходная матрица и вектор')
    11 format (6f12.6)
    12 format (/, ' Решенная матрица ')
    13 format (/, ' Корни ')
end

subroutine gauss(a, b, x, n)

    implicit none
    integer n
    double precision a(n, n), b(n), x(n)
    double precision s(n)
    double precision c, pivot, store
    integer i, j, k, l

    do k = 1, n - 1

        do i = k, n
            s(i) = 0.0
            do j = k, n
                s(i) = max(s(i), abs(a(i, j)))
            end do
        end do

        pivot = abs(a(k, k) / s(k))
        l = k
        do j = k + 1, n
            if(abs(a(j, k) / s(j)) > pivot) then
                pivot = abs(a(j, k) / s(j))
                l = j
            end if
        end do

        if(pivot == 0.0) then
            write(*, *) ' det=0 !'
            return
        end if

        if (l /= k) then
            do j = k, n
                store = a(k, j)
                a(k, j) = a(l, j)
                a(l, j) = store
            end do
            store = b(k)
            b(k) = b(l)
            b(l) = store
        end if

        do i = k + 1, n
            c = a(i, k) / a(k, k)
            a(i, k) = 0.0
            b(i) = b(i) - c * b(k)
            do j = k + 1, n
                a(i, j) = a(i, j) - c * a(k, j)
            end do
        end do
    end do

    x(n) = b(n) / a(n, n)
    do i = n - 1, 1, -1
        c = 0.0
        do j = i + 1, n
            c = c + a(i, j) * x(j)
        end do
        x(i) = (b(i) - c) / a(i, i)
    end do

end subroutine gauss
