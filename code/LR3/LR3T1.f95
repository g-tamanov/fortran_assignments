program lr3t1
    real, parameter :: PI = 3.1415927
    real h
    real iLevPr, iPravPr, iCentPr, iTrap, iSimpson
    real xn, xk
    integer n,k

    OPEN (1, FILE = 'lr3t1.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')
    write(*, *) 'Введите начальную, конечную координату и шаг'
    read(*, *) xn,xk,k
    write(*, '("   h    |  lev sum  | prav sum  | centr sum  | trapecia |  simpson | analytical")')
    write(1, '("   h    |  lev sum  | prav sum  | centr sum  | trapecia |  simpson | analytical")')

    do n = 10, 50, k
        h = abs(xk - xn) / n
        iLevPr = levPr(xn, xk, n, h)
        iPravPr = pravPr(xn, xk, n, h)
        iCentPr = centPr(xn, xk, n, h)
        iTrap = trap(xn, xk, n, h)
        iSimpson = simpson(xn, xk, n, h)
        write (*, 10) h, iLevPr, iPravPr, iCentPr, iTrap, iSimpson, fp(xk) - fp(xn)
        write (1, 10) h, iLevPr, iPravPr, iCentPr, iTrap, iSimpson, fp(xk) - fp(xn)
    end do

    close(1)

    10 format (" "F5.3, "  | ", F8.4, "  | ", F8.4, "  |  ", F8.4, "  | ", F8.4, " | ", F8.4, " | ", F8.4)
CONTAINS
    real function f(x)
        real x
        f=exp(x)
    end function

    real function fp(x)
        real x
        fp=exp(x)
    end function

    real function levPr(xn, xk, n, h)
        real x, h, xn, xk, nx, sum
        sum = 0

        x = xn
        do i = 0, n - 1, 1
            sum = sum + h * f(x)
            x = x + h
        end do
        levPr = sum
    end function

    real function pravPr(xn, xk, n, h)
        real x, h, xn, xk, nx, sum
        sum = 0

        x = xk
        do i = 1, n, 1
            sum = sum + h * f(x)
            x = (x - h)
        end do
        pravPr = sum
    end function

    real function centPr(xn, xk, n, h)
        real x, h, xn, xk, nx, sum
        sum = 0

        x = xn
        do i = 1, n, 1
            sum = sum + f(x + 0.5 * h)
            x = (x + h)
        end do
        centPr = h * sum
    end function

    real function trap(xn, xk, n, h)
        real x, h, xn, xk, nx, sum
        sum = 0

        x = xn
        do i = 0, n, 1
            if(i == 0 .or. i == n) then
                sum = sum + 0.5 * f(x)
            else
                sum = sum + f(x)
            end if
            x = x + h
        end do
        trap = h * sum
    end function

    real function simpson(xn, xk, n, h)
        real x, h, xn, xk, nx, sum, sumChet, sumNeChet
        sum = 0

        x = xn
        sumChet = 0
        sumNeChet = 0
        do i = 0, n, 1
            if(i == 0 .or. i == n) then
                sum = sum + f(x)
            else
                if(mod(i, 2) == 0) then
                    sumChet = sumChet + f(x)
                else
                    sumNeChet = sumNeChet + f(x)
                end if
            end if
            x = x + h
        end do
        simpson = (h / 3) * (sum + 2 * sumChet + 4 * sumNeChet)
    end function
end program lr3t1
