!Функция
real  function f(x)
    real, intent (in) :: x
    !    fx = log(1 + x**2)
    fx = sin(x)
end function f

program l2t1

    implicit none
    integer, parameter :: size = 100
    real  x(0:size), y(0:size), dy(0:size), ddy(0:size)
    real a(0:3, 0:size)
    real h, p, f
    real xn, xk, xi
    integer i, n

    OPEN (1, fILE = "lr2t1.txt ", ACCESS = "SEQUENTIAL ", STATUS = "unknown ")

    write(*, *) "Введите начальную,конечную координату и кол-во узлов "
    read(*, *) xn, xk, n
    write (*, *) " "
    write (*, *) "====INTRPOLATION NEWTON===== "
    write (1, *) "====INTRPOLATION NEWTON===== "
    write (1, *) " "
    write (*, *) " "
    write (*, *) "interval - [xs,xf],  number points - n "
    write (1, *) "interval - [xs,xf],  number points - n "
    write (*, 50) xn, xk, n
    write (1, 50) xn, xk, n

    x(0) = xn
    y(0) = f(x(0))
    !h шаг между n
    h = (xk - xn) / n

    !xi
    do i = 1, n
        !координаты узлов табличной функции
        x(i) = x(i - 1) + h
        !массив значений функции в этих узлах
        y(i) = f(x(i))
    end do

    !конечная разность 1-го порядка
    do i = 0, n - 1
        !дельта y
        dy(i) = y(i + 1) - y(i)
    end do

    !конечная разность 2-го порядка
    do i = 0, n - 2
        !дельта y^2
        ddy(i) = dy(i + 1) - dy(i)
        !Вычислить коэффициенты a0,a1,a2
        a(0, i) = y(i)
        a(1, i) = dy(i) / (1 * h)
        a(2, i) = ddy(i) / (2 * h**2)
    end do

    write(*, *) " "
    write(1, *) " "
    write(*, *) "---tabeles x y dy ddy a0 a1 a2"
    write(1, *) "---tabeles x y dy ddy a0 a1 a2"
    write(*, *) "    x         y        dy       ddy        a0        a1       a2 "
    write(1, *) "    x         y        dy       ddy        a0        a1       a2 "

    !формат по образцу из почты
    do i = 0, n
        if(i<9) then
            write(*, 10)x(i), y(i), dy(i), ddy(i), a(0, i), a(1, i), a(2, i)
            write(1, 10)x(i), y(i), dy(i), ddy(i), a(0, i), a(1, i), a(2, i)
        else
            if(i<10) then
                write(*, 20)x(i), y(i), dy(i)
                write(1, 20)x(i), y(i), dy(i)
            else
                write(*, 30)x(i), y(i)
                write(1, 30)x(i), y(i)
            end if
        end if

    end do

    !Для этих значений аргумента вычислить значения и оценить разницу полученных значений
    write(*, *) " "
    write(1, *) " "
    write(*, *) "---tabeles x1 y1pdp"
    write(1, *) "---tabeles x1 y1pdp"
    write(*, *) "    x1       y1          p         dp  "
    write(1, *) "    x1       y1          p         dp  "

    do i = 0, n - 2
        xi = (x(i) + x(i + 1)) / 2
        !P_n (x)=a_0+a_1 (x-x_0)+a_2 (x-x_0)(x-x_1)+a_3 (x-x_0)(x-x_1)(x-x_2)
        p = a(0, i) + a(1, i) * (xi - x(i)) + a(2, i) * (xi - x(i)) * (xi - x(i + 1))
        write (*, 40) xi, f(xi), P, f(xi) - P
        write (1, 40) xi, f(xi), P, f(xi) - P
    end do
    write(*, *) "=================="
    close(1)

    !formats
    10 format(f7.3, f10.6, f10.6, f10.6, f10.6, f10.6, f10.6)
    20 format(f7.3, f10.6, f10.6)
    30 format(f7.3, f10.6)

    40 format(f7.3, f11.6, f11.6, f11.6)
    50 format(" xs="f6.3, "  xf="f6.3, "  n=" 1I3)
    !formats end

end program l2t1
