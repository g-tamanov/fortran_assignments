!Функция
real  function f(x)
    real, intent (in) :: x

    fx = (x - 1)**2 - 2
end function f

!Производная
real  function df(x)
    real, intent (in) :: x
    dx = -((exp**(-x)) + sin(PI * x / 2) + (1 * PI * x * cos(PI * x / 2)) / 2)
end function df

program lr1t1
    integer n, m
    real(4) :: fa, fb, a, b, del, eps, fxsr, xsr
    OPEN (1, FILE = 'l1t1.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    !    Шаг 0
    write(*, *) 'Введите начальную координату a'
    a=0
    !    write(1, *) 'Введите начальную координату a ',a
    write(*, *) 'Введите конечную координату b'
    b=3
    !    write(1, *) 'Введите конечную координату b ',b
    write(*, *) 'Введите точность приближения интервала del'
    del=0.01
    !    write(1, *) 'Введите точность приближения интервала del ',del
    write(*, *) 'Введите точность вычислений eps'
    eps=0.01
    !    write(1, *) 'Введите точность вычислений eps ',eps

    write(*, *) "------корень н/л уравнения---------деление отрезка пополам----------"
    write(1, *) "------корень н/л уравнения---------деление отрезка пополам----------"
    write(*, *) "функция fx = log(1 + x) - (1 - x)**2, интервал [a,b], точность del,eps"
    write(1, *) "функция fx = log(1 + x) - (1 - x)**2, интервал [a,b], точность del,eps"
    write(*, *) "a=", a, "b=", b, "del=", del, ",eps=", eps
    write(1, *) "a=", a, "b=", b, ", точность del=", del, ",eps=", eps

    !Шаг 1
    fa = f(a)
    !    write(*, *) "функция f(", a, ")=(", a, "-1)^2-2 =", fa
    !    write(1, *) "функция f(", a, ")=(", a, "-1)^2-2 =", fa
    fb = f(b)
    !    write(*, *) "функция f(", b, ")=(", b, "-1)^2-2 =", fb
    !    write(1, *) "функция f(", b, ")=(", b, "-1)^2-2 =", fb
    xsr = (a + b) / 2
    write(*, *) "xsr=(", a, "+", b, ")/2 =", xsr
    write(1, *) "xsr=(", a, "+", b, ")/2 =", xsr
    !    fxsr = f(xsr)
    !    write(*, *) "функция fxsr(", xsr, ")=(sqrt(", xsr, "-1))-2 =", fxsr
    !    write(1, *) "функция fxsr(", xsr, ")=(sqrt(", xsr, "-1))-2 =", fxsr
    n = 0
    m = 2
    if(f(a)==0)then
        write(*, 30)
        write(*, 31) xsr, fxsr, n, m
        m = m + 1
        close(1)
        stop 0
    elseif(f(b)==0)then
        write(*, 30)
        write(*, 31) xsr, fxsr, n, m
        m = m + 1
        close(1)
        stop 0
    else
        !Шаг 2:
        do while (abs(b - a)>del) !Шаг 4:
            !Шаг 3:
            write(*, *) "-----границы интервала----функция---"
            write(1, *) "-----границы интервала----функция---"
            write(*, *) "a=", a, "b=", b
            write(1, *) "a=", a, "b=", b
            write(*, *) "fxsr=", f(xsr)
            write(1, *) "fxsr=", f(xsr)
            xsr = a + (b - a) / 2
            fxsr = f(xsr)
            n = n + 1

            if(fa * fxsr<0)then
                b = xsr
                fb = fxsr
            else
                a = xsr
                fa = fxsr
            end if
            m = m + 3
        end do
    end if

    write(*, 30)
    write(1, 30)
    write(*, 31) xsr, fxsr, n, m
    write(1, 31) xsr, fxsr, n, m

    30 format('----------РЕШЕНИЕ------')
    31 format('корень=', f9.6, ' функция=', f9.5, " циклы n=", i4, " функции m=", i4)
    close(1)
end program lr1t1
