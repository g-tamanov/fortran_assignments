!Функция
real  function f(x)
    real, intent (in) :: x
    fx = (x - 1)**2 - 2
!        fx = log(1+x)-(1-x)**2
    !    fx = x**3-18*x-83
end function f

!Производная
real  function df(x)
    real, intent (in) :: x
    df = 2 * x - 2
end function df

!Производная
real  function df2(x)
    real, intent (in) :: x
!    df2 =-(2+(1/(x+1)**2))
    df2 =2*x-2
end function df2

program lr1t2
    !метод хорд

    real(4) :: f, df, df2, a, b, tmp, eps, fa, fb, x_next, x, x1, x2, x0

    integer  i !Счетчик итераций
    OPEN (1, fILE = 'lr1t2.txt', aCCESS = 'SEQUENTIaL', STaTUS = 'unknown')

    write(*, *) 'Введите начальную, конечную координату и точность вычислений'
    read(*, *) a,b,eps

    write(*, *) "------корень н/л уравнения---------метод хорд----------"
    write(1, *) "------корень н/л уравнения---------метод хорд----------"
    write(*, *) "функция fx = (x - 1)**2 - 2, интервал [a,b], точность del,eps"
    write(1, *) "функция fx = (x - 1)**2 - 2, интервал [a,b], точность del,eps"
    write(*, *) "a=", a, "b=", b, ",eps=", eps
    write(1, *) "a=", a, "b=", b, ",eps=", eps
    write(*, *) "-----------------------------"
    write(1, *) "-----------------------------"

    i = 0
    write(*, 33)
    write(1, 33)
    if ((df2(a) * f(a))> 0) then
        x = b
        x0 = a
        fa = f(a)
        do while (abs(x0 - x)>eps)
            x0 = x
            fx = f(x0)
            x = x0 - fx * (x0 - a) / (fx - fa)

            write(*, 32) i, x, f(fx)
            write(1, 32) i, x, f(fx)
            i = i + 1
        end do
    else
        x = a
        x0 = b
        fb = f(b)
        do while (abs(x0 - x)>eps)
            x0 = x
            fx = f(x0)
            x = x0 - fx * (b - x0) / (fb - fx)

            write(*, 32) i, x, fx
            write(1, 32) i, x, fx
            i = i + 1
        end do
        fx = f(x)

    end if

    write(*, *)
    write(1, *)
    write(*, 30)
    write(1, 30)
    write(*, 31) x, fx
    write(1, 31) x, fx

    30 format('----------РЕШЕНИЕ------------------------')
    31 format('|корень=', f9.6, '|     функция=', f9.5, '|')
    32 format('|x(', i2, ')=', f9.5, '|     f=', f9.5, ' |')
    33 format('|   корень-x    |     функция-f   |')
    close(1)
end program lr1t2
