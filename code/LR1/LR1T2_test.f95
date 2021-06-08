!Функция
real  function f(x)
    real, intent (in) :: x
    fx = (x - 1)**2 - 2
end function f

program lr1t2
    !метод хорд

    implicit none

    real(4) :: f, a, b, tmp, eps, fa, fb, x_next
    integer  i !Счетчик итераций
    OPEN (1, FILE = 'lr1t2.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    write(*, *) 'Введите начальную координату a'
    read(*, *) a
    !    write(1, *) 'Введите начальную координату a ',a
    write(*, *) 'Введите конечную координату b'
    read(*, *) b
    !    write(1, *) 'Введите конечную координату b ',b
    write(*, *) 'Введите точность вычислений eps'
    read(*, *) eps
    !    write(1, *) 'Введите точность вычислений eps ',eps

    !    a = -1 !интернвал
    !    b = 10 !интернвал
    !    eps = 1.00000005E-03

    write(*, *) "------корень н/л уравнения---------метод хорд----------"
    write(1, *) "------корень н/л уравнения---------метод хорд----------"
    write(*, *) "функция fx = log(1+x)-(1-x)**2, интервал [a,b], точность del,eps"
    write(1, *) "функция fx = log(1+x)-(1-x)**2, интервал [a,b], точность del,eps"
    write(*, *) "a=", a, "b=", b, ",eps=", eps
    write(1, *) "a=", a, "b=", b, ",eps=", eps

    x_next = 0
    i = 0 !переменная цикла
    do while(abs(x_next - b)>eps)
        tmp = x_next;
        x_next = b - f(b) * (a - b) / (f(a) - f(b));
        a = b;
        b = tmp;
        i = i + 1
        write(*, *) "корень-x,             функция-f"
        write(1, *) "корень-x,             функция-f"
        write(*, 32) i, x_next, f(x_next)
        write(1, 32) i, x_next, f(x_next)
    end do

    write(*, 30)
    write(1, 30)
    write(*, 31) x_next, f(x_next)
    write(1, 31) x_next, f(x_next)

    30 format('----------РЕШЕНИЕ------')
    31 format('корень=', f9.6, '     функция=', f9.5)
    32 format('x(', i4, ')=', f9.5, '     f=', f9.5)
    close(1)
end program lr1t2
