!Функция
real  function f(x)
    real, intent (in) :: x
    fx = (exp**(-x)) - x * sin(PI * x / 2)
end function f

!Производная
real  function df(x)
    real, intent (in) :: x
    dx = -((exp**(-x)) + sin(PI * x / 2) + (1 * PI * x * cos(PI * x / 2)) / 2)
end function df

program l1t3
    !Решение нелинейного уравнения методом Ньютона
    !eps - требуемая точность
    !dx  - приращение для численного определения значения производной df

    implicit none

    real(4) :: x10, xi, a, b, f, df, x0
    real(4), parameter :: eps = 0.01
    integer  i !Счетчик итераций

    x10 = 10.0 !Начальное приближение

    xi = x10

    a = 0 !интернвал
    b = 10.0 !интернвал
!    eps = 0.001

    x0 = 1.0

    i = 0 !переменная цикла

    write(*, *) "------корень н/л уравнения---------метод касательных----------"
    write(1, *) "------корень н/л уравнения---------метод касательных----------"
    write(*, *) " интервал [a,b], начальная точка x10,eps"
    write(1, *) " интервал [a,b],eps"
    !    write(*, *) "a=", a, "b=", b, "x10=", x0, ",eps=", eps
    !    write(1, *) "a=", a, "b=", b, "x10=", x0, ",eps=", eps

    do while (abs(b - a)>eps) !пока не достигнута точность eps
        i=i+1
        b = a;
        a = b - f(a) / df(a); !последующие приближения
        write(*,*) "x(",i,")=",a
        write(*,*) "f(",a,")=",f(a)
    end do
        write(*, *) a


    !    print "(a, 1p, e15.5)", " Корень функции, X    = ", b
    !    print "(a, i5)", " Количество итераций    = ", i

    write(*, 30)
!    write(*, 31) b, f(b), i

    30 format('----------РЕШЕНИЕ------')
    31 format('корень=', g10.4, 'функция=', g10.4, " циклы n=", g2.4)
    close(1)
end program l1t3
