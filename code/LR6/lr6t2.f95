program lr6t2

    real(8) :: e = 0.001d0, a, b, sig = 0.01 ! e - точность; a, b - границы отрезка нахождение решение функции
    integer :: ii=0

    OPEN (1, fILE = 'lr6t2.txt', aCCESS = 'SEQUENTIaL', STaTUS = 'unknown')

    write(*, *) "Метод деления отрезка пополам, функция = (x - 1)**2 + 3"
    write(1, *) "Метод деления отрезка пополам, функция = (x - 1)**2 + 3"
    a = -1.d0; b = 3.d0
    call MethodDivSegHalf(a, b, e, Func_a) ! вызов  метода половинного деления
    write(*,*) "всего вызовов функции ", ii
    write(1,*) "всего вызовов функции ", ii

contains

    real(8) function Func_a(x) ! функциия где находим значение функции
        real(8), intent (in) :: x
        ii=ii+1
        Func_a = (x - 1)**2 + 3  ! тестированная функция
    end function Func_a
    ! Метод дихотомии
    subroutine MethodDivSegHalf(a, b, eps, Func)
        real(8), intent (in) :: eps
        integer :: i = 1
        real(8) :: a, b, xsr, x1, x2, fx1, fx2, xmin, f
        interface
            real(8) function Func(xt)
                real(8), intent (in) :: xt
            end function Func
        end interface
        do
            7 continue
            xsr = (a + b) / 2.d0 ! находение середины отрезка
            f = Func(xsr)
            x1 = xsr - abs(b - a) / 4.0
            x2 = xsr + abs(b - a) / 4.0
            fx1 = Func(x1)
            fx2 = Func(x2)
            if (fx1<f) then
                b = xsr
                xsr = x1
                f = fx1
                x1 = xsr - abs(b - a) / 4.0
                x2 = xsr + abs(b - a) / 4.0
                go to 10
            else
                if (fx2<f) then
                    a = xsr
                    xsr = x2
                    f = fx2
                    x1 = xsr - abs(b - a) / 4.0
                    x2 = xsr + abs(b - a) / 4.0
                    go to 10
                else
                    a = x1
                    b = x2
                    x1 = xsr - abs(b - a) / 4.0
                    x2 = xsr + abs(b - a) / 4.0
                end if
            end if
            10 continue
            if ((abs(b - a)>sig).and.(abs(Func(b) - Func(a))>e)) then
                go to 7
            else
                go to 8
            end if
        end do
        8   continue
        xmin = (a + b) / 2.0
        f = Func(xmin)
        write(*, *) "x мин= ", xmin
        write(1, *) "x мин= ", xmin

        write(*, *) "F(x мин)= ", f
        write(1, *) "F(x мин)= ", f

    end subroutine MethodDivSegHalf

end program  lr6t2 

