program lr6t1

    real(8) :: e = 0.001d0, a, b, sig = 0.01 ! e - точность; a, b - границы отрезка нахождение решение функции
    integer :: ii=0
    OPEN (1, fILE = 'lr6t1.txt', aCCESS = 'SEQUENTIaL', STaTUS = 'unknown')

    write(*, *) "Метод дихотомии, функция = (x - 1)**2 + 3"
    write(1, *) "Метод дихотомии, функция = (x - 1)**2 + 3"
    a = -1.d0; b = 3.d0
    call methodDichotomy(a, b, e, Func_a) ! вызов  метода половинного деления
    write(*,*) "всего вызовов функции ", ii
    write(1,*) "всего вызовов функции ", ii
contains

    real(8) function Func_a(x) ! функциия где находим значение функции
        real(8), intent (in) :: x
        ii=ii+1
        Func_a = (x - 1)**2 + 3  ! тестированная функция
!        Func_a = (x - 1)**2 + 3  ! тестированная функция
    end function Func_a
    ! Метод дихотомии
    subroutine methodDichotomy(a, b, eps, Func)
        real(8), intent (in) :: eps
        integer :: i = 1
        real(8) :: a, b, xsr, x1, x2, fx1, fx2, xmin, f
        interface
            real(8) function Func(xt)
                real(8), intent (in) :: xt
            end function Func
        end interface
        do while ((abs(b - a)>sig).and.(abs(Func(b) - Func(a))>e))
            xsr = (a + b) / 2.d0 ! нахождение середины отрезка
            x1 = xsr - sig
            x2 = xsr + sig
            fx1 = Func(x1)
            fx2 = Func(x2)

            if (fx1<fx2) then
                b = xsr
            else
                a = xsr
            end if
        end do
        xmin = (a + b) / 2.0
        f = Func(xmin)
        write(*, *) "x мин = ", xmin
        write(1, *) "x мин = ", xmin

        write(*, *) "F(x мин)= ", f
        write(1, *) "F(x мин)= ", f

    end subroutine methodDichotomy

end program  lr6t1

