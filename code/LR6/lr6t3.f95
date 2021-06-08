program lr6t3

    real(8) :: e = 0.01, beta = 0.01, a, b, l1, l2 ! e - точность; a, b - границы отрезка нахождение решение функции
    integer :: ii=0

    OPEN (1, fILE = 'lr6t3.txt', aCCESS = 'SEQUENTIaL', STaTUS = 'unknown')

    write(*, *) "Метод золотое сечение, функция = (x - 1)**2 + 3"
    write(1, *) "Метод золотое сечение, функция = (x - 1)**2 + 3"
    a = -1.d0; b = 3.d0
    l1 = 0.38; l2 = 0.62
    call MethodGolden(a, b, l1, l2, e, beta, Func_a) ! вызов  метода половинного деления
    write(*,*) "всего вызовов функции ", ii
    write(1,*) "всего вызовов функции ", ii

contains

    real(8) function Func_a(x) ! функциия где находим значение функции
        real(8), intent (in) :: x
        ii=ii+1
!        Func_a = (10-x)**2  ! тестированная функция
        Func_a = (x - 1)**2 + 3  ! тестированная функция
    end function Func_a
    ! Метод золотое сечение
    subroutine MethodGolden(a, b, l1, l2, eps, beta, Func)
        real(8), intent (in) :: eps,beta
        real(8) :: a, b, v, w, fv, fw, l1, l2, xmin, l
        interface
            real(8) function Func(xt)
                real(8), intent (in) :: xt
            end function Func
        end interface

        do
            7   continue
            l = abs(b - a)
            v = a + abs(b - a) * l1
            w = a + abs(b - a) * l2
            fv = Func(v)
            fw = Func(w)
            if(fw<fv) then
                a = v
                v = w
                fv = fw
                l = abs(b - a)
                w = a + l * l2
                fw = Func(w)
                go to 10
            else
                b = w
                w = v
                fw = fv
                l = abs(b - a)
                v = a + l * l1
                fv = Func(v)
                go to 10
            end if
            10 continue
!            write(*,*) a,b
!            write(*,*) abs(b - a)
!            write(*,*) beta
!            write(*,*) abs(fw - fv)
!            write(*,*) eps
            if (abs(b - a)<beta .and. abs(fw - fv)<eps) then
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

    end subroutine MethodGolden

end program  lr6t3

