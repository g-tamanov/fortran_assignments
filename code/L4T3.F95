program L4T3
    real a, b  !Инициализация суммы
    integer s
    print *, "введите первую сторону прямоугольника"
    read(*, *) a
    print *, "введите вторую сторону прямоугольника"
    read(*, *) b
    do while (mod(b, a)>0 .or. mod(a, b)>0)
        if (a>b) then
            s = a / b
            print *, "для прямоугольника", a, " на ", b, "существует : "
            print *, s, " квадрат(а) ", b, " * ", b
            a = mod(a, b)
        else
            s = b / a
            print *, "для прямоугольника", a, " на ", b, "существует : "
            print *, s, " квадрат(а) ", a, " * ", a
            b = mod(b, a)
        end if
    end do
end program L4T3