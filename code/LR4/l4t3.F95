program L4T3
    real a, b  !Инициализация суммы
    integer s
    OPEN (1, FILE = 'l4t3.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')
    print *, "введите первую сторону прямоугольника"
    read(*, *) a
    write(1, *) "первая сторона прямоугольника", a
    print *, "введите вторую сторону прямоугольника"
    read(*, *) b
    write(1, *) "вторая сторона прямоугольника", b

    do while (mod(b, a)>0 .or. mod(a, b)>0)
        if (a>b) then
            s = a / b
            print *, "для прямоугольника", a, " на ", b, "существует : "
            write(1, *) "для прямоугольника", a, " на ", b, "существует : "
            print *, s, " квадрат(а) ", b, " * ", b
            write(1, *) " квадрат(а) ", b, " * ", b
            a = mod(a, b)
        else
            s = b / a
            print *, "для прямоугольника", a, " на ", b, "существует : "
            write(1, *) "для прямоугольника", a, " на ", b, "существует : "
            print *, s, " квадрат(а) ", a, " * ", a
            write(1, *) " квадрат(а) ", a, " * ", a
            b = mod(b, a)
        end if
    end do
    close(1)
end program L4T3