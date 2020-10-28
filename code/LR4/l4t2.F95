program L4T2
    real a  !Инициализация суммы
    integer s  !Инициализация суммы
    OPEN (1, FILE = 'l4t2.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')
    print *, "введите число"
    read(*, *) a
    write(1, *) "число =", a
    s = 0
    do while (a>=0)
        print *, "введите число"
        read(*, *) a
        write(1, *) "число =", a
        s = s + 1
    end do
    print *, " отрицательное число ", a, " положительных чисел было введено", s
    write(1, *) " отрицательное число ", a, " положительных чисел было введено", s
    close(1)
end program L4T2