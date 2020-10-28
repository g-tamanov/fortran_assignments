program L4T1
    integer a, d, n   !Инициализация суммы
    OPEN (1, FILE = 'l4t1.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    print *, "введите a"
    read(*, *) a
    write(1, *) "a=", a
    print *, "введите d"
    read(*, *) d
    write(1, *) "d=", d
    print *, "введите n"
    read(*, *) n
    write(1, *) "n=", n

    do
        if(n<=0) exit
        SUM = SUM + (a + (n - 1) * d)
        n = n - 1
    end do
    print *, "сумма членов арифметической прогрессии SUM + (a + (n - 1) * d)=", SUM
    write(1, *) "сумма членов арифметической прогрессии SUM + (a + (n - 1) * d)=", SUM
    close(1)
end program L4T1