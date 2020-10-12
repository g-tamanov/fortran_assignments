program L4T1
    integer a, d, n   !Инициализация суммы
    print *, "введите a"
    read(*, *) a
    print *, "введите d"
    read(*, *) d
    print *, "введите n"
    read(*, *) n

    do
        if(n<=0) exit
        SUM = SUM + (a + (n - 1) * d)
        n=n-1
    end do
    print *, "сумма членов арифметической прогрессии = ", SUM

end program L4T1