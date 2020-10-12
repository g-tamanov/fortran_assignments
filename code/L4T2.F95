program L4T2
    real a,s  !Инициализация суммы
    print *, "введите положительное число"
    read(*, *) a
    do while (a>=0)
        print *, "введите положительное число"
        read(*, *) a
        s=s+1
    end do
    print *, " отрицательное число ", a , " положительныъ чисел было ", s

end program L4T2