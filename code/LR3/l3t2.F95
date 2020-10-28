program L3T2
    integer x
    OPEN (1, FILE = 'l3t2.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    write(*, *) 'введите номер'
    read(*, *) x
    write(1, *) 'номер',x

    Select case (x)
    case (1)
        write(*, *) "1.	Квартира в Москве"
        write(1, *) "1.	Квартира в Москве"
    case (2)
        write(*, *) "2.	Автомобиль"
        write(1, *) "2.	Автомобиль"
    case (3)
        write(*, *) "3.	Туристическая путёвка"
        write(1, *) "3.	Туристическая путёвка"
    case (4)
        write(*, *) "4.	Зубная щётка"
        write(1, *) "4.	Зубная щётка"
    case (5:)
        write(*, *) "наилучшие пожелания в следующей лотерее"
        write(1, *) "наилучшие пожелания в следующей лотерее"
    End Select
    close(1)
end program L3T2