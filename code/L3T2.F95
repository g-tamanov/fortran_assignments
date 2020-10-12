program L3T2
    integer x

    write(*, *) 'введите номер месяца'
    read(*, *) x

    Select case (x)
    case (1)
        write(*, *) "1.	Квартира в Москве"
    case (2)
        write(*, *) "2.	Автомобиль"
    case (3)
        write(*, *) "3.	Туристическая путёвка"
    case (4)
        write(*, *) "4.	Зубная щётка"
    case (5:)
        write(*, *) "наилучшие пожелания в следующей лотерее"
    End Select

end program L3T2