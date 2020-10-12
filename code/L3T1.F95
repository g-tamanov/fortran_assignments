program L3T1
    integer x

    write(*, *) 'введите номер месяца'
    read(*, *) x

    Select case (x)
    case (1:2, 12)
        write(*, *) "зима"
    case (3:5)
        write(*, *) "весна"
    case (6:8)
        write(*, *) "лето"
    case (9:11)
        write(*, *) "осень"
    End Select

end program L3T1