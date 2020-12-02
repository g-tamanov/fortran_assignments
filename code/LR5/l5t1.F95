program L5T1
    integer mass(10), sum
    real hav
    OPEN (1, FILE = 'l5t1.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    sum=0
    do i = 1, size(mass,1)
        call random_number(hav)
        mass(i) =  anint(hav * 100) - 50
        sum = sum + mass(i)
    end do

    write(*,*)'массив:'
    write(*,2)(mass(i),i=1,10)

    write(1,*)'массив:'
    write(1,2)(mass(i),i=1,10)

    2 format(10(2x, i4))

    write(*,3)(sum)
    write(1,3)(sum)

    3 format('Сумма чисел:  ', i4)
    close(1)

end program L5T1