program L5T1
    integer mass(10), sum
    real hav
    OPEN (1, FILE = 'l5t1.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    sum=0
    do i = 1, size(mass,1)
        call random_number(hav)
        mass(i) =  anint(hav * 100) - 50
        sum = sum + mass(i)
        write(1, *) "mass(i)", i,"=",mass(i)
    end do
    write(*, *) " сумма элементов массива = ",sum
    write(1, *) " сумма элементов массива = ",sum
    close(1)

end program L5T1