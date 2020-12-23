!Сортировка вставками

program L9T3

    integer:: array(1), n, bof, srav=0, perest=0, h, vperest=0
    real:: arrayTemp(1)

    OPEN (1, FILE = 'l9t3.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    write(*,*)'сортировка пузырьком'
    write(*,*)'Введите размерность массива (1-100): '
    write(1,*)'сортировка пузырьком'
    write(1,*)'Введите размерность массива (1-100): '
    read(*,*) n

    do while ((n<1) .or. (n>100))
        write(*,*)'Неверная размерность. Введите новое значение:'
        write(1,*)'Неверная размерность. Введите новое значение:'
        read(*,*) n
    end do

    write(*,5)(n)
    write(1,5)(n)


    do i=1,n
        call random_number(arrayTemp(i))
        array(i)= floor(arrayTemp(i)*101)
    enddo

    write(*,*)''
    write(*,*)'Исходный массив:'
    write(*,6)(array(i),i=1,n)
    write(1,*)''
    write(1,*)'Исходный массив:'
    write(1,6)(array(i),i=1,n)

    i=2
    write(*,*)'--------------------------'
    write(1,*)'--------------------------'
    do while (i<=n)
        bof=array(i)
        j=i-1
        vperest=0
        do while ((j>0) .and. (array(j)>bof))
            srav=srav+1
            vperest=vperest+1
            array(j+1)=array(j)
            j=j-1
        end do
        array(j+1)=bof
        perest=perest+1
        write(*,4)(i-1)
        write(*,3) (vperest)
        write(*,3)(array(h),h=1,n)
        write(*,*)'--------------------------'
        write(1,4)(i-1)
        write(1,3) (vperest)
        write(1,3)(array(h),h=1,n)
        write(1,*)'--------------------------'
        i=i+1
    end do



    write(*,*)''
    write(*,*)'ОТСОРТИРОВАННЫЙ МАССИВ:'
    write(*,3)(array(i),i=1,n)
    write(1,*)''
    write(1,*)'ОТСОРТИРОВАННЫЙ МАССИВ:'
    write(1,3)(array(i),i=1,n)

    write(*,*)''
    write(*,1) (srav)
    write(1,*)''
    write(1,1) (srav)
    write(*,2) (perest)
    write(1,2) (perest)
    
    1 format("Сравнений произведено:", i4)
    2 format("Перестановок выполнено:", i3)
    3 format("  Внутренних перестановок в проходе:", i3)
    4 format(i3, " проход:")
    5 format(' Размерность массива = ', i3)
    6 format(10(2x, i4))

    close(2)

end program L9T3
