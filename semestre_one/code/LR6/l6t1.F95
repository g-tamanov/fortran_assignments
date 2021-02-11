program L6T1
    REAL x, matrix (5, 6), min, max
    integer iimin,ijmin ,iimax,ijmax
    OPEN (1, FILE = 'l6t1.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    iimin=1
    ijmin=1
    iimax=1
    ijmax=1
    CALL RANDOM_SEED()

    do i = 1, size(matrix, 1)
        do j = 1, size(matrix, 2)
            CALL RANDOM_NUMBER(x)
            matrix(i,j)=2*x-1
            if(abs(matrix(i,j))>abs(matrix(iimax,ijmax))) then
                iimax=i
                ijmax=j
            end if
            if(abs(matrix(i,j))<abs(matrix(iimin,ijmin))) then
                iimin=i
                ijmin=j
            end if
        end do
    end do
    write (*, *) "массив: "
    write (1, *) "массив: "
    write(*, 2)  ((matrix(i,j),i=1,5),j=1,6)
    write(1, 2)  ((matrix(i,j),i=1,5),j=1,6)

    write (*, 3) iimin,ijmin
    write (1, 3) iimin,ijmin
    write (*, 4) iimax,ijmax
    write (1, 4) iimax,ijmax

    2   format(2x,5f6.2)
    3 format("индекс минимального числа по модолю: ", i3,i3)
    4 format("индекс максимального числа по модолю: ", i3,i3)

    close(1)

end program L6T1