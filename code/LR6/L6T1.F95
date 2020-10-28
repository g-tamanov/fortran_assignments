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
        write (*, *) "matrix(i,j): ",i,j,"=",matrix(i,j)
    end do
    write(*, 10)  ((matrix(i,j),i=1,5),j=1,6)
    write(1, 10)  ((matrix(i,j),i=1,5),j=1,6)
    10   format(2x,5f6.2)
    write (*, *) "индекс минимального числа по модолю: ",iimin,ijmin
    write (1, *) "индекс минимального числа по модолю: ",iimin,ijmin
    write (*, *) "индекс максимального числа по модолю: ",iimax,ijmax
    write (1, *) "индекс максимального числа по модолю: ",iimax,ijmax
    close(1)

end program L6T1