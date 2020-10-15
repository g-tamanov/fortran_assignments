program L6T1
    REAL x, matrix (5, 6), min, max
    integer iimin,ijmin ,iimax,ijmax
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
    write(*, 10)  ((matrix(i,j),i=1,5),j=1,6)
    10   format(2x,5f6.2)
    write (*, *) "индекс минимального числа по модолю: ",iimin,ijmin
    write (*, *) "индекс максимального числа по модолю: ",iimax,ijmax

end program L6T1