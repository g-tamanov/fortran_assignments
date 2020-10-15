program L6T2
    integer  matrix (10, 10), min, max
    integer iimin,ijmin ,iimax,ijmax
    real x
    character position(100)

    CALL RANDOM_SEED()

    do i = 1, size(matrix, 1)
        do j = 1, size(matrix, 2)
            call random_number(x)
            matrix(i,j)=anint(10*x)-5
            if(matrix(i,j)==0) then
                position=position//char(i)//':'//char(j)//'='//char(matrix(i,j))//';'
                print *, position
            end if
        end do
    end do
    write(*, 10)  ((matrix(i,j),i=1,10),j=1,10)
    10   format(2x,10i10)
    write (*, *) "индексы значения =0 : ",position

end program L6T2