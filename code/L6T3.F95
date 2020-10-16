program L6T3
    integer  matrix (5, 5)
    real x

    CALL RANDOM_SEED()

    do i = 1, size(matrix, 1)
        do j = 1, size(matrix, 2)
            matrix(i, j) = 4
            if(j==i) then
                matrix(i, j) = 0
            else if(i==size(matrix, 1) - j + 1) then
                matrix(i, j) = 0
            else if(i>j .and. i/=j .and. i<size(matrix, 1) - j + 1) then
                matrix(i, j) = 1
            else if(i<j .and. i/=j .and. i/=size(matrix, 1) - j + 1) then
                matrix(i, j) = 3
            else if(i<j .and. i/=j .and. i>size(matrix, 1) - j + 1) then
                matrix(i, j) = 2
            end if

        end do
    end do
    write(*, 10)  ((matrix(i, j), i = 1, size(matrix, 1)), j = 1, size(matrix, 2))
    10   format(2x, 5i5)

end program L6T3