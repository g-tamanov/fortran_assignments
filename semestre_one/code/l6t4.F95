program L6T4
    integer  matrix (5, 5), s
    real x
    OPEN (1, FILE = 'l6t4.txt', ACCESS = 'SEQUENTIAL', STATUS = 'unknown')

    s = 0
    CALL RANDOM_SEED()
    matrix(1, 1) = 4
    do i = 1, size(matrix, 1)
        do j = 1, size(matrix, 2)
        do k =i,1
            s = s + 1
            matrix(k, j) = 1
            end do
!            matrix(i, j) = 1
!            ii = i
!            jj = j
!            do while(ii>0 .and. jj>0)
!                s = s + 1
!                matrix(ii, jj) = s
!                ii = ii - 1
!                jj = jj - 1
!            end do

        end do
    end do
    write(*, 10)  ((matrix(i, j), i = 1, size(matrix, 1)), j = 1, size(matrix, 2))
    write(1, 10)  ((matrix(i, j), i = 1, size(matrix, 1)), j = 1, size(matrix, 2))
    10   format(2x, 5i5)
    close(1)

end program L6T4