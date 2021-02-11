program l1t1
    integer a,b
    real c,d
    OPEN (1, FILE = 'l1t1.txt',ACCESS = 'SEQUENTIAL', STATUS = 'unknown')
    write(*,*) 'write a'
    read(*,*) a
    write(1,*) 'a=',a
    write(*,*) 'write b'
    read(*,*) b
    write(1,*) 'b=',b
    a=a+b
    write(1,*) 'a=a+b=',a

    write(*,*) 'write c'
    read(*,*) c
    write(1,*) 'c=',c
    write(*,*) 'write d'
    read(*,*) d
    write(1,*) 'd=',d
    c=c-d
    write(1,*) 'c=c-d=',c
    close(1)
end program l1t1