program l2t1
    integer a,b
    real c,d
    write(*,*) 'write a'
    read(*,*) a
    write(*,*) 'a=',a
    write(*,*) 'write b'
    read(*,*) b
    write(*,*) 'b=',b
    a=a+b
    write(*,*) 'a=a+b=',a

    write(*,*) 'write c'
    read(*,*) c
    write(*,*) 'c=',c
    write(*,*) 'write d'
    read(*,*) d
    write(*,*) 'd=',d
    c=c-d
    write(*,*) 'c=c-d=',c
end program l2t1