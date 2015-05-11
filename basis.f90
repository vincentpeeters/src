
    program test_GS

!    use grammSmith
    use handymanfunctions
    use polynomialmodule
    use lagrange

    implicit none
    save
    INTEGER :: i
    CHARACTER(len=32) :: arg, filename
    real(dp), allocatable :: x(:),fx(:)


    write(*,*) '*********PROGRAM START*********'

!    DO i = 1, argc()
!              CALL getarg(i, arg)
!              write(*,*) i
!              WRITE (*,*) arg
!                if (arg == '--help') then
!                    call printHelp()
!                endif
!                if (arg == '--test') then
!                    call runTests()
!                endif
!            END DO
        i=1
        CALL getarg(i, arg)
        if (arg == '--help') then
            call printHelp()
        endif
        if (arg == '--test') then
            call runTests()
        endif
        if (arg == '--lagrange') then
            i= i+1
            CALL getarg(i, arg)
            call runlagrange(arg)
        endif


    write(*,*) '**************PROGRAM TERMINATES***************'

    contains


! *************** - SUBROUTINES - ************************

!    logical function test_initiateBasisGS() result(pass)
!
!    end function


    subroutine runTests()
!         TODO RUN TESTS
    end subroutine
    
    subroutine printHelp()
        write(*,*) "***************HELP****************"
!         TODO WRITE HELP
        write(*,*) "Usage: ./basis [options] file..."
        write(*,*) "Options:"
        write(*,*) "    --help          Display this information"
        write(*,*) "    --test          Run Some Tests, get some time measures"
        write(*,*) "    --lagrange      A non orthonormal basis will be used for interpolation"
        write(*,*) "***************END HELP****************"
    end subroutine

    subroutine runlagrange(filename)
        CHARACTER(len=32) :: filename
        real(dp), allocatable :: x(:),fx(:),y(:),fy(:)
        integer :: i
        call readfileXFxY(filename,x,fx,y)
        call initiateLagrange(x,fx)
        allocate(fy(size(y)))
        do i = 1,size(y)
            fy(i) = lagrangeValue(y(i))
        enddo
        write(*,*) fy
    end subroutine


    subroutine readfileXFxY(filename,x,fx,y)
        CHARACTER(len=32) :: filename
        integer :: I
        real(dp), allocatable :: x(:),fx(:),y(:)
        open(10, FILE=filename, STATUS='OLD', ACTION='READ')
        READ(10,*), I
!        write(*,*) I
        allocate(x(I))
        allocate(fx(I))
        READ(10,*), fx
!        write(*,*) fx
        READ(10,*), x
!        write(*,*) x
        READ(10,*), I
        allocate(y(I))
        READ(10,*), y
    end subroutine

    subroutine readfile(filename,x,fx)
        CHARACTER(len=32) :: filename
        integer :: I
        real(dp), allocatable :: x(:),fx(:)
        open(10, FILE=filename, STATUS='OLD', ACTION='READ')
        READ(10,*), I
!        write(*,*) I
        allocate(x(I))
        allocate(fx(I))
        READ(10,*), fx
!        write(*,*) fx
        READ(10,*), x
!        write(*,*) x
    end subroutine






    end program