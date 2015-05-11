
    program test_Integrator

    use integrator
    use handymanfunctions
    implicit none
    save
!    integer, parameter :: dp = kind(1.d0)
!    integer,parameter :: ip = selected_int_kind(15)

    real(dp) :: toll = 0.0001


    write(*,*) '*********INTEGRATION TESTS*********'

    write(*,*) test_samengesteldeTrapezium()
    write(*,*) test_samengesteldeTrapeziumVastePunten()
    write(*,*) test_gaussKwadratuur()

    write(*,*) '**************END***************'

    contains



! *************** - TESTS - ************************

    logical function test_samengesteldeTrapeziumVastePunten()
        real(dp):: x(100), fx(100) = 0
        integer(ip):: i
        real(dp):: res = 0
        do i = 1,100
            x(i)=real(i)/100.0d0
            fx(i) = square(x(i))
        enddo
        res =  samengesteldeTrapeziumVastePunten(x,fx)
        test_samengesteldeTrapeziumVastePunten = abs(res - 1.0d0/3.0d0) < toll
    end function

    logical function test_samengesteldeTrapezium()
        integer(ip):: i
        real(dp):: res
        res = samengesteldeTrapezium(square,const_1,0.0d0,1.0d0,100)
        test_samengesteldeTrapezium = abs(res - 1.0d0/3.0d0) < toll
    end function

    logical function test_gaussKwadratuur()
        real(dp) :: x=5
        integer(ip):: i
        real(dp):: res = 0.0d0
        res = gaussKwadratuur(square,0.0d0,1.0d0,1.0d0,1.0d0,100)
        test_gaussKwadratuur = abs(res - 1.0d0/3.0d0) < toll
    end function




    end program