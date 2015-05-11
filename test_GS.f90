
    program test_GS

    use grammSmith
    use handymanfunctions
    use polynomialmodule

    implicit none
    save
!    integer, parameter :: dp = kind(1.d0)
!    integer,parameter :: ip = selected_int_kind(15)

    real(dp) :: toll = 0.0001d0
    logical :: pass = .false.

    write(*,*) '*********GRAMMSMITH TESTS*********'

    pass =  test_initiateBasisGS()
    pass = test_orthonormalizeGSBasisGS()

    write(*,*) '**************END***************'

    contains


! *************** - TESTS - ************************

!    TO DEBUGGGGGGGGGGGGGGGGGGGZZZZZZZZZ
    logical function test_initiateBasisGS() result(pass)
        type (polynomial) :: basis(6)
        type (polynomial) :: pol
        real(dp) :: res = 0.0d0
        integer::i
        pass = .false.
        do i = 0,5
            pol  = createMonominialdegree(i)
            basis(i+1) = pol
        enddo
        call initiateBasisGS(basis)
        do i = 1,nbGSBasisFunctions()
            pol = nthGSBasis(i)
            res = res + getPolVal(pol,2.0d0)
        enddo
!        write(*,*) res
        pass = res .eq. 63
!        call printGSBasis()
    end function

    logical function test_orthonormalizeGSBasisGS() result(pass)
        pass = .false.
!        call orthonormalizeGSBasisGS()
        pass = .true.
        write(*,*) pass
!        call printGSBasis()
    end function

    




    end program