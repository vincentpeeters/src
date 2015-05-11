    module grammSmith
    use polynomialModule
    use handymanfunctions
    use lagrange

	implicit none
	save

    type (polynomial), allocatable, private :: basis(:)
    real(dp):: alpha = 1.0d0,beta = 1.0d0
    real(dp):: orthToll = 0.0001d0

	contains

    subroutine initiateBasisGS(newBasis)
        type (polynomial):: newBasis(:)
        integer :: n
        n = size(newBasis)
        allocate(basis(n))
        basis = newBasis
    end subroutine

    subroutine makeGSBasis(x,fx)
        real(dp) :: x(:),fx(:)
        integer :: N,i 
        N = size(x)
        allocate(basis(N))
        do i=1,N
            basis(i) = getLagrangePol(x(1:i),fx(1:i))
        enddo
        call orthonormalizeGSBasisGS()
    end subroutine

    integer function nbGSBasisFunctions()
        nbGSBasisFunctions = size(basis)
    end function

    type(polynomial) function nthGSBasis(n)
        integer, intent(in) :: n
        nthGSBasis = basis(n)
    end function


    subroutine orthonormalizeGSBasisGS()
        integer:: i,j
        real(dp) :: dot
        do i = 1, nbGSBasisFunctions()
            do j = 1,i-1
!            substract the parts
                dot = polDot(weight,-1.0d0,1.0d0,basis(i),basis(j))
                write(*,*) dot
                basis(i) = subPol(basis(i),multPol(basis(j),dot))
            end do
!            normalize
            basis(j) = devidePol(basis(i),polNorm(weight,-1.0d0,1.0d0,basis(i)))
        end do
    end subroutine




    real(dp) function weight(x)
        real(dp), intent(in) :: x
        weight = ((1-x)**alpha)*((1+x)**beta)
    end function

    subroutine printGSBasis()
    integer:: i
    do i = 1,size(basis)
        call printPol(basis(i))
    enddo
    end subroutine

!    real(dp)(:) function 

    end module grammSmith

    