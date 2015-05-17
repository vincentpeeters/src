    module recurrence
!    use polynomialModule
    use handymanfunctions
    use lagrange
    use integrator

	implicit none
	save

!    type (polynomial), allocatable, private :: basis(:)
    real(dp):: alpha = 1.0d0,beta = 1.0d0
    real(dp):: orthToll = 0.0001d0
    real(dp) :: xlow, xhigh



	contains

!    Give the value of the monoic orthogonal basis, not normalised
    real(dp) function recurrenceBaseValue(x,degree)
        real(dp), intent(in) ::x
        integer, intent(in) :: degree
        integer::i
        real(dp) :: fi, fiold, fitemp
        fiold = 0.0d0
        fi = 1.0d0
        do i = 1,degree
            fitemp = fi
            fi = (x - alpha)*fi - beta*fiold
            fiold = fitemp
        enddo
        recurrenceBaseValue = fi
    end function

    real(dp) function normOfRecurenceBasisOfDegree(degree) result(res)
        integer:: degree
!        real(dp), intent(out) :: res
        res = 
    end function

    subroutine prepareInterpolationCoeficients(x,fx)
        real(dp) :: x
        real(dp) :: fx

    end subroutine

    real(dp) function NormIntegratorFunctionWrapper(x)
        real(dp) intent(in) :: x
         NormIntegratorFunctionWrapper = weightFunction(x)*weightFunction(x)
    end function

    real(dp) function weightFunction(x)
        weightFunction = ((1-x)**alpha)*((1+x)**beta)
    end function

!    subroutine initiateBasisGS(newBasis, n)
!        type (polynomial):: newBasis(:)
!        integer :: n
!        n = size(newBasis)
!        allocate(basis(n))
!        basis = newBasis
!    end subroutine

!    subroutine makeGSBasis(x,fx)
!        real(dp) :: x(:),fx(:)
!        integer :: N,i 
!        N = size(x)
!        allocate(basis(N))
!        do i=1,N
!            basis(i) = getLagrangePol(x(1:i),fx(1:i))
!        enddo
!        call orthonormalizeGSBasisGS()
!    end subroutine
!
!    integer function nbGSBasisFunctions()
!        nbGSBasisFunctions = size(basis)
!    end function
!
!    type(polynomial) function nthGSBasis(n)
!        integer, intent(in) :: n
!        nthGSBasis = basis(n)
!    end function


!    subroutine orthonormalizeGSBasisGS()
!        integer:: i,j
!        real(dp) :: dot
!        do i = 1, nbGSBasisFunctions()
!            do j = 1,i-1
!!            substract the parts
!                dot = polDot(weight,-1.0d0,1.0d0,basis(i),basis(j))
!                write(*,*) dot
!                basis(i) = subPol(basis(i),multPol(basis(j),dot))
!            end do
!!            normalize
!            basis(j) = devidePol(basis(i),polNorm(weight,-1.0d0,1.0d0,basis(i)))
!        end do
!    end subroutine




!    real(dp) function weight(x)
!        real(dp), intent(in) :: x
!        weight = ((1-x)**alpha)*((1+x)**beta)
!    end function
!
!    subroutine printGSBasis()
!    integer:: i
!    do i = 1,size(basis)
!        call printPol(basis(i))
!    enddo
!    end subroutine

!    real(dp)(:) function 

    end module grammSmith

