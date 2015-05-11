    module integrator
	implicit none
	save

    public :: samengesteldeTrapezium, samengesteldeTrapeziumVastePunten, gaussKwadratuur
!    private ::

	contains
	
    function samengesteldeTrapezium(f,w,a,b,steps) result(res)
		real, intent(in):: a,b
		integer, intent(in):: steps
        real, external:: f
        real, external:: w
        integer ::i
        real:: res
        real:: dx
        dx = (b-a)/steps
        res = 0
        do i = 1,steps
            res  = res + dx*(f((i-1)*dx)*w((i-1)*dx)+f(i*dx)*w(i*dx))/2
        end do
	end function

    function samengesteldeTrapeziumVastePunten(x,fx) result(res)
        real:: x(:)
        real:: fx(:)
        integer ::i
        real:: res
        res = 0
        do i = 1,size(x)-1
            res  = res + (x(i+1)-x(i))*(fx(i+1)+fx(i))/2
        end do
    end function

!    integrates the function f from a to b according to the weight function w(x)=(1-x)^alpha * (1+x)^beta in steps steps
!
!   f   (real function): real function to integrate
!   a   (real): start of integration interval
!   b   (real): end of integration interval
!   alpha:  (real): w(x)=(1-x)^alpha * (1+x)^beta
!   beta:   (real): w(x)=(1-x)^alpha * (1+x)^beta
!
!   return: (real) result of the integration
    function gaussKwadratuur(f,a,b,alhpa,beta,steps) result(res)
        real, external :: f
        real, intent(in) :: a,b,alhpa,beta
        integer, intent(in) :: steps
        real:: res
        res = 5.0
    end function

!    function weightsForGaussQuadrature(a,b,alpha,beta,steps) result(res)
!        real, intent(in):: a,b,alpha,beta
!        
!    end function

    end module integrator

    