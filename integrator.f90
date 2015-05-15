    module integrator
    use handymanfunctions
!    use dqag
	implicit none
	save

    public :: samengesteldeTrapezium, samengesteldeTrapeziumVastePunten, gaussKwadratuur

	contains

	!    integrates the function f from a to b according to the weight function in steps steps
!
!   f   (real function): real function to integrate
!   a   (real): start of integration interval
!   b   (real): end of integration interval
!   w   (real function): weight function
!   steps (integer) : number of steps used in integration
!
!   result: (real) result of the integration
    function samengesteldeTrapezium(f,w,a,b,steps) result(res)
		real(dp), intent(in):: a,b
		integer, intent(in):: steps
        real(dp), external:: f
        real(dp), external:: w
        integer ::i
        real(dp):: res
        real(dp):: dx
        dx = (b-a)/steps
        res = 0.0d0
        do i = 1,steps
            res  = res + dx*(f((i-1)*dx)*w((i-1)*dx)+f(i*dx)*w(i*dx))/2
        end do
	end function

!    integrates the function fusing the given points fx determined as values for f at points x
!
!   x   real(:)     points where the function is known
!   fx   real(:)    values of the function at known points
!
!   result: (real) result of the integration
    function samengesteldeTrapeziumVastePunten(x,fx) result(res)
        real(dp):: x(:)
        real(dp):: fx(:)
        integer ::i
        real(dp):: res
        res = 0.0d0
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
        real(dp), external :: f
        real(dp), intent(in) :: a,b,alhpa,beta
        integer, intent(in) :: steps
        real(dp):: res
        res = 5.0d0
    end function

!    function weightsForGaussQuadrature(a,b,alpha,beta,steps) result(res)
!        real, intent(in):: a,b,alpha,beta
!
!    end function

    function quadIntegrator(f,a,b) result(res)
        real(dp), external :: f
        real(dp), intent(in) :: a,b
        real(dp):: epsabs = 0.00001
        real(dp):: epsrel = 0.00001
        integer:: key = 5
        real(dp) :: result, abserr
        integer:: neval, ier
        integer :: limit = 1000
        integer:: lenw = 4000
        integer:: last
        integer :: iwork(1000)
        real(dp) :: work(4000)
        real(dp) :: res

        call dqag(f,a,b,epsabs,epsrel,key,result,abserr,neval,ier,limit,lenw,last,iwork,work)
        res = result
    end function

    end module integrator

    