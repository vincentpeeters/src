    module handymanfunctions
	implicit none
	save
	
	
	contains

    real function square(x)
        real, intent(in):: x
        square = x*x
    end function

    real function const_1(x)
        real, intent(in):: x
        const_1 = 1
    end function


    end module handymanfunctions

    