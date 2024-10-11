program calculate_pi
  use FMZM
  implicit none

  type(fm)           :: pi
  type(im)           :: n
  character(len=500) :: argv

  call get_command_argument(1, argv)
  call fm_set(10)

  n = to_im(argv)

  pi = 1 / inverse_pi(n)

  call fm_print(pi)
  call fm_print(1/pi)

  contains
  
  function inverse_pi(n)
    type(im), intent(in) :: n
    type(fm)             :: inverse_pi
    type(fm)             :: y, factnum, factdenom, num, denom
    type(im)             :: i

    y = 0
    i = 0

    do while(i.le.n)

      factnum = (-1)**i * gamma(to_fm(6*i + 1))
      factdenom = (gamma(to_fm(3*i + 1)) * (gamma(to_fm(i + 1))**3) )
      denom = 640320**((3*i+3)/2) 
      num =13591409 + 545140134*i 

      y = y + ((factnum/factdenom)*(num/denom))
      
      call fm_print(y)

      i = i + 1
    end do

    inverse_pi = 12 * y
        
  end function

end program
