program calculate_pi
  use FMZM
  implicit none

  type(fm)           :: pi
  type(im)           :: n
  character(len=500) :: argv

  call get_command_argument(1, argv)
  call fm_set(100)

  n = to_im(argv)
  call im_print(n)

  pi = inverse_pi(n)

  call fm_print(pi)

  contains
  
  function inverse_pi(n)
    type(im), intent(in) :: n
    type(fm)             :: inverse_pi
    type(fm)             :: y
    type(im)             :: i

    y = 0
    i = 0

    do while(i.lt.n)
      i = i + 1

      y = ( (-1)**i * gamma(to_fm((6*i) + 1)) )&
          / (gamma(to_fm((3*i)+1)) * gamma(to_fm(i+1)) **3 )&
          * ( (13591409 + 545140134*i)/640320**((3*i+3)/2) )
    end do

    inverse_pi = 12 * y
        
  end function

end program
