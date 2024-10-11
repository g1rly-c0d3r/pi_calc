program calculate_pi
  use FMZM
  implicit none

  type(fm)           :: pi, fmpival
  type(im)           :: n
  character(len=500) :: argv
  real*16            :: start, finish

  call get_command_argument(1, argv)
  call fm_set(10000)

  call fm_pi(fmpival)

  n = to_im(argv)

  call cpu_time(start)
  pi = 1 / inverse_pi(n)
  call cpu_time(finish)

  call fm_print(pi - fmpival)

  write(*, "('Computation time: ', f0.6, ' seconds.')") finish - start

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
      num =13591409 + 545140134*i 
      denom = 640320**((3*i)+(3.0/2.0)) 

      y = y + ((factnum/factdenom)*(num/denom))
      
      i = i + 1
    end do

    inverse_pi = 12 * y
        
  end function

end program
