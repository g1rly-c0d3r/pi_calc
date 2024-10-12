program calculate_pi
  use FMZM
  implicit none

  type(fm)           :: pi, fmpival
  type(im)           :: n
  character(len=500) :: fmsize, nstr
  real*16            :: start, finish

  call get_command_argument(1, fmsize)
  call get_command_argument(2, nstr)

  call fm_set(to_int(to_im(fmsize)))

  n = to_im(nstr)

  call cpu_time(start)
  pi = 1 / inverse_pi(n)
  call cpu_time(finish)

  write(*, "('Computation time: ', f0.6, ' seconds.')") finish - start

  contains
  
  function inverse_pi(n)
    type(im), intent(in) :: n
    type(fm)             :: inverse_pi
    type(fm)             :: y, pos, neg
    type(im)             :: i

    y = 0
    i = 0

    do while(i.le.n)

      pos = (gamma(to_fm(6*i + 1))&
            * (13591409 + 545140134*i))&
            / (gamma(to_fm(3*i + 1)) * (gamma(to_fm(i + 1))**3) &
            * 640320**((3*i)+(3.0/2.0))) 

      neg = (gamma(to_fm(6*(i+1) + 1))&
            * (13591409 + 545140134*(i+1)))&
            / (gamma(to_fm(3*(i+1) + 1)) * (gamma(to_fm((i+1) + 1))**3) &
            * 640320**((3*(i+1))+(3.0/2.0))) 


      y = y + pos - neg
      
      i = i + 2
    end do

    inverse_pi = 12 * y
        
  end function

end program
