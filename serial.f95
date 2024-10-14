program calculate_pi
  use FMZM
  implicit none

  integer            :: argc
  type(fm)           :: pi, fmpival
  type(im)           :: n
  character(len=500) :: numdigs, printpistr
  real*16            :: start, finish
  logical            :: piprint

  argc = command_argument_count()
  call get_command_argument(1, numdigs)
  call get_command_argument(2, printpistr)

  if(argc.lt.2) then
    piprint = .false.
  else
    piprint = .true.
  end if

  call fm_set(to_int(to_im(numdigs)))

  n = to_im(numdigs) / 14

  call cpu_time(start)
  pi = 1 / inverse_pi(n)
  call cpu_time(finish)

  if(piprint.eqv..true.) then
    print*, "pi = "
    call fm_print(pi)
  endif
  write(*, "('Computation time: ', f0.6, ' seconds.')") finish - start

  contains
  
  function inverse_pi(n)
    type(im), intent(in) :: n
    type(fm)             :: inverse_pi
    type(fm)             :: y, pos, neg
    type(im)             :: i

    y = 0
    i = 0

    do while(i.lt.n)

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
