program calculate_pi
  use FMZM
  implicit none

  contains
  
  function inverse_pi(n)
    integer (kind=selected_int_kind(18)), intent(in) :: n
    integer (kind=selected_int_kind(18)) :: i
    type(fm)                             :: inverse_pi
    type(fm)                             :: y
    type(im)                             :: mp_i

    y = 0

    do i = 0, n
      mp_i = to_im(i)

      y = ( (-1)**i * gamma(to_fm((6*mp_i) + 1)) )&
          / (gamma(to_fm((3*mp_i)+1)) * gamma(to_fm(mp_i+1)) **3 )&
          * ( (13591409 + 545140134*i)/640320**((3*n+3)/2) )
    end do

    inverse_pi = 12 * y
        
  end function

end program
