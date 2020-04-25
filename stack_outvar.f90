! Taken from https://rosettacode.org/wiki/Stack#Fortran
! And modified for the type wanted, type(out_var)
! Could be re-written with polymorphism?
! dmr - 2020-04-22
module stack_outvar

  use cf_elements_mod, only: out_var, get_undefined_outvar

  public
 
  ! Define the data-structure to hold the data
  type stack_var
!~      integer, allocatable :: data(:)
     type(out_var), dimension(:), allocatable :: data
     integer              :: size = 0
  end type stack_var
 
  ! Set the size of allocated memory blocks
  integer, parameter, private :: block_size = 10
 
contains
 
  ! Push ----------------------------------------------------------------------
  subroutine push(s, e)
    type(stack_var), intent(inout) :: s
    type(out_var), intent(in)      :: e
!~     integer, intent(in)            :: e
!~     integer, allocatable :: wk(:)
    type(out_var), dimension(:), allocatable :: wk
    
    if (.not. allocated(s%data)) then
       ! Allocate space if not yet done
       allocate(s%data(block_size))
 
    elseif (s%size == size(s%data)) then
       ! Grow the allocated space
       allocate(wk(size(s%data)+block_size))
       wk(1:s%size) = s%data
       call move_alloc(wk,s%data)
 
    end if
 
    ! Store the data in the stack
    s%size = s%size + 1
    s%data(s%size) = e
  end subroutine push
 
  ! Pop -----------------------------------------------------------------------
  type(out_var) function pop(s)
!~     integer :: pop
    type(stack_var), intent(inout) :: s
    if (s%size == 0 .or. .not. allocated(s%data)) then
!~        pop = 0
         pop = get_undefined_outvar()
       return
    end if
    pop = s%data(s%size)
    s%size = s%size - 1
  end function pop
 
  ! Peek ----------------------------------------------------------------------
  type(out_var) function peek(s)
    type(stack_var), intent(inout) :: s
    if (s%size == 0 .or. .not. allocated(s%data)) then
!~        peek = 0
         peek = get_undefined_outvar()
       return
    end if
    peek = s%data(s%size)
  end function peek
 
  ! Empty ---------------------------------------------------------------------
  logical function empty(s)
    type(stack_var), intent(inout) :: s
    empty = (s%size == 0 .or. .not. allocated(s%data))
  end function empty
 
end module stack_outvar
