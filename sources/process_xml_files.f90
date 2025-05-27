!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

!   Copyright 2020-2021 Didier M. Roche (a.k.a. dmr)

!   Licensed under the Apache License, Version 2.0 (the "License");
!   you may not use this file except in compliance with the License.
!   You may obtain a copy of the License at

!       http://www.apache.org/licenses/LICENSE-2.0

!   Unless required by applicable law or agreed to in writing, software
!   distributed under the License is distributed on an "AS IS" BASIS,
!   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!   See the License for the specific language governing permissions and
!   limitations under the License.

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|


module process_xml_files

  implicit none
  
  private
  
  public :: read_param_file_to_stack
  
  contains 
  
  function read_param_file_to_stack(f_name, stackvar, mini_field) result(success)

      use characters_strings_more, only: extd_line, at_char, tb_char, char_false, char_true, key_varout, zero_char
      use stack_outvar, only: out_var, stack_var, push
      use cf_elements_mod, only: nb_char_varname

      character(len=*), intent(in) :: f_name
      type(stack_var), intent(out) :: stackvar

      integer, optional, intent(in) :: mini_field
  
      logical :: success
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
! dmr   Local variables
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
  
      logical :: file_exists
      integer :: f_unit, stat, count_var=0, min_field=5
      character(len=extd_line) :: file_line
      integer :: nb_fields, i, indx_char, len_line, field, sub_indx, size_field
      
      character(len=:), allocatable  :: char_field

      type(out_var)   :: stackelt, new_stackelt


      if (PRESENT(mini_field) ) then
        min_field = mini_field
      endif
      
      success = .false.
      
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!         IN : a filename
!         OUT: a stack_var with the variables main information elements
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
                                                          
      INQUIRE(FILE=f_name, EXIST=file_exists)
      open(file=f_name, form="formatted", newunit=f_unit)

      do
        
        read(f_unit,'(A)',iostat=stat) file_line
        
        if (stat /= 0) exit
        
        ! process buffer
        if ( file_line(1:1) /= at_char) cycle
        
        count_var = count_var + 1
        
        nb_fields = COUNT([(file_line(i:i),i=1,len_trim(file_line))].eq.tb_char)+1

        if ( nb_fields .lt. min_field) then
           write(*,*) "[FATAL ERROR] number of fields read from "//f_name//" is less than "//char(min_field+zero_char)
           exit
        endif

        indx_char=1
        len_line=len_trim(file_line)
        
        do field=1,min_field-1 ! last field is a comment, so skip
           
           ! Find where the "tab" character is and read whatever is before
           sub_indx = index(file_line(indx_char:len_line),tb_char)
           size_field=sub_indx-1
           allocate(character(len=size_field) :: char_field)
           read(file_line(indx_char:indx_char+size_field),*)  char_field
           
           ! key to specify an output variable
           if ((trim(char_field).eq.key_varout).and.(field.eq.1)) then
               stackelt%var_type = key_varout
           ! name of the variable               
           else if ((len_trim(char_field).le.nb_char_varname).and.(field.eq.2)) then
               stackelt%var_name = trim(char_field)
           ! type of output requested               
           else if ((trim(char_field).eq.char_true).or.(trim(char_field).eq.char_false)) then
              if (trim(char_field).eq.char_true) then
                if (field.eq.3) then
                   stackelt%monthly = .true.
                else
                   stackelt%yearly = .true.
                endif
              else
                if (field.eq.3) then
                  stackelt%monthly = .false.
                else
                  stackelt%yearly = .false.
                endif              
              endif
           endif  
           indx_char = indx_char + size_field + 1 
           deallocate(char_field)

        end do ! finished line parsing
        
        call push(stackvar,stackelt) ! add the given variable to the stack
      end do ! cycle, next buffer line ...

      close(f_unit)

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
! dmr   The section before could be moved in a coherent subroutine
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
  
  end function read_param_file_to_stack
  
end module process_xml_files
