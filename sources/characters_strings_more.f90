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


module characters_strings_more

  implicit none
  
  public

  integer, parameter            :: str_len = 256          ! Maximum string size
  integer, parameter            :: extd_line = 132        ! FORTRAN EXTENDED LINE
  character(len=1), parameter   :: at_char="@"            ! An "@" character
  character(len=1), parameter   :: sp_char=" "            ! A space
  character(len=1), parameter   :: tb_char=""//achar(9)   ! A "tab" (\t)

  character(len=7), parameter   :: error_char = ".error." ! An error in string format
  
  integer, parameter            :: zero_char=48           ! ASCII code of character "0"


  character(len=10),parameter   :: key_varout="@varoutput"
  character(len=7), parameter   :: key_unknown="Unknown"
  character(len=12),parameter   :: key_coordout="@coordoutput"
  character(len=3) ,parameter   :: char_true=".T."
  character(len=3) ,parameter   :: char_false=".F."
  
  
  contains
  
  
  function count_words(string_in) result(nb_word)
      
    character(len=*), intent(in) :: string_in
    integer                      :: nb_word
        
    character(len=1), parameter  :: blank = " "
    integer                      :: i=0
         
    nb_word = COUNT([(string_in(i:i),i=1,len_trim(string_in))].eq.blank)+1
        
    return
  end function count_words
      
  function pop_word(string_inout) result(word_out)
        
    character(len=str_len), intent(inout) :: string_inout
    character(len=1), parameter           :: blank = " "
    character(len=str_len)                :: word_out
    character(len=str_len)                :: chataract
        
    integer                         :: size_string
    integer                         :: i
         
    chataract = trim(adjustl(string_inout))
    size_string = len_trim(chataract)

    i = 1

    do          
      if (chataract(i:i).ne.blank) then
        i = i + 1
        cycle
      else
        if (len_trim(chataract(1:i)).ne.1) then
          word_out = chataract(1:i-1)
          if (i.ge.size_string) then
            string_inout = ""
          else
            string_inout = chataract(i:size_string)
          endif
          exit
        else
          cycle
        endif
      endif
    enddo
        
    return
  end function pop_word
      
  function HAS(A,B) result(yes) !Text B appears somewhere in text A?
    CHARACTER*(*) A,B
    INTEGER L
    logical :: yes
    
    L = INDEX(A,B)              !The first position in A where B matches.
    IF (L.LE.0) THEN
      yes=.false.
    ELSE
      yes=.true.
    END IF
  END function HAS

  function ADD(a,b) result(done) ! Add string b into string a if necessary space exists

    CHARACTER(*), intent(inout) :: a
    CHARACTER(*), intent(in)    :: b
    INTEGER      ::  L,L_a, L_b
    character(len=:), allocatable :: c
       
    logical :: done
       

    L = len(a)
    L_a = len_trim(a)
    L_b = len_trim(b)
               
    if ( (L_a+L_b) .gt. L) then
      done = .false. ! no space to add string b in a
    else
      allocate(character(len=L_a+L_b+1) :: c)
      write(c,'(A)') trim(a)
      a = ""
      write(a,'(A)') ""//trim(c)//sp_char//trim(b)
      deallocate(c)
      done = .true.
    endif

  end function ADD

end module characters_strings_more
