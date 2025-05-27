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
  
end module characters_strings_more
