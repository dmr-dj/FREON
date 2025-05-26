      program load_vars
      
      use cf_elements_mod, only: xfs_list, XFSL_createRoot, XFSL_addNode, out_var, nb_char_varname

      use stack_outvar, only: stack_var, push, pop, empty
    
      implicit none


#define VERBOSE 0
      
      character(len=1)   :: at_char="@"
      character(len=1)   :: sp_char=" "
      character(len=1)   :: tb_char=""//achar(9)
      character(len=35)  :: f_name ="../inputdata/NewGen_netcdfout.param"
      character(len=132) :: file_line
      character(len=:), allocatable  :: char_field
      character(len=7)   :: error_char = ".error."
      
      integer, parameter :: str_len = 256
      
      logical :: reussi

! --- dmr ASCII code of character "0"
      integer, parameter :: zero_char=48
      
! --- dmr  Variables for handling file openings      
      logical :: file_exists
      integer :: f_unit
      integer :: stat

! --- dmr        
      integer :: nb_fields, i, indx_char, len_line, field, sub_indx, size_field

! --- dmr Minimum number of fields in the "netCDF param" file opened
      integer, parameter :: min_field=5

      character(len=10),parameter:: key_varout="@varoutput"
      character(len=7),parameter :: key_unknown="Unknown"
      character(len=12),parameter:: key_coordout="@coordoutput"
      character(len=3) ,parameter:: char_true=".T.", char_false=".F."
      
!~       integer, parameter :: data_chunk = 10
      integer                    :: count_var = 0
!~       integer            :: nb_chunk = 1, count_var = 0              
              
      character(len=str_len) :: xml_file_wrk  
      character(len=str_len) :: terugkeer, word_n
      
      character(len=13)  :: path_XML = "../xml_files/"
      integer :: n_xmlvarfiles = 0
      
      character(len=str_len) :: liste_dependent_var =" "
                              
                              
      type(xfs_list), pointer :: xml_variables, xml_variableswk, xfs_work
                             
                        
      type(out_var)   :: stackelt, new_stackelt
      type(stack_var) :: stackvar
                              
      ! start main code ...                               
                                                          
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
! dmr   The section below could be moved in a coherent subroutine
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

     ! Finished reading the input file. Now process the variables on stack
      do 
        stackelt = pop(stackvar)

        write(*,*)
        write(*,*) " PROCESSING ... "
        write(*,*) "| ", stackelt%var_name
#if ( VERBOSE == 1 )
        write(*,*) stackelt%monthly
        write(*,*) stackelt%yearly
        write(*,*) stackelt%var_type
#endif        
        write(*,*) " ============= "
         
        xml_file_wrk = inquire_after_varfile(stackelt%var_name,path_XML)
                  
        ! --- dmr Piece of code taken from xmlcocogen_loaddb (c) Guy Munhoven
        ! --- Aim: Create the list where we store all the variables as elements
        ! --- Need three elements of type XFS_list: one will be the actual list, the other ones are pointer for work
        
        IF (n_xmlvarfiles == 0) THEN
          ! Now create the XFS_LIST for the XML composition files
          xml_variables  => XFSL_createRoot()
          xml_variableswk => xml_variables
        ELSE
          xfs_work => xml_variableswk
          xml_variableswk => XFSL_addNode(xfs_work)
        ENDIF

#if ( VERBOSE == 1 )              
              write(*,*) "Trying to load ==", trim(xml_file_wrk)
#endif            
        terugkeer = XMLLOAD_oneFILE(trim(xml_file_wrk),stackelt,xml_variableswk)
        
        if ( trim(terugkeer).ne.error_char ) then 
          n_xmlvarfiles = n_xmlvarfiles + 1 
        endif
        
        ! Here add treatment for new variables that need to be taken care of (dependencies ...)
        if ( xml_variableswk%dep_sze .ge. 1 ) then
           !treatment of the variables, namely add them to the stack
           do
              word_n = pop_word(terugkeer)
#if ( VERBOSE == 1 )              
              write(*,*) "Word ==", trim(word_n)
#endif              
              if ( word_n == "time" .and. (.not.(has(liste_dependent_var,trim(word_n)))) ) then
                 reussi = add(liste_dependent_var,trim(word_n))
                 cycle
              else
                 xml_file_wrk = inquire_after_varfile(word_n,path_XML)
                 
                 if (.not.(has(liste_dependent_var,trim(word_n))) ) then
                      ! good dependency, not taken into account yet
                      ! create an element for the stack and push it to the stack
                      ! [NOTA] Known drawback, will call inquire_after_varfile a second time. Costless anyhow.
                      new_stackelt%var_name = trim(word_n)
                      
                      call push(stackvar,new_stackelt)
                      reussi = add(liste_dependent_var,trim(word_n))
#if ( VERBOSE == 1 )                      
                      write(*,*) "list dependencies == ", trim(liste_dependent_var)
#endif                      
                  else
                      ! do nothing, already there, ignored                      
                      write(*,*) "Requested dependency: ", word_n, "already met"
                  endif
              endif
              if ( len_trim(terugkeer).gt.0 ) then
                cycle
              else
                exit
              endif
           enddo
           
        endif
        
        if ( empty(stackvar) ) exit
        
      enddo
      
      contains
      
      
      ! --- Lookup whether there is a file with that name, if yes return it. If not, abort
      function inquire_after_varfile(varname,filespath) result(filename)
      
      
        character(len=*), intent(in) :: varname
        character(len=str_len)       :: filename
        
        character(len=*), optional, intent(in) :: filespath ! could be used to add a directory for example
        logical                      :: file_exists
        
        if (present(filespath)) then
          filename = trim(filespath)//trim(varname)//".xml"
        else
          filename = trim(varname)//".xml"
        endif

        INQUIRE(FILE=filename, EXIST=file_exists)
        if ( .not. file_exists ) then
           write(*,*) " [FATAL, ABORT] no XML description file: = "//trim(filename)
           call abort()
        endif
        
        return
        
      end function inquire_after_varfile
      
      
      
      ! --- input needed:  name of the XML file, one node pointing to variable list
      function XMLLOAD_oneFILE(nameXML_file,node_sngl_var,xml_var_wrk) result(to_return)
      
        USE modmxm_stkxe, only: stack_xmlevents
        USE modmxm_stkmx, only: stack_minixml, stkmx_ptr, STKMX_getElementNodeByName, STKMX_getAttcnttByIdx &
                        , STKMX_getAttIdxByName, STKRC_copyStkrcToStr
      
        USE MODMXM_STRUCTLOAD, only: XMLSTRUCT, XMLLOAD
        
        USE modmxm_stkrc, only: stack_recchunks
      
        use cf_elements_mod, only: varia_element, coord_element
        use cf_elements_mod, only: cp_elttag_varname, cp_elttag_axeslst, cp_elttag_fixsize
        use cf_elements_mod, only: xfs_list
        use cf_elements_mod, only: attr_coordelt, attr_eltgenui, attr_eltpsdo ! [DEPRECATED] , attr_eltalia
        use cf_elements_mod, only: cp_elttag_redirfn
        
        character(len=*), intent(in)    :: nameXML_file
        type(out_var),    intent(inout) :: node_sngl_var
        type(xfs_list), pointer, intent(inout) :: xml_var_wrk        
        
        logical :: outcome
        character(len=str_len) :: to_return
                
        TYPE(stack_xmlevents), POINTER           :: xml_base_event_p
        TYPE(stack_minixml), POINTER             :: xml_base_stack_p
        TYPE(stkmx_ptr), DIMENSION(:), POINTER   :: list_elements
        TYPE(stack_minixml), POINTER             :: element_XMLnode
        TYPE(stack_recchunks), POINTER           :: stkrc_attcntt
        
        integer :: n_maxlen_eltname, n_maxlen_attname, n_maxlen_attcont
        integer :: n_maxdepth, n_elements, nlen_returned, nb_words
        integer :: i_att 
        
        character(len=str_len)         :: name_element
        character(len=str_len)         :: c_attcntt, wrk_char_var
        character(len=:), allocatable  :: string_wrk, var_typic,lokaal_XMLf
        
        to_return = ""
                        


        allocate(character(len=len_trim(nameXML_file)) :: lokaal_XMLf)        
        lokaal_XMLf=trim(nameXML_file)
                
        do
        
        if (allocated(var_typic)) then
          deallocate(var_typic)
        endif
        
        allocate(character(len=len_trim(node_sngl_var%var_type)) :: var_typic)
        var_typic=trim(node_sngl_var%var_type)
        
!~         write(*,*) "Working with filename ==", lokaal_XMLf
        
        var_type: select case (var_typic) 
        
        ! --- dmr
        ! --- dmr First case: we already know that the variable is a @varout -> Handle       
        ! --- dmr
           case (key_varout,key_coordout)          
            ! call the section to handle @varout

            !--- Premier parsing de la liste xml fournie 
            xml_base_event_p => XMLSTRUCT(lokaal_XMLf, n_maxlen_eltname, n_maxlen_attname, n_maxlen_attcont)
            
#if ( VERBOSE == 1 )           
            write(*,*) "MaxS read: (  I) ", n_maxlen_eltname, n_maxlen_attname, n_maxlen_attcont
#endif            

           ! [GUY DIXIT] :
           ! Could check here if maxlen's do not exceed currently adopted parameterizations
           xml_base_stack_p => XMLLOAD(lokaal_XMLf, xml_base_event_p, n_maxdepth)

           if (var_typic == key_varout) then
             name_element = varia_element ! looking for elements describing variable types          
           else
             name_element = coord_element ! looking for elements describing coordinate types          
           endif
      
           list_elements => STKMX_getElementNodeByName(xml_base_stack_p, name_element)
           
           n_elements = SIZE(list_elements)
           
#if ( VERBOSE == 1 )
           write(*,*) "I have ===", n_elements, " elements"
#endif           
           
           if ( n_elements.eq.1 ) then ! for now I assume each variable has its own file
              element_XMLnode => list_elements(1)%ptr
           
           else if ( n_elements.gt.1) then ! multiple element file, need to find index of the matching one, by name ...

              ! [TODO] CODE to find which element is the one matching node_sngl_var%var_name
              write(*,*) "node_sngl_var%var_name == ", trim(node_sngl_var%var_name)
              write(*,*) "[ERROR, WAIT]"
              read(*,*)
           else
           
             write(*,*) " [ERROR, PASS] unkown var element, not matched in XML = "//trim(node_sngl_var%var_type)
             to_return = error_char
             exit
           endif
             
              ! Now need to retrieve the base elements to populate a CF_ELEMENTS_MOD, XFS_list element

              ! Start with the variable name ...
              xml_var_wrk%var_name = get_tagged_element(element_XMLnode,cp_elttag_varname)
              
              ! then with the dependencies in axes ...
              xml_var_wrk%dep_list = get_tagged_element(element_XMLnode,cp_elttag_axeslst)
              
              allocate(character(len=len_trim(xml_var_wrk%dep_list)) :: string_wrk)
              string_wrk = trim(xml_var_wrk%dep_list)
              if ( string_wrk /= "" ) then
                nb_words = count_words(string_wrk)
                write(*,*) "NB dependencies = ", nb_words, string_wrk
              else
                nb_words = 0
                write(*,*) "NO dependencies for given variable "
              endif

              xml_var_wrk%dep_sze = nb_words
              to_return = ""//string_wrk
              deallocate(string_wrk)

              ! then check is a size element exists (case of a pseudo-axis only for now)
              allocate(character(len=str_len) :: string_wrk)
              string_wrk = get_tagged_element(element_XMLnode,cp_elttag_fixsize)
              if ( len_trim(string_wrk) == 0 ) then ! no assumed size of the element
                 xml_var_wrk%s_ize = -1
              else
                 read(string_wrk,'(I256)') xml_var_wrk%s_ize
              endif
              deallocate(string_wrk)

              ! Finally associate the rest of the elements to the pointer, for future use
              xml_var_wrk%xml_datastack => element_XMLnode
              exit

        ! --- dmr
        ! --- dmr Second case: the element passed has an unknown type (case of elements added as dependencies ...)
        ! --- dmr                      
           case (key_unknown)           
           ! call the section to handle automatic discovery of variable type

            ! --- Premier parsing de la liste xml fournie 
            xml_base_event_p => XMLSTRUCT(lokaal_XMLf, n_maxlen_eltname, n_maxlen_attname, n_maxlen_attcont)
            
#if ( VERBOSE == 1 )        
            write(*,*) "MaxS read: (  I) ", n_maxlen_eltname, n_maxlen_attname, n_maxlen_attcont
#endif            
            ! [GUY DIXIT] :
            ! Could check here if maxlen's do not exceed currently adopted parameterizations
            xml_base_stack_p => XMLLOAD(lokaal_XMLf, xml_base_event_p, n_maxdepth)

            
            ! --- The most likely case is the existence of a coordinate variable
            name_element = coord_element ! looking for elements describing variable types
            
            
            list_elements => STKMX_getElementNodeByName(xml_base_stack_p, name_element)
           
            n_elements = SIZE(list_elements)
#if ( VERBOSE == 1 )             
            write(*,*) "Looking for element, ", coord_element, " , found : ", n_elements
#endif            
            if ( n_elements.eq.1 ) then ! found one element of coordinate type
               ! --- check the coordinate type ...
               element_XMLnode => list_elements(1)%ptr
               i_att = STKMX_getAttIdxByName(element_XMLnode, attr_coordelt)
               stkrc_attcntt => STKMX_getAttcnttByIdx(element_XMLnode, i_att)               
               CALL STKRC_copyStkrcToStr(stkrc_attcntt, c_attcntt, nlen_returned)
#if ( VERBOSE == 1 )                
               write(*,*) "That element has a type of ==", trim(c_attcntt)
#endif               
               
               ! --- For now, three cases arise, two with similar treatment 
               elt_coord: select case (trim(c_attcntt))
                 case (attr_eltgenui,attr_eltpsdo) ! pseudo or genuine coordinate => cycle with coordinate type
                   node_sngl_var%var_type = key_coordout
                   ! --- garbage handling
                   nullify(xml_base_event_p)
                   nullify(xml_base_stack_p)
                   nullify(list_elements)
                   nullify(element_XMLnode)
                   nullify(stkrc_attcntt)
                   cycle
                 case default
                   ! Dunno what to do there!
                   write(*,*) " [ERROR, PASS] unkown element type for var = "//trim(node_sngl_var%var_type)
                   to_return = error_char
               end select elt_coord
               
            else
           
              write(*,*) " [ERROR, PASS] unkown var element, not matched in XML = "//trim(node_sngl_var%var_type)
              to_return = error_char
              exit
            endif
            
            cycle
            
           case default          
           ! Dunno what to do there!
           write(*,*) " [ERROR, PASS] unkown element type for var = "//trim(node_sngl_var%var_type)
           to_return = error_char
           exit
        end select var_type
        
        enddo
              
      end function XMLLOAD_oneFILE

      function get_tagged_element(elt_XMLnode,cp_elttag) result(string_elt)
      
        USE modmxm_stkmx, only: stack_minixml, STKMX_getUniqueChildEltByName, STKMX_getPCDatacntt
        USE modmxm_stkrc, only: stkrc_ptr, STKRC_copyStkrcToStr
                
        TYPE(stack_minixml), POINTER :: elt_XMLnode
        character(len=*), intent(in) :: cp_elttag
                
                
        TYPE(stack_minixml), POINTER :: stkmx_work
        TYPE(stkrc_ptr), DIMENSION(:), POINTER ::  stkrcp_work
      
        character(len=str_len)       :: c_attcntt
        character(len=str_len)       :: string_elt
        
        
        NULLIFY(stkmx_work)
        stkmx_work=>STKMX_getUniqueChildEltByName(elt_XMLnode,cp_elttag)
#if ( VERBOSE == 1 )         
        write(*,*) "cp_elttag ::", cp_elttag
#endif        
        if (ASSOCIATED(stkmx_work)) then
           stkrcp_work => STKMX_getPCDatacntt(stkmx_work)
           CALL STKRC_copyStkrcToStr(stkrcp_work(1)%ptr, c_attcntt)
           string_elt = c_attcntt
        else
#if ( VERBOSE == 1 )                 
           write(*,*) "Elt, "//cp_elttag//" does not exist in XML file"
#endif           
           string_elt = ""
        endif      
         
      end function get_tagged_element

      function count_words(string_in) result(nb_word)
        
        character(len=*), intent(in) :: string_in
        
        character(len=1), parameter  :: blank = " "
        integer                      :: nb_word
         
        nb_word = COUNT([(string_in(i:i),i=1,len_trim(string_in))].eq.blank)+1
        
        return
      end function count_words
      
      function pop_word(string_inout) result(word_out)
        
        character(len=str_len), intent(inout) :: string_inout
        character(len=1), parameter           :: blank = " "
        character(len=str_len)                :: word_out
        character(len=str_len)                :: chataract
        
        integer                         :: nb_word
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
      
      function HAS(A,B) result(yes)	!Text B appears somewhere in text A?
       CHARACTER*(*) A,B
       INTEGER L
       logical :: yes
        L = INDEX(A,B)		!The first position in A where B matches.
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

      end program load_vars
