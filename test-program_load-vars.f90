      program load_vars
      
      use cf_elements_mod , only: xfs_list, XFSL_createRoot, XFSL_addNode, out_var, nb_char_varname

      use stack_outvar, only: stack_var, push, pop, empty

      
      implicit none
      
      character(len=1)   :: at_char="@"
      character(len=1)   :: tb_char=""//achar(9)
      character(len=22)  :: f_name ="NewGen_netcdfout.param"
      character(len=132) :: file_line
      character(len=:), allocatable  :: char_field
      character(len=7)   :: error_char = ".error."
      
      integer, parameter :: str_len = 256
      
      logical :: file_exists
      integer :: f_unit
      integer :: stat
      integer :: nb_fields, i, indx_char, len_line, field, sub_indx, size_field
      integer, parameter :: min_field=5
      integer, parameter :: zero_char=48

      character(len=10),parameter:: key_varout="@varoutput"
      character(len=3) ,parameter:: char_true=".T.", char_false=".F."
      
      integer, parameter :: data_chunk = 10
      integer            :: nb_chunk = 1, count_var = 0
              
              
      character(len=str_len) :: xml_file_wrk  
      character(len=str_len) :: terugkeer, word_n
      
      character(len=10)  :: path_XML = "xml_files/"
      integer :: n_xmlvarfiles = 0
                              
                              
      type(xfs_list), pointer :: xml_variables, xml_variableswk, xfs_work
                             
                        
!~       type(out_var), dimension(:), allocatable :: out_var_list
!~       type(out_var), dimension(:), allocatable :: out_var_storage
      type(out_var)   :: stackelt, new_stackelt
      type(stack_var) :: stackvar
                              
      ! start main code ...                               
                                            
              
      INQUIRE(FILE=f_name, EXIST=file_exists)
      open(file=f_name, form="formatted", newunit=f_unit)

!~       allocate(out_var_list(data_chunk*nb_chunk))

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
        
           sub_indx = index(file_line(indx_char:len_line),tb_char)
           size_field=sub_indx-1
           allocate(character(len=size_field) :: char_field)
           read(file_line(indx_char:indx_char+size_field),*)  char_field
           
           if ((trim(char_field).eq.key_varout).and.(field.eq.1)) then
               stackelt%var_type = key_varout
           else if ((len_trim(char_field).le.nb_char_varname).and.(field.eq.2)) then
              stackelt%var_name = trim(char_field)
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
        
        call push(stackvar,stackelt)
      end do ! cycle, next buffer line ...

      do 
        stackelt = pop(stackvar)

        write(*,*) " ============= "
        write(*,*) stackelt%var_type
        write(*,*) stackelt%var_name
        write(*,*) stackelt%monthly
        write(*,*) stackelt%yearly
        write(*,*) " PROCESSING ... "
         
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
         
        terugkeer = XMLLOAD_oneFILE(trim(xml_file_wrk),stackelt,xml_variableswk)
        
        if ( trim(terugkeer).ne.error_char ) then 
          n_xmlvarfiles = n_xmlvarfiles + 1 
        endif
        
        ! Here add treatment for new variables that need to be taken care of (dependencies ...)
        if ( xml_variableswk%dep_sze .ge. 1 ) then
           !treatment of the variables, namely add them to the stack
           do
              word_n = pop_word(terugkeer)
              write(*,*) "Word ==", trim(word_n)
              if ( word_n == "time" ) then
                 cycle
              else
                 xml_file_wrk = inquire_after_varfile(word_n,path_XML)
                 
                 ! create an element for the stack and push it to the stack
                 ! [NOTA] None drawback, will call inquire_after_varfile a second time. Costless anyhow.
                 new_stackelt%var_name = trim(word_n)
                 call push(stackvar,new_stackelt)
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
        USE modmxm_stkmx, only: stack_minixml, stkmx_ptr, STKMX_getElementNodeByName
      
        USE MODMXM_STRUCTLOAD, only: XMLSTRUCT, XMLLOAD
      
        use cf_elements_mod, only: varia_element
        use cf_elements_mod, only: cp_elttag_varname, cp_elttag_axeslst, cp_elttag_fixsize
        use cf_elements_mod, only: xfs_list
        
        character(len=*), intent(in) :: nameXML_file
        type(out_var),    intent(in) :: node_sngl_var
        type(xfs_list), pointer, intent(inout) :: xml_var_wrk        
        
        logical :: outcome
        character(len=str_len) :: to_return
                
        TYPE(stack_xmlevents), POINTER           :: xml_base_event_p
        TYPE(stack_minixml), POINTER             :: xml_base_stack_p
        TYPE(stkmx_ptr), DIMENSION(:), POINTER   :: list_elements
        TYPE(stack_minixml), POINTER             :: element_XMLnode

        
        integer :: n_maxlen_eltname, n_maxlen_attname, n_maxlen_attcont
        integer :: n_maxdepth, n_elements, nlen_returned, nb_words                        
        
        character(len=str_len)         :: name_element
        character(len=:), allocatable  :: string_wrk
        
        to_return = ""
                        
        !--- Premier parsing de la liste xml fournie
        xml_base_event_p => XMLSTRUCT(nameXML_file, n_maxlen_eltname, n_maxlen_attname, n_maxlen_attcont)
        
        write(*,*) "MaxS read: (  I) ", n_maxlen_eltname, n_maxlen_attname, n_maxlen_attcont

        ! [GUY DIXIT] :
        ! Could check here if maxlen's do not exceed currently adopted parameterizations
        xml_base_stack_p => XMLLOAD(nameXML_file, xml_base_event_p, n_maxdepth)

        if ( trim(node_sngl_var%var_type) .eq. key_varout ) then ! found a variable of type @varout
          
           name_element = varia_element ! looking for elements describing variable types          
           list_elements => STKMX_getElementNodeByName(xml_base_stack_p, name_element)
           
           n_elements = SIZE(list_elements)
           
           if ( n_elements.eq.1 ) then ! for now I assume each variable has its own file
              element_XMLnode => list_elements(1)%ptr
              
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
                 read(string_wrk,'(I)') xml_var_wrk%s_ize
              endif
              deallocate(string_wrk)

              ! Finally associate the rest of the elements to the pointer, for future use
              xml_var_wrk%xml_datastack => element_XMLnode
                            
           else
           
             write(*,*) " [ERROR, PASS] unkown var element, not matched in XML = "//trim(node_sngl_var%var_type)
             to_return = error_char
           
           endif
        else
           write(*,*) " [ERROR, PASS] unkown element type for var = "//trim(node_sngl_var%var_type)
           to_return = error_char
        endif
                
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
        write(*,*) "cp_elttag ::", cp_elttag
        if (ASSOCIATED(stkmx_work)) then
           stkrcp_work => STKMX_getPCDatacntt(stkmx_work)
           CALL STKRC_copyStkrcToStr(stkrcp_work(1)%ptr, c_attcntt)
           string_elt = c_attcntt
        else
           write(*,*) "Elt does not exist in XML file"
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



      end program load_vars
