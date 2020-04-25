!=======================================================================
 MODULE CF_ELEMENTS_MOD
!=======================================================================
! --- [NOTA]: THIS MODULE IS A RE-VAMP OF mod_xmlcocogen, in MEDUSA
! ---       (C) Guy MUNHOVEN, Ulg
!=======================================================================

   USE modmxm_stkmx, ONLY: stack_minixml

   IMPLICIT NONE

!~    INTEGER, PARAMETER :: p_maxlen_eltname  = 31
!~    INTEGER, PARAMETER :: p_maxlen_attname  = 31
!~    INTEGER, PARAMETER :: p_maxlen_attcntt  = 1023
   INTEGER, PARAMETER :: str_len = 256, ip = 4

   ! Element-tag names in the µXML files
   ! For the axes ...
   ! Parent element: Coordinate
   CHARACTER(LEN=*), PARAMETER :: coord_element  = "Coordinate"
   CHARACTER(LEN=*), PARAMETER :: attr_coordelt  = "type"
   CHARACTER(LEN=*), PARAMETER :: attr_eltgenui  = "genuine"
   CHARACTER(LEN=*), PARAMETER :: attr_eltpsdo   = "pseudo"
   CHARACTER(LEN=*), PARAMETER :: attr_eltalia   = "alias"

   
   CHARACTER(LEN=*), PARAMETER :: varia_element  = "Variable"
   CHARACTER(LEN=*), PARAMETER :: attr_varadelt  = "type"
   CHARACTER(LEN=*), PARAMETER :: attr_eltreal   = "real"
   CHARACTER(LEN=*), PARAMETER :: attr_eltdble   = "double"
   
   CHARACTER(LEN=*), PARAMETER :: cp_elttag_varname = 'VarName'
   CHARACTER(LEN=*), PARAMETER :: cp_elttag_axeslst = 'Axes_List'   
   CHARACTER(LEN=*), PARAMETER :: cp_elttag_fixsize = 'Size'   
   CHARACTER(LEN=*), PARAMETER :: cp_elttag_codebit = 'CodeBits'
   CHARACTER(LEN=*), PARAMETER :: cp_elttag_varasrc = 'VarSource'
   CHARACTER(LEN=*), PARAMETER :: cp_elttag_modusrc = 'MODSource'
   
! --- dmr
! ---- [NOTA] Predefined CF-compliant attributes
! ----        long_name,standard_name,units,axis,calendar,unlimited   
   CHARACTER(LEN=*), PARAMETER :: cp_elttag_lngname = 'Long_Name'   
   CHARACTER(LEN=*), PARAMETER :: cp_elttag_stdname = 'STD_Name'
   CHARACTER(LEN=*), PARAMETER :: cp_elttag_vrunits = 'Units'
   CHARACTER(LEN=*), PARAMETER :: cp_elttag_varaxis = 'Axis'
   CHARACTER(LEN=*), PARAMETER :: cp_elttag_calendr = 'Calendar'
   CHARACTER(LEN=*), PARAMETER :: cp_elttag_unlimit = 'Unlimited'
 
                              
                             
   integer, parameter :: nb_char_varname=6
   
   type out_var
      character(len=str_len)         :: var_type = "Unknown"
      character(len=nb_char_varname) :: var_name = "Noknow"
      logical                        :: monthly  = .false.
      logical                        :: yearly   = .false.
   end type out_var     
 
 
   TYPE xfs_list
     CHARACTER(LEN=str_len)       :: var_name      ! Mandatory
     CHARACTER(LEN=str_len)       :: dep_list      ! Optional
     INTEGER(kind=ip)             :: dep_sze       ! Optional
     INTEGER(kind=ip)             :: s_ize         ! Optionnal
     TYPE(stack_minixml), POINTER :: xml_datastack ! Mandatory
     TYPE(xfs_list), POINTER      :: prev
     TYPE(xfs_list), POINTER      :: next
     TYPE(xfs_list), POINTER      :: dep_x
     TYPE(xfs_list), POINTER      :: dep_y
     TYPE(xfs_list), POINTER      :: dep_z
   END TYPE xfs_list


CONTAINS

function get_undefined_outvar() result(return_var)
    type(out_var) ::  return_var
    return
end function get_undefined_outvar

!-----------------------------------------------------------------------
 FUNCTION XFSL_createRoot() RESULT(xfs_list_root)
!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE(xfs_list), POINTER :: xfs_list_root

NULLIFY(xfs_list_root)
ALLOCATE(xfs_list_root)
NULLIFY(xfs_list_root%prev)

CALL XFSL_initNode(xfs_list_root)

RETURN
!-----------------------------------------------------------------------
END FUNCTION XFSL_createRoot
!-----------------------------------------------------------------------



!-----------------------------------------------------------------------
 FUNCTION XFSL_addNode(xfs_list_node) RESULT(xfs_list_newnode)
!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE(xfs_list), POINTER :: xfs_list_node
TYPE(xfs_list), POINTER :: xfs_list_newnode

IF (ASSOCIATED(xfs_list_node)) THEN
  IF (.NOT. ASSOCIATED(xfs_list_node%next)) THEN
    ALLOCATE(xfs_list_node%next)
    xfs_list_newnode      => xfs_list_node%next
    xfs_list_newnode%prev => xfs_list_node
    CALL XFSL_initNode(xfs_list_newnode)
  ELSE
    WRITE(*,*) '[XFSL_addNode] error: <xfs_list_node> has already a %next node -- Aborting!'
    CALL ABORT()

  ENDIF
ELSE
  WRITE(*,*) '[XFSL_addNode] error: <xfs_list_node> not yet ASSOCIATEd -- Aborting!'
  CALL ABORT()
ENDIF
  
RETURN

!-----------------------------------------------------------------------
END FUNCTION XFSL_addNode
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
 SUBROUTINE XFSL_initNode(xfs_list_node)
!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE(xfs_list), POINTER :: xfs_list_node

IF (ASSOCIATED(xfs_list_node)) THEN
  xfs_list_node%var_name = 'Unknown'
  xfs_list_node%dep_list   = 'Unknown'
  xfs_list_node%dep_sze    = -1
  xfs_list_node%s_ize      = -1
  NULLIFY(xfs_list_node%dep_x)
  NULLIFY(xfs_list_node%dep_y)
  NULLIFY(xfs_list_node%dep_z)
  NULLIFY(xfs_list_node%next)
ELSE
  WRITE(*,*) '[XFSL_initNode] error: <xfs_list_node> not yet ASSOCIATEd -- Aborting!'
  CALL ABORT()
ENDIF

RETURN
  
!-----------------------------------------------------------------------
END SUBROUTINE XFSL_initNode
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
 FUNCTION XFSL_deleteTailNode(xfs_tail_node) RESULT(xfs_newtail_node)
!-----------------------------------------------------------------------

IMPLICIT NONE

TYPE(xfs_list), POINTER :: xfs_tail_node
TYPE(xfs_list), POINTER :: xfs_newtail_node


NULLIFY(xfs_newtail_node)

IF (ASSOCIATED(xfs_tail_node)) THEN

  IF (.NOT. ASSOCIATED(xfs_tail_node%next)) THEN
                                    ! If <xfs_tail_node> is actually a tail node
    CALL XFSL_initNode(xfs_tail_node)       ! re-initialise the node
    
    IF (ASSOCIATED(xfs_tail_node%prev)) THEN
                                    ! If <xfs_tail_node> is not the root node,
      xfs_newtail_node => xfs_tail_node%prev! step back to the previous node
      NULLIFY(xfs_newtail_node%next)
      DEALLOCATE(xfs_tail_node)             ! and deallocate the old tail node.

    ELSE
                                    ! If <xfs_tail_node> is the root node,
      DEALLOCATE(xfs_tail_node)     ! deallocate it. xfs_newtail_node => NULL() already
    ENDIF

    RETURN
    
  ELSE

    WRITE(*,*) '[XFSL_deleteTailNode] error: <xfs_tail_node> is not a tail node -- Aborting!'
    CALL ABORT()

  ENDIF

ELSE

  WRITE(*,*) '[XFSL_deleteTailNode] error: <xfs_tail_node> not yet ASSOCIATEd -- Aborting!'
  CALL ABORT()

ENDIF
  
RETURN

!-----------------------------------------------------------------------
END FUNCTION XFSL_deleteTailNode
!-----------------------------------------------------------------------




!~ !-----------------------------------------------------------------------
!~  SUBROUTINE XFSL_dumpList(xfs_list_node, iunit)
!~ !-----------------------------------------------------------------------

!~ IMPLICIT NONE

!~ TYPE(xfs_list), POINTER :: xfs_list_node
!~ INTEGER, INTENT(IN), OPTIONAL :: iunit
!~ TYPE(xfs_list), POINTER :: xfs_work

!~ IF (ASSOCIATED(xfs_list_node)) THEN

!~   xfs_work => xfs_list_node
  
!~   DO WHILE(ASSOCIATED(xfs_work))

!~     IF (PRESENT(iunit)) THEN
!~       WRITE(iunit, '(" %fname = <", A, ">")') TRIM(xfs_work%fname)
!~       WRITE(iunit, '(" %c_type = <", A, ">")') TRIM(xfs_work%c_type)
!~       WRITE(iunit, '(" %i_order = ", I0)') xfs_work%i_order
!~       WRITE(iunit, '(" %i_system = ", I0)') xfs_work%i_system
!~       IF (ASSOCIATED(xfs_work%xe)) THEN
!~         WRITE(iunit, '(" %xe is associated")')
!~       ELSE
!~         WRITE(iunit, '(" %xe => NULL")')
!~       ENDIF
!~       IF (ASSOCIATED(xfs_work%xe)) THEN
!~         WRITE(iunit, '(" %mx is associated")')
!~       ELSE
!~         WRITE(iunit, '(" %mx => NULL")')
!~       ENDIF
!~       IF (ASSOCIATED(xfs_work%prev)) THEN
!~         WRITE(iunit, '(" %prev is associated")')
!~       ELSE
!~         WRITE(iunit, '(" %prev => NULL")')
!~       ENDIF
!~       IF (ASSOCIATED(xfs_work%next)) THEN
!~         WRITE(iunit, '(" %next is associated")')
!~       ELSE
!~         WRITE(iunit, '(" %next => NULL")')
!~       ENDIF
!~       IF (ASSOCIATED(xfs_work%next)) THEN
!~         WRITE(iunit, '("#")')
!~       ENDIF

!~     ELSE
    
!~       WRITE(*, '(" %fname = <", A, ">")') TRIM(xfs_work%fname)
!~       WRITE(*, '(" %c_type = <", A, ">")') TRIM(xfs_work%c_type)
!~       WRITE(*, '(" %i_order = ", I0)') xfs_work%i_order
!~       WRITE(*, '(" %i_system = ", I0)') xfs_work%i_system
!~       IF (ASSOCIATED(xfs_work%xe)) THEN
!~         WRITE(*, '(" %xe is associated")')
!~       ELSE
!~         WRITE(*, '(" %xe => NULL")')
!~       ENDIF
!~       IF (ASSOCIATED(xfs_work%xe)) THEN
!~         WRITE(*, '(" %mx is associated")')
!~       ELSE
!~         WRITE(*, '(" %mx => NULL")')
!~       ENDIF
!~       IF (ASSOCIATED(xfs_work%prev)) THEN
!~         WRITE(*, '(" %prev is associated")')
!~       ELSE
!~         WRITE(*, '(" %prev => NULL")')
!~       ENDIF
!~       IF (ASSOCIATED(xfs_work%next)) THEN
!~         WRITE(*, '(" %next is associated")')
!~       ELSE
!~         WRITE(*, '(" %next => NULL")')
!~       ENDIF
!~       IF (ASSOCIATED(xfs_work%next)) THEN
!~         WRITE(*, '("#")')
!~       ENDIF
!~     ENDIF

!~     xfs_work => xfs_work%next

!~   ENDDO

!~ ELSE

!~   IF (PRESENT(iunit)) THEN
!~     WRITE(iunit, '(" => NULL ")')
!~   ELSE
!~     WRITE(*, '(" => NULL ")')
!~   ENDIF

!~ ENDIF

!~ RETURN
  
!~ !-----------------------------------------------------------------------
!~ END SUBROUTINE XFSL_dumpList
!~ !-----------------------------------------------------------------------


!===============================================================================
 END MODULE CF_ELEMENTS_MOD
!===============================================================================
