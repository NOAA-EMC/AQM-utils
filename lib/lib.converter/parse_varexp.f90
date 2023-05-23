!-----------------------------------------------------------------------------
!
! parse_varexp -- Parse a string containing a subscripted variable reference.
!
! 1.00	2008-jul-01	Original version, undebugged.  By Dave Allured.
!			This version uses a state transition table.
! 1.01	2008-jul-02	Change error handling to simplify for user.
!			All errors print immediate error message and abort.
!			Improve diagnostics.
! 1.02	2008-jul-04	Fix bug at close parentheisis with single subscript.
!			Remove residual traps for testing exceptions.
! 1.03	2008-dec-15	Compatibility fix for XLF 8.1 compiler.
!			XLF can't compile internal read from a char parameter.
! 1.04	2010-dec-16	Fix minor warning with gfortran -Wcharacter-truncation.
!
! This routine parses and validates a string expression of this form.
! The subscript portion after the var name is optional:
!
!     var (sub1, sub2, ... subN)
!
! Allowed characters in the varname and subscripts are letters,
! digits, and underscore.  All names must be legal Fortran names
! beginning with a letter.  Letters may be upper or lower case.
! Case is preserved on output.
!
! Subscripts must be single variable names only, not constants or
! expressions.  Any number of subscripts greater than zero is
! allowed.
!
! Spaces are allowed anywhere in the expression, except within names.
! Tabs are prohibited.
!
!-----------------------------------------------------------------------------

module parse_varexp_mod
contains

subroutine parse_varexp (string, varname, subnames, nsubs)

   implicit none
   
   character(*), intent(in ) :: string		! input expression string
   character(*), intent(out) :: varname		! output variable name
   character(*), intent(out) :: subnames(:)	! output subscript names
   integer,      intent(out) :: nsubs		! number of subscripts found

! Local variables.

   character c*1, detail*80, prefix*2
   
   integer i, j, start, eol, ccond, vlen, slen
   integer state, next_state, err_code
   
   logical err					! error status flag

!---------------------------------------
! State transition table.
!---------------------------------------

! Condition codes for current character.  Columns in transition table.

   integer, parameter :: cblank = 1	! blank or space
   integer, parameter :: calpha = 2	! letters A-Z and a-z
   integer, parameter :: cdigit = 3	! digits 0-9 and underscore
   integer, parameter :: copen  = 4	!  (  open parenthesis
   integer, parameter :: ccomma = 5	!  ,  comma
   integer, parameter :: cclose = 6	!  )  close parenthesis
   integer, parameter :: c_eol  = 7	! end of line (last char + 1)

   integer, parameter :: nconditions = 7	! number of defined conditions

! State definitions.  Rows in transition table.

   integer, parameter :: vwait = 1	! waiting for start of var name
   integer, parameter :: vname = 2	! in middle of var name
   integer, parameter :: pwait = 3	! waiting for open parenthesis
   integer, parameter :: swait = 4	! waiting for start of subscript name
   integer, parameter :: sname = 5	! in middle of subscript name
   integer, parameter :: spost = 6	! after subscript name
   integer, parameter :: done  = 7	! after close paren or valid EOL

   integer, parameter :: nstates = 7		! number of defined states

! Error action definitions.

   integer, parameter :: evbeg = 10	! var name must start with letter
   integer, parameter :: evmis = 11	! missing var name
   integer, parameter :: esbeg = 12	! subscript name must start with letter
   integer, parameter :: esmis = 13	! missing subscript name
   integer, parameter :: eomis = 14	! missing open parenthesis
   integer, parameter :: e2opn = 15	! more than one open parenthesis
   integer, parameter :: ecmis = 16	! missing comma
   integer, parameter :: extra = 17	! extra characters after expression

! Message lookup table for above errors.

   character(*), parameter :: err_messages(8) = (/  &
      '10 Var name must start with a letter      ', &
      '11 Missing var name                       ', &
      '12 Subscript name must start with a letter', &
      '13 Missing subscript name                 ', &
      '14 Missing open parenthesis               ', &
      '15 More than one open parenthesis         ', &
      '16 Missing comma                          ', &
      '17 Extra characters after expression      '  /)

! State transition table.

! Table output is one of two possibilities: Next state, or error code.
! All error codes start with letter "e".
!
! This table is processed in three phases for each character in the
! expression.  This method results in a simple and consistent parsing
! algorithm with minimal exceptions.
!
! 1.  Error check.  Any error code from the state table is
! intercepted here.  All errors halt parsing and return an error.
!
! 2.  Action phase.  Action is triggered at the start and end of each
! variable or subscript name.  This is detected by transition into or
! out of the VNAME and SNAME states.
!
! 3.  State transition.  The current state is changed to the next
! state from the transition table.
!
! One special case is handled outside of the table:
!
! 1.  Invalid characters halt parsing and return an error.

   integer ttable (nstates, nconditions)

!!!!!                     -------- NEW CHARACTER CONDITION CODE ---------
!!!!!   CURRENT STATE     BLANK  ALPHA  DIGIT    (    COMMA    )     EOL

   data ttable(vwait,:) / vwait, vname, evbeg, evmis, evmis, evmis, evmis /
   data ttable(vname,:) / pwait, vname, vname, swait, eomis, eomis, done  /
   data ttable(pwait,:) / pwait, eomis, eomis, swait, eomis, eomis, extra /
   data ttable(swait,:) / swait, sname, esbeg, e2opn, esmis, done,  ecmis /
   data ttable(sname,:) / spost, sname, sname, e2opn, swait, done,  ecmis /
   data ttable(spost,:) / spost, ecmis, ecmis, e2opn, swait, done,  ecmis /
   data ttable(done, :) / extra, extra, extra, extra, extra, extra, done  /

!---------------------------------------
! Parsing loop, single pass.
!---------------------------------------
   
   state = vwait				! initial state = var name wait
   nsubs = 0
   err = .false.
   eol = len_trim (string)			! get non-blank length of string
   
char_loop: &
   do i = 1, eol+1				! scan full expression;
   						! last pass = dummy pass for EOL

      if (i <= eol) c = string (i:i)		! get current character

! Determine the next character condition.
      
      if (i > eol) then				! end of line
         ccond = c_eol
      
      else if (c == ' ') then			! blank, i.e. space
         ccond = cblank
      
      else if ((c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')) then
         ccond = calpha				! letters A-Z and a-z
      
      else if ((c >= '0' .and. c <= '9') .or. c == '_') then
         ccond = cdigit				! digits 0-9 and underscore

      else if (c == '(') then			! open parenthesis
         ccond = copen

      else if (c == ',') then			! comma
         ccond = ccomma

      else if (c == ')') then			! close parenthesis
         ccond = cclose

      else					! all others: illegal character
         ccond = -99
         detail = 'Illegal character'
         err = .true.
         exit char_loop
      end if

! Get next state from the state transition table.

      next_state = ttable(state, ccond)

! Handle error codes from the transition table.

      if (next_state < 1 .or. next_state > nstates) then
      
         do j = 1, size (err_messages)		! find matching error message
            prefix = trim (err_messages(j))	! prefix to var, make XLF happy;
            					! use trim to fix trunc warning
            read (prefix, *) err_code
            if (err_code == next_state) then	! return error and abort
               detail = err_messages(j)(4:)
               err = .true.
               exit char_loop
            end if
         end do
         
         detail = 'Internal error, invalid code from state transition table'
         err = .true.				! unknown code = program error;
         exit char_loop				! force message and abort

      end if

! Action: Start of var name or subscript name.

      if (next_state /= state .and. (next_state == vname &
       .or. next_state == sname)) then		! detect transition to start
          start = i				! simply remember start position
      end if

! Action: End of var name.

      if (next_state /= state .and. state == vname) then    ! detect transition

         vlen = i - start			! check length of var name
         if (vlen > len (varname)) then
            detail = 'Variable name too long'
            err = .true.
            exit char_loop
         end if
            
         varname = string(start:i-1)		! output the var name
      end if

! Action: End of subscript name.

      if (next_state /= state .and. state == sname) then    ! detect transition

         slen = i - start			! check length of subscript name
         if (slen > len (subnames)) then
            detail = 'Subscript name too long'
            err = .true.
            exit char_loop
         end if
            
         nsubs = nsubs + 1
         if (nsubs > size (subnames)) then
            detail = 'Too many subscripts'
            err = .true.
            exit char_loop
         end if
            
         subnames(nsubs) = string(start:i-1)	 ! output the subscript name
      end if

! Check for empty parentheses.

      if (ccond == cclose .and. nsubs < 1) then
         detail = 'Empty parentheses not allowed'
         err = .true.
         exit char_loop
      end if

! Finally, transition to next state.

      state = next_state

   end do char_loop

! Check for invalid final state.

   if ((state /= done) .and. .not. err) then
      detail = 'Internal error, invalid final state'
      err = .true.
   end if

! Report all errors, and abort.

   if (err) then
      print *
      print *, '*** parse_varexp: ', trim (detail), '.'
      print *, '*** input string = [', trim (string), ']'
      write (*, '(a, i0)') ' *** string length =  ', eol
      write (*, '(a, i0)') ' *** char and position = "'// c // '"  ', i
      write (*, '(a, i0, 1x, i0)') ' *** state, ccond = ', state, ccond
      stop 99
   end if

! Otherwise, return normally with all outputs defined.

end subroutine parse_varexp

end module parse_varexp_mod
