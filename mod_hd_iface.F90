      module mod_hd_iface
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_hd_param
      use mod_hd_io
      use mod_hd_model
      use mod_hd_diags
!
      implicit none
      private
!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
      public :: hd_init
      public :: hd_run
      public :: hd_finalize
!
      contains
!
      subroutine hd_init()
      implicit none
!
!-----------------------------------------------------------------------
!     Local variable declarations  
!-----------------------------------------------------------------------
!
      integer :: ios
!
!-----------------------------------------------------------------------
!     Read configuration parameters 
!-----------------------------------------------------------------------
!
      call read_config('hdini.inp')
!
!-----------------------------------------------------------------------
!     Initialize variables 
!     Read parameter, restart and masking files
!-----------------------------------------------------------------------
!
      call read_init()
!
!-----------------------------------------------------------------------
!     Open output files 
!-----------------------------------------------------------------------
!
      call hd_out_init()
!
      if (iswrit /= 0) then
        call hd_rst_init()
      end if
!
!-----------------------------------------------------------------------
!     Open input files 
!-----------------------------------------------------------------------
!
      open(lurun, file=trim(dnrun),  form='unformatted',                &
           status='old', iostat=ios)
      if (ios /= 0) then
        write(*,*) '[error] -- file '//trim(dnrun)//' could not opened!'
        stop
      end if
!
      open(lubas, file=trim(dnbas),  form='unformatted',                &
           status='old', iostat=ios)
      if (ios /= 0) then
        write(*,*) '[error] -- file '//trim(dnbas)//' could not opened!'
        stop
      end if
!
!-----------------------------------------------------------------------
!     Allocate variables 
!-----------------------------------------------------------------------
!
      if (.not. allocated(runoff)) allocate(runoff(nlon,nlat))
      if (.not. allocated(drain)) allocate(drain(nlon,nlat))
      if (.not. allocated(fdata)) allocate(fdata(nl,nb))
      if (.not. allocated(friv)) allocate(friv(nl,nb))
!
      end subroutine hd_init
!
      subroutine hd_run(istart, iend)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      integer, intent(in) :: istart
      integer, intent(in) :: iend
!
!-----------------------------------------------------------------------
!     Local variable declarations  
!-----------------------------------------------------------------------
!
      integer :: i, istep, icount
      integer :: ihead(8)
!
!-----------------------------------------------------------------------
!     Run the model
!-----------------------------------------------------------------------
!
      if (isread /= 0 .and. iswrit /= 0) then
        icount = mod(istart, iswrit)
      else
        icount = 0
      end if
      print*, istart, iend, pstep
!
      do istep = istart, iend
!
!-----------------------------------------------------------------------
!     Get input 
!-----------------------------------------------------------------------
!
#ifdef CPL
      ! initial run
      if (istep == 1) then
        read(lurun) ihead
        read(lurun) runoff
        if (abs(ufakru-1.0) > 1.e-6) runoff = runoff*ufakru
!
        if (ibase /= 0) then
          read(lubas) ihead
          read(lubas) drain
          if (abs(ufakru-1.0) > 1.e-6) drain = drain*ufakru
        end if
      else
        if (ibase == 0) then
          drain = 0.0
        end if
      end if
#else     
      ! information comes from input file
      if (istep == istart .and. istart /= 1) then
        print*, "restarting the model ..."
      else
        read(lurun) ihead
        read(lurun) runoff
        if (abs(ufakru-1.0) > 1.e-6) runoff = runoff*ufakru
!
        if (ibase /= 0) then
          read(lubas) ihead
          read(lubas) drain
          if (abs(ufakru-1.0) > 1.e-6) drain = drain*ufakru
        end if
      end if
#endif
!
!-----------------------------------------------------------------------
!     Write to restart file 
!-----------------------------------------------------------------------
!
      if (iswrit /= 0) then
        icount = icount+1
        if (iswrit == icount) then
          call hd_rst(istep)
          write(*,fmt='(A,I8)') 'restart data are written', istep
          icount = 0
        end if
      end if
!
!-----------------------------------------------------------------------
!     Run the model
!-----------------------------------------------------------------------
!
      call hdmodel(istep, runoff, drain, fdata, friv)
!
!-----------------------------------------------------------------------
!     Print diagnostics 
!-----------------------------------------------------------------------
!
      call hd_diags(istep, friv, isolog)
!
!-----------------------------------------------------------------------
!     Write output to file
!-----------------------------------------------------------------------
!
      call hd_out(istep)
!
      end do
!
      end subroutine hd_run
!
      subroutine hd_finalize()
      implicit none
!
!-----------------------------------------------------------------------
!     Local variable declarations  
!-----------------------------------------------------------------------
!
      integer :: ios
!
!-----------------------------------------------------------------------
!     Close input files
!-----------------------------------------------------------------------
!
      if (ibase /= 0) then
        close(lubas, iostat=ios)
        if (ios /= 0) then
          write(*,*) '[error] -- '//trim(dnbas)//' could not closed!'
        end if
      end if
!
      close(lurun, iostat=ios)
      if (ios /= 0) then
        write(*,*) '[error] -- '//trim(dnrun)//' could not closed!'
      end if
!
      end subroutine hd_finalize
!
      end module mod_hd_iface
