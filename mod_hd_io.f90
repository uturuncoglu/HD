!
!-----------------------------------------------------------------------
!     Module for HD model I/O subroutines
!-----------------------------------------------------------------------
!
      module mod_hd_io
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mod_hd_param
!
      implicit none
      private
!
      public :: read_config 
      public :: read_init 
      public :: hd_out_init
      public :: hd_out
      public :: hd_rst_init
      public :: hd_rst
      public :: hd_ini
!
      contains
!
      subroutine read_config(ifile)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      character(len=*) :: ifile
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      real :: fdum
      real :: fllog1, fllog2, fllog3, fllog4
      real :: fblog1, fblog2, fblog3, fblog4
      character(len=100) :: pname
!
!-----------------------------------------------------------------------
!     Read parameters 
!-----------------------------------------------------------------------
!
      call globinp(ifile, 'IQUE', fdum, pname)
      ique = int(fdum+0.001)
      call globinp(ifile, 'IOUT', fdum, pname)
      iout = int(fdum+0.001)
      call globinp(ifile, 'IBASE', fdum, pname)
      ibase = int(fdum+0.001)
      call globinp(ifile, 'ISREAD', fdum, pname)
      isread = int(fdum+0.001)
      call globinp(ifile, 'ISWRIT', fdum, pname)
      iswrit = int(fdum+0.001)
      call globinp(ifile, 'IOCEAN', fdum, pname)
      iocean = int(fdum+0.001)
      call globinp(ifile, 'ISOLOG', fdum, pname)
      isolog = int(fdum+0.001)
!
      if (isolog == 100) then
        call globinp(ifile, 'FLLOG1', fllog1, pname)
        call globinp(ifile, 'FBLOG1', fblog1, pname)
        jllog1 = int((fllog1-florg)/fscal+1.0001)
        jblog1 = int(1.00001+(fborg-fblog1)/fscal)
        call globinp(ifile, 'FLLOG2', fllog2, pname)
        call globinp(ifile, 'FBLOG2', fblog2, pname)
        jllog2 = int((fllog2-florg)/fscal+1.0001)
        jblog2 = int(1.00001+(fborg-fblog2)/fscal)
        call globinp(ifile, 'FLLOG3', fllog3, pname)
        call globinp(ifile, 'FBLOG3', fblog3, pname)
        jllog3 = int((fllog3-florg)/fscal+1.0001)
        jblog3 = int(1.00001+(fborg-fblog3)/fscal)
        call globinp(ifile, 'FLLOG4', fllog4, pname)
        call globinp(ifile, 'FBLOG4', fblog4, pname)
        jllog4 = int((fllog4-florg)/fscal+1.0001)
        jblog4 = int(1.00001+(fborg-fblog4)/fscal)
      end if
!
      call globinp(ifile, 'JAHR1', fdum, pname)
      jahr1 = int(fdum+0.001)
      call globinp(ifile, 'JAHR2', fdum, pname)
      jahr2 = int(fdum+0.001)
      call globinp(ifile, 'JAHR3', fdum, pname)
      jahr3 = int(fdum+0.001)
      call globinp(ifile, 'NSTEP', fdum, pname)
      nstep = int(fdum+0.001)
      fdum = 0.0
      call globinp(ifile, 'UFAKRU', fdum, pname)
      ufakru = fdum
!
      call globinp(ifile, 'TDNRUN', fdum, dnrun)
      call globinp(ifile, 'TDNBAS', fdum, dnbas)
      call globinp(ifile, 'TDNRES', fdum, dnres)
      call globinp(ifile, 'TDNINI', fdum, dnini)
      call globinp(ifile, 'TDNOUT', fdum, dnout)
      call globinp(ifile, 'TDNPAR', fdum, dnpar)
      call globinp(ifile, 'TDNOLM', fdum, dnolm)
!
      end subroutine read_config
!
      subroutine read_init()
      implicit none
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, ios
      integer :: ihead(8)      
!
!-----------------------------------------------------------------------
!     Allocate variables 
!-----------------------------------------------------------------------
!
      if (.not. allocated(fdir)) allocate(fdir(nl,nb))
      if (.not. allocated(area)) allocate(area(nb))
      if (.not. allocated(alf_k)) allocate(alf_k(nl,nb))
      if (.not. allocated(alf_n)) allocate(alf_n(nl,nb))
      if (.not. allocated(arf_k)) allocate(arf_k(nl,nb))
      if (.not. allocated(arf_n)) allocate(arf_n(nl,nb))
      if (.not. allocated(agf_k)) allocate(agf_k(nl,nb))
      if (.not. allocated(finfl)) allocate(finfl(nl,nb))
      if (.not. allocated(fgmem)) allocate(fgmem(nl,nb))
      if (.not. allocated(hd_lsm)) allocate(hd_lsm(nl,nb))
      if (.not. allocated(frfmem)) allocate(frfmem(nl,nb,nmemrf))
      if (.not. allocated(flfmem)) allocate(flfmem(nl,nb,nmemlf))

!
!-----------------------------------------------------------------------
!     Open parameter file 
!-----------------------------------------------------------------------
!
      open(lupar, file=trim(dnpar),  form='unformatted',                &
           status='old', iostat=ios)
      if (ios /= 0) then
         write(*,*) '[error] -- file '//trim(dnpar)//' not found!'
         stop
      endif
!
!-----------------------------------------------------------------------
!     Read parameter file 
!-----------------------------------------------------------------------
!
      read(lupar) ihead
      read(lupar) hd_lsm 
      read(lupar) ihead
      read(lupar) fdir
!
      read(lupar) ihead
      read(lupar) alf_k
      read(lupar) ihead
      read(lupar) alf_n
!
      read(lupar) ihead
      read(lupar) arf_k
      read(lupar) ihead
      read(lupar) arf_n
!
      read(lupar) ihead
      read(lupar) agf_k
!
      read(lupar) ihead
      read(lupar) area
!
!-----------------------------------------------------------------------
!     Close parameter file 
!-----------------------------------------------------------------------
! 
      close(lupar, iostat=ios)
      if (ios /= 0) then
        write(*,*) '[error] -- '//trim(dnpar)//'is not closed!'
      endif
!
!-----------------------------------------------------------------------
!     Open, read and close restart file 
!-----------------------------------------------------------------------
!
      pstep = 0
      if (isread /= 0) then
        print*, "read hd restart data"
        call hd_ini()
      end if
!
!-----------------------------------------------------------------------
!     Open, read and close ocean-atmosphere land-sea mask file 
!-----------------------------------------------------------------------
!
      if (iocean /= 0) then
        open(lupar, file=trim(dnolm),  form='unformatted',              &
             status='old', iostat=ios)
        if (ios /= 0) then
           write(*,*) '[error] -- file '//trim(dnolm)//' not found!'
           stop
        endif
!
        read(lupar) ihead
        read(lupar) foclsm
!
        close(lupar, iostat=ios)
        if (ios /= 0) then
          write(*,*) '[error] -- '//trim(dnolm)//'is not closed!'
        endif        
      end if
!
      end subroutine read_init
!
      subroutine globinp(ifile, key, value, pname)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      character(len=*), intent(in) :: ifile, key
      character(len=*), intent(inout) :: pname 
      real, intent(inout) :: value
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: ios
!
!-----------------------------------------------------------------------
!     Open configuration file 
!-----------------------------------------------------------------------
!
      open(lu, file=trim(ifile), access='sequential', form='formatted', &
           status='old', iostat=ios) 
      if (ios /= 0) then  
         write(*,*) '[error] -- file '//trim(ifile)//' not found!' 
         stop
      endif
!
!-----------------------------------------------------------------------
!     Read and find the parameter 
!-----------------------------------------------------------------------
!
      ios = 0
      do while (ios == 0)
        read(lu, fmt='(A80)', iostat=ios) pname
        if (index(pname, key) /= 0) then
          if (key(1:1) == 't' .or. key(1:1) == 'T') then
            read(lu, '(A80)') pname
            write(*,*) 'Parameter: '//trim(key)//' = ', trim(pname) 
          else
            read(lu, *) value
            write(*,*) 'Parameter: '//trim(key)//' = ', value
          end if
          exit 
        end if
      end do
!
!-----------------------------------------------------------------------
!     Close configuration file 
!-----------------------------------------------------------------------
!
      close(lu, status='keep', iostat=ios) 
      if (ios /= 0) then 
        write(*,*) '[error] -- '//trim(ifile)//'is not closed!' 
      endif 
      end subroutine globinp
!
      subroutine hd_out_init()
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use netcdf
!
      implicit none 
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i
      real :: lats(nb), lons(nl)
      character(len=100) :: str
!
!-----------------------------------------------------------------------
!     Create netCDF file 
!-----------------------------------------------------------------------
!
      if (.not. allocated(hdout%dimid)) allocate(hdout%dimid(3))
      if (.not. allocated(hdout%varid)) allocate(hdout%varid(4))
!
      call nio_check(nf90_create(trim(dnout), nf90_clobber, hdout%ncid))
!
!-----------------------------------------------------------------------
!     Define dimensions 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_def_dim(hdout%ncid, 'lon',                    &
                                  nl, hdout%dimid(1)))
      call nio_check(nf90_def_dim(hdout%ncid, 'lat',                    &
                                  nb, hdout%dimid(2)))
      call nio_check(nf90_def_dim(hdout%ncid, 'time',                   &
                                  nf90_unlimited, hdout%dimid(3)))
!
!-----------------------------------------------------------------------
!     Define dimension variables 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_def_var(hdout%ncid, 'lon', nf90_real,         &
                     hdout%dimid(1), hdout%varid(1)))
      call nio_check(nf90_put_att(hdout%ncid, hdout%varid(1),           &
                     'long_name', 'Longitude'))
      call nio_check(nf90_put_att(hdout%ncid, hdout%varid(1),           &
                     'units', 'degrees_east'))
!
      call nio_check(nf90_def_var(hdout%ncid, 'lat', nf90_real,         &
                     hdout%dimid(2), hdout%varid(2)))
      call nio_check(nf90_put_att(hdout%ncid, hdout%varid(2),           &
                     'long_name', 'Latitude'))
      call nio_check(nf90_put_att(hdout%ncid, hdout%varid(2),           &
                     'units', 'degrees_north'))
!
      call nio_check(nf90_def_var(hdout%ncid, 'time', nf90_int,         &
                     hdout%dimid(3), hdout%varid(3)))
      call nio_check(nf90_put_att(hdout%ncid, hdout%varid(3),           &
                     'long_name', 'Time'))
      write(str,fmt='("days since ",I4,"-",I2.2,"-",I2.2," 00:00:00")') &
           jahr1, jahr2, jahr3
      call nio_check(nf90_put_att(hdout%ncid, hdout%varid(3),           &
                     'units', str))      
!
!-----------------------------------------------------------------------
!     Define variables 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_def_var(hdout%ncid, 'dis', nf90_real,         &
                     hdout%dimid, hdout%varid(4)))
      call nio_check(nf90_put_att(hdout%ncid, hdout%varid(4),           &
                     'long_name', 'River Discharge'))
      call nio_check(nf90_put_att(hdout%ncid, hdout%varid(4),           &
                     'missing_value', 1.0e20))
!
!-----------------------------------------------------------------------
!     Exit define mode 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_enddef(hdout%ncid))
!
!-----------------------------------------------------------------------
!     Fill coordinate variables 
!-----------------------------------------------------------------------
!
      do i = 1, nl
        lons(i) = -180.0+180.0/nl+(i-1)*umfang/nl
      end do 
      do i = 1, nb
        lats(i) = 90.0-90.0/nb-(i-1)*180.0/nb
      end do
!
      call nio_check(nf90_put_var(hdout%ncid, hdout%varid(1), lons))
      call nio_check(nf90_put_var(hdout%ncid, hdout%varid(2), lats))
!
!-----------------------------------------------------------------------
!     Sync file 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_sync(hdout%ncid))
!
      end subroutine hd_out_init
!
      subroutine hd_rst_init()
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use netcdf
!
      implicit none 
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i
      real :: lats(nb), lons(nl)
      character(len=100) :: str
!
!-----------------------------------------------------------------------
!     Create netCDF file 
!-----------------------------------------------------------------------
!
      if (.not. allocated(hdrst%dimid)) allocate(hdrst%dimid(5))
      if (.not. allocated(hdrst%varid)) allocate(hdrst%varid(9))
!
      call nio_check(nf90_create(trim(dnres), nf90_clobber, hdrst%ncid))
!
!-----------------------------------------------------------------------
!     Define dimensions 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_def_dim(hdrst%ncid, 'rf',                     &
                                  nmemrf, hdrst%dimid(1)))
      call nio_check(nf90_def_dim(hdrst%ncid, 'lf',                     &
                                  nmemlf, hdrst%dimid(2)))
      call nio_check(nf90_def_dim(hdrst%ncid, 'lon',                    &
                                  nl, hdrst%dimid(3)))
      call nio_check(nf90_def_dim(hdrst%ncid, 'lat',                    &
                                  nb, hdrst%dimid(4)))
      call nio_check(nf90_def_dim(hdrst%ncid, 'time',                   &
                                  nf90_unlimited, hdrst%dimid(5)))
!
!-----------------------------------------------------------------------
!     Define dimension variables 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_def_var(hdrst%ncid, 'lon', nf90_real,         &
                     hdrst%dimid(3), hdrst%varid(1)))
      call nio_check(nf90_put_att(hdrst%ncid, hdrst%varid(1),           &
                     'long_name', 'Longitude'))
      call nio_check(nf90_put_att(hdrst%ncid, hdrst%varid(1),           &
                     'units', 'degrees_east'))
!
      call nio_check(nf90_def_var(hdrst%ncid, 'lat', nf90_real,         &
                     hdrst%dimid(4), hdrst%varid(2)))
      call nio_check(nf90_put_att(hdrst%ncid, hdrst%varid(2),           &
                     'long_name', 'Latitude'))
      call nio_check(nf90_put_att(hdrst%ncid, hdrst%varid(2),           &
                     'units', 'degrees_north'))
!
      call nio_check(nf90_def_var(hdrst%ncid, 'time', nf90_int,         &
                     hdrst%dimid(5), hdrst%varid(3)))
      call nio_check(nf90_put_att(hdrst%ncid, hdrst%varid(3),           &
                     'long_name', 'Time'))
      write(str,fmt='("days since ",I4,"-",I2.2,"-",I2.2," 00:00:00")') &
           jahr1, jahr2, jahr3
      call nio_check(nf90_put_att(hdrst%ncid, hdrst%varid(3),           &
                     'units', str))      
!
!-----------------------------------------------------------------------
!     Define variables 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_def_var(hdrst%ncid, 'frfmem', nf90_real,      &
                     (/ hdrst%dimid(3:4), hdrst%dimid(1),               &
                     hdrst%dimid(5) /), hdrst%varid(4)))
      call nio_check(nf90_put_att(hdrst%ncid,                           &
                     hdrst%varid(4), 'long_name',                       &
                     'Intermediate reservoir, inflow cascade'))
      call nio_check(nf90_put_att(hdrst%ncid, hdrst%varid(4),           &
                     'missing_value', 1.0e20))
!
      call nio_check(nf90_def_var(hdrst%ncid, 'flfmem', nf90_real,      &
                     (/ hdrst%dimid(3:4), hdrst%dimid(2),               &
                     hdrst%dimid(5) /), hdrst%varid(5)))
      call nio_check(nf90_put_att(hdrst%ncid,                           &
                     hdrst%varid(5), 'long_name',                       &
                     'Intermediate reservoir, linear overflow'))
      call nio_check(nf90_put_att(hdrst%ncid, hdrst%varid(5),           &
                     'missing_value', 1.0e20))
!
      call nio_check(nf90_def_var(hdrst%ncid, 'fgmem', nf90_real,       &
                     hdrst%dimid(3:5), hdrst%varid(6)))
      call nio_check(nf90_put_att(hdrst%ncid,                           &
                     hdrst%varid(6), 'long_name',                       &
                     'Intermediate linear baseflow reservoir'))
      call nio_check(nf90_put_att(hdrst%ncid, hdrst%varid(6),           &
                     'missing_value', 1.0e20))
!
      call nio_check(nf90_def_var(hdrst%ncid, 'rnof', nf90_real,        &
                     hdrst%dimid(3:5), hdrst%varid(7)))
      call nio_check(nf90_put_att(hdrst%ncid,                           &
                     hdrst%varid(7), 'long_name',                       &
                     'Surface runoff'))
      call nio_check(nf90_put_att(hdrst%ncid, hdrst%varid(7),           &
                     'missing_value', 1.0e20))
!
      call nio_check(nf90_def_var(hdrst%ncid, 'snof', nf90_real,        &
                     hdrst%dimid(3:5), hdrst%varid(8)))
      call nio_check(nf90_put_att(hdrst%ncid,                           &
                     hdrst%varid(8), 'long_name',                       &
                     'Sub-surface runoff'))
      call nio_check(nf90_put_att(hdrst%ncid, hdrst%varid(8),           &
                     'missing_value', 1.0e20))
!
      call nio_check(nf90_def_var(hdrst%ncid, 'finfl', nf90_real,       &
                     hdrst%dimid(3:5), hdrst%varid(9)))               
      call nio_check(nf90_put_att(hdrst%ncid,                           &
                     hdrst%varid(9), 'long_name', 'Inflow data'))
      call nio_check(nf90_put_att(hdrst%ncid, hdrst%varid(9),           &
                     'missing_value', 1.0e20))
!
!-----------------------------------------------------------------------
!     Exit define mode 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_enddef(hdrst%ncid))
!
!-----------------------------------------------------------------------
!     Fill coordinate variables 
!-----------------------------------------------------------------------
!
      do i = 1, nl
        lons(i) = -180.0+180.0/nl+(i-1)*umfang/nl
      end do 
      do i = 1, nb
        lats(i) = 90.0-90.0/nb-(i-1)*180.0/nb
      end do
!
      call nio_check(nf90_put_var(hdrst%ncid, hdrst%varid(1), lons))
      call nio_check(nf90_put_var(hdrst%ncid, hdrst%varid(2), lats))
!
!-----------------------------------------------------------------------
!     Sync file 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_sync(hdrst%ncid))
!
      end subroutine hd_rst_init
!
      subroutine hd_out(istep)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use netcdf
!
      implicit none 
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      integer, intent(in) :: istep
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: len, start(4), count(4)
!
!-----------------------------------------------------------------------
!     Write data to file 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_inquire_dimension(hdout%ncid,                 &
                     hdout%dimid(3), len=len))
!
      start = (/ len+1, 1, 1, 1 /)
      count = (/ 1, 1, 1, 1 /)
      call nio_check(nf90_put_var(hdout%ncid, hdout%varid(3),           &
                    (/ istep-1 /), start, count))
!
      start = (/ 1, 1, len+1, 1 /)
      count = (/ nl, nb, 1, 1 /)
      call nio_check(nf90_put_var(hdout%ncid, hdout%varid(4),           &
                     friv, start, count)) 
!
!-----------------------------------------------------------------------
!     Sync file 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_sync(hdout%ncid))
!
!-----------------------------------------------------------------------
!     Close file 
!-----------------------------------------------------------------------
!
      if (istep == nstep) call nio_check(nf90_close(hdout%ncid))
!
      end subroutine hd_out
!
      subroutine hd_rst(istep)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use netcdf
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      integer, intent(in) :: istep
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i
      integer :: start(4), count(4), len
!
!-----------------------------------------------------------------------
!     Write data to file 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_inquire_dimension(hdrst%ncid,                 &
                     hdrst%dimid(5), len=len))
      start = (/ len+1, 1, 1, 1 /)
      count = (/ 1, 1, 1, 1 /)
      call nio_check(nf90_put_var(hdrst%ncid, hdrst%varid(3),           &
                    (/ istep-1 /), start, count))
!
      do i = 1, nmemrf 
        start = (/ 1, 1, i, len+1 /)
        count = (/ nl, nb, 1, 1 /)
        call nio_check(nf90_put_var(hdrst%ncid, hdrst%varid(4),         &
                       frfmem(:,:,i), start, count))
      end do
!
      do i = 1, nmemlf
        start = (/ 1, 1, i, len+1 /)
        count = (/ nl, nb, 1, 1 /)
        call nio_check(nf90_put_var(hdrst%ncid, hdrst%varid(5),         &
                       flfmem(:,:,i), start, count))
      end do
!
      start = (/ 1, 1, len+1, 1 /)
      count = (/ nl, nb, 1, 1 /)
      call nio_check(nf90_put_var(hdrst%ncid, hdrst%varid(6),           &
                     fgmem, start, count))
!
      call nio_check(nf90_put_var(hdrst%ncid, hdrst%varid(7),           &
                     runoff, start, count))
!
      call nio_check(nf90_put_var(hdrst%ncid, hdrst%varid(8),           &
                     drain, start, count))
!
      call nio_check(nf90_put_var(hdrst%ncid, hdrst%varid(9),           &
                     finfl, start, count))
!
!-----------------------------------------------------------------------
!     Sync file 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_sync(hdrst%ncid))
!
!-----------------------------------------------------------------------
!     Close file 
!-----------------------------------------------------------------------
!
      if (istep == nstep) call nio_check(nf90_close(hdrst%ncid))
!
      end subroutine hd_rst
!
      subroutine hd_ini()
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use netcdf
!
      implicit none
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, ncid, varid, start(4), count(4)
      character(len=100) :: str
!
!-----------------------------------------------------------------------
!     Open netCDF file 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_open(trim(dnini), nf90_nowrite, ncid))
!
!-----------------------------------------------------------------------
!     Read variables 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_inq_varid(ncid, 'frfmem', varid))
      do i = 1, nmemrf
        start = (/ 1, 1, i, 1 /)
        count = (/ nl, nb, 1, 1 /)
        call nio_check(nf90_get_var(ncid, varid, frfmem(:,:,i),         &
                       start=start, count=count))
      end do
!
      call nio_check(nf90_inq_varid(ncid, 'flfmem', varid))
      do i = 1, nmemlf
        start = (/ 1, 1, i, 1 /)
        count = (/ nl, nb, 1, 1 /)
        call nio_check(nf90_get_var(ncid, varid, flfmem(:,:,i),         &
                       start=start, count=count))
      end do
!
      call nio_check(nf90_inq_varid(ncid, 'fgmem', varid))
      call nio_check(nf90_get_var(ncid, varid, fgmem))
!
      call nio_check(nf90_inq_varid(ncid, 'finfl', varid))
      call nio_check(nf90_get_var(ncid, varid, finfl))
!
      call nio_check(nf90_inq_varid(ncid, 'rnof', varid))
      call nio_check(nf90_get_var(ncid, varid, runoff))
!
      call nio_check(nf90_inq_varid(ncid, 'snof', varid))
      call nio_check(nf90_get_var(ncid, varid, drain))
!
      call nio_check(nf90_inq_varid(ncid, 'time', varid))
      call nio_check(nf90_get_var(ncid, varid, pstep))
      pstep = pstep+1
!
!-----------------------------------------------------------------------
!     Close file 
!-----------------------------------------------------------------------
!
      call nio_check(nf90_close(ncid))
!
      end subroutine hd_ini
!
      subroutine nio_check(status)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use netcdf
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      integer, intent(in) :: status
     
      if (status /= nf90_noerr) then
        print*, trim(nf90_strerror(status))
        stop 2
      end if
!
      end subroutine nio_check
!
      end module mod_hd_io
