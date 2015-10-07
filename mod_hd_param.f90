      module mod_hd_param

      implicit none
!
!-----------------------------------------------------------------------
!     HD model grid dimensions
!
!     nl: number of longitudes
!     nb: number of latitudes 
!     florg, fborg: north-west corner of gridbox(1,1)
!     fscal: resolution of the model
!-----------------------------------------------------------------------
!
      integer, parameter :: nl = 720
      integer, parameter :: nb = 360 
      real, parameter :: florg = -180.0
      real, parameter :: fborg = 90.0
      real, parameter :: fscal = 0.5
      real, parameter :: umfang = 360.0
!
!-----------------------------------------------------------------------
!     AO model grid dimensions
!
!     nlon: longitudes of global ocean grid
!     nlat: atitudes of global ocean grid
!-----------------------------------------------------------------------
!
      integer, parameter :: nlon = 720
      integer, parameter :: nlat = 360
      real, parameter :: flnp = -90.0
      real, parameter :: fbnp = 25.0
      real, parameter :: dlr = 0.5     
      real, parameter :: dbr = 0.5     
      real, parameter :: oclorg = 0.0
      real, parameter :: ocborg = 90.0
      real, parameter :: ocscal = 2.8125
!
!-----------------------------------------------------------------------
!     HD model I/O unit params
!-----------------------------------------------------------------------
!
      integer, parameter :: lu = 11
      integer, parameter :: lurun = 12
      integer, parameter :: lubas = 13
      integer, parameter :: lures = 14
      integer, parameter :: lupar = 15
!
!-----------------------------------------------------------------------
!     HD model netCDF data type 
!-----------------------------------------------------------------------
!
      type HD_IO
        integer :: ncid
        integer, allocatable :: dimid(:)
        integer, allocatable :: varid(:)
      end type HD_IO
!
      type(HD_IO) :: hdout, hdrst
!
!-----------------------------------------------------------------------
!     HD model config parameters 
!-----------------------------------------------------------------------
!
      real :: ufakru
      logical :: restarted = .false.
      integer :: ique, iout, ibase, isread, iswrit, iocean, isolog
      integer :: jahr1, jahr2, jahr3, nstep, pstep
      integer :: jllog1, jllog2, jllog3, jllog4
      integer :: jblog1, jblog2, jblog3, jblog4
      character(len=80) :: dnrun, dnbas, dnres, dnini, dnout,           &
                           dnpar, dnolm
!
!-----------------------------------------------------------------------
!     HD model parameters 
!-----------------------------------------------------------------------
!      
      ! retention constant k, overflow
      real, allocatable :: alf_k(:,:)
      ! number of reservoirs n, overflow
      real, allocatable :: alf_n(:,:)
      ! retention constant k, riverflow
      real, allocatable :: arf_k(:,:)
      ! number of reservoirs  n, riverflow
      real, allocatable :: arf_n(:,:)
      ! retention constant k, baseflow
      real, allocatable :: agf_k(:,:)
      ! river direction
      real, allocatable :: fdir(:,:)
      ! inflow data
      real, allocatable :: finfl(:,:)
      ! intermediate array of reservoir cascade for the inflows
      real, allocatable :: frfmem(:,:,:)
      ! intermediate array of reservoir for surface runoff
      real, allocatable :: flfmem(:,:,:) 
      ! intermediate linear baseflow reservoir
      real, allocatable :: fgmem(:,:)
      ! area of the grid cells
      real, allocatable :: area(:)
      ! land-sea mask
      real, allocatable :: hd_lsm(:,:)
      ! land-sea mask on ocean model grid
      real, allocatable :: foclsm(:,:) 
!
      integer, parameter :: nmemrf = 5
      integer, parameter :: nmemlf = 1
      integer, parameter :: mm = 4
!
      integer, parameter :: OVERLANDFLOW = 1
      integer, parameter :: RIVERFLOW = 2
!
      real, allocatable :: runoff(:,:), drain(:,:)
      real, allocatable :: fdata(:,:), friv(:,:)

      end module mod_hd_param
