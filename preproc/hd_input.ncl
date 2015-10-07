;-----------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
;-----------------------------------------------------------
; **********************************************************
; *** Ufuk Turuncoglu **************************************
; *** Send bug reports to turuncu@be.itu.edu.tr ************
; **********************************************************
; **********************************************************
; *** Prepares input data for HD model from RegCM output ***
; **********************************************************
;-----------------------------------------------------------
begin
  ;---------------------------------------------------------
  ; Parameters
  ;---------------------------------------------------------

  ;--- input file/s ---
  ROFF_FILE = "mrro_day.nc" 

  ;--- output format ---
  ;oformat = (/ "nc", "srv" /)
  oformat = (/ "srv" /)

  ;--- output endianess ---
  endian = "little"

  ;--- basin id (13: volga, 84: kura, 72: ural) ---
  ;bid = 13
  ;bid = 78
  ;bid = 93

  ;--- fresh run (creates grid description files) ---
  freshrun = True

  ;---------------------------------------------------------
  ; Part 1 - Interpolation (RCM -> HD Model Grid)
  ;          HD Model is fixed in 0.5 x 0.5 deg. resolution
  ;---------------------------------------------------------

  ;--- create coordinate data for HD Model ---
  nlatr = 360
  nlonr = 720
  umfang = 360.0

  lon1d = new((/ nlonr /), "float")
  lat1d = new((/ nlatr /), "float")

  do i = 0, nlonr-1
    lon1d(i) = 0.25+tofloat(i)/tofloat(nlonr)*umfang
  end do
  lon1d = lon1d-180.0
  do i = 0, nlatr-1
    lat1d(i) = 0.25-umfang/4.0+umfang/2.0*tofloat(i)/tofloat(nlatr)
    lat1d(i) = -lat1d(i)
  end do

  if (isatt(lat1d, "_FillValue")) then
    delete(lat1d@_FillValue)
    delete(lon1d@_FillValue)
  end if

  ;--- get coordinate data for RCP ---
  ncrun = addfile(ROFF_FILE, "r")
  lat2d = ncrun->xlat
  lon2d = ncrun->xlon
  time = ncrun->time
  if (isfilevar(ncrun, "mrro")) then
    varr_raw = ncrun->mrro(0,0,:,:)
  end if
  if (isfilevar(ncrun, "smw")) then
    varr_raw = ncrun->smw(0,0,:,:)
  end if
  
  ;--- create SCRIP definition of source grid ---
  sfile = "src_rcm.nc"
  if (.not. isfilepresent(sfile) .or. freshrun) then
    print("[debug] -- creating source grid '"+sfile+"'.")
    opt = True
    opt@ForceOverwrite = True
    opt@PrintTimings = True
    opt@Title = "RegCM4 grid"
    curvilinear_to_SCRIP(sfile, lat2d, lon2d, opt)
    delete(opt)
  end if

  ;--- create SCRIP definition of destination grid ---
  dfile = "dst_hd.nc"
  if (.not. isfilepresent(dfile) .or. freshrun) then
    print("[debug] -- creating destination grid '"+dfile+"'.")
    opt = True
    opt@ForceOverwrite = True
    opt@PrintTimings = True
    opt@Title = "HD Model 0.5x0.5 degree grid"
    rectilinear_to_SCRIP(dfile, lat1d, lon1d, opt)
    delete(opt)
  end if    

  ;--- create weight matrix file ---
  wfile = "wgt_rcm_to_hd.nc"
  if (.not. isfilepresent(wfile) .or. freshrun) then
    print("[debug] -- creating weight file '"+wfile+"'.")
    opt = True
    opt@ForceOverwrite = True
    opt@PrintTimings = True
    opt@SrcRegional = True
    opt@DstRegional = True
    opt@InterpMethod = "bilinear"
    ;opt@InterpMethod = "conserve"
    ESMF_regrid_gen_weights(sfile, dfile, wfile, opt)
  end if

  ;---------------------------------------------------------
  ; Part 2 - Output data in NetCDF and SRV format 
  ;---------------------------------------------------------

  do i = 0, dimsizes(oformat)-1
    print("[debug] -- creating output in >"+oformat(i)+"< format")

    ;--- write to a netcdf file ---
    if (oformat(i) .eq. "nc") then
      fname = "output.nc"
      system("/bin/rm -f "+fname)
      fout = addfile(fname, "c")

      ;--- define mode on ---
      setfileoption(fout, "DefineMode", True)

      ;--- add global attributes ---
      attr = True
      attr@title = "RCM data interpolated to HD model grid"
      attr@source_file = "rcm_input.ncl"
      attr@creation_date = systemfunc ("date")
      fileattdef(fout, attr)
      delete(attr)

      ;--- define dimensions ---
      dimNames = (/ "lat", "lon", "time" /)
      dimSizes = (/ dimsizes(lat1d), dimsizes(lon1d), 1 /)
      dimUnlim = (/ False, False, True /)
      filedimdef(fout, dimNames, dimSizes, dimUnlim)

      filevardef(fout, "lat", typeof(lat1d), "lat")
      filevardef(fout, "lon", typeof(lon1d), "lon")
      filevardef(fout, "time", typeof(time), "time")
      filevardef(fout, "runoff", "double", (/ "time", "lat", "lon" /))

      ;--- add attributes ---
      attr = True
      attr@long_name = "Time"
      attr@units = time@units 
      attr@calendar = time@calendar
      filevarattdef(fout, "time", attr)
      delete(attr)

      attr = True
      attr@long_name = "Longitude"
      attr@units = "degrees_east"
      filevarattdef(fout, "lon", attr)
      delete(attr)

      attr = True
      attr@long_name = "Latitude"
      attr@units = "degrees_north"
      filevarattdef(fout, "lat", attr)
      delete(attr)

      attr = True
      attr@long_name = varr_raw@long_name 
      attr@units = varr_raw@units
      filevarattdef(fout, "runoff", attr)
      delete(attr)

      ;--- exit file definition mode ---
      setfileoption(fout, "DefineMode", False)

      ;--- fill data ---
      fout->lat = (/ lat1d /)
      fout->lon = (/ lon1d /)
      fout->time = (/ time /)

      ;--- interpolate and fill data ---
      opt = True
      opt@PrintTimings = False

      do j = 0, dimsizes(time)-1
        print("[debug] -- time step >> "+j)

        ;--- interpolation ---
        varr = ESMF_regrid_with_weights(ncrun->runoff(j,:,:), wfile, opt)            

        ;--- unit conversion (mm/day to m/s) and assign zero to masked regions ---
        varr = varr*(1.0/86400.0*1000.0)
        varr = where(ismissing(varr), 0.0, varr)
        if (isatt(varr, "_FillValue")) then
          delete(varr@_FillValue)
        end if

        ;--- fill data --- 
        fout->runoff(j,:,:) = (/ varr /)
      end do
    end if

    ;--- write to a srv file ---
    if (oformat(i) .eq. "srv") then
      ;--- get time ---
      date = cd_calendar(time, -2)

      ;--- create file --- 
      fname = "runoff.srv"
      system("/bin/rm -f "+fname)
      if (endian .ne. "little") then
        setfileoption("bin", "WriteByteOrder", "BigEndian")
      end if

      do j = 0, dimsizes(date)-1
        print("[debug] -- time step >> "+date(j)+" "+j)

        ihead = (/ 501, 0, date(j), 0, 720, 360, 98, 0 /)
        fbinrecwrite(fname, -1, ihead)

        ;--- interpolation ---
        opt = True
        opt@PrintTimings = False
        if (isfilevar(ncrun, "mrro")) then
          roff = ncrun->mrro(j,0,:,:)
        end if
        if (isfilevar(ncrun, "smw")) then
          roff = ncrun->smw(j,0,:,:)
          roff = roff/600.0
        end if
        varr = ESMF_regrid_with_weights(roff, wfile, opt)
        varr = mask(varr, varr .lt. 0.0, False)
        varr = where(ismissing(varr), 0.0, varr)

        ;--- unit conversion (mm/day to m/s) and assign zero to masked regions ---
        ;varr = varr*(1.0/(86400.0*1000.0))
        ;--- unit conversion (mm/s to m/s) and assign zero to masked regions ---
        varr = varr*1.0e-3

        ;--- write to file ---
        fbinrecwrite(fname, -1, tofloat(varr))
      end do

      ;--- create drainage file (all zeros) ---
      fname = "drainage.srv"
      system("/bin/rm -f "+fname)

      do j = 0, dimsizes(date)-1
        print("[debug] -- time step >> "+date(j)+" "+j)

        ihead = (/ 502, 0, date(j), 0, 720, 360, 98, 0 /)
        fbinrecwrite(fname, -1, ihead)      

        ;--- interpolation ---
        opt = True
        opt@PrintTimings = False
        if (isfilevar(ncrun, "mrro")) then
          roff = ncrun->mrro(j,1,:,:)
        end if
        if (isfilevar(ncrun, "smw")) then
          roff = ncrun->smw(j,1,:,:)
          roff = roff/600.0
        end if
        varr = ESMF_regrid_with_weights(roff, wfile, opt)
        varr = mask(varr, varr .lt. 0.0, False)
        varr = where(ismissing(varr), 0.0, varr)

        ;--- unit conversion (mm/day to m/s) and assign zero to masked regions ---
        ;varr = varr*(1.0/(86400.0*1000.0))
        ;--- unit conversion (mm/s to m/s) and assign zero to masked regions ---
        varr = varr*1.0e-3

        ;--- write to file ---
        fbinrecwrite(fname, -1, tofloat(varr))
      end do

    end if
  end do
end
