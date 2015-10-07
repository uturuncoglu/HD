      module mod_hd_diags
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
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
      public :: hd_diags
!
      contains
!
      subroutine hd_diags(istep, friv, isolog)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      integer, intent(in) :: istep
      real, intent(in) :: friv(:,:)
      integer, intent(in) :: isolog
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: fl, fb, jl, jb
      real :: f1, f2, f3, f4
!
!-----------------------------------------------------------------------
!     Filling  f1, f2 at chosen coordiantes with
!     OUTFLOW per Gridbox --> finp, not friv or finfl
!-----------------------------------------------------------------------
!
      f1 = 0.0
      f2 = 0.0
      f3 = 0.0
      f4 = 0.0
!
      if (isolog == 1) then
        ! Bothnian Bay: B=65.5 ,L=21.5 .... (glob = (404, 50))
        fl = 21.5
        fb = 65.5
        jl = int((fl-florg)/fscal+1.0001)
        jb = int(1.00001+(fborg-fb)/fscal)
        f1 = friv(jl,  jb  )+friv(jl+1,jb  )+friv(jl+2,jb  )            &
             +friv(jl+3,jb  )+friv(jl+4,jb  )+friv(jl+5,jb  )           &
             +friv(jl+6,jb  )+friv(jl-1,jb+1)+friv(jl+5,jb+1)           &
             +friv(jl-1,jb+2)+friv(jl+1,jb+2)+friv(jl+2,jb+2)           &
             +friv(jl+3,jb+2)+friv(jl+4,jb+2)+friv(jl-2,jb+3)           &
             +friv(jl-1,jb+4)+friv(jl,  jb+4)
        ! Bothnian Sea: B=63.5 ,L=19.0 ....
        f2 = friv(jl-5,jb+4 )+friv(jl-4,jb+4 )+friv(jl-7,jb+5 )         &
             +friv(jl-8,jb+6 )+friv(jl-2,jb+6 )+friv(jl-8,jb+7 )        &
             +friv(jl-2,jb+7 )+friv(jl-1,jb+7 )+friv(jl-9,jb+8 )        &
             +friv(jl-1,jb+8 )+friv(jl-8,jb+9 )+friv(jl-1,jb+9 )        &
             +friv(jl-6,jb+10)+friv(jl-1,jb+10)+friv(jl,  jb+10)        &
             +friv(jl,  jb+11)+friv(jl+1,jb+11)+friv(jl+2,jb+11)
      else if (isolog == 2 .or. isolog == 3) then
        ! Torneaelven-Outflow = (22.0 E, 65.5 N), (22.5 E, 65.5 N)
        !                       (23.5 E, 65.5 N)
        ! regional System:
        fl = 22.0
        fb = 65.5
        jl = int((fl-florg)/fscal+1.0001)
        jb = int(1.00001+(fborg-fb)/fscal)
        f1 = float(istep)
        f2 = friv(jl,jb)+friv(jl+1,jb)+friv(jl+3,jb)
      else if (isolog == 4) then
        ! St.Lawrence-Outflow = (-71.5 W, 47.0 N)
        ! regional System: Measurement station at (-75.5 W, 45.5 N)
        fl = -71.5
        fb =  47.0
        jl = int((fl-florg)/fscal+1.0001)
        jb = int(1.00001+(fborg-fb)/fscal)
        f1 = friv(jl,jb)
        f2 = friv(jl-8,jb+3)
      else if (isolog == 5) then
        ! Paraguay-Outflow = (-59 W, -27 N)
        fl = -59.0
        fb = -27.0
        jl = int((fl-florg)/fscal+1.0001)
        jb = int(1.00001+(fborg-fb)/fscal)
        f1 = float(istep)
        f2 = friv(jl,jb)
      else if (isolog == 6) then
        ! Oder-Outflow = (14.0 E, 54.5 N), Hohensaaten-Finow (14 E, 53W)
        fl = 14.0
        fb = 54.5
        jl = int((fl-florg)/fscal+1.0001)
        jb = int(1.00001+(fborg-fb)/fscal)
        f1 = friv(jl,jb)
        f2 = friv(jl+1,jb+2)+friv(jl+2,jb+2)
      else if (isolog == 7) then
        ! Elbe-Outflow = (8.5 E, 54.5 N), Neu-Darchau (10.5 E, 53.5 N)
        fl = 8.5
        fb = 54.5
        jl = int((fl-florg)/fscal+1.0001)
        jb = int(1.00001+(fborg-fb)/fscal)
        f1 = friv(jl,jb)
        f2 = friv(jl+4,jb+2)
      else if (isolog == 8) then
        ! Oranje-Outflow = (-28.5 S, 16.0 E)
        fl = 16.0
        fb = -28.5
        jl = int((fl-florg)/fscal+1.0001)
        jb = int(1.00001+(fborg-fb)/fscal)
        f1 = friv(jl,jb)
        ! Congo (-6.0 S, 12.0 E)
        fl = 12.0
        fb = -6.0
        jl = int((fl-florg)/fscal+1.0001)
        jb = int(1.00001+(fborg-fb)/fscal)
        f2 = friv(jl,jb)
      else if (isolog == 9) then
        ! Amudarya-Outflow (47) = (43 N, 59 E)
        fl = 59.0
        fb = 43.0
        jl = int((fl-florg)/fscal+1.0001)
        jb = int(1.00001+(fborg-fb)/fscal)
        f1 = friv(jl,jb)
        ! Syrdarya (49) = (46 N, 62 E) 
        fl = 62.0
        fb = 46.0
        jl = int((fl-florg)/fscal+1.0001)
        jb = int(1.00001+(fborg-fb)/fscal)
        f1 = friv(jl,jb)
      else if (isolog == 10) then
        ! Lena-Outflow (40) = (72 N, 127 E)
        fl = 127.0
        fb = 72.0
        jl = int((fl-florg)/fscal+1.0001)
        jb = int(1.00001+(fborg-fb)/fscal)
        f1 = friv(jl,jb)
        ! Ob (46) = (67 N, 71.5 E)
        fl = 71.5
        fb = 67.0
        jl = int((fl-florg)/fscal+1.0001)
        jb = int(1.00001+(fborg-fb)/fscal)
        f1 = friv(jl,jb)
      else if (isolog == 100) then
        ! Custom point discharge
        f1 = friv(jllog1, jblog1)
        f2 = friv(jllog2, jblog2) 
        f3 = friv(jllog3, jblog3) 
        f4 = friv(jllog4, jblog4) 
      end if 
!
      if (isolog /= 0) then
        write(*,fmt='(I10,4F16.4,2E16.6)') istep, f1, f2, f3, f4, runoff(jllog1, jblog1), drain(jllog1, jblog1)
      end if
!
      end subroutine hd_diags
!
      end module mod_hd_diags
