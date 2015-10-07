      module mod_hd_model
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
      public :: hdmodel
!
      contains
!
      subroutine hdmodel(istep, runoff, drain, fdata, friv)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      integer, intent(in) :: istep
      real, intent(in) :: runoff(:,:)
      real, intent(in) :: drain(:,:)
      real, intent(inout) :: fdata(:,:)
      real, intent(inout) :: friv(:,:)
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: jl, jb, jlnew, jbnew, ib, il, iflow, isub, idum
      real :: finp(nl,nb)
      real :: xresi
!
!-----------------------------------------------------------------------
!     Simulation overlandflow per gridbox
!-----------------------------------------------------------------------
!
      iflow = OVERLANDFLOW 
!
      ! input runoff
      finp = runoff 
      ! apply correction to data -> 0.5deg transformation
      !if (iocean /= 0) then
      !  xresi = 0.0
      !  call hdcorr(istep, runoff, finp)
      !endif
      ! runoff is in m/s --> multiply with area to convert it to m^3/s
      do jb = 1, nb
        do jl = 1, nl
          finp(jl,jb) = finp(jl,jb)*area(jb)
        end do
      end do
      ! global flow simulation
      call kasglob(istep, finp, fdata, flfmem, alf_k, alf_n, iflow)
!
!-----------------------------------------------------------------------
!     Simulation baseflow per gridbox 
!-----------------------------------------------------------------------
!
      if (ibase /= 0) then
        ! input drainage 
        finp = drain 
        ! apply correction to data -> 0.5deg transformation
        !if (iocean /= 0) then
        !  call hdcorr(istep, drain, finp)
        !end if 
        ! drainage is in m/s --> multiply with area to convert it to m^3/s
        ! only for land points
        do jl = 1, nl
          finp(jl,:) = finp(jl,:)*area(:)*hd_lsm(jl,:)
        enddo
        ! linear reservoir - Application to baseflow as done in kasglob
        ! the intermediate content will be used in [m^3/s], in order to
        ! avoid back and forth multiplication with Unit 1 day = 86400 sec.
        fgmem = fgmem+finp
        finp = fgmem/(agf_k+1.0)
        fgmem = fgmem-finp
        finp = fdata+finp
       else
        finp = fdata
      endif
!
!-----------------------------------------------------------------------
!     Computing Riverflow with input finfl from preceeding Sub-time step 
!-----------------------------------------------------------------------
!
      iflow = RIVERFLOW
!
      friv = 0.0
      ! Computation of riverflow in MM Sub (internal) time steps
      ! i.e.  dt = 1/MM days instead of 1 day
      ! Up to now a daily call of routine is forseen: may be changed
      ! to every 6 hours in later applications
      do isub = 1, mm
        call kasglob(istep, finfl, fdata, frfmem, arf_k, arf_n, iflow)
        finfl = 0.0
        do jb = 1, nb
          do jl = 1, nl
            ! relative direction coordinates
            ib = -(int((fdir(jl,jb)-1.0)/3.0+0.001)-1)
            il = int(((fdir(jl,jb)+2.0)/3.0-                            &
                 int((fdir(jl,jb)+2.0)/3.0+0.001))*3.0+0.001)-1
            idum = 1
            if (fdir(jl,jb) <= 0.1) idum = 0
            jlnew = jl+il*idum
            jbnew = jb+ib*idum
            if (jlnew == 0) jlnew = nl
            if (jlnew == nl+1) jlnew = 1
            ! Inflow per Gridbox = Inflow+Overlandf.+Basef.+act.riverf.
            finfl(jlnew, jbnew) = finfl(jlnew, jbnew)+                  &
                                  finp(jl,jb)+fdata(jl,jb)
          end do
        end do        
        friv = friv+finfl
      end do
!
      friv = friv/float(mm)
!
      end subroutine hdmodel
!
      subroutine kasglob(istep, finp, fdata, fmem, a_k, a_n, iflow)
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      integer, intent(in) :: istep
      real, intent(in) :: finp(:,:)
      real, intent(inout) :: fdata(:,:)
      real, intent(inout) :: fmem(:,:,:)
      real, intent(in) :: a_k(:,:), a_n(:,:)
      integer, intent(in) :: iflow
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: j, jb, jl, nn
      real :: amod, akdiv, divmm, fakmm, fdum 
!
!-----------------------------------------------------------------------
!     Transformation of input values of time step II and 
!     temporary storage in array finp
!-----------------------------------------------------------------------
!
      if (iflow == OVERLANDFLOW) THEN
        divmm = 1.0
        fakmm = 1.0
      else if (iflow == RIVERFLOW) THEN
        divmm = 1.0/float(mm)
        fakmm = float(mm)
      end if
!
!-----------------------------------------------------------------------
!     Computing modeled value in each grid point 
!-----------------------------------------------------------------------
!
      do jb = 1, nb
        do jl = 1, nl      
          if (a_n(jl,jb) > 0.5) then
            nn = int(a_n(jl,jb)+0.5)
            amod = a_k(jl,jb)*a_n(jl,jb)/float(nn)
            ! dt = 1 day -> akdiv = 1.0/(amod+1.0)
            akdiv = 1.0/(amod+divmm)
            ! input at time step II
            fdum = finp(jl,jb)*divmm
            ! nash-cascade
            ! it is [amod] = day ==> amod(sec) = amod(day) * 1 day
            ! remember: in principle, it is -> fdum = finp * 1 day
            !           ==> fmem = x * 1 day
            !           ==> fdum = x * 1 day * 1 / amod(sec)
            !                    = x * 1 day / (amod(day) * 1 day)
            !                    = x / amod(day)
            !           ==> fmem = x * 1 day - fdum * 1 day
            !                    = (x - fdum) * 1 day
            ! outflow fdum is computed correctly, Intermediate reservoir 
            ! unit is a volume flow instead of a volume. This is to 
            ! avoid back and forth multiplication with factor 
            ! 1 day = 86400 sec
            do j = 1, nn
              fmem(jl,jb,j) = fmem(jl,jb,j)+fdum
              fdum = fmem(jl,jb,j)*akdiv*divmm
              fmem(jl,jb,j) = fmem(jl,jb,j)-fdum
            end do
            fdata(jl,jb) = fdum*fakmm
          end if
        end do
      end do
!
      end subroutine kasglob
!
      end module mod_hd_model
