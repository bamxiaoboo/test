#include <misc.h>
#include <params.h>

module inidat

    use shr_kind_mod, only: r8 => shr_kind_r8
    use chemistry, only: chem_init_mix

    implicit none

    real(r8), allocatable :: ps_tmp(:,:)
    real(r8), allocatable :: u3_tmp(:,:,:)
    real(r8), allocatable :: v3_tmp(:,:,:)
    real(r8), allocatable :: t3_tmp(:,:,:)
    real(r8), allocatable :: q3_tmp(:,:,:,:)
    real(r8), allocatable :: qcwat_tmp(:,:,:)
    real(r8), allocatable :: lcwat_tmp(:,:,:)
    real(r8), allocatable :: phis_tmp(:,:)
    real(r8), allocatable :: landfrac_tmp(:,:)
    real(r8), allocatable :: landm_tmp(:,:)
    real(r8), allocatable :: sgh_tmp(:,:)
    real(r8), allocatable :: ts_tmp(:,:)
    real(r8), allocatable :: tsice_tmp(:,:)
    real(r8), allocatable :: tssub_tmp(:,:,:)
    real(r8), allocatable :: sicthk_tmp(:,:)
    real(r8), allocatable :: snowhice_tmp(:,:)

contains

    subroutine read_inidat
    !-----------------------------------------------------------------------
    !
    ! Purpose:
    ! Read initial dataset and spectrally truncate as appropriate.
    !
    !-----------------------------------------------------------------------

        use pmgrid
        use rgrid, only: nlon
        use comsrf, only: plevmx, srfflx_state
        use constituents, only: pcnst, pnats, cnst_name, qmin
        use tracers, only: nusr_adv, nusr_nad, ixuadv, ixunad, ixcldw

        include 'netcdf.inc'

#include <comctl.h>
#include <comhyb.h>
#include <comlun.h>
#include <perturb.h>

        integer i, j, k, m, lat
        real(r8) pertval          ! perturbation value

        integer lonsiz, latsiz, levsiz       ! Dimension sizes
        integer londimid, levdimid, latdimid ! Dimension ID's
        integer uid, vid, tid, qid           ! Variable ID's
        integer tracid(pcnst+pnats)          ! Variable ID's
        integer phisid, sghid, psid          ! Variable ID's
        integer landmid
#ifndef COUP_CSM
        integer tsid, ts1id, ts2id, ts3id, ts4id ,tsiceid! Variable ID's
#endif
#ifdef COUP_SOM
        integer sicid
#endif
        integer snowhiceid        ! Variable ID's
        integer landfracid        ! Variable ID's

        integer strt2d(3)         ! start lon, lat, time indices for netcdf 2-d
        integer strt3d(4)         ! start lon, lev, lat, time for netcdf 3-d
        data strt2d/3*1/          ! Only index 2 will ever change
        data strt3d/4*1/          ! Only indices 2,3 will ever change

        integer cnt2d(3)          ! lon, lat, time counts for netcdf 2-d
        integer cnt3d(4)          ! lon, lat, lev, time counts for netcdf 2-d
        data cnt2d/plon,1,1/      ! 2-d arrs: Always grab only a "plon" slice
        data cnt3d/plon,plev,plat,1/ ! 3-d arrs: Always grab a full time slice

        integer ndims2d           ! number of dimensions
        integer dims2d(NF_MAX_VAR_DIMS) ! variable shape
        integer ndims3d           ! number of dimensions
        integer dims3d(NF_MAX_VAR_DIMS) ! variable shape
        integer tmptype
        integer natt, ret, attlen ! netcdf return values
        real(r8) arr3d(plon,plev,plat)
        character(NF_MAX_NAME) tmpname
        character(256) text
        character(80) trunits      ! tracer untis

        real(r8) ply, wk5,wk6
        integer  kpp

        allocate(ps_tmp(plond,plat))
        allocate(u3_tmp(plond,plev,plat))
        allocate(v3_tmp(plond,plev,plat))
        allocate(t3_tmp(plond,plev,plat))
        allocate(q3_tmp(plond,plev,pcnst+pnats,plat))
        allocate(qcwat_tmp(plond,plev,plat))
        allocate(lcwat_tmp(plond,plev,plat))
        allocate(phis_tmp(plond,plat))
        allocate(landm_tmp(plond,plat))
        allocate(sgh_tmp(plond,plat))
        allocate(ts_tmp(plond,plat))
        allocate(tsice_tmp(plond,plat))
        allocate(tssub_tmp(plond,plevmx,plat))
        allocate(sicthk_tmp(plond,plat))
        allocate(snowhice_tmp(plond,plat))
        allocate(landfrac_tmp(plond,plat))

        if (masterproc) then
            call wrap_inq_dimid(ncid_ini, 'lat', latdimid)
            call wrap_inq_dimlen(ncid_ini, latdimid, latsiz)
            call wrap_inq_dimid(ncid_ini, 'lev', levdimid)
            call wrap_inq_dimlen(ncid_ini, levdimid, levsiz)
            call wrap_inq_dimid(ncid_ini, 'lon', londimid)
            call wrap_inq_dimlen(ncid_ini, londimid, lonsiz)
!
! Get variable id's
! Check that all tracer units are in mass mixing ratios
!
            call wrap_inq_varid(ncid_ini, 'U'   , uid)
            call wrap_inq_varid(ncid_ini, 'V'   , vid)
            call wrap_inq_varid(ncid_ini, 'T'   , tid)
            call wrap_inq_varid(ncid_ini, 'Q'   , qid)
            call wrap_inq_varid(ncid_ini, 'PS'  , psid)
            call wrap_inq_varid(ncid_ini, 'PHIS', phisid)
            call wrap_inq_varid(ncid_ini, 'SGH' , sghid)
            call wrap_inq_varid(ncid_ini, 'LANDM', landmid)

#ifndef COUP_CSM
            call wrap_inq_varid(ncid_ini, 'LANDFRAC', landfracid)
            call wrap_inq_varid(ncid_ini, 'TS', tsid)
            call wrap_inq_varid(ncid_ini, 'TSICE', tsiceid)
            call wrap_inq_varid(ncid_ini, 'TS1', ts1id)
            call wrap_inq_varid(ncid_ini, 'TS2', ts2id)
            call wrap_inq_varid(ncid_ini, 'TS3', ts3id)
            call wrap_inq_varid(ncid_ini, 'TS4', ts4id)
            call wrap_inq_varid(ncid_ini, 'SNOWHICE', snowhiceid)
#ifdef COUP_SOM
            call wrap_inq_varid (ncid_ini, 'SICTHK', sicid)
#endif
#endif
            if (readtrace) then
               do m = 2, 2   !sxj--only read cloud water    !  sxj
                    call wrap_inq_varid(ncid_ini, "CWAT", tracid(m))
                    call wrap_get_att_text(ncid_ini,tracid(m),'units', trunits)
                    if (trunits(1:5) /= "kg/kg") then
                        write(6, *) "[Error]: inidat: Tracer units for tracer """, &
                            trim(cnst_name(m)), """ must be in ""kg/kg""."
                        call endrun
                    end if
                end do
            end if
!
! Check dimension ordering for one 2-d and one 3-d field.
! Assume other arrays of like rank will have dimensions ordered the same.
!
            call wrap_inq_var(ncid_ini, uid, tmpname, tmptype, ndims3d, dims3d, natt)
            if (dims3d(1) /= londimid .or. dims3d(2) /= levdimid .or. &
                dims3d(3) /= latdimid .or. ndims3d /= 4) then
                write(6, *) "[Error]: inidat: Bad number of dims or ordering on 3D field"
                call endrun
            end if
            call wrap_inq_var(ncid_ini, psid, tmpname, tmptype, ndims2d, dims2d, natt)
            if (dims2d(1) /= londimid .or. dims2d(2) /= latdimid .or. ndims2d > 3) then
                write(6, *) "[Error]: inidat: Bad number of dims or ordering on 2D field"
                call endrun
            end if
!
! Read in 2d fields.
! For stand alone run: get surface temp and 4 (sub)surface temp fields
!
         do j=1,plat
            strt2d(2) = j
            if (ideal_phys .or. aqua_planet) then
             do i=1,nlon(j)
                phis_tmp(i,j) = 0.
                sgh_tmp (i,j) = 0.
             end do
            else
               call wrap_get_vara_realx (ncid_ini, phisid, strt2d, cnt2d, phis_tmp(1,j))
               call wrap_get_vara_realx (ncid_ini, sghid , strt2d, cnt2d, sgh_tmp(1,j))
            end if
            call wrap_get_vara_realx(ncid_ini, landmid, strt2d, cnt2d, landm_tmp(1,j))
            call wrap_get_vara_realx(ncid_ini, psid, strt2d, cnt2d, ps_tmp(1,j))
#ifndef COUP_CSM
            if (aqua_planet) then
               do i=1,nlon(j)
                  landfrac_tmp(i,j) = 0.
               end do
            else
               call wrap_get_vara_realx (ncid_ini, landfracid, strt2d, cnt2d, landfrac_tmp(1,j))
            endif
            call wrap_get_vara_realx (ncid_ini, tsid, strt2d, cnt2d, ts_tmp(1,j))
            call wrap_get_vara_realx (ncid_ini, tsiceid, strt2d, cnt2d, tsice_tmp(1,j))
            call wrap_get_vara_realx (ncid_ini, ts1id, strt2d, cnt2d, tssub_tmp(1,1,j))
            call wrap_get_vara_realx (ncid_ini, ts2id, strt2d, cnt2d, tssub_tmp(1,2,j))
            call wrap_get_vara_realx (ncid_ini, ts3id, strt2d, cnt2d, tssub_tmp(1,3,j))
            call wrap_get_vara_realx (ncid_ini, ts4id, strt2d, cnt2d, tssub_tmp(1,4,j))
!
! Set sea-ice thickness and snow cover:
!
#ifdef COUP_SOM
            call wrap_get_vara_realx(ncid_ini, sicid, strt2d, cnt2d, sicthk_tmp(1,j))
#endif
            call wrap_get_vara_realx(ncid_ini, snowhiceid, strt2d, cnt2d, snowhice_tmp(1,j))
#endif
         end do
!
! Read in 3d fields.
! Copies are done instead of reading directly into
! prognostic arrays to address netcdf slowness on Cray.
! Array syntax would be really nice here.
! Initialize tracers if not read in from input data.
! Initialize all user tracers (advected and non-advectec to 0.)
!
         call wrap_get_vara_realx(ncid_ini, uid, strt3d, cnt3d, arr3d)
         u3_tmp(:plon,:plev,:plat) = arr3d(:plon,:plev,:plat)

         call wrap_get_vara_realx(ncid_ini, vid, strt3d, cnt3d, arr3d)
         v3_tmp(:plon,:plev,:plat) = arr3d(:plon,:plev,:plat)

         call wrap_get_vara_realx(ncid_ini, tid, strt3d, cnt3d, arr3d)
         t3_tmp(:plon,:plev,:plat) = arr3d(:plon,:plev,:plat)

!         if (ideal_phys) then
!            do k=1,plev
!               PLY = (PS0-pmtop)*SIGL(K) + PMTOP
!               WK5 = PLY*0.01/DPALIB
!               KPP = INT(WK5)
!               WK6 = WK5-DFLOAT(KPP)
!               t3_tmp(:plon,k,:plat)=(1.D0-WK6)*TBB(KPP)+WK6*TBB(KPP+1)
!            enddo
!
!             do j=1,plat
!               do k=1,plev
!                 do i=1,plon
!                    t3_tmp(i,k,j)= t3_tmp(i,k,j)+ 1.0-ABS(real(j-plat/2)/plat)
!                 enddo
!               enddo
!             enddo
!         endif

         call wrap_get_vara_realx(ncid_ini, qid, strt3d, cnt3d, arr3d)
         q3_tmp(:plon,:plev,1,:plat) = arr3d(:plon,:plev,:plat)

! sxj---if RK readtrace CWAT ( 3 testtrace not read)
! 2008-11-10 if MG only read CLDLIQ
         if (readtrace) then
            !do m=2,pcnst+pnats
            do m=2,2
			    write(6,*) "sxj--tracid(m)",m
               call wrap_get_vara_realx(ncid_ini, tracid(m), strt3d, cnt3d, arr3d)
               q3_tmp(:plon,:plev,m,:plat) = arr3d(:plon,:plev,:plat)
            end do
			do m=3,pcnst+pnats ! 3~5 CLDICE NUMLIQ NUMICE not read
			   q3_tmp(:plon,:plev,m,:plat) = 0.
			enddo
         else
            do m=2,pcnst+pnats
               q3_tmp(:plon,:plev,m,:plat) = 0.
            end do
         endif
!
! Add random perturbation to temperature if required
!
         if (pertlim.ne.0.0) then
            write(6,*)'INIDAT: Adding random perturbation bounded by +/-', &
                      pertlim,' to initial temperature field'
            do lat=1,plat
               do k=1,plev
                  do i=1,nlon(lat)
                     call random_number (pertval)
                     pertval = 2.*pertlim*(0.5 - pertval)
                     t3_tmp(i,k,lat) = t3_tmp(i,k,lat)*(1. + pertval)
                  end do
               end do
            end do
         endif
!
!
!
! Initialize tracers if not read in from input data.
! Initialize all user tracers (advected and non-advectec to 0.)
! Ensure sufficient constituent concentration at all gridpoints
! The following appears here for consistency with the SLD branch (mvertens).
!
         if (.not. readtrace) then
            do lat=1,plat
               q3_tmp(:plon,:plev,ixcldw,lat) = 0.
               if (nusr_adv .gt. 0) then
                  do m = ixuadv,ixuadv+nusr_adv-1
                     do k=1,plev
                        do i=1,nlon(lat)
                           q3_tmp(i,k,m,lat) = q3_tmp(i,k,1,lat)*10.**(m-ixuadv)
                        end do
                     end do
                  end do
               endif
               if (nusr_nad .gt. 0) then
                  do m = ixunad,ixunad+nusr_nad-1
                     do k=1,plev
                        do i=1,nlon(lat)
                           q3_tmp(i,k,m,lat) = q3_tmp(i,k,1,lat)*10.**(m-ixunad)
                        end do
                     end do
                  end do
               end if
               if (trace_gas) then
                  if (doRamp_ghg ) call ramp_ghg
                  call chem_init_mix(lat, ps_tmp(1,lat), q3_tmp(1,1,1,lat), nlon(lat))
               endif
               if (trace_test1 .or. trace_test2 .or. trace_test3) then
                  call initesttr( q3_tmp(1,1,1,lat),nlon(lat) )
               endif
            end do
         endif

         do lat=1,plat
            call qneg3('INIDAT  ',lat   ,nlon(lat),plond   ,plev    , &
                       pcnst+pnats,qmin ,q3_tmp(1,1,1,lat))
         end do


!
      endif                     ! end of if-masterproc

!!      write(6,*) 'all variables read'
!
!-----------------------------------------------------------------------
! Copy temporary arrays to model arrays
!-----------------------------------------------------------------------
!
!!      write(6,*) 'inidat copyint...'
      call copy_inidat
!
!-----------------------------------------------------------------------
! Deallocate memory for temporary arrays
!-----------------------------------------------------------------------
!
      deallocate ( ps_tmp )
      deallocate ( u3_tmp )
      deallocate ( v3_tmp )
      deallocate ( t3_tmp )
      deallocate ( q3_tmp )
      deallocate ( qcwat_tmp )
      deallocate ( lcwat_tmp )
      deallocate ( phis_tmp )
      deallocate ( landfrac_tmp )
      deallocate ( landm_tmp )
      deallocate ( sgh_tmp )
      deallocate ( ts_tmp )
      deallocate ( tsice_tmp )
      deallocate ( tssub_tmp )
!!    deallocate ( dpsl_tmp )
!!    deallocate ( dpsm_tmp )
!!    deallocate ( vort_tmp )
!!    deallocate ( div_tmp )
      deallocate ( sicthk_tmp )
      deallocate ( snowhice_tmp )
!
      return
   end subroutine read_inidat

!*********************************************************************C

   subroutine copy_inidat
!-----------------------------------------------------------------------
! Copy temporary arrays to model arrays
! note that the use statements below contain the definitions
! of the model arrays
!-----------------------------------------------------------------------
      use pmgrid
      use prognostics
      use buffer
      use comsrf
      use phys_grid
      use tracers, only: ixcldw
#if ( defined SPMD )
      use mpishorthand
      use spmd_dyn, only: npes, compute_gsfactors
#endif
!------------------------------Commons----------------------------------
!!#include <comqfl.h>
!
! Local workspace
!
      real(r8), allocatable :: tmpchunk3d(:,:,:)
      real(r8), allocatable :: tmpchunk(:,:)
      integer, parameter :: iend = i1+plon-1
      integer, parameter :: jend = j1+plat-1
      integer begj
      integer n,i,j

#if ( defined SPMD )
      integer :: numperlat         ! number of values per latitude band
      integer :: numsend(0:npes-1) ! number of items to be sent
      integer :: numrecv           ! number of items to be received
      integer :: displs(0:npes-1)  ! displacement array
#endif
!-----------------------------------------------------------------------
#ifdef HADVTEST
!
!JR Overwrite fields for no mountains solid-body rotation
!
      call hadvtest_init
#endif

      begj = beglatex + numbnd

!PW Dynamics fields
#if ( defined SPMD )
      numperlat = plond
      call compute_gsfactors (numperlat, numrecv, numsend, displs)

      call mpiscatterv (ps_tmp,     numsend, displs, mpir8, ps(1,beglat,1),   numrecv, mpir8, 0, mpicom)
      call mpiscatterv (phis_tmp,   numsend, displs, mpir8, phis(1,beglat),   numrecv, mpir8, 0, mpicom)

      numperlat = plndlv
      call compute_gsfactors (numperlat, numrecv, numsend, displs)

      call mpiscatterv (u3_tmp,   numsend, displs, mpir8, u3(i1,1,begj,1),    numrecv, mpir8, 0, mpicom)
      call mpiscatterv (v3_tmp,   numsend, displs, mpir8, v3(i1,1,begj,1),    numrecv, mpir8, 0, mpicom)
      call mpiscatterv (t3_tmp,   numsend, displs, mpir8, t3(i1,1,begj,1),    numrecv, mpir8, 0, mpicom)

      numperlat = plndlv*(pcnst+pnats)
      call compute_gsfactors (numperlat, numrecv, numsend, displs)

      call mpiscatterv (q3_tmp, numsend, displs, mpir8, q3(i1,1,1,begj,1), numrecv, mpir8, 0, mpicom)

#else

      u3(i1:iend,:,j1:jend,1) = u3_tmp(:plon,:,:)
      v3(i1:iend,:,j1:jend,1) = v3_tmp(:plon,:,:)
      t3(i1:iend,:,j1:jend,1) = t3_tmp(:plon,:,:)
      q3(i1:iend,:,:,j1:jend,1) = q3_tmp(:plon,:,:,:)

      ps(:plon,:,1)    = ps_tmp(:plon,:)
      phis(:plon,:)    = phis_tmp(:plon,:)

!-----------------------------------------------------------------------
! set the periodic boundary conditions
!-----------------------------------------------------------------------

              u3(1,:,:,1) = u3_tmp(plon,:,:)
              v3(1,:,:,1) = v3_tmp(plon,:,:)
              t3(1,:,:,1) = t3_tmp(plon,:,:)
              q3(1,:,:,:,1) = q3_tmp(plon,:,:,:)
              u3(plond,:,:,1) = u3_tmp(1,:,:)
              v3(plond,:,:,1) = v3_tmp(1,:,:)
              t3(plond,:,:,1) = t3_tmp(1,:,:)
              q3(plond,:,:,:,1) = q3_tmp(1,:,:,:)

              ps(plon+1,:,1) = ps_tmp(1,:)
              ps(plond ,:,1) = ps_tmp(2,:)
              phis(plon+1,:) = phis_tmp(1,:)
              phis(plond ,:) = phis_tmp(2,:)
#endif

!!      write(6,*) 'prognostics ok'

      allocate ( tmpchunk(pcols,begchunk:endchunk) )
      allocate ( tmpchunk3d(pcols,plevmx,begchunk:endchunk) )

!!      write(6,*) 'tmpchunks allocated'

!PW Physics fields
      call scatter_field_to_chunk(1,1,1,plond,landfrac_tmp,landfrac(1,begchunk))

!!      write(6,*) 'landfrac scattered'

      call scatter_field_to_chunk(1,1,1,plond,landm_tmp,landm(1,begchunk))
      call scatter_field_to_chunk(1,1,1,plond,sgh_tmp,sgh(1,begchunk))
      call scatter_field_to_chunk(1,1,1,plond,tsice_tmp,tsice(1,begchunk))
      call scatter_field_to_chunk(1,1,1,plond,ts_tmp,tmpchunk)
      do i =begchunk,endchunk
         srfflx_state2d(i)%ts(:) = tmpchunk(:,i)
      end do

!!      write(6,*) 'landfrac, landm, sgh, tsice, ts  : scattered'

#ifdef COUP_SOM
      call scatter_field_to_chunk(1,1,1,plond,sicthk_tmp,sicthk(1,begchunk))
#endif
      call scatter_field_to_chunk(1,1,1,plond,snowhice_tmp,snowhice(1,begchunk))

      call scatter_field_to_chunk(1,plevmx,1,plond,tssub_tmp,tmpchunk3d)
      do i =begchunk,endchunk
         surface_state2d(i)%tssub(:,:) = tmpchunk3d(:,:,i)
      end do
!
!JR cloud and cloud water initialization.  Does this belong somewhere else?
!
      if (masterproc) then
         qcwat_tmp(:plon,:,:) = q3_tmp(:plon,:,1,:)
         lcwat_tmp(:plon,:,:) = q3_tmp(:plon,:,ixcldw,:)
      endif
      call scatter_field_to_chunk(1,plev,1,plond,qcwat_tmp,qcwat(1,1,begchunk,1))
      call scatter_field_to_chunk(1,plev,1,plond,lcwat_tmp,lcwat(1,1,begchunk,1))
      call scatter_field_to_chunk(1,plev,1,plond,t3_tmp,tcwat(1,1,begchunk,1))
      cld   (:,:,:,1) = 0.
      do n=2,3
         cld(:,:,:,n) = 0.
         qcwat(:,:,:,n) = qcwat(:,:,:,1)
         tcwat(:,:,:,n) = tcwat(:,:,:,1)
         lcwat(:,:,:,n) = lcwat(:,:,:,1)
      end do
!!!
!!! Global integerals
!!!
!!      if (masterproc) then
!!         tmassf = tmassf_tmp
!!         qmass1 = qmass1_tmp
!!         qmass2 = qmass2_tmp
!!         qmassf = qmassf_tmp
!!         zgsint = zgsint_tmp
!!      endif

!!#if ( defined SPMD )
!!      call mpibcast (tmass0,1,mpir8,0,mpicom)
!!      call mpibcast (tmassf,1,mpir8,0,mpicom)
!!      call mpibcast (qmass1,1,mpir8,0,mpicom)
!!      call mpibcast (qmass2,1,mpir8,0,mpicom)
!!      call mpibcast (qmassf,1,mpir8,0,mpicom)
!!      call mpibcast (zgsint,1,mpir8,0,mpicom)
!!#endif
      deallocate ( tmpchunk )
      deallocate ( tmpchunk3d)

   end subroutine copy_inidat

#ifdef HADVTEST
   subroutine hadvtest_init

      use pmgrid
      use rgrid
      use commap
      use physconst, only:

#include <comhyb.h>
#include <hadvtest.h>

      integer i,k,lat

      real(r8) h0, u0, small_r, big_r, theta, theta_c, lambda, lambda_c
      real(r8) alfa, dlam, pie
      real(r8) pie
!
! First: zero sgh and phis fields
!
      sgh_tmp(:,:) = 0.
      phis_tmp(:,:) = 0.
!
!JR Analytic IC and wind
!
      pie = acos(-1.)
!
!jr Define wind and constituent fields
!
      h0 = 1000.
      u0 = 2.*pie*rearth/(12.*86400.)
      big_r = rearth/3.
      theta_c = +60.*pie/180.   ! 60 deg north
      theta_c = -60.*pie/180.   ! 60 deg south
      theta_c = 0.              ! equator
      lambda_c = 0.             ! Greenwich
      lambda_c = 3.*pie/2.

      do lat=1,plat
         theta = clat(lat)
         do k=1,plev
            alfa = 0.
            if (k.eq.1) then
               alfa = 0.
            else if (k.eq.2) then
               alfa = 0.05
            else if (k.eq.plev-1) then
               alfa = 0.5*pie - 0.05
            else if (k.eq.plev) then
               alfa = 0.5*pie      ! blows north
            else
               alfa = (k-2)*pie/(2.*(plev-3))
            end if

            do i=1,nlon(lat)
               lambda = 2.*pie*(i-1)/nlon(lat)
!
!jr Use these settings in conjunction with theta_c to start the blob at
!jr Greenwich
!
               usave(i,k,lat) = u0*(cos(theta)*cos(alfa) +  &
                  sin(theta)*cos(lambda-0.5*pie)*sin(alfa))
               vsave(i,k,lat) = -u0*sin(lambda-0.5*pie)*sin(alfa)
!
!jr Use these settings in conjunction with theta_c to start the blob at 270.
!
               usave(i,k,lat) = u0*(cos(theta)*cos(alfa) + &
                  sin(theta)*cos(lambda)*sin(alfa))
               vsave(i,k,lat) = -u0*sin(lambda)*sin(alfa)
               u3_tmp(i,k,lat) = usave(i,k,lat)
               v3_tmp(i,k,lat) = vsave(i,k,lat)
               dlam = lambda - lambda_c
               small_r = rearth*acos(sin(theta_c)*sin(theta) +  &
                  cos(theta_c)*cos(theta)*cos(dlam))
               q3_tmp(i,k,1,lat) = 0.
               if (small_r .lt. big_r) then
                  q3_tmp(i,k,1,lat) = h0/2.*(1. + cos(pie*small_r/big_r))
               end if
!
!jr Stick Q into T to test spectral advection (of what's in T)
!jr Or put 300 in T.
!
               t3_tmp(i,k,lat) = 300.
               t3_tmp(i,k,lat) = q3_tmp(i,k,1,lat)
            end do
         end do
!
!jr Save surface pressure for future timesteps.  Set to 1.e5 everywhere
!
         do i=1,nlon(lat)
            ps_tmp(i,lat) = ps0
            pssave(i,lat) = ps_tmp(i,lat)
         end do
      end do

      return
   end subroutine hadvtest_init
#endif

end module inidat
