# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ramp_scon.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ramp_scon.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ramp_scon.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ramp_scon.F90" 2

subroutine rampnl_scon( year )

!----------------------------------------------------------------------- 
! 
! Purpose: 
! Initialize the ramp options that are controlled by namelist input.
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: <Who is primarily responsible for the code> 
! 
!-----------------------------------------------------------------------

   use shr_kind_mod, only: r8 => shr_kind_r8
   use pmgrid, only: masterproc
   implicit none


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ramp.h" 1
      logical fixYear_ghg   ! true => Ramped gases fixed at specified year.
      logical fixYear_so4   ! true => Ramped gases fixed at specified year.
      logical fixYear_scon  ! true => Ramped gases fixed at specified year.
      common /ramp_l/ fixYear_ghg, fixYear_so4, fixYear_scon

      integer rampYear_ghg   ! ramped gases fixed at this year
      integer rampYear_so4   ! ramped gases fixed at this year
      integer rampYear_scon  ! ramped gases fixed at this year
      common /ramp_i/ rampYear_ghg, rampYear_so4, rampYear_scon


# 24 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ramp_scon.F90" 2

   integer, intent(in) :: year ! Ramped gases fixed at this year
!-----------------------------------------------------------------------
   rampYear_scon = year
   fixYear_scon = .false.
   if ( year > 0 ) then
      fixYear_scon = .true.
      if (masterproc) &
         write(6,*) 'RAMP_SCON: Ramped gases being fixed at year ',rampYear_scon
   end if
   return
end subroutine rampnl_scon

!##############################################################################

subroutine ramp_scon
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Computes ramping of solar constant
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: <Who is primarily responsible for the code> 
! 
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use pmgrid,       only: masterproc
   use time_manager, only: get_curr_date, get_curr_calday

   implicit none


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ramp.h" 1
      logical fixYear_ghg   ! true => Ramped gases fixed at specified year.
      logical fixYear_so4   ! true => Ramped gases fixed at specified year.
      logical fixYear_scon  ! true => Ramped gases fixed at specified year.
      common /ramp_l/ fixYear_ghg, fixYear_so4, fixYear_scon

      integer rampYear_ghg   ! ramped gases fixed at this year
      integer rampYear_so4   ! ramped gases fixed at this year
      integer rampYear_scon  ! ramped gases fixed at this year
      common /ramp_i/ rampYear_ghg, rampYear_so4, rampYear_scon


# 59 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ramp_scon.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/comsol.h" 1
!
!	Common's to do with solar radiation
!
!	$Id: comsol.h,v 1.3 2000/06/02 16:20:40 jet Exp $
!
! Visible optical depth
!
      real(r8) tauvis     ! Visible optical depth

      common /comvis/ tauvis
!
! Solar constant
!
      real(r8) scon       ! Solar constant

      common /comsol/ scon
!
! Earth's orbital characteristics
!	
      real(r8) eccen       ! Earth's eccentricity factor (unitless) (typically 0 to 0.1)
      real(r8) obliq       ! Earth's obliquity angle (degree's) (-90 to +90) (typically 22-26)
      real(r8) mvelp       ! Earth's moving vernal equinox at perhelion (degree's) (0 to 360.0)
      integer iyear_AD ! Year (AD) to simulate above earth's orbital parameters for
!
! Orbital information after processed by orbit_params
!
      real(r8) obliqr      ! Earth's obliquity in radians
      real(r8) lambm0      ! Mean longitude of perihelion at the 
!                          ! vernal equinox (radians)
      real(r8) mvelpp      ! Earth's moving vernal equinox longitude
!                          ! of perihelion plus pi (radians)
!
      common /comorb/ eccen   , obliq   , mvelp   , obliqr  
      common /comorb/ lambm0  , mvelpp  , iyear_AD

# 60 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ramp_scon.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ramp_scon.h" 1
! Declarations

      integer ntim
      parameter(ntim=231)
      integer yrdata(ntim)      ! yearly data values
      real(r8)    sconst(ntim)      ! input time-varying solar const (W/m2)

! Ouput type declaration

      character*64, parameter :: ramp_type = &
                       'RAMP_SCON: using ramp_scon data'
      logical :: ramp_write
      data ramp_write / .true. /

! Input data values

      data yrdata / &
           1870  ,1871  ,1872  ,1873  ,1874  , &
           1875  ,1876  ,1877  ,1878  ,1879  , &
           1880  ,1881  ,1882  ,1883  ,1884  , &
           1885  ,1886  ,1887  ,1888  ,1889  , &
           1890  ,1891  ,1892  ,1893  ,1894  , &
           1895  ,1896  ,1897  ,1898  ,1899  , &
           1900  ,1901  ,1902  ,1903  ,1904  , &
           1905  ,1906  ,1907  ,1908  ,1909  , &
           1910  ,1911  ,1912  ,1913  ,1914  , &
           1915  ,1916  ,1917  ,1918  ,1919  , &
           1920  ,1921  ,1922  ,1923  ,1924  , &
           1925  ,1926  ,1927  ,1928  ,1929  , &
           1930  ,1931  ,1932  ,1933  ,1934  , &
           1935  ,1936  ,1937  ,1938  ,1939  , &
           1940  ,1941  ,1942  ,1943  ,1944  , &
           1945  ,1946  ,1947  ,1948  ,1949  , &
           1950  ,1951  ,1952  ,1953  ,1954  , &
           1955  ,1956  ,1957  ,1958  ,1959  , &
           1960  ,1961  ,1962  ,1963  ,1964  , &
           1965  ,1966  ,1967  ,1968  ,1969  , &
           1970  ,1971  ,1972  ,1973  ,1974  , &
           1975  ,1976  ,1977  ,1978  ,1979  , &
           1980  ,1981  ,1982  ,1983  ,1984  , &
           1985  ,1986  ,1987  ,1988  ,1989  , &
           1990  ,1991  ,1992  ,1993  ,1994  , &
           1995  ,1996  ,1997  ,1998  ,1999  , &
           2000  ,2001  ,2002  ,2003  ,2004  , &
           2005  ,2006  ,2007  ,2008  ,2009  , &
           2010  ,2011  ,2012  ,2013  ,2014  , &
           2015  ,2016  ,2017  ,2018  ,2019  , &
           2020  ,2021  ,2022  ,2023  ,2024  , &
           2025  ,2026  ,2027  ,2028  ,2029  , &
           2030  ,2031  ,2032  ,2033  ,2034  , &
           2035  ,2036  ,2037  ,2038  ,2039  , &
           2040  ,2041  ,2042  ,2043  ,2044  , &
           2045  ,2046  ,2047  ,2048  ,2049  , &
           2050  ,2051  ,2052  ,2053  ,2054  , &
           2055  ,2056  ,2057  ,2058  ,2059  , &
           2060  ,2061  ,2062  ,2063  ,2064  , &
           2065  ,2066  ,2067  ,2068  ,2069  , &
           2070  ,2071  ,2072  ,2073  ,2074  , &
           2075  ,2076  ,2077  ,2078  ,2079  , &
           2080  ,2081  ,2082  ,2083  ,2084  , &
           2085  ,2086  ,2087  ,2088  ,2089  , &
           2090  ,2091  ,2092  ,2093  ,2094  , &
           2095  ,2096  ,2097  ,2098  ,2099  , &
           2100/                              

! solar constanst 1870-1997. Use 1997 value for 1997-2100

       data  sconst / &
           1.3650e+06, 1.3649e+06, 1.3649e+06, 1.3647e+06, 1.3647e+06, &
           1.3645e+06, 1.3646e+06, 1.3646e+06, 1.3646e+06, 1.3645e+06, &
           1.3647e+06, 1.3648e+06, 1.3649e+06, 1.3645e+06, 1.3644e+06, &
           1.3643e+06, 1.3640e+06, 1.3638e+06, 1.3637e+06, 1.3637e+06, &
           1.3637e+06, 1.3638e+06, 1.3641e+06, 1.3642e+06, 1.3642e+06, &
           1.3642e+06, 1.3641e+06, 1.3641e+06, 1.3643e+06, 1.3644e+06, &
           1.3645e+06, 1.3646e+06, 1.3647e+06, 1.3649e+06, 1.3650e+06, &
           1.3652e+06, 1.3652e+06, 1.3653e+06, 1.3652e+06, 1.3653e+06, &
           1.3652e+06, 1.3652e+06, 1.3652e+06, 1.3653e+06, 1.3654e+06, &
           1.3657e+06, 1.3658e+06, 1.3663e+06, 1.3662e+06, 1.3661e+06, &
           1.3660e+06, 1.3660e+06, 1.3660e+06, 1.3660e+06, 1.3661e+06, &
           1.3664e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3665e+06, 1.3664e+06, 1.3663e+06, 1.3663e+06, 1.3663e+06, &
           1.3665e+06, 1.3668e+06, 1.3670e+06, 1.3670e+06, 1.3669e+06, &
           1.3668e+06, 1.3667e+06, 1.3667e+06, 1.3666e+06, 1.3666e+06, &
           1.3668e+06, 1.3671e+06, 1.3676e+06, 1.3673e+06, 1.3673e+06, &
           1.3668e+06, 1.3667e+06, 1.3662e+06, 1.3658e+06, 1.3656e+06, &
           1.3656e+06, 1.3662e+06, 1.3664e+06, 1.3662e+06, 1.3660e+06, &
           1.3657e+06, 1.3653e+06, 1.3652e+06, 1.3651e+06, 1.3650e+06, &
           1.3651e+06, 1.3654e+06, 1.3658e+06, 1.3660e+06, 1.3660e+06, &
           1.3660e+06, 1.3658e+06, 1.3659e+06, 1.3656e+06, 1.3658e+06, &
           1.3657e+06, 1.3657e+06, 1.3658e+06, 1.3663e+06, 1.3673e+06, &
           1.3674e+06, 1.3675e+06, 1.3672e+06, 1.3670e+06, 1.3666e+06, &
           1.3664e+06, 1.3664e+06, 1.3665e+06, 1.3668e+06, 1.3673e+06, &
           1.3672e+06, 1.3672e+06, 1.3671e+06, 1.3667e+06, 1.3666e+06, &
           1.3664e+06, 1.3663e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, 1.3666e+06, &
           1.3666e+06 /
# 61 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/ramp_scon.F90" 2
!---------------------------Local variables-----------------------------

   integer yrmodel           ! model year
   integer nyrm              ! year index
   integer nyrp              ! year index
   integer :: yr, mon, day   ! components of a date
   integer :: ncdate         ! current date in integer format [yyyymmdd]
   integer :: ncsec          ! current time of day [seconds]

   real(r8) :: calday            ! current calendar day
   real(r8) doymodel             ! model day of year
   real(r8) doydatam             ! day of year for input data yrdata(nyrm)
   real(r8) doydatap             ! day or year for input data yrdata(nyrp)
   real(r8) deltat               ! delta time
   real(r8) fact1, fact2         ! time interpolation factors
!
! ---------------------------------------------------------------------
!
   calday = get_curr_calday()
   call get_curr_date(yr, mon, day, ncsec)
   ncdate = yr*10000 + mon*100 + day

   if (ramp_write) then
      if (masterproc) &
        write(6,*) ramp_type
      ramp_write = .false.
   endif
!
! determine index into input data
!
   if ( fixYear_scon ) then
      yrmodel  = rampYear_scon
   else
      yrmodel  = ncdate/10000
   end if

   nyrm       = yrmodel - yrdata(1) + 1
   nyrp       = nyrm + 1
!
! if current date is before 1870, quit
!
   if (nyrm < 1) then
      if (masterproc) then
         write(6,*)'RAMP_SCON: data time index is out of bounds'
         write(6,*)'nyrm = ',nyrm,' nyrp= ',nyrp, ' ncdate= ', ncdate
      end if
      call endrun
   endif
!
! if current date later than ntim, just use ntim values
!
   if (nyrp > ntim) then
      scon = sconst(ntim)
      return
   endif
!
! determine time interpolation factors, check sanity
! of interpolation factors to within 32-bit roundoff
! assume that day of year is 1 for all input data
!
   doymodel = yrmodel*365.    + calday
   doydatam = yrdata(nyrm)*365. + 1.
   doydatap = yrdata(nyrp)*365. + 1.
   deltat   = doydatap - doydatam !365
   fact1    = (doydatap - doymodel)/deltat
   fact2    = (doymodel - doydatam)/deltat

   if (abs(fact1+fact2-1.) > 1.e-6 .or. &
       fact1 > 1.000001 .or. &
       fact1 < -1.e-6 .or. &
       fact2 > 1.000001 .or. &
       fact2 < -1.e-6) then
      if (masterproc) &
         write(6,*)'RAMP_SCON: Bad fact1 and/or fact2=',fact1,fact2
      call endrun
   end if
!
! do time interpolation:
!
   scon = (sconst(nyrm)*fact1 + sconst(nyrp)*fact2)

   return
end subroutine ramp_scon

