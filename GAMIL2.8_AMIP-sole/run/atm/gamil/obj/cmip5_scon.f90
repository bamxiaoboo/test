# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/cmip5_scon.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/cmip5_scon.F90"

# 1 "./misc.h" 1
# 2 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/cmip5_scon.F90" 2

# 1 "./params.h" 1
# 3 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/cmip5_scon.F90" 2

subroutine cmip5nl_scon( year )

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


# 24 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/cmip5_scon.F90" 2

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
end subroutine cmip5nl_scon

!##############################################################################

subroutine cmip5_scon
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


# 59 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/cmip5_scon.F90" 2

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

# 60 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/cmip5_scon.F90" 2

# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/cmip5_scon.h" 1
! Declarations

      integer ntim
      parameter(ntim=453)
      integer yrdata(ntim)      ! yearly data values
      real(r8)    sconst(ntim)      ! input time-varying solar const (W/m2)

! Ouput type declaration

      character*64, parameter :: ramp_type = &
                       'RAMP_SCON: using ramp_scon data'
      logical :: ramp_write
      data ramp_write / .true. /

! Input data values

      data yrdata / 1849  ,&
           1850  ,1851  ,1852  ,1853  ,1854  , &
           1855  ,1856  ,1857  ,1858  ,1859  , &
           1860  ,1861  ,1862  ,1863  ,1864  , &
           1865  ,1866  ,1867  ,1868  ,1869  , & 
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
           2100  ,2101  ,2102  ,2103  ,2104  , &
           2105  ,2106  ,2107  ,2108  ,2109  , &
           2110  ,2111  ,2112  ,2113  ,2114  , &
           2115  ,2116  ,2117  ,2118  ,2119  , &
           2120  ,2121  ,2122  ,2123  ,2124  , &
           2125  ,2126  ,2127  ,2128  ,2129  , &
           2130  ,2131  ,2132  ,2133  ,2134  , &
           2135  ,2136  ,2137  ,2138  ,2139  , &
           2140  ,2141  ,2142  ,2143  ,2144  , &
           2145  ,2146  ,2147  ,2148  ,2149  , &
           2150  ,2151  ,2152  ,2153  ,2154  , &
           2155  ,2156  ,2157  ,2158  ,2159  , &
           2160  ,2161  ,2162  ,2163  ,2164  , &
           2165  ,2166  ,2167  ,2168  ,2169  , &
           2170  ,2171  ,2172  ,2173  ,2174  , &
           2175  ,2176  ,2177  ,2178  ,2179  , &
           2180  ,2181  ,2182  ,2183  ,2184  , &
           2185  ,2186  ,2187  ,2188  ,2189  , &
           2190  ,2191  ,2192  ,2193  ,2194  , &
           2195  ,2196  ,2197  ,2198  ,2199  , &
           2200  ,2201  ,2202  ,2203  ,2204  , &
           2205  ,2206  ,2207  ,2208  ,2209  , &
           2210  ,2211  ,2212  ,2213  ,2214  , &
           2215  ,2216  ,2217  ,2218  ,2219  , &
           2220  ,2221  ,2222  ,2223  ,2224  , &
           2225  ,2226  ,2227  ,2228  ,2229  , &
           2230  ,2231  ,2232  ,2233  ,2234  , &
           2235  ,2236  ,2237  ,2238  ,2239  , &
           2240  ,2241  ,2242  ,2243  ,2244  , &
           2245  ,2246  ,2247  ,2248  ,2249  , &
           2250  ,2251  ,2252  ,2253  ,2254  , &
           2255  ,2256  ,2257  ,2258  ,2259  , &
           2260  ,2261  ,2262  ,2263  ,2264  , &
           2265  ,2266  ,2267  ,2268  ,2269  , &
           2270  ,2271  ,2272  ,2273  ,2274  , &
           2275  ,2276  ,2277  ,2278  ,2279  , &
           2280  ,2281  ,2282  ,2283  ,2284  , &
           2285  ,2286  ,2287  ,2288  ,2289  , &
           2290  ,2291  ,2292  ,2293  ,2294  , &
           2295  ,2296  ,2297  ,2298  ,2299  , &
           2300  ,2301  / 

! solar constanst 1849-2009. Use 1997 value for 1997-2100

       data  sconst /    1.36596910e+06,  &
        1.36576980e+06,  1.36577440e+06,  1.36569470e+06,  1.36558480e+06,  1.36546140e+06,   &
        1.36538280e+06,  1.36538530e+06,  1.36549460e+06,  1.36568750e+06,  1.36590540e+06,   &
        1.36597180e+06,  1.36586230e+06,  1.36571200e+06,  1.36562830e+06,  1.36556600e+06,   &
        1.36547550e+06,  1.36541190e+06,  1.36535970e+06,  1.36551940e+06,  1.36575570e+06,   &
        1.36599440e+06,  1.36593430e+06,  1.36588760e+06,  1.36568660e+06,  1.36555820e+06,   &
        1.36540950e+06,  1.36535930e+06,  1.36535960e+06,  1.36533090e+06,  1.36535330e+06,   &
        1.36550000e+06,  1.36564430e+06,  1.36566760e+06,  1.36571470e+06,  1.36561660e+06,   &
        1.36550700e+06,  1.36534170e+06,  1.36529820e+06,  1.36526280e+06,  1.36523440e+06,   &
        1.36526900e+06,  1.36552040e+06,  1.36561900e+06,  1.36576000e+06,  1.36585530e+06,   &
        1.36573900e+06,  1.36555810e+06,  1.36541260e+06,  1.36538990e+06,  1.36533810e+06,   &
        1.36530740e+06,  1.36522920e+06,  1.36523780e+06,  1.36544790e+06,  1.36571800e+06,   &
        1.36552910e+06,  1.36572550e+06,  1.36560970e+06,  1.36567480e+06,  1.36556420e+06,   &
        1.36543090e+06,  1.36534730e+06,  1.36530100e+06,  1.36531750e+06,  1.36538440e+06,   &
        1.36568900e+06,  1.36589900e+06,  1.36604800e+06,  1.36600960e+06,  1.36578020e+06,   &
        1.36561780e+06,  1.36551270e+06,  1.36539480e+06,  1.36542650e+06,  1.36545810e+06,   &
        1.36566220e+06,  1.36576330e+06,  1.36594680e+06,  1.36582450e+06,  1.36578330e+06,   &
        1.36576550e+06,  1.36564360e+06,  1.36553640e+06,  1.36541560e+06,  1.36552750e+06,   &
        1.36574390e+06,  1.36613330e+06,  1.36606760e+06,  1.36600310e+06,  1.36598680e+06,   &
        1.36592420e+06,  1.36584510e+06,  1.36574190e+06,  1.36558410e+06,  1.36561400e+06,   &
        1.36588100e+06,  1.36597910e+06,  1.36621850e+06,  1.36634900e+06,  1.36625550e+06,   &
        1.36601310e+06,  1.36577650e+06,  1.36576760e+06,  1.36562840e+06,  1.36565640e+06,   &
        1.36577730e+06,  1.36631090e+06,  1.36666810e+06,  1.36663280e+06,  1.36638280e+06,   &
        1.36627670e+06,  1.36591990e+06,  1.36574840e+06,  1.36569630e+06,  1.36569760e+06,   &
        1.36573410e+06,  1.36591780e+06,  1.36611430e+06,  1.36616440e+06,  1.36624760e+06,   &
        1.36624260e+06,  1.36595800e+06,  1.36605250e+06,  1.36579910e+06,  1.36572710e+06,   &
        1.36553450e+06,  1.36564530e+06,  1.36583310e+06,  1.36627470e+06,  1.36663480e+06,   &
        1.36664820e+06,  1.36669510e+06,  1.36628590e+06,  1.36619920e+06,  1.36581030e+06,   &
        1.36564160e+06,  1.36563790e+06,  1.36578990e+06,  1.36608260e+06,  1.36664790e+06,   &
        1.36655330e+06,  1.36644570e+06,  1.36630210e+06,  1.36602860e+06,  1.36579710e+06,   &
        1.36569960e+06,  1.36561210e+06,  1.36573990e+06,  1.36610210e+06,  1.36638510e+06,   &
        1.36668360e+06,  1.36660220e+06,  1.36668070e+06,  1.36623000e+06,  1.36604800e+06,   &
        1.36585450e+06,  1.36581070e+06,  1.36572400e+06,  1.36569180e+06,  1.36561210e+06,   &
        1.36573990e+06,  1.36610210e+06,  1.36638510e+06,  1.36668360e+06,  1.36660220e+06,   &
        1.36668070e+06,  1.36623000e+06,  1.36604800e+06,  1.36585450e+06,  1.36581070e+06,   &
        1.36572400e+06,  1.36569180e+06,  1.36561210e+06,  1.36573990e+06,  1.36610210e+06,   &
        1.36638510e+06,  1.36668360e+06,  1.36660220e+06,  1.36668070e+06,  1.36623000e+06,   &
        1.36604800e+06,  1.36585450e+06,  1.36581070e+06,  1.36572400e+06,  1.36569180e+06,   &
        1.36561210e+06,  1.36573990e+06,  1.36610210e+06,  1.36638510e+06,  1.36668360e+06,   &
        1.36660220e+06,  1.36668070e+06,  1.36623000e+06,  1.36604800e+06,  1.36585450e+06,   &
        1.36581070e+06,  1.36572400e+06,  1.36569180e+06,  1.36561210e+06,  1.36573990e+06,   &
        1.36610210e+06,  1.36638510e+06,  1.36668360e+06,  1.36660220e+06,  1.36668070e+06,   &
        1.36623000e+06,  1.36604800e+06,  1.36585450e+06,  1.36581070e+06,  1.36572400e+06,   &
        1.36569180e+06,  1.36561210e+06,  1.36573990e+06,  1.36610210e+06,  1.36638510e+06,   &
        1.36668360e+06,  1.36660220e+06,  1.36668070e+06,  1.36623000e+06,  1.36604800e+06,   &
        1.36585450e+06,  1.36581070e+06,  1.36572400e+06,  1.36569180e+06,  1.36561210e+06,   &
        1.36573990e+06,  1.36610210e+06,  1.36638510e+06,  1.36668360e+06,  1.36660220e+06,   &
        1.36668070e+06,  1.36623000e+06,  1.36604800e+06,  1.36585450e+06,  1.36581070e+06,   &
        1.36572400e+06,  1.36569180e+06,  1.36561210e+06,  1.36573990e+06,  1.36610210e+06,   &
        1.36638510e+06,  1.36668360e+06,  1.36660220e+06,  1.36668070e+06,  1.36623000e+06,   &
        1.36604800e+06,  1.36585450e+06,  1.36581070e+06,  1.36572400e+06,  1.36569180e+06,   &
        1.36561210e+06,  1.36573990e+06,  1.36610210e+06,  1.36638510e+06,  1.36668360e+06,   &
        1.36660220e+06,  1.36668070e+06,  1.36623000e+06,  1.36604800e+06,  1.36585450e+06,   &
        1.36581070e+06,  1.36572400e+06,  1.36569180e+06,  1.36561210e+06,  1.36573990e+06,   &
        1.36610210e+06,  1.36638510e+06,  1.36668360e+06,  1.36660220e+06,  1.36668070e+06,   &
        1.36623000e+06,  1.36604800e+06,  1.36585450e+06,  1.36581070e+06,  1.36572400e+06,   &
        1.36569180e+06,  1.36561210e+06,  1.36573990e+06,  1.36610210e+06,  1.36638510e+06,   &
        1.36668360e+06,  1.36660220e+06,  1.36668070e+06,  1.36623000e+06,  1.36604800e+06,   &
        1.36585450e+06,  1.36581070e+06,  1.36572400e+06,  1.36569180e+06,  1.36561210e+06,   &
        1.36573990e+06,  1.36610210e+06,  1.36638510e+06,  1.36668360e+06,  1.36660220e+06,   &
        1.36668070e+06,  1.36623000e+06,  1.36604800e+06,  1.36585450e+06,  1.36581070e+06,   &
        1.36572400e+06,  1.36569180e+06,  1.36561210e+06,  1.36573990e+06,  1.36610210e+06,   &
        1.36638510e+06,  1.36668360e+06,  1.36660220e+06,  1.36668070e+06,  1.36623000e+06,   &
        1.36604800e+06,  1.36585450e+06,  1.36581070e+06,  1.36572400e+06,  1.36569180e+06,   &
        1.36561210e+06,  1.36573990e+06,  1.36610210e+06,  1.36638510e+06,  1.36668360e+06,   &
        1.36660220e+06,  1.36668070e+06,  1.36623000e+06,  1.36604800e+06,  1.36585450e+06,   &
        1.36581070e+06,  1.36572400e+06,  1.36569180e+06,  1.36561210e+06,  1.36573990e+06,   &
        1.36610210e+06,  1.36638510e+06,  1.36668360e+06,  1.36660220e+06,  1.36668070e+06,   &
        1.36623000e+06,  1.36604800e+06,  1.36585450e+06,  1.36581070e+06,  1.36572400e+06,   &
        1.36569180e+06,  1.36561210e+06,  1.36573990e+06,  1.36610210e+06,  1.36638510e+06,   &
        1.36668360e+06,  1.36660220e+06,  1.36668070e+06,  1.36623000e+06,  1.36604800e+06,   &
        1.36585450e+06,  1.36581070e+06,  1.36572400e+06,  1.36569180e+06,  1.36561210e+06,   &
        1.36573990e+06,  1.36610210e+06,  1.36638510e+06,  1.36668360e+06,  1.36660220e+06,   &
        1.36668070e+06,  1.36623000e+06,  1.36604800e+06,  1.36585450e+06,  1.36581070e+06,   &
        1.36572400e+06,  1.36569180e+06,  1.36561210e+06,  1.36573990e+06,  1.36610210e+06,   &
        1.36638510e+06,  1.36668360e+06,  1.36660220e+06,  1.36668070e+06,  1.36623000e+06,   &
        1.36604800e+06,  1.36585450e+06,  1.36581070e+06,  1.36572400e+06,  1.36569180e+06,   &
        1.36561210e+06,  1.36573990e+06,  1.36610210e+06,  1.36638510e+06,  1.36668360e+06,   &
        1.36660220e+06,  1.36668070e+06,  1.36623000e+06,  1.36604800e+06,  1.36585450e+06,   &
        1.36581070e+06,  1.36572400e+06,  1.36569180e+06,  1.36561210e+06,  1.36573990e+06,   &
        1.36610210e+06,  1.36638510e+06,  1.36668360e+06,  1.36660220e+06,  1.36668070e+06,   &
        1.36623000e+06,  1.36604800e+06,  1.36585450e+06,  1.36581070e+06,  1.36572400e+06,   &
        1.36569180e+06,  1.36561210e+06,  1.36573990e+06,  1.36610210e+06,  1.36638510e+06,   &
        1.36668360e+06,  1.36660220e+06,  1.36668070e+06,  1.36623000e+06,  1.36604800e+06,   &
        1.36585450e+06,  1.36581070e+06,  1.36572400e+06,  1.36569180e+06,  1.36561210e+06,   &
        1.36573990e+06,  1.36610210e+06,  1.36638510e+06,  1.36668360e+06,  1.36660220e+06,   &
        1.36668070e+06,  1.36623000e+06,  1.36604800e+06,  1.36585450e+06,  1.36581070e+06,   &
        1.36572400e+06,  1.36569180e+06,  1.36561210e+06,  1.36573990e+06,  1.36610210e+06,   &
        1.36638510e+06,  1.36668360e+06,  1.36660220e+06,  1.36668070e+06,  1.36623000e+06,   &
        1.36604800e+06,  1.36585450e+06,  1.36581070e+06,  1.36572400e+06,  1.36569180e+06,   &
        1.36561210e+06,  1.36573990e+06,  1.36610210e+06,  1.36638510e+06,  1.36668360e+06,   &
        1.36660220e+06,  1.36668070e+06 /
# 61 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/cmip5_scon.F90" 2
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
end subroutine cmip5_scon
