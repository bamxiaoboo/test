# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/boundarydata.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/boundarydata.F90"

!! sxj- 20090303
module boundarydata
  use shr_kind_mod,    only: r8 => shr_kind_r8
  use pmgrid,          only: plev, plevp,masterproc
  !use spmd_utils,      only: masterproc
  use ppgrid,          only: pcols, pver, begchunk, endchunk
  use physics_types,   only: physics_state
  !use abortutils,      only: endrun



  !use netcdf
  use error_messages, only : handle_ncerr  
  implicit none

# 1 "/usr/include/netcdf.inc" 1 3 4
!     NetCDF-3.
!
! netcdf version 3 fortran interface:
!

!
! external netcdf data types:
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double

      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)

!
! default fill values:
!
      integer           nf_fill_byte
      integer           nf_fill_int1
      integer           nf_fill_char
      integer           nf_fill_short
      integer           nf_fill_int2
      integer           nf_fill_int
      real              nf_fill_float
      real              nf_fill_real
      doubleprecision   nf_fill_double

      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)

!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit

      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_64bit = 2)

!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)

!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)

!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims

      parameter (nf_max_dims = 1024)
      parameter (nf_max_attrs = 8192)
      parameter (nf_max_vars = 8192)
      parameter (nf_max_name = 256)
      parameter (nf_max_var_dims = nf_max_dims)

!
! error codes:
!
      integer nf_noerr
      integer nf_ebadid
      integer nf_eexist
      integer nf_einval
      integer nf_eperm
      integer nf_enotindefine
      integer nf_eindefine
      integer nf_einvalcoords
      integer nf_emaxdims
      integer nf_enameinuse
      integer nf_enotatt
      integer nf_emaxatts
      integer nf_ebadtype
      integer nf_ebaddim
      integer nf_eunlimpos
      integer nf_emaxvars
      integer nf_enotvar
      integer nf_eglobal
      integer nf_enotnc
      integer nf_ests
      integer nf_emaxname
      integer nf_eunlimit
      integer nf_enorecvars
      integer nf_echar
      integer nf_eedge
      integer nf_estride
      integer nf_ebadname
      integer nf_erange
      integer nf_enomem
      integer nf_evarsize
      integer nf_edimsize
      integer nf_etrunc

      parameter (nf_noerr = 0)
      parameter (nf_ebadid = -33)
      parameter (nf_eexist = -35)
      parameter (nf_einval = -36)
      parameter (nf_eperm = -37)
      parameter (nf_enotindefine = -38)
      parameter (nf_eindefine = -39)
      parameter (nf_einvalcoords = -40)
      parameter (nf_emaxdims = -41)
      parameter (nf_enameinuse = -42)
      parameter (nf_enotatt = -43)
      parameter (nf_emaxatts = -44)
      parameter (nf_ebadtype = -45)
      parameter (nf_ebaddim = -46)
      parameter (nf_eunlimpos = -47)
      parameter (nf_emaxvars = -48)
      parameter (nf_enotvar = -49)
      parameter (nf_eglobal = -50)
      parameter (nf_enotnc = -51)
      parameter (nf_ests = -52)
      parameter (nf_emaxname = -53)
      parameter (nf_eunlimit = -54)
      parameter (nf_enorecvars = -55)
      parameter (nf_echar = -56)
      parameter (nf_eedge = -57)
      parameter (nf_estride = -58)
      parameter (nf_ebadname = -59)
      parameter (nf_erange = -60)
      parameter (nf_enomem = -61)
      parameter (nf_evarsize = -62)
      parameter (nf_edimsize = -63)
      parameter (nf_etrunc = -64)
!
! error handling modes:
!
      integer  nf_fatal
      integer nf_verbose

      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)

!
! miscellaneous routines:
!
      character*80   nf_inq_libvers
      external       nf_inq_libvers

      character*80   nf_strerror
!                         (integer             ncerr)
      external       nf_strerror

      logical        nf_issyserr
!                         (integer             ncerr)
      external       nf_issyserr

!
! control routines:
!
      integer         nf_inq_base_pe
!                         (integer             ncid,
!                          integer             pe)
      external        nf_inq_base_pe

      integer         nf_set_base_pe
!                         (integer             ncid,
!                          integer             pe)
      external        nf_set_base_pe

      integer         nf_create
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             ncid)
      external        nf_create

      integer         nf__create
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             initialsz,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__create

      integer         nf__create_mp
!                         (character*(*)       path,
!                          integer             cmode,
!                          integer             initialsz,
!                          integer             basepe,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__create_mp

      integer         nf_open
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             ncid)
      external        nf_open

      integer         nf__open
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__open

      integer         nf__open_mp
!                         (character*(*)       path,
!                          integer             mode,
!                          integer             basepe,
!                          integer             chunksizehint,
!                          integer             ncid)
      external        nf__open_mp

      integer         nf_set_fill
!                         (integer             ncid,
!                          integer             fillmode,
!                          integer             old_mode)
      external        nf_set_fill

      integer         nf_set_default_format
!                          (integer             format,
!                          integer             old_format)
      external        nf_set_default_format

      integer         nf_redef
!                         (integer             ncid)
      external        nf_redef

      integer         nf_enddef
!                         (integer             ncid)
      external        nf_enddef

      integer         nf__enddef
!                         (integer             ncid,
!                          integer             h_minfree,
!                          integer             v_align,
!                          integer             v_minfree,
!                          integer             r_align)
      external        nf__enddef

      integer         nf_sync
!                         (integer             ncid)
      external        nf_sync

      integer         nf_abort
!                         (integer             ncid)
      external        nf_abort

      integer         nf_close
!                         (integer             ncid)
      external        nf_close

      integer         nf_delete
!                         (character*(*)       ncid)
      external        nf_delete

!
! general inquiry routines:
!

      integer         nf_inq
!                         (integer             ncid,
!                          integer             ndims,
!                          integer             nvars,
!                          integer             ngatts,
!                          integer             unlimdimid)
      external        nf_inq

      integer         nf_inq_ndims
!                         (integer             ncid,
!                          integer             ndims)
      external        nf_inq_ndims

      integer         nf_inq_nvars
!                         (integer             ncid,
!                          integer             nvars)
      external        nf_inq_nvars

      integer         nf_inq_natts
!                         (integer             ncid,
!                          integer             ngatts)
      external        nf_inq_natts

      integer         nf_inq_unlimdim
!                         (integer             ncid,
!                          integer             unlimdimid)
      external        nf_inq_unlimdim

      integer         nf_inq_format
!                         (integer             ncid,
!                          integer             format)
      external        nf_inq_format

!
! dimension routines:
!

      integer         nf_def_dim
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             len,
!                          integer             dimid)
      external        nf_def_dim

      integer         nf_inq_dimid
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             dimid)
      external        nf_inq_dimid

      integer         nf_inq_dim
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name,
!                          integer             len)
      external        nf_inq_dim

      integer         nf_inq_dimname
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name)
      external        nf_inq_dimname

      integer         nf_inq_dimlen
!                         (integer             ncid,
!                          integer             dimid,
!                          integer             len)
      external        nf_inq_dimlen

      integer         nf_rename_dim
!                         (integer             ncid,
!                          integer             dimid,
!                          character(*)        name)
      external        nf_rename_dim

!
! general attribute routines:
!

      integer         nf_inq_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len)
      external        nf_inq_att

      integer         nf_inq_attid
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             attnum)
      external        nf_inq_attid

      integer         nf_inq_atttype
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype)
      external        nf_inq_atttype

      integer         nf_inq_attlen
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             len)
      external        nf_inq_attlen

      integer         nf_inq_attname
!                         (integer             ncid,
!                          integer             varid,
!                          integer             attnum,
!                          character(*)        name)
      external        nf_inq_attname

      integer         nf_copy_att
!                         (integer             ncid_in,
!                          integer             varid_in,
!                          character(*)        name,
!                          integer             ncid_out,
!                          integer             varid_out)
      external        nf_copy_att

      integer         nf_rename_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        curname,
!                          character(*)        newname)
      external        nf_rename_att

      integer         nf_del_att
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_del_att

!
! attribute put/get routines:
!

      integer         nf_put_att_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             len,
!                          character(*)        text)
      external        nf_put_att_text

      integer         nf_get_att_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          character(*)        text)
      external        nf_get_att_text

      integer         nf_put_att_int1
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          nf_int1_t           i1vals(1))
      external        nf_put_att_int1

      integer         nf_get_att_int1
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          nf_int1_t           i1vals(1))
      external        nf_get_att_int1

      integer         nf_put_att_int2
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          nf_int2_t           i2vals(1))
      external        nf_put_att_int2

      integer         nf_get_att_int2
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          nf_int2_t           i2vals(1))
      external        nf_get_att_int2

      integer         nf_put_att_int
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          integer             ivals(1))
      external        nf_put_att_int

      integer         nf_get_att_int
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             ivals(1))
      external        nf_get_att_int

      integer         nf_put_att_real
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          real                rvals(1))
      external        nf_put_att_real

      integer         nf_get_att_real
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          real                rvals(1))
      external        nf_get_att_real

      integer         nf_put_att_double
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             xtype,
!                          integer             len,
!                          double              dvals(1))
      external        nf_put_att_double

      integer         nf_get_att_double
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          double              dvals(1))
      external        nf_get_att_double

!
! general variable routines:
!

      integer         nf_def_var
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             datatype,
!                          integer             ndims,
!                          integer             dimids(1),
!                          integer             varid)
      external        nf_def_var

      integer         nf_inq_var
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name,
!                          integer             datatype,
!                          integer             ndims,
!                          integer             dimids(1),
!                          integer             natts)
      external        nf_inq_var

      integer         nf_inq_varid
!                         (integer             ncid,
!                          character(*)        name,
!                          integer             varid)
      external        nf_inq_varid

      integer         nf_inq_varname
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_inq_varname

      integer         nf_inq_vartype
!                         (integer             ncid,
!                          integer             varid,
!                          integer             xtype)
      external        nf_inq_vartype

      integer         nf_inq_varndims
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ndims)
      external        nf_inq_varndims

      integer         nf_inq_vardimid
!                         (integer             ncid,
!                          integer             varid,
!                          integer             dimids(1))
      external        nf_inq_vardimid

      integer         nf_inq_varnatts
!                         (integer             ncid,
!                          integer             varid,
!                          integer             natts)
      external        nf_inq_varnatts

      integer         nf_rename_var
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        name)
      external        nf_rename_var

      integer         nf_copy_var
!                         (integer             ncid_in,
!                          integer             varid,
!                          integer             ncid_out)
      external        nf_copy_var

!
! entire variable put/get routines:
!

      integer         nf_put_var_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        text)
      external        nf_put_var_text

      integer         nf_get_var_text
!                         (integer             ncid,
!                          integer             varid,
!                          character(*)        text)
      external        nf_get_var_text

      integer         nf_put_var_int1
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int1_t           i1vals(1))
      external        nf_put_var_int1

      integer         nf_get_var_int1
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int1_t           i1vals(1))
      external        nf_get_var_int1

      integer         nf_put_var_int2
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int2_t           i2vals(1))
      external        nf_put_var_int2

      integer         nf_get_var_int2
!                         (integer             ncid,
!                          integer             varid,
!                          nf_int2_t           i2vals(1))
      external        nf_get_var_int2

      integer         nf_put_var_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ivals(1))
      external        nf_put_var_int

      integer         nf_get_var_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             ivals(1))
      external        nf_get_var_int

      integer         nf_put_var_real
!                         (integer             ncid,
!                          integer             varid,
!                          real                rvals(1))
      external        nf_put_var_real

      integer         nf_get_var_real
!                         (integer             ncid,
!                          integer             varid,
!                          real                rvals(1))
      external        nf_get_var_real

      integer         nf_put_var_double
!                         (integer             ncid,
!                          integer             varid,
!                          doubleprecision     dvals(1))
      external        nf_put_var_double

      integer         nf_get_var_double
!                         (integer             ncid,
!                          integer             varid,
!                          doubleprecision     dvals(1))
      external        nf_get_var_double

!
! single variable put/get routines:
!

      integer         nf_put_var1_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          character*1         text)
      external        nf_put_var1_text

      integer         nf_get_var1_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          character*1         text)
      external        nf_get_var1_text

      integer         nf_put_var1_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int1_t           i1val)
      external        nf_put_var1_int1

      integer         nf_get_var1_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int1_t           i1val)
      external        nf_get_var1_int1

      integer         nf_put_var1_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int2_t           i2val)
      external        nf_put_var1_int2

      integer         nf_get_var1_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          nf_int2_t           i2val)
      external        nf_get_var1_int2

      integer         nf_put_var1_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          integer             ival)
      external        nf_put_var1_int

      integer         nf_get_var1_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          integer             ival)
      external        nf_get_var1_int

      integer         nf_put_var1_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          real                rval)
      external        nf_put_var1_real

      integer         nf_get_var1_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          real                rval)
      external        nf_get_var1_real

      integer         nf_put_var1_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          doubleprecision     dval)
      external        nf_put_var1_double

      integer         nf_get_var1_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             index(1),
!                          doubleprecision     dval)
      external        nf_get_var1_double

!
! variable array put/get routines:
!

      integer         nf_put_vara_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          character(*)        text)
      external        nf_put_vara_text

      integer         nf_get_vara_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          character(*)        text)
      external        nf_get_vara_text

      integer         nf_put_vara_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_vara_int1

      integer         nf_get_vara_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_vara_int1

      integer         nf_put_vara_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_vara_int2

      integer         nf_get_vara_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_vara_int2

      integer         nf_put_vara_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             ivals(1))
      external        nf_put_vara_int

      integer         nf_get_vara_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             ivals(1))
      external        nf_get_vara_int

      integer         nf_put_vara_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          real                rvals(1))
      external        nf_put_vara_real

      integer         nf_get_vara_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          real                rvals(1))
      external        nf_get_vara_real

      integer         nf_put_vara_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          doubleprecision     dvals(1))
      external        nf_put_vara_double

      integer         nf_get_vara_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          doubleprecision     dvals(1))
      external        nf_get_vara_double

!
! strided variable put/get routines:
!

      integer         nf_put_vars_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          character(*)        text)
      external        nf_put_vars_text

      integer         nf_get_vars_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          character(*)        text)
      external        nf_get_vars_text

      integer         nf_put_vars_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_vars_int1

      integer         nf_get_vars_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_vars_int1

      integer         nf_put_vars_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_vars_int2

      integer         nf_get_vars_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_vars_int2

      integer         nf_put_vars_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             ivals(1))
      external        nf_put_vars_int

      integer         nf_get_vars_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             ivals(1))
      external        nf_get_vars_int

      integer         nf_put_vars_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          real                rvals(1))
      external        nf_put_vars_real

      integer         nf_get_vars_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          real                rvals(1))
      external        nf_get_vars_real

      integer         nf_put_vars_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          doubleprecision     dvals(1))
      external        nf_put_vars_double

      integer         nf_get_vars_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          doubleprecision     dvals(1))
      external        nf_get_vars_double

!
! mapped variable put/get routines:
!

      integer         nf_put_varm_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          character(*)        text)
      external        nf_put_varm_text

      integer         nf_get_varm_text
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          character(*)        text)
      external        nf_get_varm_text

      integer         nf_put_varm_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int1_t           i1vals(1))
      external        nf_put_varm_int1

      integer         nf_get_varm_int1
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int1_t           i1vals(1))
      external        nf_get_varm_int1

      integer         nf_put_varm_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int2_t           i2vals(1))
      external        nf_put_varm_int2

      integer         nf_get_varm_int2
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          nf_int2_t           i2vals(1))
      external        nf_get_varm_int2

      integer         nf_put_varm_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          integer             ivals(1))
      external        nf_put_varm_int

      integer         nf_get_varm_int
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          integer             ivals(1))
      external        nf_get_varm_int

      integer         nf_put_varm_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          real                rvals(1))
      external        nf_put_varm_real

      integer         nf_get_varm_real
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          real                rvals(1))
      external        nf_get_varm_real

      integer         nf_put_varm_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          doubleprecision     dvals(1))
      external        nf_put_varm_double

      integer         nf_get_varm_double
!                         (integer             ncid,
!                          integer             varid,
!                          integer             start(1),
!                          integer             count(1),
!                          integer             stride(1),
!                          integer             imap(1),
!                          doubleprecision     dvals(1))
      external        nf_get_varm_double


!     NetCDF-2.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!

!      
! functions in the fortran interface
!
      integer nccre
      integer ncopn
      integer ncddef
      integer ncdid
      integer ncvdef
      integer ncvid
      integer nctlen
      integer ncsfil

      external nccre
      external ncopn
      external ncddef
      external ncdid
      external ncvdef
      external ncvid
      external nctlen
      external ncsfil


      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool


!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble

      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)

!     
!     masks for the struct nc flag field; passed in as 'mode' arg to
!     nccreate and ncopen.
!     

!     read/write, 0 => readonly 
      parameter(ncrdwr = 1)
!     in create phase, cleared by ncendef 
      parameter(nccreat = 2)
!     on create destroy existing file 
      parameter(ncexcl = 4)
!     in define mode, cleared by ncendef 
      parameter(ncindef = 8)
!     synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
!     synchronise whole header on change (x'20')
      parameter(nchsync = 32)
!     numrecs has changed (x'40')
      parameter(ncndirty = 64)  
!     header info has changed (x'80')
      parameter(nchdirty = 128)
!     prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
!     do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
!     isa link (x'8000')
      parameter(nclink = 32768)

!     
!     'mode' arguments for nccreate and ncopen
!     
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)

!     
!     'size' argument to ncdimdef for an unlimited dimension
!     
      integer ncunlim
      parameter(ncunlim = 0)

!     
!     attribute id to put/get a global attribute
!     
      parameter(ncglobal  = 0)

!     
!     advisory maximums:
!     
      parameter(maxncop = 64)
      parameter(maxncdim = 1024)
      parameter(maxncatt = 8192)
      parameter(maxncvar = 8192)
!     not enforced 
      parameter(maxncnam = 256)
      parameter(maxvdims = maxncdim)

!     
!     global netcdf error status variable
!     initialized in error.c
!     

!     no error 
      parameter(ncnoerr = nf_noerr)
!     not a netcdf id 
      parameter(ncebadid = nf_ebadid)
!     too many netcdfs open 
      parameter(ncenfile = -31)   ! nc_syserr
!     netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
!     invalid argument 
      parameter(nceinval = nf_einval)
!     write to read only 
      parameter(nceperm = nf_eperm)
!     operation not allowed in data mode 
      parameter(ncenotin = nf_enotindefine )   
!     operation not allowed in define mode 
      parameter(nceindef = nf_eindefine)   
!     coordinates out of domain 
      parameter(ncecoord = nf_einvalcoords)
!     maxncdims exceeded 
      parameter(ncemaxds = nf_emaxdims)
!     string match to name in use 
      parameter(ncename = nf_enameinuse)   
!     attribute not found 
      parameter(ncenoatt = nf_enotatt)
!     maxncattrs exceeded 
      parameter(ncemaxat = nf_emaxatts)
!     not a netcdf data type 
      parameter(ncebadty = nf_ebadtype)
!     invalid dimension id 
      parameter(ncebadd = nf_ebaddim)
!     ncunlimited in the wrong index 
      parameter(nceunlim = nf_eunlimpos)
!     maxncvars exceeded 
      parameter(ncemaxvs = nf_emaxvars)
!     variable not found 
      parameter(ncenotvr = nf_enotvar)
!     action prohibited on ncglobal varid 
      parameter(nceglob = nf_eglobal)
!     not a netcdf file 
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname) 
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)

!     
!     global options variable. used to determine behavior of error handler.
!     initialized in lerror.c
!     
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)

!
!     default fill values.  these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub

      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub = 9.9692099683868690e+36)

!     NetCDF-4.
!     This is part of netCDF-4. Copyright 2006, UCAR, See COPYRIGHT
!     file for distribution information.

!     Netcdf version 4 fortran interface.

!     $Id: netcdf4.inc,v 1.27 2010/01/20 13:23:09 ed Exp $

!     New netCDF-4 types.
      integer nf_ubyte
      integer nf_ushort
      integer nf_uint
      integer nf_int64
      integer nf_uint64
      integer nf_string
      integer nf_vlen
      integer nf_opaque
      integer nf_enum
      integer nf_compound

      parameter (nf_ubyte = 7)
      parameter (nf_ushort = 8)
      parameter (nf_uint = 9)
      parameter (nf_int64 = 10)
      parameter (nf_uint64 = 11)
      parameter (nf_string = 12)
      parameter (nf_vlen = 13)
      parameter (nf_opaque = 14)
      parameter (nf_enum = 15)
      parameter (nf_compound = 16)

!     New netCDF-4 fill values.
      integer           nf_fill_ubyte
      integer           nf_fill_ushort
!      real              nf_fill_uint
!      real              nf_fill_int64
!      real              nf_fill_uint64
      parameter (nf_fill_ubyte = 255)
      parameter (nf_fill_ushort = 65535)

!     New constants.
      integer nf_format_netcdf4
      parameter (nf_format_netcdf4 = 3)

      integer nf_format_netcdf4_classic
      parameter (nf_format_netcdf4_classic = 4)

      integer nf_netcdf4
      parameter (nf_netcdf4 = 4096)

      integer nf_classic_model
      parameter (nf_classic_model = 256)

      integer nf_chunk_seq
      parameter (nf_chunk_seq = 0)
      integer nf_chunk_sub
      parameter (nf_chunk_sub = 1)
      integer nf_chunk_sizes
      parameter (nf_chunk_sizes = 2)

      integer nf_endian_native
      parameter (nf_endian_native = 0)
      integer nf_endian_little
      parameter (nf_endian_little = 1)
      integer nf_endian_big
      parameter (nf_endian_big = 2)

!     For NF_DEF_VAR_CHUNKING
      integer nf_chunked
      parameter (nf_chunked = 0)
      integer nf_contiguous
      parameter (nf_contiguous = 1)

!     For NF_DEF_VAR_FLETCHER32
      integer nf_nochecksum
      parameter (nf_nochecksum = 0)
      integer nf_fletcher32
      parameter (nf_fletcher32 = 1)

!     For NF_DEF_VAR_DEFLATE
      integer nf_noshuffle
      parameter (nf_noshuffle = 0)
      integer nf_shuffle
      parameter (nf_shuffle = 1)

!     For NF_DEF_VAR_SZIP
      integer nf_szip_ec_option_mask
      parameter (nf_szip_ec_option_mask = 4)
      integer nf_szip_nn_option_mask
      parameter (nf_szip_nn_option_mask = 32)

!     For NF_VAR_PAR_ACCESS.
      integer nf_independent
      parameter (nf_independent = 0)
      integer nf_collective
      parameter (nf_collective = 1)

!     New error codes.
      integer nf_ehdferr        ! Error at HDF5 layer. 
      parameter (nf_ehdferr = -101)
      integer nf_ecantread      ! Can't read. 
      parameter (nf_ecantread = -102)
      integer nf_ecantwrite     ! Can't write. 
      parameter (nf_ecantwrite = -103)
      integer nf_ecantcreate    ! Can't create. 
      parameter (nf_ecantcreate = -104)
      integer nf_efilemeta      ! Problem with file metadata. 
      parameter (nf_efilemeta = -105)
      integer nf_edimmeta       ! Problem with dimension metadata. 
      parameter (nf_edimmeta = -106)
      integer nf_eattmeta       ! Problem with attribute metadata. 
      parameter (nf_eattmeta = -107)
      integer nf_evarmeta       ! Problem with variable metadata. 
      parameter (nf_evarmeta = -108)
      integer nf_enocompound    ! Not a compound type. 
      parameter (nf_enocompound = -109)
      integer nf_eattexists     ! Attribute already exists. 
      parameter (nf_eattexists = -110)
      integer nf_enotnc4        ! Attempting netcdf-4 operation on netcdf-3 file.   
      parameter (nf_enotnc4 = -111)
      integer nf_estrictnc3     ! Attempting netcdf-4 operation on strict nc3 netcdf-4 file.   
      parameter (nf_estrictnc3 = -112)
      integer nf_enotnc3        ! Attempting netcdf-3 operation on netcdf-4 file.   
      parameter (nf_enotnc3 = -113)
      integer nf_enopar         ! Parallel operation on file opened for non-parallel access.   
      parameter (nf_enopar = -114)
      integer nf_eparinit       ! Error initializing for parallel access.   
      parameter (nf_eparinit = -115)
      integer nf_ebadgrpid      ! Bad group ID.   
      parameter (nf_ebadgrpid = -116)
      integer nf_ebadtypid      ! Bad type ID.   
      parameter (nf_ebadtypid = -117)
      integer nf_etypdefined    ! Type has already been defined and may not be edited. 
      parameter (nf_etypdefined = -118)
      integer nf_ebadfield      ! Bad field ID.   
      parameter (nf_ebadfield = -119)
      integer nf_ebadclass      ! Bad class.   
      parameter (nf_ebadclass = -120)
      integer nf_emaptype       ! Mapped access for atomic types only.   
      parameter (nf_emaptype = -121)
      integer nf_elatefill      ! Attempt to define fill value when data already exists. 
      parameter (nf_elatefill = -122)
      integer nf_elatedef       ! Attempt to define var properties, like deflate, after enddef. 
      parameter (nf_elatedef = -123)
      integer nf_edimscale      ! Probem with HDF5 dimscales. 
      parameter (nf_edimscale = -124)
      integer nf_enogrp       ! No group found.
      parameter (nf_enogrp = -125)


!     New functions.

!     Parallel I/O.
      integer nf_create_par
      external nf_create_par

      integer nf_open_par
      external nf_open_par

      integer nf_var_par_access
      external nf_var_par_access

!     Functions to handle groups.
      integer nf_inq_ncid
      external nf_inq_ncid

      integer nf_inq_grps
      external nf_inq_grps

      integer nf_inq_grpname
      external nf_inq_grpname

      integer nf_inq_grpname_full
      external nf_inq_grpname_full

      integer nf_inq_grpname_len
      external nf_inq_grpname_len

      integer nf_inq_grp_parent
      external nf_inq_grp_parent

      integer nf_inq_grp_ncid
      external nf_inq_grp_ncid

      integer nf_inq_grp_full_ncid
      external nf_inq_grp_full_ncid

      integer nf_inq_varids
      external nf_inq_varids

      integer nf_inq_dimids
      external nf_inq_dimids

      integer nf_def_grp
      external nf_def_grp

!     New options for netCDF variables.
      integer nf_def_var_deflate
      external nf_def_var_deflate

      integer nf_inq_var_deflate
      external nf_inq_var_deflate

      integer nf_def_var_fletcher32
      external nf_def_var_fletcher32

      integer nf_inq_var_fletcher32
      external nf_inq_var_fletcher32

      integer nf_def_var_chunking
      external nf_def_var_chunking

      integer nf_inq_var_chunking
      external nf_inq_var_chunking

      integer nf_def_var_fill
      external nf_def_var_fill

      integer nf_inq_var_fill
      external nf_inq_var_fill

      integer nf_def_var_endian
      external nf_def_var_endian

      integer nf_inq_var_endian
      external nf_inq_var_endian

!     User defined types.
      integer nf_inq_typeids
      external nf_inq_typeids

      integer nf_inq_typeid
      external nf_inq_typeid

      integer nf_inq_type
      external nf_inq_type

      integer nf_inq_user_type
      external nf_inq_user_type

!     User defined types - compound types.
      integer nf_def_compound
      external nf_def_compound

      integer nf_insert_compound
      external nf_insert_compound

      integer nf_insert_array_compound
      external nf_insert_array_compound

      integer nf_inq_compound
      external nf_inq_compound

      integer nf_inq_compound_name
      external nf_inq_compound_name

      integer nf_inq_compound_size
      external nf_inq_compound_size

      integer nf_inq_compound_nfields
      external nf_inq_compound_nfields

      integer nf_inq_compound_field
      external nf_inq_compound_field

      integer nf_inq_compound_fieldname
      external nf_inq_compound_fieldname

      integer nf_inq_compound_fieldindex
      external nf_inq_compound_fieldindex

      integer nf_inq_compound_fieldoffset
      external nf_inq_compound_fieldoffset

      integer nf_inq_compound_fieldtype
      external nf_inq_compound_fieldtype

      integer nf_inq_compound_fieldndims
      external nf_inq_compound_fieldndims

      integer nf_inq_compound_fielddim_sizes
      external nf_inq_compound_fielddim_sizes

!     User defined types - variable length arrays.
      integer nf_def_vlen
      external nf_def_vlen

      integer nf_inq_vlen
      external nf_inq_vlen

      integer nf_free_vlen
      external nf_free_vlen

!     User defined types - enums.
      integer nf_def_enum
      external nf_def_enum

      integer nf_insert_enum
      external nf_insert_enum

      integer nf_inq_enum
      external nf_inq_enum

      integer nf_inq_enum_member
      external nf_inq_enum_member

      integer nf_inq_enum_ident
      external nf_inq_enum_ident

!     User defined types - opaque.
      integer nf_def_opaque
      external nf_def_opaque

      integer nf_inq_opaque
      external nf_inq_opaque

!     Write and read attributes of any type, including user defined
!     types.
      integer nf_put_att
      external nf_put_att
      integer nf_get_att
      external nf_get_att

!     Write and read variables of any type, including user defined
!     types.
      integer nf_put_var
      external nf_put_var
      integer nf_put_var1
      external nf_put_var1
      integer nf_put_vara
      external nf_put_vara
      integer nf_put_vars
      external nf_put_vars
      integer nf_get_var
      external nf_get_var
      integer nf_get_var1
      external nf_get_var1
      integer nf_get_vara
      external nf_get_vara
      integer nf_get_vars
      external nf_get_vars

!     64-bit int functions.
      integer nf_put_var1_int64
      external nf_put_var1_int64
      integer nf_put_vara_int64
      external nf_put_vara_int64
      integer nf_put_vars_int64
      external nf_put_vars_int64
      integer nf_put_varm_int64
      external nf_put_varm_int64
      integer nf_put_var_int64
      external nf_put_var_int64
      integer nf_get_var1_int64
      external nf_get_var1_int64
      integer nf_get_vara_int64
      external nf_get_vara_int64
      integer nf_get_vars_int64
      external nf_get_vars_int64
      integer nf_get_varm_int64
      external nf_get_varm_int64
      integer nf_get_var_int64
      external nf_get_var_int64

!     For helping F77 users with VLENs.
      integer nf_get_vlen_element
      external nf_get_vlen_element
      integer nf_put_vlen_element
      external nf_put_vlen_element

!     For dealing with file level chunk cache.
      integer nf_set_chunk_cache
      external nf_set_chunk_cache
      integer nf_get_chunk_cache
      external nf_get_chunk_cache

!     For dealing with per variable chunk cache.
      integer nf_set_var_chunk_cache
      external nf_set_var_chunk_cache
      integer nf_get_var_chunk_cache
      external nf_get_var_chunk_cache
# 17 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/boundarydata.F90" 2
  private
  integer, parameter :: ptrtim=12, ptrlon=1

  type boundarydata_type
     integer           :: ncid
     integer           :: fieldcnt
     integer           :: nm
     integer           :: np
     integer           :: latsiz
     integer           :: levsiz
     integer           :: ncolsiz
     integer           :: timsiz
     integer           :: vertextrap
     logical           :: iszonal, isncol
     integer           :: ndims
     integer           :: thistimedim
     integer           :: psid
     integer  :: map(4)
     integer  :: dimids(4)
     integer,  pointer :: dataid(:)           
     integer,  pointer :: columnmap(:)
     integer, pointer  :: start(:,:)
     integer, pointer  :: count(:,:)
     real(r8), pointer :: lat(:)
     real(r8), pointer :: zi(:)
     real(r8), pointer :: pin(:)
     real(r8), pointer :: cdates(:)
     real(r8), pointer :: fields(:,:,:,:,:)
     real(r8), pointer :: datainst(:,:,:,:)
     real(r8), pointer :: hybi(:)
     real(r8), pointer :: ps(:,:,:)
  end type boundarydata_type

  public boundarydata_init
  public boundarydata_update
  public boundarydata_vert_interp
  public boundarydata_type

contains
  subroutine boundarydata_init(bndyfilename,phys_state,fieldnames,fieldcnt,bndydata,vertextrap)
    implicit none    
    character(len=*),intent(in) :: bndyfilename
    type(physics_state), intent(in):: phys_state(begchunk:endchunk)
    integer,intent(in) :: fieldcnt
    character(len=*), intent(in) :: fieldnames(fieldcnt)
    type(boundarydata_type),intent(out) :: bndydata 
    integer,intent(in), optional :: vertextrap ! if 0 set values outside output grid to 0
                                               ! if 1 set to boundary value
                                               ! if 2 set to cyclic boundaries 
                                               ! if 3 leave on input data grid and extrapolate later
    real(r8), pointer :: datain(:,:,:,:,:)
    integer :: lchnk
    
    bndydata%fieldcnt=fieldcnt
    if(present(vertextrap)) then
       bndydata%vertextrap=vertextrap
    else
       bndydata%vertextrap=0
    end if
    nullify(bndydata%fields)

    call boundarydata_read(phys_state,bndyfilename,fieldcnt,fieldnames,bndydata,datain)

    if(bndydata%iszonal) then
       call boundarydata_interpolate(phys_state,datain,bndydata)

       allocate(bndydata%datainst(size(bndydata%fields,1),size(bndydata%fields,2), &
            begchunk:endchunk,bndydata%fieldcnt))
    
       deallocate(datain)    
    end if
  end subroutine boundarydata_init

  subroutine boundarydata_update(phys_state, bndydata, update_out)
    use interpolate_data,only : get_timeinterp_factors
    type(physics_state), intent(in) :: phys_state(begchunk:endchunk)
    type(boundarydata_type), intent(inout) :: bndydata
    logical, intent(out), optional :: update_out
    real(r8) :: cdate
    integer :: nm, np, lchnk, j, k, fld, cols, cole, ncol, ndims
    real(r8) :: fact1, fact2
    real(r8), allocatable :: datain(:,:,:,:,:)
    logical :: update
    integer :: latspan
    integer :: kmax
    integer :: count(4), start(4), ierr
    
    call get_data_bounding_date_indices(bndydata%cdates,bndydata%nm,bndydata%np,cdate,update)
    if(present(update_out)) update_out=update
    nm= bndydata%nm
    np= bndydata%np

    call get_timeinterp_factors(.true., np, bndydata%cdates(nm), bndydata%cdates(np), &
         cdate, fact1, fact2, 'physics/cam1/boundarydata.F90 ')

    if(size(bndydata%fields,5).eq.2) then 
       nm=1
       np=2
       if(update) then ! we need to read in the next month and interpolate
          if(bndydata%isncol) then
             bndydata%fields(:,:,:,:,nm)=bndydata%fields(:,:,:,:,np)
             do lchnk=begchunk,endchunk
                ncol=phys_state(lchnk)%ncol
                cols=1
                cole=cols+bndydata%count(cols,lchnk)-1
                do while(cole<=ncol)
                   
                   if(bndydata%levsiz==1) then               
                      ndims=2
                      start=(/bndydata%start(cols,lchnk),bndydata%np,-1,-1/)
                      count=(/bndydata%count(cols,lchnk),1,-1,-1/)
                   else
                      ndims=3
                      start=(/bndydata%start(cols,lchnk),bndydata%levsiz,bndydata%np,-1/)
                      count=(/bndydata%count(cols,lchnk),1,1,-1/)
                   end if
                   do fld=1,bndydata%fieldcnt
                     ! call handle_ncerr( nf90_get_var(bndydata%ncid, bndydata%dataid(fld) , &
                      !     bndydata%fields(cols:cole,:,lchnk,fld,np),  &
                      !     start(1:ndims), count(1:ndims)),&
                      !     'physics/cam1/boundarydata.F90 ',137)
						   !!!  wrap_get_vara_realx (nfid, varid, start, count, arr)
                     call wrap_get_vara_realx( bndydata%ncid, bndydata%dataid(fld) , &
                           start(1:ndims), count(1:ndims),& 
                           bndydata%fields(cols:cole,:,lchnk,fld,np) )
                   end do
                   if(cols==ncol) exit
                   cols=cols+bndydata%count(cols,lchnk)
                   cole=cols+bndydata%count(cols,lchnk)-1
                end do
             end do

          else
             allocate(datain(ptrlon,bndydata%levsiz,bndydata%latsiz,1,bndydata%fieldcnt))
             if(masterproc) then
                count(1)=ptrlon
                count(2)=bndydata%levsiz
                count(3)=bndydata%latsiz
                count(4)=1
                start(1)=1
                start(2)=1
                start(3)=1
                start(4)=bndydata%np
                print *, 'boundarydata reading data for month: ',bndydata%np
                do fld=1,bndydata%fieldcnt
                  ! call handle_ncerr( nf90_get_var(bndydata%ncid, bndydata%dataid(fld), &
                  !      datain(:,:,:,:,fld), start, count),'physics/cam1/boundarydata.F90 ',163)
				   call wrap_get_vara_realx(bndydata%ncid, bndydata%dataid(fld), &
                        start, count,datain(:,:,:,:,fld))
                end do
             end if



             bndydata%fields(:,:,:,:,nm) = bndydata%fields(:,:,:,:,np) 
             call boundarydata_interpolate(phys_state,datain,bndydata)
             deallocate(datain)
          end if
       end if
    end if
    kmax = size(bndydata%fields,2)
   
    do fld=1,bndydata%fieldcnt
       do lchnk=begchunk,endchunk
          if(bndydata%isncol) then
             latspan = phys_state(lchnk)%ncol
          else
             latspan=phys_state(lchnk)%ulatcnt
          end if
          do k=1,kmax
             do j=1,latspan
                bndydata%datainst(j,k,lchnk,fld)=bndydata%fields(j,k,lchnk,fld,nm)*fact1 + &
                     bndydata%fields(j,k,lchnk,fld,np)*fact2
             end do
          end do
       end do
    end do
  end subroutine boundarydata_update


  subroutine boundarydata_read(phys_state,bndyfilename,fieldcnt,fieldnames,bndydata,datain)
    !----------------------------------------------------------------------- 
    ! 
    ! Purpose: 
    ! Do initial read of time-variant boundary dataset, containing
    ! 12 monthly fields as a function of latitude and pressure.  Determine the two
    ! consecutive months between which the current date lies.
    ! 
    ! Method: 
    ! 
    ! Author: NCAR CMS
    !-----------------------------------------------------------------------
    use ioFileMod, only : getfil


    implicit none    
    type(physics_state), intent(in) :: phys_state(begchunk:endchunk)
    character(len=*),intent(in) :: bndyfilename
    integer,intent(in) :: fieldcnt
    character(len=*), intent(in) :: fieldnames(fieldcnt)
    type(boundarydata_type), intent(out) :: bndydata
    real(r8), pointer :: datain(:,:,:,:,:)  ! 
    !
    ! Local variables
    !
    integer :: londimid
    integer :: latdimid
    integer :: ncoldimid
    integer :: levdimid
    integer :: ilevdimid
    integer :: timdimid
    integer :: ndims
    integer :: dimlen
    integer :: ilevsiz
    integer :: ncolsiz
    !character(len=nf90_max_name) :: dimname  !!
	character(len=nf_max_name) :: dimname  !!


!    integer ::  ncid              ! netcdf id for file
    integer ::  dateid            ! netcdf id for date variable
    integer ::  secid             ! netcdf id for seconds variable
    integer ::  lonid             ! netcdf id for longitude variable
    integer ::  ncolid             ! netcdf id for longitude variable
    integer ::  latid             ! netcdf id for latitude variable
    integer ::  levid             ! netcdf id for level variable
    integer ::  timid             ! netcdf id for time variable
    integer :: hybid

    integer ::  dataid  ! netcdf id for data fields

    integer ::  lonsiz            ! size of longitude dimension on tracer dataset
    integer ::  levsiz            ! size of level dimension on tracer dataset
    integer ::  latsiz            ! size of latitude dimension on tracer dataset

    integer ::  j,n,k,nt,id          ! indices
    integer ::  ki,ko,ji,jo       ! indices
    integer :: date_tr(ptrtim), sec_tr(ptrtim)

    integer :: dimids(4), start(4), count(4)
    integer, pointer :: columnmap(:)
    real(r8) :: calday        ! current calendar day
    real(r8), pointer :: pin(:)
    real(r8), allocatable :: tmp_ps(:,:), tmp_fld(:,:,:)
    integer :: mincid,maxcid
    real(r8), allocatable, target:: lati(:)
    integer :: cols, cole
    integer ::  ierr, dimcnt
    integer ::  i, ncol, lchnk
	integer :: nvars, ngatts,varname, xtype,natts !!! sxj add for nf_
    character(len=256) :: locfn    ! netcdf local filename to open 

    !
    !-----------------------------------------------------------------------
    !
    ! SPMD: Master reads dataset and does broadcast.  All subsequent interpolation is
    !       done in every process.  This is not required, one could remove this conditional
    !       and read the dataset independently on each task.
    !
    if(masterproc) then
       write(6,*)'boundarydata_read: Reading from: ', trim(bndyfilename)

    end if

    call getfil(bndyfilename, locfn)
    !call handle_ncerr( nf90_open(locfn, 0, bndydata%ncid),&
     !    'physics/cam1/boundarydata.F90 ',283)
	 call wrap_open(locfn, 0, bndydata%ncid)

    !       write(6,*)'boundarydata_read: NCOPN returns id ',bndydata%ncid,' for file ',trim(locfn)
    !
    !------------------------------------------------------------------------
    ! Read tracer data
    !------------------------------------------------------------------------
    !
    ! Get dimension info
    !
    nullify(columnmap)
    nullify(pin)
   ! call handle_ncerr( nf90_inquire(bndydata%ncid, bndydata%ndims, unlimiteddimid=timdimid), &
    !     'physics/cam1/boundarydata.F90 ',297)
		!!  nf_inq(ncidin, ndim, nvars, ngatts, unlimdimid) 
	 call handle_ncerr( nf_inq(bndydata%ncid, bndydata%ndims,nvars, ngatts, timdimid), &
         'physics/cam1/boundarydata.F90 ',300)
    ncolsiz=-1
    levsiz=-1
    lonsiz=-1
    latsiz=-1
    do i=1,bndydata%ndims
       !call handle_ncerr( nf90_inquire_dimension(bndydata%ncid, i, dimname, dimlen),&
        !    'physics/cam1/boundarydata.F90 ',307)
		!!	wrap_inq_dim (nfid, dimid, dimname, dimlen)
       call wrap_inq_dim(bndydata%ncid, i, dimname, dimlen)
       if (dimname(1:3).eq.'lat') then
          latdimid=i
          latsiz=dimlen
       else if (dimname(1:3) .eq.'lon') then
          londimid=i
          lonsiz=dimlen
       else if (dimname(1:4) .eq. 'ncol') then
          ncoldimid=i
          ncolsiz=dimlen
          bndydata%isncol=.true. 
       else if (dimname(1:3) .eq. 'lev') then
          levdimid=i
          levsiz=dimlen
       else if (dimname(1:4) .eq. 'ilev') then
          ilevdimid=i
          ilevsiz=dimlen
       else if (dimname(1:4) .eq. 'time') then
          if(timdimid/=i) then
             timdimid=i
          end if
          bndydata%timsiz=dimlen
       else
          write(6,*) 'Warning: do not know how to handle dimension ',&
               trim(dimname), ' in boundarydata.F90:313'
       end if
    end do

    if (latsiz>0 .and. lonsiz<=1) then
       bndydata%iszonal=.true.
       allocate(bndydata%lat(latsiz))
    end if
    if(bndydata%isncol) then
!       allocate (columnmap(ncolsiz))
!       call handle_ncerr( nf90_inq_varid(bndydata%ncid, 'ncol'    , ncolid),&
!            'physics/cam1/boundarydata.F90 ',344)
!       call handle_ncerr( nf90_get_var(bndydata%ncid,ncolid,columnmap), &
!            'physics/cam1/boundarydata.F90 ',346)
       if(levsiz>0) then
          allocate(bndydata%fields(pcols,levsiz,begchunk:endchunk,fieldcnt,2))

          !ierr = nf90_inq_varid(bndydata%ncid, 'PS', bndydata%psid)
		  ierr = nf_inq_varid(bndydata%ncid, 'PS', bndydata%psid)
          !if(ierr.eq.NF90_NOERR) then
		  if(ierr.eq.NF_NOERR) then
             allocate(bndydata%ps(pcols,begchunk:endchunk,2))
             allocate(bndydata%hybi(levsiz+1))
             !call handle_ncerr(nf90_inq_varid(bndydata%ncid,'hybi',hybid),&
             !     'physics/cam1/boundarydata.F90 ',357)
			 call handle_ncerr(nf_inq_varid(bndydata%ncid,'hybi',hybid),&
                  'physics/cam1/boundarydata.F90 ',359)
             !call handle_ncerr( nf90_get_var(bndydata%ncid, hybid, bndydata%hybi ),&
             !     'physics/cam1/boundarydata.F90 ',361)
			 call wrap_get_var_realx(bndydata%ncid, hybid, bndydata%hybi )
				  !wrap_get_var_realx (nfid, varid, arr)
          else 
             call endrun('Did not recognize a vertical coordinate variable')
          end if
       else
          levsiz=1
          allocate(bndydata%fields(pcols,1,begchunk:endchunk,fieldcnt,2))
       end if
    else
       allocate(datain(lonsiz,levsiz,latsiz,2,fieldcnt))
       !
       ! Check dimension info
       !
       if (lonsiz/=ptrlon) then
          call endrun ('BOUNDARYDATA_READ: longitude dependence not implemented')
       endif

       if (bndydata%timsiz /= ptrtim) then
          write(6,*)'BOUNDARYDATA_READ: timsiz=',bndydata%timsiz,' must = ptrtim=',ptrtim
          call endrun
       end if
       if( bndydata%vertextrap.lt.3) then
          allocate(pin(levsiz))
       else
          allocate(bndydata%pin(levsiz))
          pin => bndydata%pin
       end if
       allocate(bndydata%lat(latsiz))


       allocate(datain(ptrlon,levsiz,latsiz,2,fieldcnt))

       !call handle_ncerr( nf90_inq_varid(bndydata%ncid, 'lat'    , latid),&
        !    'physics/cam1/boundarydata.F90 ',396)
       call handle_ncerr( nf_inq_varid(bndydata%ncid, 'lat'    , latid),&
            'physics/cam1/boundarydata.F90 ',398)
    end if
    !
    ! Determine necessary dimension and variable id's
    !
    allocate(bndydata%cdates(bndydata%timsiz))

    !call handle_ncerr(nf90_inq_varid(bndydata%ncid, 'date'   , dateid), &
     !    'physics/cam1/boundarydata.F90 ',406)
	call handle_ncerr(nf_inq_varid(bndydata%ncid, 'date'   , dateid), &
         'physics/cam1/boundarydata.F90 ',408)
  !  call handle_ncerr( nf90_get_var(bndydata%ncid, dateid, date_tr),&
   !      'physics/cam1/boundarydata.F90 ',410)
    call  wrap_get_var_realx(bndydata%ncid, dateid, date_tr)
    !ierr = nf90_inq_varid(bndydata%ncid, 'datesec', secid)
     ierr = nf_inq_varid(bndydata%ncid, 'datesec', secid)
    !if(ierr==NF90_NOERR) then
	if(ierr==NF_NOERR) then
       !call handle_ncerr( nf90_get_var(bndydata%ncid, secid , sec_tr),&
        !    'physics/cam1/boundarydata.F90 ',417)
		call handle_ncerr( nf_get_var_int(bndydata%ncid, secid , sec_tr),&
            'physics/cam1/boundarydata.F90 ',419)
    else
       sec_tr=0
    end if

    if (mod(date_tr(1),10000)/100 /= 1) then
       call endrun ('(boundarydata_read): error when cycling data: 1st month must be 1')
    end if
    if (mod(date_tr(ptrtim),10000)/100 /= 12) then
       call endrun ('(boundarydata_read): error when cycling data: last month must be 12')
    end if
    !
    !    return the calander dates of the file data
    !
    do n=1,ptrtim
       call bnddyi(date_tr(n), sec_tr(n), bndydata%cdates(n))
    end do
!    else
!       call handle_ncerr( nf90_inq_varid(bndydata%ncid, 'time', dateid),&
!            'physics/cam1/boundarydata.F90 ',438)
!
!       call handle_ncerr( nf90_get_var(bndydata%ncid, dateid, bndydata%cdates),&
!            'physics/cam1/boundarydata.F90 ',441)
!
!    end if
# 452 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/boundarydata.F90"
 bndydata%nm=12
 bndydata%np=1
 call get_data_bounding_date_indices(bndydata%cdates,bndydata%nm,bndydata%np)



    !
    ! Obtain entire date and sec variables. Assume that will always
    ! cycle over 12 month data.
    !
    !
    ! Obtain input data latitude and level arrays.
    !
    if(bndydata%iszonal) then
       !call handle_ncerr( nf90_get_var(bndydata%ncid, latid, bndydata%lat),&
       !     'physics/cam1/boundarydata.F90 ',467)
	   call wrap_get_var_realx (bndydata%ncid, latid, bndydata%lat)
       !ierr = nf90_inq_varid(bndydata%ncid, 'lev'    , levid)
	   ierr = nf_inq_varid(bndydata%ncid, 'lev'    , levid)
       !call handle_ncerr( nf90_get_var(bndydata%ncid, levid, pin ),&
       !     'physics/cam1/boundarydata.F90 ',472)
	  call  wrap_get_var_realx(bndydata%ncid, levid, pin )
    end if

    allocate(bndydata%dataid(fieldcnt))
    if(masterproc) then
       print *, 'boundarydata reading data for months: ',bndydata%nm,bndydata%np
    end if
    do i=1,fieldcnt
       !call handle_ncerr( nf90_inq_varid(bndydata%ncid, fieldnames(i)   , bndydata%dataid(i)),&
       !     'physics/cam1/boundarydata.F90 ',482)
       call wrap_get_var_realx(bndydata%ncid, fieldnames(i)   , bndydata%dataid(i))
    end do
    if(bndydata%isncol) then
       allocate(bndydata%start(pcols,begchunk:endchunk), &
            bndydata%count(pcols,begchunk:endchunk))

!
!  For i/o efficiency we read in a block of data which includes the data needed on this 
!  processor but which may in fact include data not needed here.  physics cids are just the
!  offset into the file.
! 

       bndydata%start=-1
       bndydata%count=1
       mincid=2147483647
       maxcid=-1
       do lchnk=begchunk,endchunk
          ncol=phys_state(lchnk)%ncol
          i=minval(phys_state(lchnk)%cid(1:ncol)) 
          if(i < mincid) mincid = i
          i=maxval(phys_state(lchnk)%cid(1:ncol))
          if(i > maxcid) maxcid = i
       end do

       allocate(tmp_ps(mincid:maxcid,2))
       start=(/mincid,bndydata%nm,1,-1/)
       if(bndydata%np>bndydata%nm) then
          count=(/maxcid-mincid+1,2,-1,-1/)
       else
          count=(/maxcid-mincid+1,1,-1,-1/)
       end if
       if(associated(bndydata%ps) ) then
          !call handle_ncerr( nf90_get_var(bndydata%ncid, bndydata%psid , &
           !    tmp_ps(:,1:count(2)), start(1:2), &
           !    count(1:2)),&
            !   'physics/cam1/boundarydata.F90 ',518)
         call wrap_get_vara_realx (bndydata%ncid, bndydata%psid , &
                start(1:2), &
               count(1:2),tmp_ps(:,1:count(2)))
          if(bndydata%np<bndydata%nm) then
             start(2)=bndydata%np
             !call handle_ncerr( nf90_get_var(bndydata%ncid, bndydata%psid , &
                 ! tmp_ps(:,2:2), start(1:2), &
                  !count(1:2)),&
                  !'physics/cam1/boundarydata.F90 ',527)
          call   wrap_get_vara_realx(bndydata%ncid, bndydata%psid , &
                   start(1:2), &
                  count(1:2),tmp_ps(:,2:2))
			!!wrap_get_vara_realx (nfid, varid, start, count, arr)
          end if

          do lchnk=begchunk,endchunk
             do i=1,phys_state(lchnk)%ncol
                bndydata%ps(i ,lchnk,:) = tmp_ps(phys_state(lchnk)%cid(i),:)
             end do
          end do
          deallocate(tmp_ps)

       end if

       if(levsiz>1) then
          dimcnt=3
       else
          dimcnt=2
       end if
       start(2)=1
       count(2)=levsiz
                
       if(bndydata%np>bndydata%nm) then
          count(dimcnt)=2
       else
          count(dimcnt)=1
       end if
       start(dimcnt)=bndydata%nm

       allocate(tmp_fld(mincid:maxcid,count(2),2))

       do i=1,fieldcnt
          !call handle_ncerr( nf90_get_var(bndydata%ncid, bndydata%dataid(i) , &
          !     tmp_fld(:,:,1:count(dimcnt)), &
          !     start(1:dimcnt), count(1:dimcnt)),&
          !     'physics/cam1/boundarydata.F90 ',564)
         call  wrap_get_vara_realx(bndydata%ncid, bndydata%dataid(i) , &
               start(1:dimcnt), count(1:dimcnt), &
               tmp_fld(:,:,1:count(dimcnt)) )
!!wrap_get_vara_realx (nfid, varid, start, count, arr)
          do lchnk=begchunk,endchunk
             do n=1,phys_state(lchnk)%ncol
                bndydata%fields(n,:,lchnk,i,1:count(dimcnt)) = tmp_fld(phys_state(lchnk)%cid(i),:,:)
             end do
          end do
       end do
       if(bndydata%np<bndydata%nm) then
          start(dimcnt)=bndydata%np
          do i=1,fieldcnt
             !call handle_ncerr( nf90_get_var(bndydata%ncid, bndydata%dataid(i) , &
             !     tmp_fld(:,:,2:2), &
             !     start(1:dimcnt), count(1:dimcnt)),&
             !     'physics/cam1/boundarydata.F90 ',581)
		  call  wrap_get_vara_realx(bndydata%ncid, bndydata%dataid(i) , &
                  start(1:dimcnt), count(1:dimcnt), &
                  tmp_fld(:,:,2:2))
             do lchnk=begchunk,endchunk
                do n=1,phys_state(lchnk)%ncol
                   bndydata%fields(n,:,lchnk,i,1:count(dimcnt)) = tmp_fld(phys_state(lchnk)%cid(i),:,:)
                end do
             end do
          end do
       end if
       deallocate(tmp_fld)
!       deallocate(columnmap)
    else
       !
       ! get the dimension orientation info from the first variable assume but verify that
       ! all variables requested have the same orientation
       !  
       allocate(bndydata%start(4,1),bndydata%count(4,1))
       !call handle_ncerr( nf90_inquire_variable(bndydata%ncid,bndydata%dataid(1), &
           ! ndims=bndydata%ndims,dimids=bndydata%dimids),'physics/cam1/boundarydata.F90 ',601)
      !! wrap_inq_var (nfid, varid, varname, xtype, ndims, dimids, natts)
       call wrap_inq_var(bndydata%ncid,bndydata%dataid(1), varname, xtype, &
            bndydata%ndims,bndydata%dimids, natts)
       bndydata%start=1
       do id=1,bndydata%ndims
          if(bndydata%dimids(id).eq.londimid) then
             bndydata%map(id)=1
             bndydata%count(id,1)=lonsiz
          else if(bndydata%dimids(id).eq.levdimid) then
             bndydata%map(id)=lonsiz
             bndydata%count(id,1)=levsiz
          else if(bndydata%dimids(id).eq.latdimid) then
             bndydata%map(id)=lonsiz
             if(any(bndydata%dimids.eq.levdimid)) bndydata%map(id)=bndydata%map(id)*levsiz
             bndydata%count(id,1)=latsiz
          else if(bndydata%dimids(id).eq.timdimid) then
             bndydata%map(id)=lonsiz*latsiz
             if(any(bndydata%dimids.eq.levdimid)) bndydata%map(id)=bndydata%map(id)*levsiz
             bndydata%count(id,1)=2
             bndydata%start(id,1)=bndydata%nm
             bndydata%thistimedim=id
          else
             print *, 624,fieldnames(i),id,bndydata%dimids(id),londimid, &
                  levdimid,latdimid,timdimid
             call endrun('physics/cam1/boundarydata.F90 ')
          end if
       end do

       do i=1,fieldcnt
         ! call handle_ncerr( nf90_inq_varid(bndydata%ncid, fieldnames(i), bndydata%dataid(i)),&
            !   'physics/cam1/boundarydata.F90 ',632)
          call handle_ncerr( nf_inq_varid(bndydata%ncid, fieldnames(i), bndydata%dataid(i)),&
               'physics/cam1/boundarydata.F90 ',634)
          !call handle_ncerr( nf90_inquire_variable(bndydata%ncid,bndydata%dataid(i), &
           !    ndims=ndims,dimids=dimids),'physics/cam1/boundarydata.F90 ',636)
         call wrap_inq_var(bndydata%ncid,bndydata%dataid(i), varname, xtype, &
            ndims,dimids, natts)
          if(ndims/=bndydata%ndims .or. dimids(1)/=bndydata%dimids(1).or.&
               dimids(2)/=bndydata%dimids(2) .or. dimids(3)/=bndydata%dimids(3)) then
             call endrun('Variable dims or order does not match')
          end if

          if(bndydata%np .gt. bndydata%nm) then
             !call handle_ncerr( nf90_get_var(bndydata%ncid, bndydata%dataid(i), &
              !    datain(:,:,:,:,i),bndydata%start(:,1), bndydata%count(:,1), &
              !    map=bndydata%map),'physics/cam1/boundarydata.F90 ',647)
				  !!wrap_get_vara_realx (nfid, varid, start, count, arr)
            call wrap_get_vara_realx(bndydata%ncid, bndydata%dataid(i), &
                  bndydata%start(:,1), bndydata%count(:,1),datain(:,:,:,:,i))
          else
             bndydata%count(bndydata%thistimedim,1)=1
             !call handle_ncerr( nf90_get_var(bndydata%ncid, bndydata%dataid(i), &
             !     datain(:,:,:,1:1,i), bndydata%start(:,1), bndydata%count(:,1), &
             !     map=bndydata%map), 'physics/cam1/boundarydata.F90 ',655)
			 call wrap_get_vara_realx(bndydata%ncid, bndydata%dataid(i), &
                   bndydata%start(:,1), bndydata%count(:,1),datain(:,:,:,1:1,i))

             bndydata%start(bndydata%thistimedim,1)=bndydata%np
             !call handle_ncerr( nf90_get_var(bndydata%ncid, bndydata%dataid(i), &
              !    datain(:,:,:,2:2,i), bndydata%start(:,1), bndydata%count(:,1), &
              !    map=bndydata%map), 'physics/cam1/boundarydata.F90 ',662)
             call wrap_get_vara_realx(bndydata%ncid, bndydata%dataid(i), &
                   bndydata%start(:,1), bndydata%count(:,1), datain(:,:,:,2:2,i)) 

          endif
       end do

    end if







 bndydata%latsiz=latsiz
 bndydata%levsiz=levsiz
# 697 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/boundarydata.F90"
 ! Convert input pressure from millibars to pascals.
 if(associated(pin)) then
    pin=pin*100._r8
    if(bndydata%vertextrap.lt.3) then
       allocate(bndydata%zi(levsiz))
    !
    !
    ! Convert input pressure levels to height (m).
    !
       do k=1,levsiz
          bndydata%zi(k) = 7.0e3_r8 * log (1.0e5_r8 / pin(k))
       end do
       deallocate(pin)
    end if
 end if
end subroutine boundarydata_read

  subroutine boundarydata_interpolate(phys_state, datain, bndydata)
    !!use hycoef, only : hypm
    use interpolate_data,only : interp_type, lininterp_init, &
         lininterp_finish, lininterp
    use shr_const_mod, only: shr_const_pi
    use infnan, only : nan

    type(physics_state), intent(in) :: phys_state(begchunk:endchunk)
    real(r8),intent(in)                  :: datain(:,:,:,:,:)
    type(boundarydata_type), intent(inout) :: bndydata

    type(interp_type) :: interp_wgts, lev_wgts
      
    integer :: k, lchnk, nt, j, fcnt
    real(r8) :: zo(plev)
    real(r8) :: lato(pcols)
    integer :: ulatcnt
    integer :: maxlatcnt
    integer :: timesize, tvalout


# 1 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/control/comhyb.h" 1
!----------------------------------------------------------------------- 
! 
! Purpose: Hybrid level definitions: p = a*p0 + b*ps
!          interfaces   p(k) = hyai(k)*ps0 + hybi(k)*ps
!          midpoints    p(k) = hyam(k)*ps0 + hybm(k)*ps
! 
!-----------------------------------------------------------------------
!!!  vertical level definitions in LASG dynamical core: p = pes*sigma + pt
!!!        interfaces   ply(k) = ps*sig (k) + pmtop
!!!        midpoints    ply(k) = ps*sigl(k) + pmtop
!!!---------------------------------------------------------------------
!!!(wanhui 2003.04.30)
!!!(wanhui 2003.10.23)  (std.atm. variables removed)

      real(r8) hyai(plevp)       ! ps0 component of hybrid coordinate - interfaces
      real(r8) hybi(plevp)       ! ps component of hybrid coordinate - interfaces
      real(r8) hyam(plev)        ! ps0 component of hybrid coordinate - midpoints
      real(r8) hybm(plev)        ! ps component of hybrid coordinate - midpoints

!!    real(r8) hybd(plev)        ! difference  in b (hybi) across layers
      real(r8) hypi(plevp)       ! reference pressures at interfaces
      real(r8) hypm(plev)        ! reference pressures at midpoints
!!    real(r8) hypd(plev)        ! reference pressure layer thickness

      real(r8) ps0         ! base state sfc pressure for level definitions
!!    real(r8) psr         ! reference surface pressure for linearization
!!    real(r8) prsfac      ! log pressure extrapolation factor (time, space independent)

!!    integer nprlev       ! number of pure pressure levels at top

      real(r8) :: pmtop              !
      real(r8) :: sig (plevp)        !
      real(r8) :: sigl(plev)         !  fm2003 VPAR variables
      real(r8) :: dsig(plev)         !

!!(wanhui 2003.10.23)
!!------------------------------------------------------------
!!    real(r8) :: tbb (plevstd)         !
!!    real(r8) :: hbb (plevstd)         !
!!    real(r8) :: cbb (plevstd)         !
!!    real(r8) :: dcbb(plevstd)         !  fm2003 std. atm.
!!    real(r8) :: p00, t00              !
!!    real(r8) :: psb (plond,plat)      !
!!    real(r8) :: tsb (plond,plat)      !
!!------------------------------------------------------------
!!(2003.10.23)(these variables are in module stdatm now)


!!      common /comhyb/ hyai ,hyam  ,hybi ,hybm
!!      common /comhyb/ hybd ,hypi ,hypm  ,hypd
!!      common /comhyb/ ps0         ,psr         ,prsfac      ,nprlev

      common /comhyb/ hyai ,hybi ,hyam ,hybm
      common /comhyb/ hypi ,hypm
      common /comhyb/ ps0  ,pmtop
      common /comhyb/ sig  ,sigl , dsig
!!    common /comhyb/ tbb  ,hbb  , cbb  ,dcbb  ,p00 ,t00 ,psb ,tsb    !!(wh 2003.10.23)
 
# 735 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/boundarydata.F90" 2

    !------------------------------------------------------------------------
    ! Interpolate tracer data to model grid
    !------------------------------------------------------------------------
    !
    ! Loop over all input times.
    !

    timesize=2

    maxlatcnt=1
    do lchnk=begchunk,endchunk
       maxlatcnt=max(maxlatcnt,phys_state(lchnk)%ulatcnt)
    end do
    if(bndydata%vertextrap.lt.3) then
       !
       ! Convert approximate cam pressure levels to height (m).
       !
       do k=1,plev
          zo (k) = 7.0e3_r8 * log (1.0e5_r8 / hypm(k))
       end do

       call lininterp_init(bndydata%zi,size(bndydata%zi),zo,plev,bndydata%vertextrap,lev_wgts)
       if(.not. associated(bndydata%fields)) then
          allocate(bndydata%fields(maxlatcnt,plev,begchunk:endchunk,bndydata%fieldcnt,timesize))
          bndydata%fields=0_r8
       end if
    else
       if(.not. associated(bndydata%fields)) then
          allocate(bndydata%fields(maxlatcnt,bndydata%levsiz,begchunk:endchunk,bndydata%fieldcnt,timesize))
          bndydata%fields=0_r8
       end if
    endif
    do lchnk=begchunk,endchunk
       ulatcnt=phys_state(lchnk)%ulatcnt

       !
       ! Convert cam model latitudes to degrees.
       ! Input model latitudes already in degrees.
       !
       do j=1,ulatcnt
          lato(j) = phys_state(lchnk)%ulat(j)*180._r8/SHR_CONST_PI          
       end do

       call lininterp_init(bndydata%lat,size(bndydata%lat),lato(1:ulatcnt),ulatcnt,1,interp_wgts)
       timesize =  size(datain,4)
       do fcnt=1,bndydata%fieldcnt
          do nt = 1,timesize
             if(timesize.gt.1) then
                tvalout=nt
             else
                tvalout=2
             end if
            if(bndydata%vertextrap.lt.3) then
                call lininterp(transpose(datain(1,:,:,nt,fcnt)),bndydata%latsiz,bndydata%levsiz, &
                     bndydata%fields(1:ulatcnt,:,lchnk,fcnt,tvalout), ulatcnt, plev, interp_wgts, lev_wgts) 
             else
                do k=1,bndydata%levsiz
                   call lininterp(datain(1,k,:,nt,fcnt),bndydata%latsiz, &
                        bndydata%fields(1:ulatcnt,k,lchnk,fcnt,tvalout), ulatcnt, interp_wgts)
                end do
             end if
          end do
       end do                 ! end loop over time samples
       call lininterp_finish(interp_wgts)
    end do
    if(bndydata%vertextrap.lt.3) &
         call lininterp_finish(lev_wgts)

    return
  end subroutine boundarydata_interpolate

  subroutine get_data_bounding_date_indices(cdates,nm,np, cdayout, update)
    use time_manager, only: get_curr_date, get_perp_date, get_curr_calday, &
         is_perpetual
    real(r8), intent(in) :: cdates(ptrtim)
    real(r8), intent(out),optional :: cdayout
    logical, intent(out) ,optional :: update
    integer, intent(inout) :: nm, np
    integer :: n, np1
    real(r8) :: calday
    integer :: yr, mon, day   ! components of a date
    integer :: ncdate         ! current date in integer format [yyyymmdd]
    integer :: ncsec          ! current time of day [seconds]

    calday = get_curr_calday()
    if(present(cdayout)) cdayout=calday
    if(present(update)) update=.false. ! initialize output variable

    if(min(nm,np) .ge. 1 .and. max(nm,np) .le. 12 .and.  &
         calday>cdates(nm) .and. calday<=cdates(np)) return
    if((nm==12 .and. np==1) .and. (calday <= cdates(np) .or. &
         calday > cdates(nm))) return

    if(present(update)) update=.true.

    if(calday <= cdates(1) .or. calday > cdates(12)) then
       nm=12
       np=1
    else
       nm=0
       do n=1,ptrtim-1
          if(calday>cdates(n) .and. calday<=cdates(n+1)) then
             nm=n
             np=n+1
          end if
       end do
       if(nm .eq. 0) then
          if ( is_perpetual() ) then
             call get_perp_date(yr, mon, day, ncsec)
          else
             call get_curr_date(yr, mon, day, ncsec)
          end if
          ncdate = yr*10000 + mon*100 + day

          write(6,*)'model date:', ncdate, ncsec,'boundary data dates:', cdates
          call endrun('BOUNDARYDATA_READ: Failed to find dates bracketing dates')
       end if
    end if

  end subroutine get_data_bounding_date_indices


  !================================================================================================
  subroutine boundarydata_vert_interp(lchnk, ncol, levsiz, fldcnt, pin, pmid, datain, dataout)
    !----------------------------------------------------------------------- 
    ! 
    ! Purpose: Interpolate ozone from current time-interpolated values to model levels
    ! 
    ! Method: Use pressure values to determine interpolation levels
    ! 
    ! Author: Bruce Briegleb
    ! 
    !--------------------------------------------------------------------------
    implicit none
    ! Arguments
    !
    integer, intent(in) :: lchnk               ! chunk identifier
    integer, intent(in) :: ncol                ! number of atmospheric columns
    integer, intent(in) :: levsiz
    integer, intent(in) :: fldcnt
    real(r8), intent(in) :: pin(levsiz)
    real(r8), intent(in) :: pmid(pcols,pver)   ! level pressures (mks)
    real(r8), intent(in) :: datain(pcols,levsiz,fldcnt)
    real(r8), intent(out) :: dataout(pcols,pver,fldcnt)    ! ozone mass mixing ratio
    !
    ! local storage
    !

    integer ::  i                   ! longitude index
    integer ::  k, kk, kkstart      ! level indices
    integer ::  kupper(pcols)       ! Level indices for interpolation
    integer ::  kount               ! Counter
    integer ::  fld
    real(r8) dpu                ! upper level pressure difference
    real(r8) dpl                ! lower level pressure difference
    !--------------------------------------------------------------------------
    !
    ! Initialize index array
    !
    do i=1,ncol
       kupper(i) = 1
    end do

    do k=1,pver
       !
       ! Top level we need to start looking is the top level for the previous k
       ! for all longitude points
       !
       kkstart = levsiz
       do i=1,ncol
          kkstart = min0(kkstart,kupper(i))
       end do
       kount = 0
       !
       ! Store level indices for interpolation
       !
       do kk=kkstart,levsiz-1
          do i=1,ncol
             if (pin(kk).lt.pmid(i,k) .and. pmid(i,k).le.pin(kk+1)) then
                kupper(i) = kk
                kount = kount + 1
             end if
          end do
          !
          ! If all indices for this level have been found, do the interpolation and
          ! go to the next level
          !
          if (kount.eq.ncol) then
             do fld=1,fldcnt
                do i=1,ncol
                   dpu = pmid(i,k) - pin(kupper(i))
                   dpl = pin(kupper(i)+1) - pmid(i,k)
                   dataout(i,k,fld) = (datain(i,kupper(i),fld )*dpl + &
                        datain(i,kupper(i)+1,fld)*dpu)/(dpl + dpu)

                end do
             end do
             goto 35
          end if
       end do
       !
       ! If we've fallen through the kk=1,levsiz-1 loop, we cannot interpolate and
       ! must extrapolate from the bottom or top data level for at least some
       ! of the longitude points.
       !
       do fld=1,fldcnt
          do i=1,ncol
             if (pmid(i,k) .lt. pin(1)) then
                dataout(i,k,fld) = datain(i,1,fld)*pmid(i,k)/pin(1)
             else if (pmid(i,k) .gt. pin(levsiz)) then
                dataout(i,k,fld) = datain(i,levsiz,fld)
             else
                dpu = pmid(i,k) - pin(kupper(i))
                dpl = pin(kupper(i)+1) - pmid(i,k)
                dataout(i,k,fld) = (datain(i,kupper(i),fld )*dpl + &
                     datain(i,kupper(i)+1,fld)*dpu)/(dpl + dpu)
             end if
          end do
       end do
       if (kount.gt.ncol) then
          call endrun ('ozone_data_vert_interp: Bad ozone data: non-monotonicity suspected')
       end if
35     continue
    end do
  end subroutine boundarydata_vert_interp
# 997 "/data3/work/yuxinzhu/test/model_platform/models/atm/GAMIL2.8_AMIP/src/physics/cam1/boundarydata.F90"
end module boundarydata
