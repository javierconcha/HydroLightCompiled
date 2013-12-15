C     Last change:  LKS  25 May 2008    7:29 pm
      subroutine initprintout(idbug)

c
      INCLUDE "DIMENS_XL.INC"
c
      COMMON /Cpirrad/ npirad,izirad(mxz)
      COMMON /Cprad/ nprad,iprad1,iprad2,iprad3,jprad1,jprad2,jprad3,
     1               izprad(mxz)
      COMMON /Cpkfcn/ npkfcn,izkfcn(mxz) 
      COMMON /CKRAD/ ipkrad,istart,istop,istep,jstart,jstop,jstep 
      COMMON /Cmisc/ imisc(30),fmisc(30)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
c
      integer nmu, nphi, nz, nznom, ibotm
      real deltazK
c
      nmu = imisc(1)
      nphi = imisc(2)
      nznom = imisc(4)
      ibotm = imisc(12)

cccccc  CHECK DEPTH DATA and ADD Kfunc DEPTHS  ccccccccccccccccccccccccc
      nz = 2 * nznom
      imisc(4) = nz   !store new value into imisc
c     check for array dimensions being larger than allowed
      if(nz.gt.mxz) then
         write(6,fmt='(" nz =",i3," gt mxz =",i3)') nz/2,mxz/2
         write(10,fmt='(" nz =",i3," gt mxz =",i3)') nz/2,mxz/2
         call HERR("initprintout",
     1        "increase LIMIT of number of ouput depths in UI")  !stop run
      endif
c
c     add depth increments for K-function computations (deltazK is
c     set in routine setdflts)
      deltazK = fmisc(10)   ! deltazK is set in routine setdeflts
c
c     for the last requested depth, let the paired depth be just
c     below the requested depth if the water is infinitely
c     deep, but let the paired depth be just above the bottom
c     if the water is finitely deep.  Printout will be set below
c     to give the last requested depth in either case.
      if(ibotm.eq.0) then
c        infinitely deep water
         zeta(2*nznom-1) = zeta(nznom)
         zeta(2*nznom) = zeta(nznom) + deltazK
      else
c        finite-depth bottom
         zeta(2*nznom-1) = zeta(nznom) - deltazK
         zeta(2*nznom) = zeta(nznom)
      endif
c     fill in the rest from depth to surface
      do iz = nznom-1, 1, -1
         zeta(2*iz-1) = zeta(iz)
         zeta(2*iz) = zeta(iz) + deltazK
      end do
c

cccccc  SET PRINTOUT PARAMETERS  cccccccccccccccccccccccccccccccccccc
c     Set parameters for how much printout is to be given on the
c     standard printout file.
c
c     The defaults as set below give output that is typically of 
c     interest to oceanographers.  These values can be changed to 
c     get more or less printout.
c
c.....IRRADIANCE and IOP printout.  DEBUG some more !!!!
c
c      use ipirad = 0 for NO printout of the irradiances or IOP values
c                 = 1  for printout only of the values IN AIR (at depth z = a)
c                      (useful for remote-sensing studies)
c                 = 2 for printout at the user-selected depths (the odd
c                     depths)
c                 = 3 for printout at all depths
c
c     DEFAULT:  print irradiances and IOPs at the user-requested depths:     
      If(idbug .eq. -1) then  
        ipirad = 0		!don't printout irrad if selected "minimal" output
      Else
        ipirad = 2		!give "standard" output
      Endif
c
cc      if(ipirad.eq.0) then
c        no printout of irradiances or IOPs
cc         npirad = 0			<-- this will crash pntgrid
cc      else...
	if(ipirad.eq.1) then
c        printout only of the air values (irradiances only)
         npirad = 1
      elseif(ipirad.eq.2 .or. ipirad.eq.0) then
c        printout at user-requested depths
         npirad = 0
         do iz=1,nz,2
            npirad = npirad + 1
            izirad(npirad) = iz
         end do
c        for finite-depth bottoms, make the last printout depth
c        exactly the bottom depth
         if(ibotm.ne.0) izirad(npirad) = nz
      elseif(ipirad.eq.3) then
c     printout at all nz depths 
      do iz=1,nz
         izirad(iz) = iz
      end do
      npirad = nz
      else
         print*,' invalid selection for irradiance printout: ipirad = ',
     1            ipirad
      endif 
c
c.....RADIANCE printout
c
c     How much of the full radiance distribution is printed to the
c     standard output file is determined by specifying do-loop 
c     parameters that select indices of the theta,phi,depth arrays
c     where printout is desired.  (NOTE: the full radiance distribution
c     is always printed to the digital output file D_name_.txt and
c     to L_name_.txt (if requested in the GUI) for graphical postprocessing).
c     
c      use iprad = 0 for NO printout of the radiance distribution
c                = 1  for printout only of the values IN AIR (at depth z = a)
c                     (useful for remote-sensing studies)
c                = 2 for printout at the user-selected depths (the odd
c                    depths)
c                = 3 for printout at all depths
c
      if(idbug.le.0) then
c     DEFAULT values:  print radiances in the air only, at all theta 
c     values, but at phi = 45 and 90 degrees only (i.e., in the plane 
c     perpendicular to the sun, if the sun is at phi = 0, i.e. phi 
c     index 1), as is used in remote-sensing work
c
       iprad = 1
c      set theta and phi do loop indices:
       iprad1=nmu
       iprad2=1
       iprad3=-1
c       jprad1= nphi/4 + 1	!  90 degrees
c       jprad2=3*nphi/8 + 1	! 135 degrees
       jprad1 = 1
       jprad2 = nphi/4 + 1  ! 90 deg
       jprad3 = nphi/8  ! 45 deg
c       jprad3= (jprad2 - jprad1)
       if(jprad3.eq.0) jprad3 = 1   !if nphi<8, loop increment jprad3=0 (illegal)
c
      else
c
c     Example of printing out radiances in the air only, at all theta
c     values, and at all phi values from 0 to 180 degrees.
c
c      For extensive printout, signal to print at all user-defined depths
       iprad = 2
c      set theta and phi do loop indices:
       iprad1=nmu
       iprad2=1
       iprad3=-1
       jprad1= 1             !   0 deg
       jprad2= nphi/2 + 1    ! 180 deg
       jprad3= 1             ! print all phi from 0 to 180 deg
      endif
c
c     Example for printing radiances in the air only, at all theta 
c     values, but at phi = 90 degrees only (i.e., in the plane 
c     perpendicular to the sun, if the sun is at phi = 0, i.e. phi 
c     index 1), as is used in remote-sensing work
c
c      iprad = 1
c     set theta and phi do loop indices:
c      iprad1=nmu
c      iprad2=1       
c      iprad3=-1
c      jprad1=nphi/4 + 1
c      jprad2=nphi/4 + 1
c      jprad3=1
c
c     Example for getting radiance printout in the air only, at all theta 
c     values, and at phi = 0, 90, 180 degrees
c
c      iprad = 1
c     set theta and phi do loop indices:
c      iprad1=nmu
c      iprad2=1       
c      iprad3=-1
c      jprad1= 1
c      jprad2=nphi/2 + 1
c      jprad3=6
c
c     Example for getting radiance printout at 
c        theta = 0, 30, 60, which is theta indices nmu=10, 7, 4
c        phi = 0, 90, 180, which is phi indices 1, 7, 13
c        depth indices 1, 3, 5,... (the user-requested depths, which
c           are the odd depths, after adding the small increments for 
c           computing K functions)
c
c      iprad = 2
c      iprad1=nmu
c      iprad2=4
c      iprad3 = -3
c      jprad1 = 1
c      jprad2 = nphi/2 + 1
c      jprad3 = 6
c
      if(iprad.eq.0) then
c        no printout of radiances
         nprad = 0
      elseif(iprad.eq.1) then
c        printout in air only
         nprad = 1
      elseif(iprad.eq.2) then
c        printout at user-selected depths
         nprad = 0
         do iz=1,nz,2
            nprad = nprad + 1
            izprad(nprad) = iz
         end do
c        for finite-depth bottoms, make the last printout depth
c        exactly the bottom depth
         if(ibotm.ne.0) izprad(nprad) = nz
      elseif(iprad.eq.3) then
c        printout at all nz depths 
         do iz=1,nz
            izprad(iz) = iz
         end do
         nprad = nz
      else
         print*,' invalid selection for radiance printout: iprad = ',
     1            iprad
      endif 
c
c.....IRRADIANCE K-FUNCTION printout.
c
c      use ipkfcn = 0 for NO printout of the irradiance K-functions
c                 = 1 for printout at the user-selected depths (the odd
c                     depths, using the closely spaced odd-even pairs 
c                     of depths to compute derivatives)

c     DEFAULT:  print IRRADIANCE K-functions at the user-selected depths
c
      ipkfcn = 1
c
      if(ipkfcn.eq.0) then
c        no printout
         npkfcn = 0
      elseif(ipkfcn.eq.1) then
c        printout at user-selected depths
         npkfcn = 0
         do iz=1,nz-1,2
            npkfcn = npkfcn + 1
            izkfcn(npkfcn) = iz
         end do
      else
         print*,' invalid selection for K-function printout: ipkfcn = ',
     1            ipkfcn
      endif
c
c.....RADIANCE K-FUNCTION and PATH FUNCTION printout.
c
c      use ipkrad = 0 for NO printout of the radiance K-functions and
c                     path functions
c                 = 1 for printout at the user-selected depths (the odd
c                     depths, using the closely spaced odd-even pairs 
c                     of depths to compute derivatives) and for the
c                     directions specified by the following theta,phi
c                     indices
c
c     DEFAULT:  NO printout of RADIANCE K-functions or path functions
c
      ipkrad = 0
      istart= iprad1
      istop= iprad2
      istep= iprad3
      jstart=jprad1
      jstop=jprad2
      jstep=jprad3
c
      return
      end subroutine
