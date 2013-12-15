      subroutine user_abmodel(depth, cconc)
c
c     This file selects the concentration routine to be used in the run,
c     in the provided as well as user-developed 'ab' 
c     routines
c
      INCLUDE 'dimens_XL.inc' 
c
      COMMON /CConstConc/ compconc(mxcomp) !array loaded here iff any conc are constant
      real depth, conc, cconc(mxcomp), compconc
c
c     Component 1 is pure water.  No calc needed.
c
c     Component 2 is CDOM
c     absorption due to CDOM is constant with depth
c     and at its reference wavelength, acdom = 1
      cconc(2) = 1
      compconc(2) = 1  !store in array
c
c     Component 3 is minerals and is constant with depth
      cconc(3) = 0.5
      compconc(3) = 0.5  !store in array
c
c     Component 4 is phyto1 and is constant with depth
      cconc(4) = 0.1
      compconc(4) = 0.1  !store in array
c
c     Component 5 is phyto2 and is constant with depth
      cconc(5) = 0.2
      compconc(5) = 0.2  !store in array
c
c     Component 6 is phyto3.  A subroutine will be called
      icomp = 6
      call conczdata(icomp, depth, conc)
      cconc(6) = conc 
c
c     Component 7 is phyto4 and is constant with depth
      cconc(7) = 0.4
      compconc(7) = 0.4  !store in array
c
c     Component 8 is phyto5.  A subroutine will be called
      icomp = 8
      call conczdata(icomp, depth, conc)
      cconc(8) = conc 
c
c     Component 9 is phyto6.  A subroutine will be called
      icomp = 9
      call conczdata(icomp, depth, conc)
      cconc(9) = conc 
c
c     Component 10 is phyto7 and is constant with depth
      cconc(10) = 0.7
      compconc(10) = 0.7  !store in array
c
      return
      end
c     -----------------------------------------

      subroutine user_abscat(depth,wavelen,ncomp,acomp,bcomp,
     1                       atotal,btotal)
c
c     This file selects the 'ab' routine to be used in the run,
c     called by routines inishamp, rhotau, zetatoz, and ztozeta.
c
      INCLUDE 'dimens_XL.inc' 
c
      integer ncomp
      real depth,wavelen,atotal,btotal
      real acomp(mxcomp),bcomp(mxcomp)
c
c
      call TenCompIOPs(depth,wavelen,ncomp,acomp,bcomp,atotal,btotal)
      return
      end
c     -----------------------------------------

      subroutine user_acdomsub(depth, wavelen, abscdom)
c
c     This file specifies the 'aCDOM' to be used in the run,
c     by calls from the 'ab' routines, and the CDOM fluorescence routine 'shatcdom'
c
      real depth,wavelen, abscdom
c
      INCLUDE 'dimens_XL.inc' 
      integer ncomp, icomp
      real atotal,btotal,acomp(mxcomp),bcomp(mxcomp)
      ncomp = 10
c
c  CDOM is component in user model
      call user_abscat(depth,wavelen,ncomp,acomp,bcomp,atotal,btotal)
      icomp = 2
      abscdom = acomp(icomp)
c
      return
      end
c     -----------------------------------------

      subroutine user_achlz(depth, wavelen, achl)
c
c     This file selects how the chlorophyll profile is specified for the run,
c     for use by the 'ab' routines and the chlorophyll fluorescence
c     routine 'shatchl'
c
      INCLUDE 'dimens_XL.inc' 
      integer ncomp, icomp
      real atotal,btotal,acomp(mxcomp),bcomp(mxcomp)
      ncomp = 10
c
c  Chlorophyll is component in user model
      call user_abscat(depth,wavelen,ncomp,acomp,bcomp,atotal,btotal)
      icomp = 10
      chlconc = acomp(icomp)
c
      return
      end
c     -----------------------------------------

      subroutine user_skyirrad(suntheta, sunphi, eddif, eddir)
c
c     This file selects the sky irradiance model to be used in the run,
c     and is called by routine qasky.
c
      Real suntheta, sunphi, eddif, eddir
      call HERR('user_skyirrad','Not subroutine')  !stop run
c
      Return
      End
c     -----------------------------------------

      subroutine user_skyrad(suntheta,sunphi,skytheta,skyphi,skyrad0)
c
c     This file selects the sky radiance model to be used in the run,
c     and is called by routine qasky.
c
      Real suntheta, sunphi, skytheta, skyphi, skyrad0
c
      call hcnrad(suntheta,sunphi,skytheta,skyphi,skyrad0)
      Return
      End
c     -----------------------------------------

      Real Function s0bioSub(depth,wavelen)
c
c     This file calls the bioluminescent function to be used in the run,
c     (if needed at all) by shat.f
c
      real depth,wavelen, S0biolum
c
c      Bioluminescence is not included in this run
       s0bioSub = 0.0
c
      return
      end
c     -----------------------------------------

