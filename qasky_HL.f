C     Last change:  LKS  22 Nov 2008    2:18 pm
      SUBROUTINE QASKY
c 
c     core routine on file qasky.f
c
c     called by inishamp [MAIN->RADAMPS->INISHAMP->QASKY]
c
c     calls sunang (included in this file) and the sky radiance and 
c           sky irradiance models 
c 
c     This routine computes the quad-averaged incident sky radiances
c     for use in subroutine amp0a.
c
c     The particular model used to compute the sky (relative or
c     absolute) radiance is inserted via a call to SKYRAD.
c
c     The particular model used to compute the sky irradiance is
c     inserted via a call to SKYRAD.
c
c     The sky radiances returned from the skyrad model are always
c     re-scaled to give agreement with the direct and diffuse
c     irradiances returned by the skyirrad model.
c
c     NOTE: for consistency, sky and solar angles are always in 
c     radians for calls to sky models.
c
      INCLUDE "DIMENS_XL.INC"
c
C     The following common block array was written on 1/19/2000 
C	to store and printout the above-water direct and diffuse Ed's 
C     at each wavelength (block shared with excel.f only)
      common /CErik/ EdifOut(mxwave), EdirOut(mxwave)
C     
C
      COMMON /Csky/ iskyflag,skyspecs(mxnsky)
      COMMON /Csky_HL/ radsky(mxmu,mxphi)
      COMMON /Cwave/ wave(mxwave),waveb(mxwave+1),fijchl(mxwave,mxwave),
     1               fijcdom(mxwave,mxwave),fijraman(mxwave,mxwave) 
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      common /cgcirr1/ iein,iblw,jday,rlon,rlat,the,GMThr,pres,am,rh,
     1                 wv,wsm,ws,vi,ro3
      common /cmisc/ imisc(30),fmisc(30)
c
      data kall/0/
      save  
c
c     temporary local storage:
      dimension thetab(mxmu),phib(mxphi),tempsky(mxmu,mxphi) 
c
      nmu = imisc(1)
      nphi = imisc(2) 
      pi = fmisc(1) 
      degrad = fmisc(2)
      radeg = fmisc(3)
c
c---------------------------------------------------------------------
c
c     Preliminary calculations for sky radiance and irradiance models.
      If(kall.eq.0) then
c
c     Various parameters needed as input by the sky radiance and
c     irradiance models are contained in the skyspecs array, or in the
c     common blocks used by the particular routines.
c
      if(iskyflag .eq. 3) then
c
c     The run uses julian day, lat, long, and GMT to compute the
c     solar zenith angle.
c     NOTE: these values will override the defaults (set in SETDFLTS)
c     for computation of ozone concentration in the skyirrad model
c     gcirrad.
c
      jday = ifix(skyspecs(6))
      rlat = skyspecs(7)
      rlon = skyspecs(8)
      GMThr = skyspecs(9)
c
c     compute the corresponding solar zenith angle
c
      call sunang(jday,GMThr,radeg,rlon,rlat,suntheta,suna)
c
      skyspecs(1) = suntheta
c     NOTE:  the computed sun azimuth angle, suna, is referenced to
c     true north.  The sun azimuth angle in Hydrolight, sunphi, is
c     referenced to the downwind direction.  Therefore, the default 
c     value of sunphi is not replaced by suna.
c
        write(10,503) jday,rlat,rlon,GMThr
        write(10,504) suntheta,suna
c
c     check for bad input
         if(suntheta.gt. 90.0) then
           call HERR("QASKY","computed solar zenith angle > 90 deg")  !stop run
         endif
      endif
!!!
      Endif  !kall = 0
c
c     thetas is known either from input or from the call to sunang
      suntheta = degrad*skyspecs(1)
      sunphi = degrad*skyspecs(2)
c
c--------------------------------------------------------------------
c
c     Compute the quad-averaged sky radiances
c
c     Set the sub-quad partitioning for quad averaging of the sky 
c     radiance distribution; nsubmu and nsubphi = 3 or 4 is good 
c     enough for most purposes.
c
c     all angles are now in radians
c
c      nsubmu = 3
c      nsubphi = 4
      nsubmu = 1
      nsubphi = 1
      dphi = 2.0*pi/float(nphi*nsubphi)
      do jv=1,nphi
         if(jv.eq.1) then
           phimin = bndphi(nphi) + 0.5*dphi
         else
           phimin = bndphi(jv-1) + 0.5*dphi
         endif
c
      do iu=1,nmu-1
         dmu = deltmu(iu)/float(nsubmu)
         if(iu.eq.1) then
           umumin = 0.5*dmu
         else
           umumin = bndmu(iu-1) + 0.5*dmu
         endif
c
c     integrate the sky radiance over quad Quv = Q(iu,jv)
      sum = 0.0
      do j=1,nsubphi
         skyphi = phimin + float(j-1)*dphi
      do i=1,nsubmu
         skymu = umumin + float(i-1)*dmu
c
c     Obtain the diffuse (background) sky radiance for the current
c     sky (theta, phi) and for the given solar (theta, phi).
c     Normalized or relative radiance values will be set to the proper 
c     magnitudes below. 
c
c---------------------------------------------------------------------
c     Insert the call to the desired "skyrad" routine, of the form:
c     call skyrad(suntheta,sunphi,skytheta,skyphi, skyrad)
c
c     NOTE:  the "skyrad" routine can be any routine that
c     returns a sky radiance value in direction (skytheta,skyphi).
c
c     skyrad routines always expect angles in radians
      skytheta = acos(skymu)
c
      call skyrad(suntheta,sunphi,skytheta,skyphi,skyrad0)
c---------------------------------------------------------------------
c
      sum = sum + skyrad0*dmu
      end do  ! i
      end do  ! j
c
      tempsky(iu,jv) = sum*dphi/omega(iu)
      end do  ! iu
      end do  ! jv
c
c     polar cap special case:
c
      sum = 0.0
      dphi = 2.0*pi/float(4*nsubphi)
      dmu = deltmu(nmu)/float(nsubmu)
      umumin = bndmu(nmu-1) + 0.5*dmu
      do j=1,4*nsubphi
         skyphi = float(j-1)*dphi
      do i=1,nsubmu
         skymu = umumin + float(i-1)*dmu
c---------------------------------------------------------------------
c        Insert the call to the desired "skyrad" routine:
c        call skyrad(suntheta,sunphi,skytheta,skyphi, skyrad)
         skytheta = acos(skymu)
         CALL skyrad(suntheta,sunphi,skytheta,skyphi,skyrad0)
cc--------------------------------------------------------------------
         sum = sum + skyrad0*dmu
      end do
      end do
c
      tempsky(nmu,1) = sum*dphi/omega(nmu)
c
c     -----------------------------------------------------------
c
c     Compute the downwelling diffuse irradiance associated with
c     the quad-averaged diffuse sky radiance
c
      Ednorm = tempsky(nmu,1)*fmu(nmu)*omega(nmu)
      do i=1,nmu-1
      do j=1,nphi
         Ednorm = Ednorm + tempsky(i,j)*fmu(i)*omega(i)
      end do
      end do
!!
c     determine which quad contains the sun:

c     convert the boundary mu and phi values to degrees 
      do i=1,nmu
         thetab(i) = radeg*acos(bndmu(i))
      end do
      do j=1,nphi
         phib(j) = radeg*bndphi(j)
      end do
c 
c     determine the (mu,phi) indices of the quad containing the sun 
      thetas = skyspecs(1)          !deg
      phis = skyspecs(2)         !deg
      ph = amod(phis + 360., 360.)
      imus = nmu
      do i=1,nmu-1
      if(thetas.lt.thetab(i) .and. thetas.ge.thetab(i+1)) imus = i + 1
      end do
      if(thetas.gt.thetab(1)) imus = 1
c
c     ... if the sun is in the polar Quad, set phi=0, else find the phi index
      IF (imus.eq.nmu) THEN
        jphis = 1
      ELSE
        do j=1,nphi
        if(ph.lt.phib(j)) go to 206
        end do
        j = 1
  206   jphis = j
      END IF
 
  200 continue
c 
c        get exact theta value at quad center for printout
         if(imus.eq.1) then
            theq = 0.5*(90.0 + radeg*acos(bndmu(imus)))
         elseif(imus.eq.nmu) then
            theq = 0.
         else
            theq = 0.5*radeg*(acos(bndmu(imus-1)) + acos(bndmu(imus)))
         endif
 
        if(kall.eq.0) write(10,510) imus,jphis,theq,radeg*phi(jphis)
!!
c
!Lc---------------------------------------------------------------------
!Lc     Insert the call to the desired "skyirrad" routine (just for msgs):
!L      call skyirrad(suntheta,sunphi, Eddif,Eddir)
!lc--------------------------------------------------------------------
        kall = 1
!!        return
!L      Endif
c
!!!   Subsequent calls start here...
c     end of preliminary calculations on the first call to qasky
c---------------------------------------------------------------------
c
c     subsequent calls start here and use the same background sky
c     UNnormalized radiance pattern
c
c---------------------------------------------------------------------
c     Insert the call to the desired "skyirrad" routine:
      call skyirrad(suntheta,sunphi, Eddif,Eddir)
c--------------------------------------------------------------------
c
c     Scale the quad-averaged diffuse sky radiances so that they
c     yield the diffuse Ed value at this wavelength
c
      factor = 0.0
      if(Ednorm.ne.0.0) factor = Eddif/Ednorm
      tempsky(nmu,1) = tempsky(nmu,1)*factor
      do i=1,nmu-1
      do j=1,nphi
         tempsky(i,j) = tempsky(i,j)*factor
      end do
      end do
c
c     Add the solar direct beam quad-averaged radiance to the
c     appropriate quad
c 
c 
      tempsky(imus,jphis) = tempsky(imus,jphis) +
     1  Eddir/(fmu(imus)*omega(imus))
c
c     tempsky(mu,phi) now contains the absolute quad-averaged sky
c     radiance distribution, where (mu,phi) gives the VIEWING
c     direction.  Reset phi to phi + 180 deg, so that (mu,phi) gives
c     the direction of PHOTON TRAVEL, as required for radiative
c     transfer calculations.
c
      do j=1,nphi
      jindx = mod(j + nphi/2, nphi)
      if(jindx.eq.0) jindx = nphi
         do i=1,nmu-1
            radsky(i,j) = tempsky(i,jindx)
         end do
      end do
      radsky(nmu,1) = tempsky(nmu,1)
c      call p2aray(radsky,nmu,nphi,mxmu,2,' final radsky')
c
c     As a check, compute the total Ed from the final quad-averaged
c     sky radiances, and see that it equals the total Ed value from
c     the sky irradiance model.
c
      Edfinal = radsky(nmu,1)*fmu(nmu)*omega(nmu)
      do i=1,nmu-1
      do j=1,nphi
        Edfinal = Edfinal + radsky(i,j)*fmu(i)*omega(i)
      end do
      end do
c
c     Print out normalization info on first call iff idbug>0 (added printout requested)
!      if(kall.eq.1.and.imisc(9).gt.0) write(10,520) Edfinal,Eddif+Eddir
c
ccc	*** Store Diffuse and Direct sky Ed's at this wavelength
      jwave = imisc(11)       !get index to current wavelength
      EdifOut(jwave) = Eddif  !store diffuse Ed (in air) into array
      EdirOut(jwave) = Eddir  !store direct Ed (in air) into array
ccc
      kall = 2

      return
c 
c     formats 
c 
  503 format(/5x,"The sun's zenith angle is computed from"/
     110x,'Julian day = ',i4/
     210x,'latitude   = ',f9.4,' degrees (N is positive)'/
     310x,'longitude  = ',f9.4,' degrees (E is positive)'/
     410x,'GMT        = ',f7.2,' hours')
  504 format(/5x,'The computed solar zenith angle is',f6.2,' degrees'//
     15x,'The computed solar azimuth angle is',f7.2,' degrees (relative 
     2to true north)'//
     35x,'The computed solar azimuth angle is not used by HydroLight, ',
     4/5x,'which places the sun at 0.00 degrees azimuth relative ',
     5'to the downwind direction.')
  510 format(/5x,'The sun is placed in quad Q(r,s) = Q(',i2,',',i2,
     1') centered at (theta, phi) = (',f6.3,',',f7.3,')'/) 
  520 format(/5x'Check on the sky radiance initialization:'/
     1'      total Ed from integrating the sky radiance =',1p,e11.4/
     2'      total Ed from the sky irradiance model     =',e11.4)
      end 

