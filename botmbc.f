C     Last change:  LKS   1 Nov 2007   12:56 pm
      subroutine botmbc(L)
c 
c     core routine botmbc on file botmbc.f
c 
c     This routine specifies the bottom boundary condition via the 
c     discrete spectral rhatmb = rhat1(m,b;L) array for the selected 
c     type of bottom boundary, for the present L value and wavelength.
c
c     if ibotm = 0 (infinitely deep water).  Set up and solve the eigenvalue
c                   problem of Eq. (9.50) and then use (9.76) to get
c                   rhat(z,infinity)  (see also Eqs. 10.8 and 10.9 of
c                   NOAA Tech Memo ERL-PMEL-75)
c
c     if ibotm .ge. 1 (finite depth at zeta = zeta(nz)), The bottom is
c                   assumed to be azimuthally isotropic, but otherwise
c                   non-Lambertian. (i.e., the only restriction on the 
c                   bottom BRRF is that it depend on (phi' - phi) and 
c                   not on phi' and phi independently)
c
c          if ibotm = 1, the same reflectance R is used at each wavelength
c          if ibotm = 2, the wavelength-dependent reflectance R is obtained
c                   from Hydrolight standard format data file Rbottomdatafile
c
      INCLUDE "DIMENS_XL.INC"
c 
      common /Cbotbc/ rhatmb(mxmu,mxmu)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      common /CMISC/ imisc(30),fmisc(30)
 
      dimension BRRFdisc(mxmu,mxmu,mxphi),rhatruL(mxmu,mxmu)
c
      data waveold /0.0/
      save 
c 
      nmu = imisc(1)
      nphi = imisc(2) 
      nL = imisc(3)
      ibotm = imisc(12) 
!      pi = fmisc(1)
      wavel = fmisc(13)
c 
      if(ibotm.eq.0) then
c
c        Infinitely deep water:
c        set up and solve the eigenvalue problem (9.50) and obtain
c        Rinfinity(L) = rhat(m,infinity;L) by (9.76).
c        infbotm sets rhatmb = rhat(m,inf;L) 
c 
         call infbotm(L)
c
      elseif(ibotm.ge.1) then 
c
c        Finite depth water:
c        Discretize the bottom BRRF and compute the spectral array
c        rhat(m,b;r,u|L) = rhatmb(r,u) for this L  
c
c        discretize the BRRF for the current wavelength.
         if(wavel.ne.waveold) then
            call qaBRRF(BRRFdisc)
            waveold = wavel
         end if
c
c        zero the rhatruL and rhat(m,b) arrays
         do ir=1,nmu
           do iu=1,nmu
             rhatruL(ir,iu) = 0.
             rhatmb(ir,iu) = 0.
           end do
         end do
c
c        compute the rhat(r,u|L) array using NOAA TM 75, eqs 5.47a-d, for
c        the current L value
c
         fL = float(L)
         if (L.eq.0) then
            epsL = nphi
            deltaL = 1
         elseif (L.eq.nL) then
            epsL = nphi
            deltaL = 0
         else
            epsL = nphi/2
            deltaL = 0
         end if
c
c        quad-to-quad case (Eq. 75/5.47a)
c
         do ir = 1,nmu-1
         do iu = 1,nmu-1
            sum = 0.0
            do iv =1,nphi
               sum = sum + BRRFdisc(ir,iu,iv)*cos(fL*phi(iv))
            end do
         rhatruL(ir,iu) = sum/epsL
         end do
         end do
c
c        quad-to-cap case (eq 75/5.47b)
c
         do ir = 1,nmu-1
            if(L.eq.0) then
               rhatruL(ir,nmu) = BRRFdisc(ir,nmu,1)
            else
               rhatruL(ir,nmu) = 0.0
            end if
         end do
c
c        cap-to-quad case (eq 75/5.47c)
c        quad-to-quad case (Eq. 75/5.47a)
c
         do iu = 1,nmu-1
            sum = 0.0
            do iv =1,nphi
               sum = sum + BRRFdisc(nmu,iu,iv)*cos(fL*phi(iv))
            end do
         rhatruL(nmu,iu) = sum/epsL
         end do
c
c         do iu = 1,nmu-1
c            if(L.eq.0) then
c               rhatruL(nmu,iu) = BRRFdisc(nmu,iu,1)
c            else
c               rhatruL(nmu,iu) = 0.0
c            end if
c         end do
c
c        cap-to-cap case (eq. 75/5.47d)
c
            if(L.eq.0) then
               rhatruL(nmu,nmu) = BRRFdisc(nmu,nmu,1)
            else
               rhatruL(nmu,nmu) = 0.0
            end if

c         print*,' sub botmbc: L = ',L
c         call p2aray(rhatruL,nmu,nmu,mxmu,2,' rhatruL')

c
c        Now compute the rhat1(m,b|L) = rhatmb array by 75/5.50.
c        Note that because of the azimuthal isotropy of the bottom BRRF,
c        rhat2(m,b|L) = 0 for L = 0 or nL, and rhat2(m,b|L) = rhat1(m,b|L) 
c        for L = 1,2,...,nL-1.  Thus only rhat1 needs to be evaluated;
c        the rest is done in routine riccati.
c
         do iu = 1,nmu
            do ir = 1,nmu-1
               rhatmb(ir,iu) = epsL*rhatruL(ir,iu)
            end do
            rhatmb(nmu,iu) = deltaL*rhatruL(nmu,iu)
         end do
c
c        debugging for CobOP:
         idbug = 0
         if(idbug.ne.0) then
         print*,' sub botmbc: L = ',L
         call p2aray(rhatmb,nmu,nmu,mxmu,2,'rhatmb for non-Lambertian')
         endif
      
      return
      end if
      end 