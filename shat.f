C     Last change:  LKS  31 Aug 2013    6:34 pm
c     File shat.f contains the core routines that compute the spectral
c     forms of the inelastic-scattering (Raman scattering, chlorophyll 
c     fluorescence, and CDOM fluorescence) and internal source
c     (bioluminescence) functions.  The spectral forms are the Shat
c     arrays of L&W Eqns (8.78), (8.79), (8.84), and (8.85).
c
c     sumshat is called by 
c        drtdzs
c
c     This file contains routines
c        sumshat       (which calls shatbiol, shatchl, and shatram)
c          shatbiol    (which calls eval1D and user-routine s0biolum)
c          shatcdom    (which calls eval1D and user-routine acdom)
c          shatchl     (which calls eval1D and astarchl)
c          shatram     (which calls eval1D)
c
c    called routine eval1D is in file dataintp.f
c
c********************************************************************
c
      subroutine sumshat(L,zetanow,znow)
c
c     core routine on file shat.f
c
c     This routine computes the total effective internal source terms for the
c     current L, optical depth, and wavelength.  The total is the sum of 
c     contributions by bioluminescence (routine shatbio), chlorophyll
c     fluorescence (routine shatchl), CDOM fluorescence (routine shatcdom),
c     and Raman scatter (routine shatram).
c     Note that only L = 0, p = 1 (cosine amplitudes) gives a nonzero
c     contribution.
c
      INCLUDE "DIMENS_XL.INC"
      PARAMETER (mxbeta=mxmu*(mxphi/2 + 1))
c
      COMMON /CsourceH/ s1hatm(mxmu),s2hatm(mxmu),s1hatp(mxmu),
     1                  s2hatp(mxmu)
      Common /Csource0/ ibiolum,ichlfl,icdomfl,iraman, ramanEXP
      COMMON /Crhotau/ rhohat(mxmu,mxmu),tauhat(mxmu,mxmu), 
     1                 betatP(mxmu,mxbeta,mxcomp),
     2                 betatM(mxmu,mxbeta,mxcomp)
      COMMON /Cmisc/ imisc(30),fmisc(30) 
c
c     temporary local storage:
      dimension s1m(mxmu),s1p(mxmu)
c
      nmu = imisc(1)
      iop = imisc(5)
c
c     zero all source arrays
c
      do i=1,nmu
      s1hatm(i) = 0.0
      s2hatm(i) = 0.0
      s1hatp(i) = 0.0
      s2hatp(i) = 0.0
      end do
c
      if(L.ne.0) return
c
c     for L = 0, compute the effective source terms
c
c     allow for routines that may want optical depth as input
c
      if(iop.eq.1) then
         depth = zetanow
      else
         depth = znow
      endif
c
      if(ibiolum.ne.0) then
c
c        compute the bioluminescence source contribution
c
         call shatbiol(depth,s1m,s1p)
c
         do i=1,nmu
            s1hatm(i) = s1hatm(i) + s1m(i)
            s1hatp(i) = s1hatp(i) + s1p(i)
         end do
      endif
c
      if(ichlfl.ne.0) then
c
c        compute the effective source contribution by chlorophyll 
c        fluorescence
c
         call shatchl(depth,s1m,s1p)
c
         do i=1,nmu
            s1hatm(i) = s1hatm(i) + s1m(i)
            s1hatp(i) = s1hatp(i) + s1p(i)
         end do
      endif
c
      if(icdomfl.ne.0) then
c
c        compute the effective source contribution by CDOM fluorescence
c 
         call shatcdom(depth,s1m,s1p)
c
         do i=1,nmu
            s1hatm(i) = s1hatm(i) + s1m(i)
            s1hatp(i) = s1hatp(i) + s1p(i)
         end do
      endif
c
      if(iraman.ne.0) then
c
c        compute the effective source contribution by Raman scattering
c 
         call shatram(depth,s1m,s1p, ramanEXP)
c
         do i=1,nmu
            s1hatm(i) = s1hatm(i) + s1m(i)
            s1hatp(i) = s1hatp(i) + s1p(i)
         end do
      endif
c
      return
      end
c
c********************************************************************
c
      subroutine shatram(z,s1m,s1p, ramanEXP)
c
c     core routine on file shat.f
c
c     This routine computes the approximate effective internal source
c     terms (in spectral form) for Raman scattering. 
c
c*****NOTE:  The present treatment of Raman scattering uses an 
c     azimuthally averaged source function that is equivalent to
c     the source function described in Appendix A of Mobley, et al.
c     1993, Comparison of numerical models..., Applied Optics 32(36),
c     7484-7504.  This azimuthally averaged source function guarantees
c     that the Raman contribution to IRRADIANCES will be computed
c     exactly, even though the Raman contribution to the RADIANCE
c     in a given phi direction may be incorrect.  This approximation
c     allows the Raman source function to depend only on theta, and
c     not on both theta and phi - a significant simplification in the
c     coding.
c
      INCLUDE "DIMENS_XL.INC"
      COMMON /CEospl/ nspl,zspl(mxz),Eospl(mxz,mxwave),
     1                E2spl(mxz,mxwave),Edspl(mxz,mxwave)
      COMMON /Cwave/ wave(mxwave),waveb(mxwave+1),fijchl(mxwave,mxwave),
     1               fijcdom(mxwave,mxwave),fijraman(mxwave,mxwave)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Cmisc/ imisc(30),fmisc(30)
c
      dimension s1m(mxmu),s1p(mxmu),sum1(mxmu)
      data i1/1/  !linear interp index
      save
c
      nmu = imisc(1)
      jwave = imisc(11)
      pi = fmisc(1)
      a0R = fmisc(21)
      wave0 = fmisc(22)
c
!dbg      write(10,*) '### Raman exp: ',RamanEXP

      if(jwave.eq.1) then
c        We are at the first wavelength:  there is no Raman 
c        contribution from shorter wavelengths
         do i=1,nmu
            s1m(i) = 0.0
            s1p(i) = 0.0
         end do
         return
      endif
c
c     loop over all wavelengths shorter than the current wavelength and
c     compute the Raman-scatter contribution
c
      depth = z
      do i=1,nmu
         sum1(i) = 0.0
      do iwave=1,jwave-1
c       Compute the scalar irradiance Eo at the current depth and incident
c       (excitation) wavelength. 
        Eozi = yinterp(i1, nspl, depth, zspl, Eospl(1,iwave))
        Eozi = exp(Eozi)
c       Compute the second moment E2 at the current depth and incident
c       (excitation) wavelength.  
        E2zi = yinterp(i1, nspl, depth, zspl, E2spl(1,iwave))
        E2zi = exp(E2zi)
c       Now compute the contribution from wavelength iwave, using the 
c       Raman absorption (scattering) coefficient (5.89) and the previously
c       discretized Raman wavelength redistribution function 
        wavei = wave(iwave)
        E2term = 0.3097*0.5*E2zi*(3.0*fmu(i)*fmu(i) - 1.0)
        sum1(i) = sum1(i) + (Eozi + E2term) *
     1            a0R*(wave0/wavei)**ramanEXP *fijraman(iwave,jwave)  
      end do
      end do
c
c     get the total attenuation at this depth and emission wavelength
c     (just computed in the call to rhotau)
c
      atotal = fmisc(19)
      btotal = fmisc(20)
c
      factor1 = 1.0/(4.0*pi*(atotal + btotal))
c
      do i=1,nmu
         s1p(i) = factor1*sum1(i)/fmu(i)
         s1m(i) = s1p(i)
      end do
c
      return
      end
c
c*******************************************************************
c
      subroutine shatchl(z,s1m,s1p)
c     core routine on file shat.f
c
c     This routine computes the effective internal source terms (in 
c     spectral-amplitude form) for chlorophyll fluorescence.  The
c     formulation of CM 24 June 94, p 13, is used.
c
      INCLUDE "DIMENS_XL.INC"
c
      integer indexchl
      common /CindexChl/ indexchl
      COMMON /CEospl/ nspl,zspl(mxz),Eospl(mxz,mxwave),
     1                E2spl(mxz,mxwave),Edspl(mxz,mxwave)
c
      COMMON /Cwave/ wave(mxwave),waveb(mxwave+1),fijchl(mxwave,mxwave),
     1               fijcdom(mxwave,mxwave),fijraman(mxwave,mxwave)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Cmisc/ imisc(30),fmisc(30)
c
      dimension s1m(mxmu),s1p(mxmu)
c
      nmu = imisc(1)
      pi = fmisc(1)
      jwave = imisc(11)
c
      if(jwave.eq.1) then
c        We are at the first wavelength:  there is no fluorescence 
c        contribution from shorter wavelengths
         do i=1,nmu
            s1m(i) = 0.0
            s1p(i) = 0.0
         end do
         return
      endif
c
c     loop over all wavelengths shorter than the current wavelength and
c     compute the chl fluorescence contribution
c
      sum0 = 0.0
      do iwave=1,jwave-1
c       Eo at excitation wavelength from interpolation routine:
        depth = z
        Eozi = yinterp(i1, nspl, depth, zspl, Eospl(1,iwave))
        Eozi = exp(Eozi)
c       Now compute the contribution from wavelength iwave, using the 
c       chlorophyll-specific absorption, the chl concentration at this 
c       depth, and the previously discretized chlorophyll wavelength
c       redistribution function 
        wavei = wave(iwave)
        icomp = indexchl
c       store wavelength and depth into dummy variable to be passed to the INC routines
        wavelen = wavei
c       insert a call to the desired Chl(z) routine:
        call achlz(depth, wavelen, achl)
        sum0 = sum0 + Eozi*achl*fijchl(iwave,jwave)
      enddo
c
c     get the total attenuation at this depth and emission wavelength
c     (just computed in the call to rhotau)
      atotal = fmisc(19)
      btotal = fmisc(20)
c
      factor1 = 1.0/(4.0*pi*(atotal + btotal))
c
      do i=1,nmu
         s1p(i) = factor1*sum0/fmu(i)
         s1m(i) = s1p(i)
      end do
c
      return
      end
c
c********************************************************************
c
      subroutine shatcdom(z,s1m,s1p)
c     core routine on file shat.f
c
c     This routine computes the effective internal source terms (in 
c     spectral form) for CDOM fluorescence.  The formulation of 
c     CM 24 June 94, p 13, is used.
c
      INCLUDE "DIMENS_XL.INC"
      COMMON /CEospl/ nspl,zspl(mxz),Eospl(mxz,mxwave),
     1                E2spl(mxz,mxwave),Edspl(mxz,mxwave)
c
      COMMON /Cwave/ wave(mxwave),waveb(mxwave+1),fijchl(mxwave,mxwave),
     1               fijcdom(mxwave,mxwave),fijraman(mxwave,mxwave)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Cmisc/ imisc(30),fmisc(30)
c
      dimension s1m(mxmu),s1p(mxmu)
      data i1/1/
      save i1
c
      nmu = imisc(1)
      pi = fmisc(1)
      jwave = imisc(11)
      ncomp = imisc(6)
      depth = z
c
      if(jwave.eq.1) then
c
c        We are at the first wavelength:  there is no CDOM fluorescence 
c        contribution from shorter wavelengths
c  
         do i=1,nmu
            s1m(i) = 0.0
            s1p(i) = 0.0
         end do
         return
      endif
c
c     loop over all wavelengths shorter than the current wavelength and
c     compute the CDOM fluorescence contribution
c
      sum0 = 0.0
      do iwave=1,jwave-1
c       compute the scalar irradiance Eo at the current depth and incident
c       wavelength
        Eozi = yinterp(i1, nspl, depth, zspl, Eospl(1,iwave))
        Eozi = exp(Eozi)
c       Now compute the contribution from wavelength iwave, using the 
c       parameterized CDOM absorption and the previously discretized
c       CDOM wavelength redistribution function 
        wavelen = wave(iwave)
c       insert a call to the desired aCDOM(z,wavelength) routine:
        call acdomsub(depth,wavelen, abscdom) 
        sum0 = sum0 + Eozi*abscdom*fijcdom(iwave,jwave)
      end do
c
c     get the total attenuation at this depth and emission wavelength
c     (just computed in the call to rhotau)
c
      atotal = fmisc(19)
      btotal = fmisc(20)
c
      factor1 = 1.0/(4.0*pi*(atotal + btotal))
c
      do i=1,nmu
         s1p(i) = factor1*sum0/fmu(i)
         s1m(i) = s1p(i)
      end do
c
      return
      end
c
c******************************************************************
c
      subroutine shatbiol(z,s1m,s1p)
c
c     on part2/shatbiol.f          (written 10/94)
c
c     Routine shatbiol returns the spectral amplitudes for the 
c     current geometric depth and wavelength (or wavelength band),
c     corresponding to an isotropic bioluminescent source.
c
      INCLUDE "DIMENS_XL.INC"
c
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Cwave/ wave(mxwave),waveb(mxwave+1),fijchl(mxwave,mxwave),
     1               fijcdom(mxwave,mxwave),fijraman(mxwave,mxwave)
      COMMON /cmisc/ imisc(30),fmisc(30)
c
      dimension s1m(mxmu),s1p(mxmu)
c
c     obtain the source function for the current depth and wavelength,
c     or wavelength band
c
      nwave = imisc(7)
      if(nwave.eq.1) then
c
c     monochromatic run:  use exact wavelength
c
      wavenm = fmisc(13)
      s0 = s0bioSub(z,wavenm)
      else
c
c     average the source over the current wavelength band:
c
      jwave = imisc(11)
      wave1 = waveb(jwave)
      wave2 = waveb(jwave+1)
      ilam1 = ifix(wave1 + 0.5)
      ilam2 = ifix(wave2 + 0.5)
      count = 0.0
      sum0 = 0.0
      do ilam=ilam1,ilam2
      count = count + 1.0
      wavenm = float(ilam)
      sum0 = sum0 + s0bioSub(z,wavenm)
      end do
      s0 = sum0/count
      endif
c
c     define the shat arrays, which for bioluminescence are nonzero only
c     if L = 0 and p = 1
c
      nmu = imisc(1)
c
c     get beam c at the current wavelength
c     (just computed in the call to rhotau)
c
      atotal = fmisc(19)
      btotal = fmisc(20)
c
c     the 4*pi is for isotropic bioluminescence:
      factor1 = 1.0/(4.0*fmisc(1)*(atotal + btotal))
c
      do i=1,nmu
         s1p(i) = factor1*s0/fmu(i)
         s1m(i) = s1p(i)
      end do
c
      return
      end
