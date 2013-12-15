C     Last change:  LKS   1 Nov 2007   12:55 pm
      subroutine AMP0a
c 
c     core routine on file amp0a.f
c
c     called by RADAMPS [MAIN->RADAMPS->AMP0a]
c
c     calls MATXMAT (in file matxmat.f)
c
c     This routine Fourier analyzes the quad-averaged incident sky radiances 
c     L0+(a) = radsky to generate the DIRECT BEAM (unscattered) spectral
c     radiance amplitudes Lhat0p+(a) = RAMP0Pa.  Lhat0p+(a) is then transmitted
c     through the air-water surface to get Lhat0p+(w).  Lhat0p+(w) is then 
c     attenuated exponentially to get Lhat0p+(zeta) = RAMP0Pz at all depths.
c     The total incident amplitude Lhatp+(a) = RAMPPa is set equal to
c     Lhat0p+(a) = RAMP0Pa, since all SKY radiance is condidered to be
c     direct-beam radiance.
c 
c     cosine amplitudes are in RAMP0Pa(i), i=1,2,...,nhat 
c     sine   amplitudes are in RAMP0Pa(i + nhat)
c 
c     Air-water-surface spectral storage arrays must be loaded with 
c        that1(a,x) in that1
c        that2(a,x) in that2
c
      INCLUDE "DIMENS_XL.INC"
      PARAMETER (mxL=mxphi/2, mxhat=mxmu*(mxL+1), mxamp=2*mxhat)
C 
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /CRAMP/ RAMPPa(mxamp),RAMPMa(mxamp),RAMPPz(mxamp,mxz),
     1               RAMPMz(mxamp,mxz)
      COMMON /CRAMP0/ RAMP0Pa(mxamp),RAMP0Pz(mxamp,mxz),RAMP0Ma(mxamp) 
      COMMON /Csky/ iskyflag,skyspecs(mxnsky)
      COMMON /Csky_HL/ radsky(mxmu,mxphi)
      COMMON /Crthat/ that1(mxhat,mxhat),that2(mxhat,mxhat), 
     1                rhat1(mxhat,mxhat),rhat2(mxhat,mxhat)
      COMMON /CMISC/ imisc(30),fmisc(30)
C
      nmu = imisc(1)
      nphi = imisc(2) 
      nL = imisc(3) 
      nz = imisc(4) 
      nhat = imisc(10) 
c 
c     Loop over L and mu to define RAMP0Pa via (8.22) and (8.23)
c 
c     First, loop over the mu bands other than the polar cap 
c 
      do i=1,nmu-1
c 
c     define the amplitudes for each L value from (8.22) and (8.23) 
c 
c     L = 0 special case:
      sum = 0.
      do j=1,nphi
         sum = sum + RADsky(i,j)
      end do
      RAMP0Pa(I) = sum/float(nphi) 
      RAMP0Pa(I+nhat) = 0.
C 
C     L = nL special case: 
      sum = 0.
      do j=1,nphi
         sum = sum + RADsky(i,j)*cos(float(nL)*phi(j))
      end do
      RAMP0Pa(nmu*nL+I) = sum/float(nphi)
      RAMP0Pa(nmu*nL+I+nhat) = 0. 
C 
C     0 .LT. L .LT. nL GENERAL CASE: 
C 
      do L=1,nL-1
         sum1 = 0.
         sum2 = 0.
            do j=1,nphi
               sum1 = sum1 + RADsky(i,j)*cos(float(L)*phi(j))
               sum2 = sum2 + RADsky(i,j)*sin(float(L)*phi(j))
            end do
         RAMP0Pa(nmu*L+I) = sum1/float(nL)
         RAMP0Pa(nmu*L+I + nhat) = sum2/float(nL)
      end do
C 
      end do   ! i loop
c 
c     Polar cap special case:  from Eq. (8.32), the cosine amplitude is
c     just the value of the polar cap quad-averaged radiance
c
      RAMP0Pa(nmu) = RADsky(nmu,1) 
      RAMP0Pa(nmu + nhat) = 0.
      DO L=1,nL
         RAMP0Pa(nmu*L+nmu) = 0.
         RAMP0Pa(nmu*L+nmu+nhat) = 0.
      end do
C 
c     Transmit Lhat0+(a) through the air-water surface (from a to w)
c     using Eq. (8.89), to get Lhat0+(w).  Note that in (8.89), Lhat0-(w) = 0
c     because there is no upward DIRECT beam
C 
c     cosine amplitudes:
      call matxmat(RAMP0Pa,that1,1,nhat,nhat,1,mxhat,RAMP0Pz(1,1),1)
c
c     sine amplitudes:
      call matxmat(RAMP0Pa(1+nhat),that2,1,nhat,nhat,1,mxhat,
     1  RAMP0Pz(1+nhat,1),1)
c
c     Transmit RAMP0P(w) to all lower zeta levels, w .lt. zeta .le. m, 
c     using the amplitude equilavent of Eq. (5.25)  (exponential beam
c     attenuation, since this is unscattered radiance)
c 
      irow = 0
      do L=0,nL
         do j=1,nmu
         irow = irow + 1
         RAMPPa(irow) = RAMP0Pa(irow)
         RAMPPa(irow+nhat) = RAMP0Pa(irow+nhat)
            do iz=2,nz
            temp = exp((zeta(1) - zeta(iz))/fmu(j))
            RAMP0Pz(irow,iz) = temp*RAMP0Pz(irow,1)
            RAMP0Pz(irow+nhat,iz) = temp*RAMP0Pz(irow+nhat,1)
            end do
         end do
      end do
C 
      RETURN
      END 

