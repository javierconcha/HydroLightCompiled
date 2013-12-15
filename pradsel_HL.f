C     Last change:  LKS   1 Jul 2008    4:55 am
      subroutine pradsel
c 
c     core routine on file pradsel.f
c 
c     called by RADANAL [MAIN->RADANAL->PRADSEL]
c 
c     This routine prints selected radiances (zenith, nadir, and horizontal
c     along-wind and cross-wind).  Radiance-irradiance ratios are also
c     computed and printed. 
c 
      INCLUDE "DIMENS_XL.INC"
c
      COMMON /Cradif/ RADMa(mxmu,mxphi),RADMz(mxmu,mxphi,mxz),
     1 RADPa(mxmu,mxphi),RADPz(mxmu,mxphi,mxz) 
      COMMON /Cradir/ RAD0Ma(mxmu,mxphi),RAD0Pa(mxmu,mxphi), 
     1 RAD0Pz(mxmu,mxphi,mxz)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Cirrad/ Eou(0:mxz),Eod(0:mxz),Eu(0:mxz),Ed(0:mxz), 
     1                fMUu(0:mxz),fMUd(0:mxz),fMUtot(0:mxz),R(0:mxz),
     2                E2(0:mxz)
      COMMON /Cpirrad/ ipirad,izirad(mxz)
      COMMON /CMISC/  IMISC(30),FMISC(30) 
c 
c     declare temporary vars
      integer nmu, nphi
      real wavelen
c
      nmu = imisc(1)
      nphi = imisc(2)
      wavelen = fmisc(13) 
c 
c     Print in-air radiances
      if(imisc(9).ge.0) CALL PNTRAD  !don't print in-air values if minimal output

c     zeta = a (in the air, just above the surface)
c 
c     radup is the TOTAL upward radiance, including sky radiance
c     that is reflected upward by the water surface.
c     radw is the "water-leaving" radiance, which does not contain
c     the reflected sky radiance.
c
      radup = RAD0Ma(nmu,1) + RADMa(nmu,1)
      radw = RADMa(nmu,1)
      raddn = RAD0Pa(nmu,1) + RADPa(nmu,1)
      rh0 = 0.5*(RAD0Ma(1,1) + RADMa(1,1) + RAD0Pa(1,1) + RADPa(1,1)) 
      j90 = nphi/4 + 1
      rh90 = 0.5*(RAD0Ma(1,j90) + RADMa(1,j90) + RAD0Pa(1,j90) +
     1  RADPa(1,j90)) 
      j180 = nphi/2 + 1 
      rh180 = 0.5*(RAD0Ma(1,j180) + RADMa(1,j180) + RAD0Pa(1,j180) +
     1  RADPa(1,j180))
      Rrstot = radup/Ed(0)
      Q = Eu(0)/radup
      Rrs = radw/Ed(0)
c
      if(imisc(9).ge.0) then
         write(10,100) wavelen
         write(10,102) radup,raddn,rh0,rh90,rh180,Rrstot,Q,radw,Rrs 
	else
         write(10,200) wavelen
         write(10,202) radup,raddn,Rrstot,Q,radw,Rrs
      endif
c 
c     depths w .le. zeta .le. m
c 
      do iiz=1,ipirad
      iz = izirad(iiz)
      radup = RADMz(nmu,1,iz)
      raddn = RAD0Pz(nmu,1,iz) + RADPz(nmu,1,iz)
      rh0 = 0.5*(RADMz(1,1,iz) + RAD0Pz(1,1,iz) + RADPz(1,1,iz)) 
      rh90 = 0.5*(RADMz(1,j90,iz) + RAD0Pz(1,j90,iz) + RADPz(1,j90,iz))
      rh180 = 0.5*(RADMz(1,j180,iz) + RAD0Pz(1,j180,iz) +
     1        RADPz(1,j180,iz))
      Rrs = radup/Ed(iz) 
      Q = Eu(iz)/radup 
      if(imisc(9).ge.0) then
		write(10,104) iz,zeta(iz),zgeo(iz),radup,raddn,
     1                rh0,rh90,rh180,Rrs,Q	
      else
         write(10,204) iz,zeta(iz),zgeo(iz),radup,raddn,Rrs,Q
      endif
      end do
c 
      return
C 
  100 FORMAT(///2x,'Selected Radiances (units of W/m^2 sr nm) and',
     1' Radiance-Irradiance Ratios at ',f6.1,' nm'//2x,
     6'[Here (theta,phi) are directions of photon travel relative',
     7' to nadir (solar photons travel along phi=180).]'//
     1'   iz   zeta     z       Lu(z)        Ld(z)    Lh(z,phi=0)',
     22X,'Lh(z,phi=90) Lh(z,phi=180)    Lu/Ed      Q = Eu/Lu',
     3'      Lw(z)     Rrs = Lw/Ed'/
     416x,'(m)   (theta=180)   (theta=0)   (theta=90)   (theta=90)',3x,
     5'(theta=90)',6x,'(1/sr)',7x,'(sr)',6x,'(theta=180)    (1/sr)'/)
  102 FORMAT(11X,'in air  ',1P,10E13.3/)
  104 FORMAT(I5,2F7.2,1P,7E13.3) 
c
  200 FORMAT(//2x,'Selected Radiances (units of W/m^2 sr nm) and',
     1' Radiance-Irradiance Ratios at ',f6.1,' nm'//,
     1'   iz   zeta     z       Lu(z)        Ld(z)        ',
     3'Lu/Ed      Q = Eu/Lu      Lw(z)     Rrs = Lw/Ed'/
     416x,'(m)   (total up)  (total down)',5x,
     5'(1/sr)',8x,'(sr)    (water-leave)    (1/sr)'/)
  202 FORMAT(11X,'in air  ',1P,6E13.3/)
  204 FORMAT(I5,2F7.2,1P,4E13.3) 
      END 
