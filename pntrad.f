C     Last change:  LKS  26 May 2008    8:44 pm
      subroutine pntrad
c 
c     core routine on file pntrad.f
c
c     not called if only minimal printout requested
c 
c     called by PRADSEL [MAIN->RADANAL->PRADSEL->PNTRAD]
c 
c     This routine prints out the final quad-averaged radiances at 
c     selected depths and directions, at the current wavelength.
c
      INCLUDE "DIMENS_XL.INC"
c
      COMMON /CRADIF/ RADMa(mxmu,mxphi),RADMz(mxmu,mxphi,mxz),
     1          RADPa(mxmu,mxphi),RADPz(mxmu,mxphi,mxz)
      COMMON /CRADIR/ RAD0Ma(mxmu,mxphi),RAD0Pa(mxmu,mxphi), 
     1 RAD0Pz(mxmu,mxphi,mxz)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Cprad/ iprad,iprad1,iprad2,iprad3,jprad1,jprad2,jprad3,
     1 izprad(mxz) 
      COMMON /Cirrad/ Eou(0:mxz),Eod(0:mxz),Eu(0:mxz),Ed(0:mxz), 
     1                fMUu(0:mxz),fMUd(0:mxz),fMUtot(0:mxz),R(0:mxz),
     2                E2(0:mxz)
      COMMON /Cmisc/ imisc(30),fmisc(30)
      COMMON /Ctitle/ ititle
      Character ititle*120
c     declare temp vars
      integer nmu, nphi
      real radeg, wavelen
c
      data kall/0/
c 
      dimension thedeg(mxmu),phideg(mxphi)
c 
      save kall,thedeg,phideg
c
      nmu = imisc(1)
      nphi = imisc(2) 
      radeg = fmisc(3)
      wavelen = fmisc(13)
c
      if(kall.eq.0) then
c        get theta and phi in degrees
         thedeg(1) = 0.5*(90.0 + radeg*acos(bndmu(1))) 
         do i=2,nmu-1
            thedeg(i) = 0.5*radeg*(acos(bndmu(i-1)) + acos(bndmu(i)))
         end do
         thedeg(nmu) = 0.
         do j=1,nphi
            phideg(j) = radeg*phi(j)
         end do
         kall = 1
      endif
c 
c     write radiances in the air (at zeta = a)
c 
      write(10,300) wavelen
      do i=iprad1,iprad2,iprad3
 
      if(i.eq.nmu) then
c
c        polar cap
         j = 1 
c        allow for a black sky
         if(Rad0pa(i,j).ne.0.0) then
            radrefl = Rad0ma(i,j)/Rad0pa(i,j)
	   else
            radrefl = 0.0
         endif

         write(10,304) i,j,thedeg(i),phideg(j),phideg(j),
     1   Rad0pa(i,j),RADMa(i,j),
     1   RAD0Ma(i,j),RADMa(i,j)+RAD0Ma(i,j),Radma(i,j)/Ed(0),radrefl

      else
c
c        nonpolar quads

      do j=jprad1,jprad2,jprad3

c        allow for a black sky
         if(Rad0pa(i,j).ne.0.0) then
            radrefl = Rad0ma(i,j)/Rad0pa(i,j)
	   else
            radrefl = 0.0
         endif

         write(10,304) i,j,thedeg(i),phideg(j),abs(180-phideg(j)),
     1    Rad0pa(i,j),RADMa(i,j),
     2    RAD0Ma(i,j),RADMa(i,j)+RAD0Ma(i,j),RADMa(i,j)/Ed(0),radrefl

      end do

      endif
      end do   ! end i loop

      if(iprad.eq.1) return
c 
c     write radiances in the water (at zeta = w, ...,m)
c 
      write(10,100) wavelen
      write(10,110)

      do izz=1,iprad
         iz = izprad(izz)

      do i=iprad1,iprad2,iprad3
c
      if(i.eq.nmu) then
c        polar cap
         j = 1 
         write(10,104) i,j,iz,thedeg(i),phideg(j),phideg(j),zeta(iz),
     1   zgeo(iz), 
     1   RAD0Pz(i,j,iz),RADPz(i,j,iz),RAD0Pz(i,j,iz)+RADPz(i,j,iz),
     2   RADMz(i,j,iz)
c
      else
c     nonpolar quads
         do j=jprad1,jprad2,jprad3  !/2
         write(10,104)i,j,iz,thedeg(i),phideg(j),abs(180-phideg(j)),
     1   zeta(iz),zgeo(iz)
     1   ,RAD0Pz(i,j,iz),RADPz(i,j,iz),RAD0Pz(i,j,iz)+RADPz(i,j,iz),
     2   RADMz(i,j,iz)
         end do
      endif

      end do     ! end i loop
c
      end do     ! end izz loop
c 
      return
c 
  100 format(//2x,'Direct, Diffuse, and Total Radiances Within the',
     1' Water (units of W/m^2 sr nm) at ',f6.1,' nm',//,
     23x,'[(Theta,Phi) are the directions of photon travel.',/
     33x,'Phi-view is the viewing direction (the direction an instrum',
     4'ent would point to measure the radiance).',/
     53x,'Theta is measured from the nadir for downwelling radiance.',/
     63x,'Theta is measured from the zenith for upwelling radiance.',
     72x,'Upwelling radiance is always diffuse.]')
  104 FORMAT(3I4,3F9.1,2F8.3,1P,4E15.5)
  110 FORMAT(/'   I   J   K',4X,'Theta',5X,'Phi   Phi-view',3X,
     1'zeta     z', 
     1 4x,'L(down direct) L(down diffuse) L(down total)',3x,
     2 'L(up total)') 

  300 format(///2x'Radiances Just Above the Water Surface (at z = a)',
     1' (units of W/m^2 sr nm) at ',f6.1,' nm'//,
     23x,'[(Theta,Phi) are the directions of photon travel.'/
     33x,'Theta is measured from 0 at the zenith.'/
     43x,'Solar photons travel in the Phi = 180 direction (the sun',
     5' is located at Phi = 0).'/
     63x,'(Thus Phi = 45 represents the',
     7' 135 degree viewing angle for minimizing sun glitter.)]'//
     1'   I   J',4X,'Theta',6X,'Phi  Phi-view',6X,'L(sky)',6x,
     2'L(water-lv)',
     24X,'L(refl-sky)',4X,'L(total up)',7x,'Rrs',11x,'rho'/,
     311x,'(zenith)', 93x, '(Lrefl/Lsky)')
  304 FORMAT(2I4,3F9.1,1P,5E15.5,0p,f12.6)
C 
      END
