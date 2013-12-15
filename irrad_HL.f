C     Last change:  LKS  26 Apr 2008    8:15 am
      subroutine irrad
c 
c     core routine on file irrad.f
c
c     called by RADANAL [MAIN->RADANAL->IRRAD]
c 
c     This routine computes various irradiances.
c     Mean cosines and the irradiance reflectance are also computed 
c     and printed.

c     Total radiances are used by default, but
c     diffuse radiances can be selected by setting totalr = 0. 

c     Irradiances are _computed_ at all zeta levels, for possible use in 
c     computing K-functions, etc., but _printout_ is only at selected 
c     depths, as specified in routine initial. 
c 
c     The zero element of arrays holds the values for zeta = a (in the air)
c 
      INCLUDE "DIMENS_XL.INC"
      COMMON /CRADIF/ RADMa(mxmu,mxphi),RADMz(mxmu,mxphi,mxz),
     1          RADPa(mxmu,mxphi),RADPz(mxmu,mxphi,mxz)
      COMMON /CRADIR/ RAD0Ma(mxmu,mxphi),RAD0Pa(mxmu,mxphi), 
     1 RAD0Pz(mxmu,mxphi,mxz)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Cirrad/ Eou(0:mxz),Eod(0:mxz),Eu(0:mxz),Ed(0:mxz), 
     1  fMUu(0:mxz),fMUd(0:mxz),fMUtot(0:mxz),R(0:mxz),E2(0:mxz)
      COMMON /Cpirrad/ ipirad,izirad(mxz)
      COMMON /Cmisc/ imisc(30),fmisc(30) 
c
c     declare temporary vars
      integer nmu, nz
      real twopi, wavelen

c     set totalr = 1. if total radiances are to be used 
c     set totalr = 0. if diffuse radiances are to be used 
      totalr = 1.0
c 
      nmu = imisc(1)
      nphi = imisc(2)
      nz = imisc(4) 
      twopi = 2.*fmisc(1)
      DELPHI = 1./FLOAT(nphi)
      wavelen = fmisc(13)
c 
c     compute quantities in the air (at zeta = a) 
c 
      sum1 = 0. 
      sum2 = 0. 
      sum3 = 0.
      sum4 = 0.
!     polar cap
      i = nmu
      dmu = deltmu(i) 
      ampM = totalr*RAD0Ma(i,1) + RADMa(i,1) 
      ampP = totalr*RAD0Pa(i,1)
      sum1 = sum1 + ampM * dmu 
      sum2 = sum2 + ampP * dmu 
      sum3 = sum3 + ampM*fmu(i) * dmu 
      sum4 = sum4 + ampP*fmu(i) * dmu
!     rest of thetas
      do i=1,nmu-1
        dmu = deltmu(i) 
        do iv=1,nphi
          ampM = totalr*RAD0Ma(i,iv) + RADMa(i,iv) 
          ampP = totalr*RAD0Pa(i,iv)
          sum1 = sum1 + ampM * dmu * delphi
          sum2 = sum2 + ampP * dmu * delphi
          sum3 = sum3 + ampM*fmu(i) * dmu * delphi  
          sum4 = sum4 + ampP*fmu(i) * dmu * delphi 
        enddo
      end do
c 
      Eou(0) = sum1 * twopi 
      Eod(0) = sum2 * twopi 
      Eu(0) = sum3 * twopi 
      Ed(0) = sum4 * twopi 
c 
      Eo = Eou(0) + Eod(0)
      fMUu(0) = Eu(0)/Eou(0)
      fMUd(0) = Ed(0)/Eod(0) 
      fMUtot(0) = (Ed(0) - Eu(0))/Eo
      R(0) = Eu(0)/Ed(0) 
c 
      if(imisc(9).ge.0) then
c         *write in air values
		write(10,200) wavelen
		write(10,203) Eou(0),Eod(0),Eo,Eu(0),Ed(0),fMUu(0),
     1                      fMUd(0),fMUtot(0),R(0)
		if(totalr.ne.1.) write(10,201)
	else
c	   ** minimal printout selected
c         *write in air values
		write(10,300) wavelen
		write(10,303) Eo,Eu(0),Ed(0),fMUu(0),
     1                      fMUd(0),fMUtot(0),R(0)
		if(totalr.ne.1.) write(10,201)
	endif
	
c
c     Compute quantities within the water (w, ...,zeta, ,,,.m)
c
      do iz=1,nz
      sum1 = 0. 
      sum2 = 0. 
      sum3 = 0.
      sum4 = 0.
      sum5 = 0.
!     polar cap
      i = nmu
      dmu = deltmu(i) 
      ampM = totalr*RADMz(i, 1, iz)
      ampP = totalr*RADPz(i, 1, iz) +RAD0Pz(i, 1, iz)  ! = direct + diffuse
      sum1 = sum1 + ampM * dmu 
      sum2 = sum2 + ampP * dmu
      sum3 = sum3 + ampM*fmu(i) * dmu
      sum4 = sum4 + ampP*fmu(i) * dmu
      sum5 = sum5 + (3.0*fmu(i)*fmu(i) - 1.0)*(ampM + ampP)*dmu * twopi 
!     rest of thetas
      do i=1,nmu-1
        dmu = deltmu(i) 
        do iv=1,nphi
          ampM = totalr*RADMz(i, iv, iz)
          ampP = totalr*RADPz(i, iv, iz) +RAD0Pz(i, iv, iz)  ! = direct + diffuse
          sum1 = sum1 + ampM * dmu * delphi
          sum2 = sum2 + ampP * dmu * delphi
          sum3 = sum3 + ampM*fmu(i) * dmu * delphi  
          sum4 = sum4 + ampP*fmu(i) * dmu * delphi 
          sum5 = sum5 + (3.0*fmu(i)*fmu(i)-1.0)*(ampM+ampP)*dmu*delphi 
        enddo
      end do
      Eou(iz) = sum1 * twopi
      Eod(iz) = sum2 * twopi
      Eu(iz) = sum3 * twopi
      Ed(iz) = sum4 * twopi
      E2(iz) = 0.5*sum5 * twopi

c 
      Eo = Eou(iz) + Eod(iz)
      fMUu(iz) = Eu(iz)/Eou(iz) 
      fMUd(iz) = Ed(iz)/Eod(iz) 
      fMUtot(iz) = (Ed(iz) - Eu(iz))/Eo
      R(iz) = Eu(iz)/Ed(iz) 
c 
c     check for printout
      if(imisc(9).ge.0) then
c         *write in water values
         iprint = 0
         do iiz=1,ipirad
           if(iz.eq.izirad(iiz)) iprint = 1
         end do
	   if(iprint.ne.0) write(10,202) iz,zeta(iz),zgeo(iz),Eou(iz), 
     1	Eod(iz),Eo,Eu(iz),Ed(iz),fMUu(iz),fMUd(iz),fMUtot(iz),R(iz)
	else
c	   ** minimal printout selected
c         *write in water values
         iprint = 0
         do iiz=1,ipirad
           if(iz.eq.izirad(iiz)) iprint = 1
        end do
	   if(iprint.ne.0) write(10,302) iz,zeta(iz),zgeo(iz), 
     1	Eo,Eu(iz),Ed(iz),fMUu(iz),fMUd(iz),fMUtot(iz),R(iz)
      endif
c
      end do
C 
      RETURN
C
  200 format(///2x,'Irradiances (units of W/m^2 nm), Mean Cosines',
     1' (Mubars), and Irradiance Reflectance at ',f6.1,' nm'//
     2'   iz   zeta   z(m)',8x,'Eou',12x,'Eod',13x,'Eo',
     3 13x,'Eu',13x,'Ed',7x,'MUBARu   MUBARd    MUBAR',6x,'R = Eu/Ed'/)
  201 FORMAT(2x,'NOTE:  only the DIFFUSE AMPLITUDES are used to',
     1' compute the irradiances')
  202 FORMAT(I5,2F7.2,1P,5E15.4,0P,3F9.4,1P,E15.4)
  203 FORMAT(11X,'in air',2X,1P,5E15.4,0P,3F9.4,1P,E15.4/)
c
  300 format(//2x,'Irradiances (units of W/m^2 nm), Mean Cosines',
     1' (Mubars), and Irradiance Reflectance at ',f6.1,' nm'//
     2'   iz   zeta   z(m)',8x,'Eo',
     3 13x,'Eu',13x,'Ed',7x,'MUBARu   MUBARd    MUBAR',6x,'R = Eu/Ed'/)
  302 FORMAT(I5,2F7.2,1P,3E15.4,0P,3F9.4,1P,E15.4)
  303 FORMAT(11X,'in air',2X,1P,3E15.4,0P,3F9.4,1P,E15.4/)
C 
      END 
