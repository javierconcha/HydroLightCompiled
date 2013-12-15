C     Last change:  LKS   7 Aug 2008    7:22 pm
      subroutine irrad_old
c 
c     core routine on file irrad.f
c
c     called by RADANAL [MAIN->RADANAL->IRRAD]
c 
c     This routine computes various irradiances (using the L = 0
c     radiance amplitudes, which are equivalent to the azimuthally
c     averaged radiances; see Eqs 8.5 and 8.8 of Tech Memo ERL PMEL-75).
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
      PARAMETER (mxamp=2*mxmu*(mxphi/2 + 1))
c 
      COMMON /CRAMP0/ RAMP0Pa(mxamp),RAMP0Pz(mxamp,mxz),RAMP0Ma(mxamp) 
      COMMON /CRAMP/ RAMPPa(mxamp),RAMPMa(mxamp),RAMPPz(mxamp,mxz),
     1  RAMPMz(mxamp,mxz)
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
      nz = imisc(4) 
      twopi = 2.*fmisc(1)
      wavelen = fmisc(13)
c 
c     compute quantities in the air (at zeta = a) 
c 
      sum1 = 0. 
      sum2 = 0. 
      sum3 = 0.
      sum4 = 0.
      do i=1,nmu
      ampM = totalr*RAMP0Ma(i) + RAMPMa(i) 
      ampP = totalr*RAMP0Pa(i)
      dmu = deltmu(i) 
      sum1 = sum1 + ampM*dmu
      sum2 = sum2 + ampP*dmu
      sum3 = sum3 + ampM*fmu(i)*dmu 
      sum4 = sum4 + ampP*fmu(i)*dmu
      end do
c 
      Eou(0) = twopi*sum1 
      Eod(0) = twopi*sum2 
      Eu(0) = twopi*sum3 
      Ed(0) = twopi*sum4 
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
c 
c     compute irradiances from amplitudes 
c
       
      do i=1,nmu
c        define the total amplitudes (diffuse + direct) for L = 0
         ampM = RAMPMz(i,iz)
         ampP = RAMPPz(i,iz) + totalr*RAMP0Pz(i,iz)
!      write(66,'(2f6.1,i4,1p2E12.3)') wavelen,zgeo(iz), i, ampM,ampP
         dmu = deltmu(i) 
         sum1 = sum1 + ampM*dmu
         sum2 = sum2 + ampP*dmu
         sum3 = sum3 + ampM*fmu(i)*dmu 
         sum4 = sum4 + ampP*fmu(i)*dmu 
         sum5 = sum5 + (3.0*fmu(i)*fmu(i) - 1.0)*(ampM + ampP)*dmu
      end do
      Eou(iz) = twopi*sum1
      Eod(iz) = twopi*sum2
      Eu(iz) = twopi*sum3
      Ed(iz) = twopi*sum4
      E2(iz) = 0.5*twopi*sum5
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
