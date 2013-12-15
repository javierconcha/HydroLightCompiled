C     Last change:  CDM 05 Dec 2008
      subroutine kfcn
c 
c     core routine on file kfcn.f   
c
c     called by RADANAL [MAIN->RADANAL->KFCN]
c 
c     This routine computes the K-functions associated with the scalar
c     and plane irradiances and Lu.  The functions are computed as rates 
c     of change with respect to geometric (and optionally, optical) depth. 
c 
c+++++WARNING:  Any pair of depths zgeo(iz) and zgeo(iz+1) can be used to 
c               estimate the LOCAL K's at the midpoint, but these estimates may be 
c               quite inaccurate if the zgeo's are not closely spaced. 

c     Changed for HE5: use finite differences of log(E), rather than E,
c                      as shown in the HE5 UG section 5.7
c 
      INCLUDE "DIMENS_XL.INC"
c 
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Cpkfcn/ ipkfcn,izkfcn(mxz) 
      COMMON /Cirrad/ Eou(0:mxz),Eod(0:mxz),Eu(0:mxz),Ed(0:mxz), 
     1                fMUu(0:mxz),fMUd(0:mxz),fMUtot(0:mxz),R(0:mxz),
     2                E2(0:mxz)
      COMMON /Cradif/ RADMa(mxmu,mxphi),RADMz(mxmu,mxphi,mxz),
     1                RADPa(mxmu,mxphi),RADPz(mxmu,mxphi,mxz)
      COMMON /Cmisc/ imisc(30),fmisc(30)
      COMMON /CKxcl/ nzKxcl,zKxcl(mxz),fKdxcl(mxz,mxwave),
     1  fKuxcl(mxz,mxwave),fKoxcl(mxz,mxwave),
     2  fKnetxcl(mxz,mxwave),fKLuxcl(mxz,mxwave)
c
c     declare temp vars
      integer nmu, iwvl
      real wavelen
c
      nmu = imisc(1)
      iwvl = imisc(11)
      wavelen = fmisc(13)
c 
c     In general, don't bother with optical-depth K functions (skip
c     optical-depth K function if iskip = 1):
      iskip = 1
      if(iskip.ne.1) then
      write(10,300) wavelen
      
      do iiz=1,ipkfcn
        iz = izkfcn(iiz)
        c = -1./(zeta(iz+1) - zeta(iz)) 
        zetamid = 0.5*(zeta(iz+1) + zeta(iz))
        fKou = c*alog(Eou(iz+1)/Eou(iz)) 
        fKod = c*alog(Eod(iz+1)/Eod(iz)) 
        fKo = c*alog((Eou(iz+1)+Eod(iz+1))/(Eou(iz) + Eod(iz)))
        fKu = c*alog(Eu(iz+1)/Eu(iz)) 
        fKd = c*alog(Ed(iz+1)/Ed(iz))
        fKnet = c*alog((Ed(iz+1) - Eu(iz+1))/(Ed(iz) - Eu(iz)))
        fKLu = c*alog(radmz(nmu,1,iz+1) / radmz(nmu,1,iz))      

      write(10,302) zeta(iz),zeta(iz+1),zetamid,fKou,fKod,fKo,fKu,fKd,
     1fKnet,fKLu 
     
      end do
      end if ! iskip
c 
c     geometric-depth K functions
c
      if(imisc(9).ge.0) then
		write(10,400) wavelen
      else
		write(10,500) wavelen
      endif

      do iiz=1,ipkfcn
        iz = izkfcn(iiz)
        c = -1.0/(zgeo(iz+1) - zgeo(iz)) 
        zmid = 0.5*(zgeo(iz+1) + zgeo(iz))
        fKou = c*alog(Eou(iz+1)/Eou(iz)) 
        fKod = c*alog(Eod(iz+1)/Eod(iz)) 
        fKo = c*alog((Eou(iz+1)+Eod(iz+1))/(Eou(iz) + Eod(iz)))
        fKu = c*alog(Eu(iz+1)/Eu(iz)) 
        fKd = c*alog(Ed(iz+1)/Ed(iz))
        fKnet = c*alog((Ed(iz+1) - Eu(iz+1))/(Ed(iz) - Eu(iz)))
        fKLu = c*alog(radmz(nmu,1,iz+1) / radmz(nmu,1,iz))
c
      if(imisc(9).ge.0) then
	  write(10,302) zgeo(iz),zgeo(iz+1),zmid,fKou,fKod,fKo,fKu,fKd,
     1                   fKnet,fKLu
      else
	  write(10,302) zgeo(iz),zgeo(iz+1),zmid,fKo,fKu,fKd,fKnet,fKLu
      endif
c
c     save selected K-functions for the Excel spreadsheet routines
c
      zKxcl(iiz) = zmid
      fKdxcl(iiz,iwvl) = fKd
      fKuxcl(iiz,iwvl) = fKu
      fKoxcl(iiz,iwvl) = fKo
      fKnetxcl(iiz,iwvl) = fKnet
      fKLuxcl(iiz,iwvl) = fKLu
c
      end do
c
      return
c 
  300 FORMAT(///2x,'OPTICAL-DEPTH K-functions (nondimensional)',
     1' at ',f6.1,' nm (valid',
     1' only when zetaupper and zetalower are closely spaced)'//
     2'   zetaupper zetalower    zeta',2x, 
     3'Kou(zeta) Kod(zeta) Ko(zeta)  Ku(zeta)  Kd(zeta)  Knet(zeta)',
     4' KLu(zeta)'/)
  302 FORMAT(3F10.3,7F10.5) 
  400 FORMAT(///2x,'K-functions (units of 1/meter)',
     1' at ',f6.1,' nm (these are accurate local values only when',
     1' zupper and zlower are closely spaced)'//
     2'    zupper    zlower',7X,'z',6X,
     3'Kou(z)    Kod(z)    Ko(z)     Ku(z)     Kd(z)    Knet(z)',
     4'    KLu(z)'/)
  500 FORMAT(//2x,'K-functions (units of 1/meter)',
     1' at ',f6.1,' nm (these are accurate local values only when',
     1' zupper and zlower are closely spaced)'//
     2'    zupper    zlower',7X,'z',6X,
     3'Ko(z)     Ku(z)     Kd(z)    Knet(z)    KLu(z)'/)
!  502 FORMAT(3F10.3,5F10.5) 
      END 
