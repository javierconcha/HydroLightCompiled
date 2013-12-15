C     Last change:  LKS  26 Apr 2008    4:00 pm
      subroutine RADCHECK
!     called by SYNHL to check that all radiances are non-negative

      INCLUDE "DIMENS_XL.INC"
      COMMON /CRADIF/ RADMa(mxmu,mxphi),RADMz(mxmu,mxphi,mxz),
     1          RADPa(mxmu,mxphi),RADPz(mxmu,mxphi,mxz)
      COMMON /CRADIR/ RAD0Ma(mxmu,mxphi),RAD0Pa(mxmu,mxphi), 
     1 RAD0Pz(mxmu,mxphi,mxz)
      COMMON /CMISC/ imisc(30),FMISC(30) 
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /Cpirrad/ npirad,izirad(mxz)

      integer nmu, nphi, nz, errL, izerr
!     --------------------------------------------------------------

      nmu = imisc(1)
      nphi = imisc(2)
      nz = izirad(npirad)
c      Check that all radiances are non-negative
      errL = 0 
      izerr= 1
      do iu=nmu-1,1,-1
        do iv=1,nphi
          if(rad0pa(iu,iv).lt.0 .or.rad0ma(iu,iv).lt.0
     1       .or. radpa(iu,iv).lt.0 .or.radma(iu,iv).lt.0) errL=1
          if(rad0pa(iu,iv).lt.0) rad0pa(iu,iv)=0
          if(rad0ma(iu,iv).lt.0) rad0ma(iu,iv)=0
          if(radpa(iu,iv).lt.0)  radpa(iu,iv)=0
          if(radma(iu,iv).lt.0)  radma(iu,iv)=0
          do iz=nz,1,-1
           If(rad0pz(iu,iv,iz).lt.0 .or.radpz(iu,iv,iz).lt.0
     1        .or. radmz(iu,iv,iz).lt.0) then
             errL = 1
             izerr = iz
           Endif
           if(rad0pz(iu,iv,iz).lt.0) rad0pz(iu,iv,iz)=0
           if(radpz(iu,iv,iz).lt.0)  radpz(iu,iv,iz)=0
           if(radmz(iu,iv,iz).lt.0)  radmz(iu,iv,iz)=0
          enddo
        enddo
        !  handle polar cap, iu = nmu 
         if(rad0pa(nmu,1).lt.0 .or. radma(nmu,1).lt.0) errL=1
        if(rad0pa(nmu,1).lt.0) rad0pa(nmu,1)=0
        if(radma(nmu,1).lt.0)  radma(nmu,1)=0
        do iz=nz,1,-1
           If(rad0pz(nmu,1,iz).lt.0 .or.radpz(nmu,1,iz).lt.0
     1        .or. radmz(nmu,1,iz).lt.0) then
             errL=1
             izerr = iz
           Endif
           if(rad0pz(nmu,1,iz).lt.0) rad0pz(nmu,1,iz)=0
           if(radpz(nmu,1,iz).lt.0)  radpz(nmu,1,iz)=0
           if(radmz(nmu,1,iz).lt.0)  radmz(nmu,1,iz)=0
        enddo
      enddo
      If(errL.gt.0) write(10,100) zgeo(izerr)

      return
  100 format(//,'WARNING:  some calculated radiances were negative.',
     1  '  Negative radiances have been set to zero. ',
     1  /5x,'Results below ',f6.1,'m may not be correct. ')
      end subroutine
