C     Last change:  LKS   1 Nov 2007   12:57 pm
      SUBROUTINE STORXCEL
c
c     This subroutine stores selected data as a function of depth
c     and wavelength, as Hydrolight works through the wavelengths in
c     a multi-wavelength run.  These stored arrays are written to
c     the multi-wavelength file (Mroot.txt) by routine WRTXCELM at
c     the end of the run, for postprocessing by the Excel macro
c     multiwl.xcl.
c
c     Information is saved at the user-selected output depths
c
      INCLUDE "DIMENS_XL.INC"
c
c     Cxcl holds arrays accumulated in routine storexcl
      COMMON /Cxcl/  nzxcl,zxcl(mxz),izxcl(mxz),
     1               axcl(mxz,mxwave,0:mxcomp),
     2		   bxcl(mxz,mxwave,0:mxcomp),
     3         	   bbxcl(mxz,mxwave,0:mxcomp),
     4               Edxcl(0:mxz,mxwave),Euxcl(0:mxz,mxwave),
     5               Eodxcl(0:mxz,mxwave),Eouxcl(0:mxz,mxwave),
     5               Rxcl(0:mxz,mxwave),
     6               Raduxcl(0:mxz,mxwave),Radwxcl(mxwave)
      COMMON /CKxcl/ nzKxcl,zKxcl(mxz),fKdxcl(mxz,mxwave),
     1  fKuxcl(mxz,mxwave),fKoxcl(mxz,mxwave),
     2  fKnetxcl(mxz,mxwave),fKLuxcl(mxz,mxwave)
      COMMON /Ciop/ acoef(mxz,0:mxcomp),bcoef(mxz,0:mxcomp),
     1		      atten(mxz),albedo(mxz), bbcoef(mxz,0:mxcomp)
      COMMON /Cradif/ RADMa(mxmu,mxphi),RADMz(mxmu,mxphi,mxz),
     1  RADPa(mxmu,mxphi),RADPz(mxmu,mxphi,mxz)
      COMMON /Cradir/ RAD0Ma(mxmu,mxphi),RAD0Pa(mxmu,mxphi), 
     1  RAD0Pz(mxmu,mxphi,mxz) 
      COMMON /Cirrad/ Eou(0:mxz),Eod(0:mxz),Eu(0:mxz),Ed(0:mxz), 
     1                fMUu(0:mxz),fMUd(0:mxz),fMUtot(0:mxz),R(0:mxz),
     2                E2(0:mxz)
      COMMON /Cpirrad/ npirad,izirad(mxz)
      COMMON /Cpkfcn/ npkfcn,izkfcn(mxz) 
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Cwave/ wave(mxwave),waveb(mxwave+1),fijchl(mxwave,mxwave),
     1               fijcdom(mxwave,mxwave),fijraman(mxwave,mxwave) 
      COMMON /Cmisc/ imisc(30),fmisc(30)
c
      data kall/0/
c
      save kall
c
      nmu = imisc(1)
      iwvl = imisc(11)
c      nz = imisc(4)
c      ibotm = imisc(12)
c
      if(kall.eq.0) then
c
c     On the first call, set up the depth indices for spreadsheet
c     output.  Just use the user-selected irradiance and IOP
c     output depths.

      nzxcl = npirad
      do i= 1,npirad
        izxcl(i) = izirad(i)
        zxcl(i) = zgeo(izxcl(i))
      enddo
c
c     use same number of depths for K-function spreadsheet output
c     as for printout (depths are set in routine kfcn)
      nzKxcl = npkfcn

      kall = 1
      endif
c
c     save in-water arrays at the current wavelength
c
      do iz=1,nzxcl
       do ii=0,imisc(23)
         axcl(iz,iwvl,ii) = acoef(izxcl(iz),ii)
         bxcl(iz,iwvl,ii) = bcoef(izxcl(iz),ii)
         bbxcl(iz,iwvl,ii) = bbcoef(izxcl(iz),ii)
       enddo
         Edxcl(iz,iwvl) = Ed(izxcl(iz))
         Euxcl(iz,iwvl) = Eu(izxcl(iz))
         Eouxcl(iz,iwvl) = Eou(izxcl(iz))
         Eodxcl(iz,iwvl) = Eod(izxcl(iz))
         Rxcl(iz,iwvl) = R(izxcl(iz))
         Raduxcl(iz,iwvl) = Radmz(nmu,1,izxcl(iz))  !Lu
      end do
c
c     save air values at depth index 0, where meaningful
c
      Edxcl(0,iwvl) = Ed(0)
      Euxcl(0,iwvl) = Eu(0)
      Eouxcl(0,iwvl) = Eou(0)
      Eodxcl(0,iwvl) = Eod(0)
      Rxcl(0,iwvl) = R(0)
      Raduxcl(0,iwvl) = Radma(nmu,1) + Rad0ma(nmu,1) !total rad
      Radwxcl(iwvl) = Radma(nmu,1)  !Lw
c
      return
      end

