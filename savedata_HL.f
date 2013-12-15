C     Last change:  LKS   2 Jul 2008    9:00 am
      SUBROUTINE SAVEDATA
c
c     core routine on file savedata.f
c
c     called by RADANAL [MAIN->RADANAL->SAVEDATA]
c
c     This routine writes the HydroLight Droot.txt files.
c 
c     This subroutine writes all pertinent data to a file for later
c     graphical or other analysis.  For convenience, a complete set
c     of data is written for each wavelength, even though the grid
c     data are independent of the wavelength.
c
c     Array elements are written in the order that is most efficient
c     for reading in by IDL graphics routines (see routine READALL.pro
c     in the IDL directory).
c
c     data format changed form 10e13.5 to 10es15.5e3 Aug 2013 for H v5.2
c     to allow for 3 numbers in the exponent, e.g., 1.23456E-100
c
      INCLUDE "DIMENS_XL.INC"
c
      integer ioD, lenD
c
      COMMON /Ciop/ acoef(mxz,0:mxcomp),bcoef(mxz,0:mxcomp),
     1		      atten(mxz),albedo(mxz), bbcoef(mxz,0:mxcomp)
      COMMON /Cradif/ RADMa(mxmu,mxphi),RADMz(mxmu,mxphi,mxz),
     1                RADPa(mxmu,mxphi),RADPz(mxmu,mxphi,mxz)
      COMMON /Cradir/ RAD0Ma(mxmu,mxphi),RAD0Pa(mxmu,mxphi), 
     1                RAD0Pz(mxmu,mxphi,mxz) 
      COMMON /Cirrad/ Eou(0:mxz),Eod(0:mxz),Eu(0:mxz),Ed(0:mxz), 
     1                fMUu(0:mxz),fMUd(0:mxz),fMUtot(0:mxz),R(0:mxz),
     2                E2(0:mxz)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Cwave/ wave(mxwave),waveb(mxwave+1),fijchl(mxwave,mxwave),
     1               fijcdom(mxwave,mxwave),fijraman(mxwave,mxwave) 
      COMMON /Csky/ iskyflag,skyspecs(mxnsky)
      COMMON /Csky_HL/ radsky(mxmu,mxphi)
      COMMON /Cfrstcls/ iabscat,iqasky,iradamps,iradanal, iradxcl
      COMMON /Cmisc/ imisc(30),fmisc(30)
      Character surfname*120,pfname*120,
     1           Drootname*120,Srootname*120,Mrootname*120,
     2           datafiles*120
      COMMON /Cfilenames/ surfname,pfname(mxcomp),
     1                    Drootname,Srootname,Mrootname,
     2                    datafiles(0:7+mxcomp)
      COMMON /Ctitle/ ititle
      Character*120 ititle
      COMMON /Cpirrad/ npirad,izirad(mxz)
c
      character*10 getHE5
      external getHE5
c     ----------------------------------------------------------------
c
!      nz = imisc(4)        !this is the last computed depth (dynZ changes)
      nz = izirad(npirad)   !this is the last requested output depth computed
      If(imisc(12).eq.0) nz = nz + 1   !add one more depth to output if inf bottom

      nmu = imisc(1)
      nphi = imisc(2)
      nwave = imisc(7)
      iwvl = imisc(11)
      nurad = imisc(16)
      wavenm = fmisc(13)
      nconc = imisc(23)
c
c     open file to append data after 1st call:
c
      if (iradanal.eq.1) then
c
         iradanal = 0
	   ioD = 0
         open(nurad,file=Drootname, status = 'unknown')
      else
         open(nurad,file=Drootname,position='append', iostat=ioD,err=66)
      endif
c
c     If cannot append file, open a new file after alerting user.
c     The cause of this rare error is not enough memory allocated to the
c     stack.  The fix for this it to increase the allocation by editting
c     the automake.fig file and adding/icreasing the -stack 500000 flag on
c     the linker line.
 66   IF(ioD.gt.0) then
		lenD = lenstr(Drootname)
		write(10,300) nurad,Drootname(1:lenD)
          write(10,302)
		Drootname = Drootname(1:lenD-4) // 'X.txt'
		lenD = lenstr(Drootname)
		write(10,301) Drootname(1:lenD)
		close(nurad)
		open(nurad,file=Drootname)
	Endif
c
c     grid information
c
 67   write(nurad,fmt='(a)') trim(getHE5()) // " Run Title: " 
     1                       // ititle(1:lenstr(ititle))
      write(nurad,fmt='(a)') "NOTE: This file is formatted for reading b
     1y IDL routine read_H_Dfile.pro, v5.2"

      write(nurad,fmt='(" nmu, nphi, nz, nwave, nconc =",5i5)')
     1 nmu,nphi,nz,nwave, nconc
      write(nurad,fmt='(" wavelength band",i3,
     1 "   nominal wavelength =",f8.2)') iwvl, wavenm
      write(nurad,fmt='(a)') 'imisc'
      write(nurad,102) (imisc(i),i=1,30)
      write(nurad,fmt='(a)') 'fmisc'
      write(nurad,104) (fmisc(i),i=1,30)
      write(nurad,fmt='(a)') 'fmu (quad-center mu values)'
      write(nurad,104) (fmu(i),i=1,nmu)
      write(nurad,fmt='(a)') 'phi (quad-center phi values)'
      write(nurad,104) (phi(i),i=1,nphi)
      write(nurad,fmt='(a)') 'zeta (optical depths of output)'
      write(nurad,104) (zeta(i),i=1,nz)
      write(nurad,fmt='(a)') 'z (geometric depths of output)'
      write(nurad,104) (zgeo(i),i=1,nz)
      write(nurad,fmt='(a)') 'bndmu (quad boundary mu values)'
      write(nurad,104) (bndmu(i),i=1,nmu)
      write(nurad,fmt='(a)') 'bndphi (quad boundary phi values)'
      write(nurad,104) (bndphi(i),i=1,nphi)
      write(nurad,fmt='(a)') 'omega (quad solid angles)'
      write(nurad,104) (omega(i),i=1,nmu)
      write(nurad,fmt='(a)') 'wave (wavelength band-center values)'
      write(nurad,104) (wave(i),i=1,nwave)
      write(nurad,fmt='(a)') 'waveb (wavelength band-boundary values)'
      write(nurad,104) (waveb(i),i=1,nwave+1)
c
c     IOP's at output depths
c
      write(nurad,fmt='(a)') 'acoef (Total absorption coefficient)'
      write(nurad,104) (acoef(i,0),i=1,nz)
      Do icomp = 1,nconc
      	  write(nurad,fmt='(a,i2,a)') 'acoef (for component ',icomp,')'
      	  write(nurad,104) (acoef(i,icomp),i=1,nz)
      Enddo
      write(nurad,fmt='(a)') 'bcoef (Total scattering coefficient)'
      write(nurad,104) (bcoef(i,0),i=1,nz)
      Do icomp = 1,nconc
      	  write(nurad,fmt='(a,i2,a)') 'bcoef (for component ',icomp,')'
      	  write(nurad,104) (bcoef(i,icomp),i=1,nz)
      Enddo
      write(nurad,fmt='(a)') 'bbcoef (backscattering coefficient)'
      write(nurad,104) (bbcoef(i,0),i=1,nz)
      Do icomp = 1,nconc
      	  write(nurad,fmt='(a,i2,a)') 'bbcoef (for component ',icomp,')'
      	  write(nurad,104) (bbcoef(i,icomp),i=1,nz)
      Enddo
      write(nurad,fmt='(a)') 'atten (beam attenuation coefficient)'
      write(nurad,104) (atten(i),i=1,nz)
      write(nurad,fmt='(a)') 'albedo (albedo of single scattering)'
      write(nurad,104) (albedo(i),i=1,nz)
c
c     AOP's in air and at output depths
c
      write(nurad,fmt='(a)') 'Eou (upwelling scalar irradiance)'
      write(nurad,104) (Eou(i),i=0,nz)
      write(nurad,fmt='(a)') 'Eod (downwelling scalar irradiance)'
      write(nurad,104) (Eod(i),i=0,nz)
      write(nurad,fmt='(a)') 'Eu (upwelling plane irradiance)'
      write(nurad,104) (Eu(i),i=0,nz)
      write(nurad,fmt='(a)') 'Ed (downwelling plane irradiance)'
      write(nurad,104) (Ed(i),i=0,nz)
      write(nurad,fmt='(a)') 'fMUu (upwelling average cosine)'
      write(nurad,104) (fmuu(i),i=0,nz)
      write(nurad,fmt='(a)') 'fMUd (downwelling average cosine)'
      write(nurad,104) (fmud(i),i=0,nz)
      write(nurad,fmt='(a)') 'fMUtot (total average cosine)'
      write(nurad,104) (fMUtot(i),i=0,nz)
      write(nurad,fmt='(a)') 'R (irradiance reflectance)'
      write(nurad,104) (R(i),i=0,nz)
c
c     diffuse radiances:
c
      write(nurad,fmt='(a)') 'RADMa (diffuse upward radiances in air (wa
     1ter-leaving radiances))'
      write(nurad,104) ((RADMa(i,j),i=1,nmu),j=1,nphi)
      write(nurad,fmt='(a)')'RADMz (diffuse upward radiances in water)'
      write(nurad,104) (((RADMz(i,j,k),i=1,nmu),j=1,nphi),k=1,nz)
      write(nurad,'(a)') 'RADPa (diffuse downward radiances in air (iden
     1tically zero))'
      write(nurad,104) ((RADPa(i,j),i=1,nmu),j=1,nphi)
      write(nurad,fmt='(a)') 'RADPz (diffuse downward radiances in water
     1)'
      write(nurad,104) (((RADPz(i,j,k),i=1,nmu),j=1,nphi),k=1,nz)
c
c     direct-beam radiances:
c
      write(nurad,fmt='(a)') 'RAD0Ma (direct upward radiances in air (su
     1rface-reflected sky radiance))'
      write(nurad,104) ((RAD0Ma(i,j),i=1,nmu),j=1,nphi)
      write(nurad,fmt='(a)') 'RAD0Pa (direct downward radiances in air (
     1incident sky radiances))'
      write(nurad,104) ((RAD0Pa(i,j),i=1,nmu),j=1,nphi)
      write(nurad,fmt='(a)') 'RAD0Pz (direct downward radiances in water
     1)'
      write(nurad,104) (((RAD0Pz(i,j,k),i=1,nmu),j=1,nphi),k=1,nz) 
c
c     incident sky radiances:
c
      write(nurad,fmt='(a)') 'radsky (total incident sky radiance)'
      write(nurad,104) ((radsky(i,j),i=1,nmu),j=1,nphi)
c
      close(nurad)
      return
c
  102 format(15i5)
  104 format(10es15.5e3)
  300 format(/2x,'There has been an unexpected error while attempting',
     1' to open the appending file: unit =',i5,/5x,a)
  301 format(/2x,'A new file will be opened for writing out the ',
     1'remaining data, named: ',/5x,a)
  302 format(/2x,'Hydrolight was unable to append this output file.',
     1/2x,'The cause of this rare error is not enough memory allocated',
     2' to the stack.',/2x,'The fix for this it to increase the',
     3' allocation by editting the automake.fig file ',/2x,
     4'and adding/icreasing the -stack 500000 flag on the linker line.',
     5//2x'To salvage this run, a new file will be opened for output.')

      end
