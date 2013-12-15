C     Last change:  LKS   7 Aug 2008    7:21 pm
      subroutine loadsurface(windspd, wavelen)
c
c     This routine reads, averages, and stores the surface reflectance 
c     and transmittance arrays from the surfwind file(s)
c
c     There are 8 arrays to be read and averaged
C        that1(a,w) into that1
C        that2(a,w) into that2
C        rhat1(w,a) into rhat1
C        rhat2(w,a) into rhat2
c
C        that1(w,a) into that1u
C        that2(w,a) into that2u
C        rhat1(a,w) into rhat1u
C        rhat2(a,w) into rhat2u
c
      real windspd, wavelen
c
      INCLUDE "DIMENS_XL.INC"
      PARAMETER (mxL=mxphi/2)
      PARAMETER (mxhat=mxmu*(mxL+1)) 
c
C     Common blocks used or defined here
      COMMON /Cmisc/ imisc(30),fmisc(30)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      Character*120 datadir, digitdir, spreadir,
     1              phasedir, surfdir,bottdir, Pdir
      COMMON /Cdirnames/ datadir,digitdir,spreadir,
     1                   phasedir,surfdir,bottdir, Pdir
      Character surfname*120,pfname*120,
     1           Drootname*120,Srootname*120,Mrootname*120,
     2           datafiles*120
      COMMON /Cfilenames/ surfname,pfname(mxcomp),
     1                    Drootname,Srootname,Mrootname,
     2                    datafiles(0:7+mxcomp)
c
      COMMON /Crthat/ that1(mxhat,mxhat),that2(mxhat,mxhat), 
     1                rhat1(mxhat,mxhat),rhat2(mxhat,mxhat)
      COMMON /Crthat_MA/ that1u(mxhat,mxhat),that2u(mxhat,mxhat), 
     1                rhat1u(mxhat,mxhat),rhat2u(mxhat,mxhat)

c     storage of SURFWIND values to compare with pf (from loadsurf)
      common /Cbbquadchk_HL/ m,n,iqpart  !loadsurf, inishrad,selfbb

c     temporary local storage:
      character*120 surftitl
      character char2*2, char4*4
      integer imisc1(30), nmu, nphi, nunit
      integer lensfdir, mdum,ndum,iqdum,nraydu
      real fmisc1(30), Udum, rt2(mxhat,mxhat), refr
c     temporary local storage for interp in n
      integer n1, n2, nexact
      real yinterp, rt3(mxhat,mxhat)
c     
      data nusrt/7/, nusrt1/15/, nusrt2/16/, nunit/10/
c
c***********************************************************************
c***********************************************************************
c
c     Call common routine that returns index of refractrion at this wavelength
      call getrefr(wavelen, windspd, refr, ireturn)
      if(ireturn.ne.0) return    !flag that no new surface file is needed
c
c     Check that this refr is valid
      call checkrefr(refr, n1, n2)
!      print *, 'LS: ',refr,n1,n2
!c     Get bracketting refr files  (assumes files every 0.01 in n!!)
!      n1 = 10*floor(100*refr)
      if(n1.eq.nint(1000*refr)) then  !n1 is exactly refr to 3 deci places
        nexact = 1
        n2 = n1
        yinterp = 0.0
      else                            !will need to interp in refr
!        n2 = n1 + 10
        nexact = 0
        yinterp = (1000*refr-n1)/float(n2-n1)
!        write(nunit,*) 'yinterp: ',yinterp
      endif
c
c     GET text for index of refraction
      write(char4,fmt='(i4)') n1
c
c     CALL common routine to find bracketing Windspeed files
      lensfdir = lenstr(surfdir) 
      surfname = surfdir(1:lensfdir) // 'windlist.txt'
      call slctsurf(i1, i2, iexact, xinterp, windspd, surfname)
c
c     read the two bracketing windspeed files
c
c      ***First file
      if(i1.lt.10) then
         write(char2,fmt='(i1,1x)') i1
      else
         write(char2,fmt='(i2)') i1
      end if
      lenchar2 = lenstr(char2)
c
c     concatenate the wind-speed character string with the data 
c     directory name to create the name of the file containing the
c     surface data for the given wind speed
      lensfdir = lenstr(surfdir) 
      surfname = surfdir(1:lensfdir) // 
     1           'surfwind_' // char4 //
     2           '.' // char2(1:lenchar2)
c
      open(nusrt1,file=surfname,form='formatted',status='old',err=666)
      read(nusrt1,fmt='(a)') surftitl
      read(nusrt1,402) Udum,m,n,iqpart,nraydum    !stored for comp with DPF
      read(nusrt1,403) imisc1,fmisc1
      nmu  = imisc1(1)
      nphi = imisc1(2)
      nhat = imisc1(10)
c
c     check for array dimensions being larger than allowed
      if(nmu.gt.mxmu) then
         write(6,fmt='(" nmu =",i3," gt mxmu =",i3)') nmu,mxmu
         write(nunit,fmt='(" nmu =",i3," gt mxmu =",i3)') nmu,mxmu
         call HERR("LOADSURFACE","increase mu LIMIT in UI")  !stop run
      endif
      if(nphi.gt.mxphi) then
         write(6,fmt='(" nphi =",i3," gt mxphi =",i3)') nphi,mxphi
         write(nunit,fmt='(" nphi =",i3," gt mxphi =",i3)') nphi,mxphi
         call HERR("LOADSURFACE","increase phi LIMIT in UI")  !stop run
      endif
c
      read(nusrt1,404) (fmu(i),i=1,nmu)
      read(nusrt1,404) (phi(i),i=1,nphi)
      read(nusrt1,404) (bndmu(i),i=1,nmu)
      read(nusrt1,404) (bndphi(i),i=1,nphi)
      read(nusrt1,404) (omega(i),i=1,nmu)
      read(nusrt1,404) (deltmu(i),i=1,nmu)
c
c       load the 8 arrays from the 1st file
      call readhat(nusrt1, nhat, that1)
      call readhat(nusrt1, nhat, that2)
      call readhat(nusrt1, nhat, rhat1)
      call readhat(nusrt1, nhat, rhat2)
      call readhat(nusrt1, nhat, that1u)
      call readhat(nusrt1, nhat, that2u)
      call readhat(nusrt1, nhat, rhat1u)
      call readhat(nusrt1, nhat, rhat2u)
      close(nusrt1)

c     check to see if windspeed exactly matches a file
      if(iexact.eq.1) goto 250     
c
c      ***Second file
      if(i2.lt.10) then
         write(char2,fmt='(i1,1x)') i2
      else
         write(char2,fmt='(i2)') i2
      end if
      lenchar2 = lenstr(char2)
      lensfdir = lenstr(surfdir) 
      surfname = surfdir(1:lensfdir) // 
     1           'surfwind_' // char4 //
     2           '.' // char2(1:lenchar2)
c
      open(nusrt2,file=surfname,form='formatted',status='old', err=666)
      read(nusrt2,fmt='(a)') surftitl
      read(nusrt2,402) Udum,mdum,ndum,iqdum,nraydum
      read(nusrt2,403) imisc1,fmisc1
      nhat2 = imisc1(10)
c
c     check to see if nhat's are the same (same sized arrays)
      if(nhat.ne.nhat2) then
         call HERR("LOADSURFACE","Surface files have different dimens")  !stop run
      endif
c        read headers
      read(nusrt2,404) (fmu(i),i=1,nmu)
      read(nusrt2,404) (phi(i),i=1,nphi)
      read(nusrt2,404) (bndmu(i),i=1,nmu)
      read(nusrt2,404) (bndphi(i),i=1,nphi)
      read(nusrt2,404) (omega(i),i=1,nmu)
      read(nusrt2,404) (deltmu(i),i=1,nmu)
c
c     read vales from 2nd windspeed and average values
      call readhat(nusrt2, nhat, rt2)
      call gethat(nhat, xinterp, that1, rt2)  !avg is returned as that1
      call readhat(nusrt2, nhat, rt2)
      call gethat(nhat, xinterp, that2, rt2)
      call readhat(nusrt2, nhat, rt2)
      call gethat(nhat, xinterp, rhat1, rt2)
      call readhat(nusrt2, nhat, rt2)
      call gethat(nhat, xinterp, rhat2, rt2)
c
      call readhat(nusrt2, nhat, rt2)
      call gethat(nhat, xinterp, that1u, rt2)
      call readhat(nusrt2, nhat, rt2)
      call gethat(nhat, xinterp, that2u, rt2)
      call readhat(nusrt2, nhat, rt2)
      call gethat(nhat, xinterp, rhat1u, rt2)
      call readhat(nusrt2, nhat, rt2)
      call gethat(nhat, xinterp, rhat2u, rt2)
c
c     close all files
  250 close(nusrt2)
c
c**************************************************
c**************************************************
c     check to see if need to interp in refr
      if(nexact.eq.1) goto 350
c
c     otherwise, will need to load 2 more files and interp as before 
c     GET text for index of refraction
      write(char4,fmt='(i4)') n2
c      ***First file
      if(i1.lt.10) then
         write(char2,fmt='(i1,1x)') i1
      else
         write(char2,fmt='(i2)') i1
      end if
      lenchar2 = lenstr(char2)
c     concatenate the wind-speed character string with the data 
c     directory name to create the name of the file containing the
c     surface data for the given wind speed
      lensfdir = lenstr(surfdir) 
      surfname = surfdir(1:lensfdir) // 
     1           'surfwind_' // char4 //
     2           '.' // char2(1:lenchar2)
c
      open(nusrt1,file=surfname,form='formatted',status='old',err=666)
      read(nusrt1,fmt='(a)') surftitl
      read(nusrt1,402) Udum,mdum,ndum,iqdum,nraydum
      read(nusrt1,403) imisc1,fmisc1
      nhat2 = imisc1(10)
c     check to see if nhat's are the same (same sized arrays)
      if(nhat.ne.nhat2) then
        write(nunit,*) 'ERR: Trying to iterate between two files ',
     1              'that have different dimensions!'
        call HERR("LOADSURFACE","Surface files have different dimens")  !stop run
      endif
      read(nusrt1,404) (fmu(i),i=1,nmu)
      read(nusrt1,404) (phi(i),i=1,nphi)
      read(nusrt1,404) (bndmu(i),i=1,nmu)
      read(nusrt1,404) (bndphi(i),i=1,nphi)
      read(nusrt1,404) (omega(i),i=1,nmu)
      read(nusrt1,404) (deltmu(i),i=1,nmu)
c
c     check to see if windspeed exactly matches a file
      if(iexact.eq.1) goto 300     
c
c      ***FOURTH file
      if(i2.lt.10) then
         write(char2,fmt='(i1,1x)') i2
      else
         write(char2,fmt='(i2)') i2
      end if
      lenchar2 = lenstr(char2)
      lensfdir = lenstr(surfdir) 
      surfname = surfdir(1:lensfdir) // 
     1           'surfwind_' // char4 //
     2           '.' // char2(1:lenchar2)
c
      open(nusrt2,file=surfname,form='formatted',status='old',err=666)
      read(nusrt2,fmt='(a)') surftitl
      read(nusrt2,402) Udum,mdum,ndum,iqdum,nraydum
      read(nusrt2,403) imisc1,fmisc1
      nhat2 = imisc1(10)
c     check to see if nhat's are the same (same sized arrays)
      if(nhat.ne.nhat2) then
        write(nunit,*) 'Trying to iterate between two files that have ',
     1              'different dimensions!'
        call HERR("LOADSURFACE","Surface files have different dimens")  !stop run
      endif
c        read headers
      read(nusrt2,404) (fmu(i),i=1,nmu)
      read(nusrt2,404) (phi(i),i=1,nphi)
      read(nusrt2,404) (bndmu(i),i=1,nmu)
      read(nusrt2,404) (bndphi(i),i=1,nphi)
      read(nusrt2,404) (omega(i),i=1,nmu)
      read(nusrt2,404) (deltmu(i),i=1,nmu)
c
c     read and average values
c     that1
 300  call readhat(nusrt1, nhat, rt2)
      if(iexact.ne.1) then
        call readhat(nusrt2, nhat, rt3)
        call gethat(nhat, xinterp, rt2, rt3)  !U avg is returned as rt2
      endif
      call gethat(nhat, yinterp, that1, rt2)  !refr avg is returned as that1
c     that2      
      call readhat(nusrt1, nhat, rt2)
      if(iexact.ne.1) then
        call readhat(nusrt2, nhat, rt3)
        call gethat(nhat, xinterp, rt2, rt3)  
      endif
      call gethat(nhat, yinterp, that2, rt2)  
c     rhat1      
      call readhat(nusrt1, nhat, rt2)
      if(iexact.ne.1) then
        call readhat(nusrt2, nhat, rt3)
        call gethat(nhat, xinterp, rt2, rt3)  
      endif
      call gethat(nhat, yinterp, rhat1, rt2)
c     rhat2      
      call readhat(nusrt1, nhat, rt2)
      if(iexact.ne.1) then
        call readhat(nusrt2, nhat, rt3)
        call gethat(nhat, xinterp, rt2, rt3)  
      endif
      call gethat(nhat, yinterp, rhat2, rt2)
c     that1u      
      call readhat(nusrt1, nhat, rt2)
      if(iexact.ne.1) then
        call readhat(nusrt2, nhat, rt3)
        call gethat(nhat, xinterp, rt2, rt3)  
      endif
      call gethat(nhat, yinterp, that1u, rt2)
c     that2u
      call readhat(nusrt1, nhat, rt2)
      if(iexact.ne.1) then
        call readhat(nusrt2, nhat, rt3)
        call gethat(nhat, xinterp, rt2, rt3)  
      endif
      call gethat(nhat, yinterp, that2u, rt2)
c     rhat1u
      call readhat(nusrt1, nhat, rt2)
      if(iexact.ne.1) then
        call readhat(nusrt2, nhat, rt3)
        call gethat(nhat, xinterp, rt2, rt3)  
      endif
      call gethat(nhat, yinterp, rhat1u, rt2)
c     rhat2u
      call readhat(nusrt1, nhat, rt2)
      if(iexact.ne.1) then
        call readhat(nusrt2, nhat, rt3)
        call gethat(nhat, xinterp, rt2, rt3)  
      endif
      call gethat(nhat, yinterp, rhat2u, rt2)
c
c     close all files
  350 close(nusrt1)
      close(nusrt2)

c*********************
      imisc(1) = imisc1(1)
      imisc(2) = imisc1(2)
      imisc(3) = imisc1(3)
      imisc(10)= imisc1(10)
c
      fmisc(1) = fmisc1(1)
      fmisc(2) = fmisc1(2)
      fmisc(3) = fmisc1(3)
c*********************
c
      return
  666 call nofile(nunit, 'LOADSURFACE', surfname )   !err opening file
c
  402 format (f6.2,3i3,i12)
  403 format (16i5,i12,3i5,10i2 / 10(e12.6,1x))
  404 format (10(e12.6,1x))

      end subroutine

!*******************************************************
      subroutine gethat(nhat, xinterp, hat1, hat2)
      INCLUDE "DIMENS_XL.INC"
      PARAMETER (mxL=mxphi/2)
      PARAMETER (mxhat=mxmu*(mxL+1)) 
c
      integer nhat
      real xinterp
      real hat1(mxhat,mxhat), hat2(mxhat,mxhat)
c
c	**** average values
      Do j=1,nhat
      Do i=1,nhat
        hat1(i,j)= (1.0 - xinterp)*hat1(i,j) +
     1             xinterp*hat2(i,j)
      Enddo
      Enddo
      
      end subroutine

!*******************************************************
      subroutine readhat(nusrt, nhat, hat)
      INCLUDE "DIMENS_XL.INC"
      PARAMETER (mxL=mxphi/2)
      PARAMETER (mxhat=mxmu*(mxL+1)) 
c
      integer nusrt, nhat
      real hat(mxhat,mxhat) 

c	**** read in pairs of arrays
      DO J=1,nhat
        READ(nusrt,404) (hat(I,J),I=1,nhat)
      end do

  404 format (10(e12.6,1x))
      end subroutine
