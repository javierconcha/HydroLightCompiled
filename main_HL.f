      program main
c
c     core program on file main.f
c
c     This is the main program for running Hydrolight version 5.2
c
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     +                                                                   +
C     +   HYDROLIGHT 5.2 Copyright (c) 2013 by Curtis D. Mobley           +
C     +                                                                   +
c     +   HYDROLIGHT IS EXPERIMENTAL AND IS LICENSED "AS IS" WITHOUT      +
C     +   REPRESENTATION OF WARRANTY OF ANY KIND, EITHER EXPRESS OR       +
C     +   IMPLIED.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE     +
C     +   OF HYDROLIGHT IS WITH THE USER.  HYDROLIGHT IS NOT FAULT        +
C     +   TOLERANT.                                                       +
C     +                                                                   +
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c     This computer program, named HYDROLIGHT and consisting of various main
c     programs and subroutines hereafter referred to collectively as 
c     "HYDROLIGHT", is licensed to the User on a non-exclusive, 
c     non-transferable basis in accordance with the License Agreement on 
c     file HE52\Documents\HE5LicenseAgreement.pdf.
c
c==========================================================================
c
c     The file "DIMENS.INC" contains parameter statements that give the
c     maximum array dimensions for the fundamental quantities such as
c     polar and azimuthal angle, depth, and wavelength.  All other
c     dimensions can be derived from these. DIMENS_XL.INC is the link to 
c     ..\COMMON\DIMENS_XL.INC allowing easier adaption to different
c     system file path conventions
c
      INCLUDE "DIMENS_XL.INC"
c
      INCLUDE "MAIN_COMMON_HL.INC"  !contains declaration of all COMMONS
c
c	Set the printout file unit # to be 10
      iout = 10
c
c     write out Copyright info as header (routine at bottom of file)
      call cpright(6)
c

      write(6,*) "HYDROLIGHT IS RUNNING, PLEASE WAIT..."
      write(6,*) "    (To stop the run before normal termination,"
      write(6,*) "    kill this command window.  Output will be lost)"

c     Initialize the run:  read the runtime input and set various defaults
c
      CALL INITIAL
c
c     Discretize the wavelength redistribution functions if inelastic
c     scattering is included in the run
c
      if(ichlfl.ne.0 .or. icdomfl.ne.0 .or.iraman.ne.0) call wrfdisc
c
c     Report time of initialization
c     Optional time stamp routine for Lahey Fortran only
      call date_stamp(iout,2, 0, 0)  ! calculate initialization runtime
c
c     Loop over all wavelengths to solve the RTE:
      nwave = imisc(7)
      nwskip = imisc(26) ! =1 for every wavelength; = 2 for every other wavelength
      if (nwskip .ne. 1) write(10,'(/2x,a,i2,/)')
     1           'Alternating wavelengths solved, nwskip = ', nwskip
      do jwave=1,nwave, nwskip
c
           imisc(11) = jwave
           fmisc(13) = wave(jwave)
c
           write(6,203) jwave, nwave, wave(jwave)
!           write(10,203) jwave, nwave, wave(jwave)
           write(10,100) jwave,waveb(jwave),waveb(jwave+1),wave(jwave)
c
c          solve for the radiance amplitudes at the current wavelength
c
           call radamps
c
c          synthesize the radiances and analyze the results at the
c          current wavelength
c
           call radanal
c
c          Report time of each wavelength
c          Optional time stamp routine for Lahey Fortran only
           call date_stamp(iout,3, jwave, nwave)  ! calculate incremental runtime
c
      enddo   !jwave
c
c     Run last wavelength if not already run  !new in v5
c     (only happens if nwskip=2 and number of wavelengths in run is even)
      if(imisc(11).lt. nwave) then
           jwave = nwave
           write(6,203) jwave, nwave, wave(jwave)
           imisc(11) = jwave
           fmisc(13) = wave(jwave)
           write(10,100) jwave,waveb(jwave),waveb(jwave+1),wave(jwave)
c          solve for the radiances at the current wavelength
           call radamps
c          analyze the results at the current wavelength
           call radanal
c          Report time of each wavelength
           call date_stamp(iout,3, jwave, nwave)  ! calculate incremental runtime
      endif
c
c     if we Alternated wavebands, use linear interpolation to fill in
c     the missing values
c
      if(nwskip.gt.1) call FillWave
c
      call PAR

c     If the run covers at least the 400-700 nm visible range, then 
c     compute CIE chromaticity coordinates for various quantities, and
c     the Secchi depth.
      if(waveb(1) .le. 400.0 .and. waveb(nwave+1) .ge. 700.0) then
         call Color
         call Secchi
      endif  ! end visible-band calculations
c
c     Write the multi-wavelength file for spreadsheet postprocessing,
c     if requested
c
      iwrtssM = imisc(21)
      if(iwrtssM .ne. 0) call wrtxcelM
c
c     Optional time stamp routine for Lahey Fortran only
      call date_stamp(iout,1, nwave, nwave)  ! 1 indicated "end" of run

      !close all file connections
      call CLOSEALL
c
c     end run
      print *, 'The HydroLight run is complete.'
      print *, 'You may now close this window.'
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
  100 format(//2x,'* * * * * Output for wavelength band',i3,
     1' (',f5.1,' to ',f5.1,' nm; nominal wavelength =',f6.1,
     2' nm)  * * * * *')
  203 format(2x,'Now beginning wavelength ',i3, ' of ',i3,
     1      ' at nominal wavelength ',f6.1, ' nm')
      end


