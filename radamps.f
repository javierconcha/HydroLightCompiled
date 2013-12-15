C     Last change:  LKS  26 Apr 2008    8:19 am
      SUBROUTINE RADAMPS
c
C     core routine on file radamps.f
c
c     called by MAIN
c
c     calls INISHAMP, AMPA0, BOTMBC, RICCATI, AMPW, AMPZETA, AMPMA
C
c     This routine solves for the radiance amplitudes as a function 
c     of depth for the current wavelength.  These calculations are
c     flowcharted in L&W Fig. 8.2.  Various auxillary quantities (such
c     as direct-beam amplitudes) are computed and saved for later use. 
c 
      INCLUDE "DIMENS_XL.INC"
      PARAMETER (mxL=mxphi/2, mxbeta=mxmu*(mxL+1), mxamp=2*mxmu*(mxL+1))
      PARAMETER (mxhat=mxmu*(mxL+1)) 
C
      common /Cbbopt/ ibbopt(mxcomp), bbfrac(mxcomp), 
     1                BfrefPL(mxcomp), Bf0PL(mxcomp), BfmPL(mxcomp)
      integer ibbopt
      real bbfrac, BfrefPL, Bf0PL, BfmPL
C 
      COMMON /CRAMP/ RAMPPa(mxamp),RAMPMa(mxamp),RAMPPz(mxamp,mxz),
     1              RAMPMz(mxamp,mxz)
      COMMON /CRAMP0/ RAMP0Pa(mxamp),RAMP0Pz(mxamp,mxz),RAMP0Ma(mxamp) 
      COMMON/CRTS/ Rzw(mxmu,mxmu,mxz),Twz(mxmu,mxmu,mxz), 
     1  S1ptwz(mxmu,mxz),S2ptwz(mxmu,mxz),
     2  R1zb(mxmu,mxmu,mxz),R2zb(mxmu,mxmu,mxz),
     3  S1mtbz(mxmu,mxz),S2mtbz(mxmu,mxz)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Crhotau/ rhohat(mxmu,mxmu),tauhat(mxmu,mxmu), 
     1  betatP(mxmu,mxbeta,mxcomp),betatM(mxmu,mxbeta,mxcomp)
      COMMON /CBOTBC/ rhatmb(mxmu,mxmu)
      COMMON /Crthat/ that1(mxhat,mxhat),that2(mxhat,mxhat), 
     1                rhat1(mxhat,mxhat),rhat2(mxhat,mxhat)
      COMMON /CMISC/ imisc(30),FMISC(30) 
      COMMON /Cfrstcls/ iabscat,iqasky,iradamps,iradanal, iradxcl
c
      Character surfname*120,pfname*120,
     1           Drootname*120,Srootname*120,Mrootname*120,
     2           datafiles*120
      COMMON /Cfilenames/ surfname,pfname(mxcomp),
     1                    Drootname,Srootname,Mrootname,
     2                    datafiles(0:7+mxcomp)
c
      dimension nuphas(mxcomp)
c
      DATA nuscr1/3/,nuscr2/4/, nurad/11/, nuLfile/17/
c 
      save
C
C     **********  INITIALIZATION  **********
C
C     Open the needed files on first call to radamps
C
      if (iradamps.eq.1) then
c
c     Open the input file of air-water-surface information 
c
c     Open files of phase functions:
*     IFF not using bbfrac(component) to select phase function
*		[ibbopt = -1 if no phase function needed (i.e., CDOM) ]
*		[ibbopt =  0 if phase function read from file ]
*		[ibbopt =  1 if phase function selected by bb/b ratio ]
*     [note: unit numbers are still 49+icomp (but some unit #s are skipped]
c
      ncomp = imisc(6)
      npfiles = iabs(ncomp)
cJC      print *,"npfiles is ",npfiles
cJC      print *,'ncomp is ',ncomp
      DO i=1,npfiles
cJC         print *,"pfname(i) is ",pfname(i)         
cJC         print *,"ibbopt(i) is ",ibbopt(i)
         IF(ibbopt(i).ne.0) goto 10
         nuphas(i) = 49+i
cJC         print *,"nuphas(i) is ",nuphas(i)
cJC         print *,"i is ",i
         OPEN(nuphas(i),file=trim(pfname(i)),status='old', err=666)
 10   enddo
c
c     Open scratch files:
c           nuscr1 holds R(zeta,w) and T(w,zeta)
c           nuscr2 holds R1(zeta,b), R2(zeta,b) S1-t(b,zeta), S2-t(b,zeta),
c                        S1+t(w,zeta), S2+t(w,zeta)
            open(nuscr1,file='scratch1.tmp',status='unknown',
     1           form='unformatted')
            open(nuscr2,file='scratch2.tmp',status='unknown',
     1           form='unformatted')
c
c     the output file for radiances will be opened in subroutine radanal
c
      endif
c 
c     Initialize for this wavelength
c  
      CALL INISHAMP(nuphas)
c 
      imisc(15) = nuLfile
      imisc(16) = nurad
      imisc(18) = nuscr1
      imisc(19) = nuscr2
C 
      nmu = imisc(1)
      nL = imisc(3)
      nz = imisc(4)
      isource = imisc(8) 
c
      idbug2 = 0
      IF(idbug2.GT.0) THEN
         CALL P2ARAY(that1,nmu,nmu,mxhat,2,'that1(a,w) as loaded')
         CALL P2ARAY(that2,nmu,nmu,mxhat,2,'that2(a,w) as loaded')
         CALL P2ARAY(rhat1,nmu,nmu,mxhat,2,'rhat1(w,a) as loaded')
         CALL P2ARAY(rhat2,nmu,nmu,mxhat,2,'rhat2(w,a) as loaded')
      ENDIF
      idbug2 = 0 
C 
C     **********  BEGIN COMPUTATIONS  **********
C 
C+++++Compute the downward direct (unscattered) beam amplitudes 
c     Lhat0+(a) = RAMP0Pa at all levels z = a, w, ...,z, ..., m
c     from the quad-averaged incident sky radiances.  Then transmit the
c     direct beam through the sea surface to get the direct-beam amplitudes
c     RAMP0Pz at all depths.
c*****NOTE:  The model solves for the TOTAL radiance amplitudes, not 
c     for the direct and diffuse parts separately.  Therefore, the direct
c     beam amplitudes are not used explicitly in the solution
c     algorithm.  However, it is sometimes desirable in data analysis to
c     partition the total radiance into direct and diffuse parts, so we
c     compute and save the direct amplitudes for possible later use.
c     Note also that all of the incident sky radiance (whether from the
c     solar beam or from the background sky) is considered to
c     be "direct beam", because it is unscattered BY THE WATER; therefore
c     we set RAMPPa = RAMP0Pa.
c
      CALL AMP0a
c
      IF(idbug2.NE.0) THEN
      write(10,1038) 
      iflag = 1
      CALL PNTAMP(z,RAMP0Pa,RAMP0Pz,mxamp,iflag)
      ENDIF
      idbug2 = 0
C
C+++++Compute the interior transport functions R, T, and S by integration
c     of the Riccati equations. 
c     Each L mode is integrated separately (in the L loop).
c
c      call SYSTEM_CLOCK(iCOUNT1,icount_rate,icount_max)
c
      DO L=0,nL
      imisc(14) = L   
      if(idbug2.gt.0) write(10,202) L
C 
C     SET DEBUGGING OUTPUT FOR SELECTED L VALUES
      IF(idbug2.GT.0) THEN
           IF(L.LE.1 .OR. L.GE.nL-1) THEN 
           idbug = idbug2 
           ELSE 
           idbug = 0
           ENDIF
      ELSE
      idbug = idbug2
      ENDIF 
ccc      imisc(9) = idbug
C 
C++++ COMPUTE rhat1(m,b) for the desired bottom boundary condition
C
      CALL BOTMBC(L)
c
      IF(idbug.GT.0) CALL P2ARAY(rhatmb,nmu,nmu,mxmu,2,'rhat1(m,b;L)')
C 
C++++ Integrate the Ricatti equations to get R(zeta,w;L), T(w,zeta;L), etc
C
      CALL RICCATI(L)
C 
C     Write R(zeta,w), T(w,zeta), etc, for this L value to scratch files.
c     Save the source terms only if isource .ne. 0
c 
      DO iz=1,nz
         WRITE(nuscr1) ((Rzw(I,J,iz),I=1,nmu),J=1,nmu)
         WRITE(nuscr1) ((Twz(I,J,iz),I=1,nmu),J=1,nmu)
         WRITE(nuscr2) ((R1zb(I,J,iz),I=1,nmu),J=1,nmu)
         WRITE(nuscr2) ((R2zb(I,J,iz),I=1,nmu),J=1,nmu)
         if(isource.ne.0) write(nuscr2) (S1Mtbz(i,iz),i=1,nmu),
     1                 (S2Mtbz(i,iz),i=1,nmu),
     2                 (S1Ptwz(i,iz),i=1,nmu),(S2Ptwz(i,iz),i=1,nmu)
      end do
      idbug = 0
      IF(idbug.EQ.2) THEN 
      CALL P3ARAY(Rzw,nmu,nmu,nz,mxmu,mxmu,2,' R(zeta,w;L)')
      CALL P3ARAY(Twz,nmu,nmu,nz,mxmu,mxmu,2,' T(w,zeta;L)')
c      CALL P3ARAY(R1zb,nmu,nmu,nz,mxmu,mxmu,2,' R1(zeta,b;L)')
c      CALL P3ARAY(R2zb,nmu,nmu,nz,mxmu,mxmu,2,' R2(zeta,b;L)')
      ENDIF 
C 
      end do   !  L loop
C 
C++++ Compute the radiance amplitudes Lhatp+(w) and Lhatp-(w) at zeta = w
c     (just below the air-water surface) using Eqns. (8.98) and (8.102)
C
      CALL AMPw
C 
C++++ Compute the radiance amplitudes Lhatp+(z) and Lhatp-(z) at all
c     interior depths, w .le. zeta .le. m, using Eqns. (8.105) and (8.106)
C
      CALL AMPzeta
c 
C++++ Compute the upward total radiance amplitudes Lhatp-(a) just
c     above the sea surface, using Eq. (8.107).  These amplitudes
c     include both the water-leaving radiance and reflected sky
c     radiance.
c
      CALL AMPMa
c
C     RAMP0Ma now contains Lhat0p-(a), the specularly reflected (direct
c     beam) incident sky radiance amplitudes.  These amplitudes
c     are needed to extract the water-leaving radiance from the total
c     upward radiance.
C 
C     **********  END OF RADIANCE AMPLITUDE COMPUTATIONS  ********** 
C 
c     Close all files.
      DO i=1,ncomp
         close(nuphas(i))
      end do
      close(nuscr1)
      close(nuscr2)
c
      return
c
  666 call nofile(10, 'RADAMPS', pfname(i) )   !err opening file
c
c     Formats:
  202 FORMAT(//2x,'+++++ Beginning the L =',i3,' loop +++++')
 1038 FORMAT(//2x,'The sky (downward) direct beam radiance amplitudes',
     1' are',//11x,'mu',7x,'Lhat0+(a)',8x,'Lhat0+(z)')
 1039 FORMAT(//2x,'The specularly reflected (upward) direct beam',
     1' radiance amplitudes are'//11x,'mu',7x,'Lhat0-(a)')
 1040 FORMAT(//2x,'The downward TOTAL radiance amplitudes are'//
     1  11x,'mu',7x,'Lhat+(a)',9x,'Lhat+(z)') 
 1042 FORMAT(//2x,'The upward TOTAL radiance amplitudes are'
     1//11x,'mu',7x,'Lhat-(a)',9x,'Lhat-(z)') 
 1045 format (10(e12.6,1x))

      end
