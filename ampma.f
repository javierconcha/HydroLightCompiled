C     Last change:  LKS  18 Nov 2007   10:04 am
      subroutine ampma
C 
C     core routine on file ampma.f 
C
C     called by RADAMPS [MAIN->RADAMPS->AMPMA]
C
C     calls MATXMAT
C 
C     This routine computes the water-leaving radiance amplitudes
c     Lhatp-(a) = RAMPMa, using Eq. (8.107)
C 
c     Air-water-surface spectral storage arrays must be loaded with 
c        that1(w,a) in that1
c        that2(w,a) in that2
c        rhat1(a,w) in rhat1
c        rhat2(a,w) in rhat2
c 
      INCLUDE "DIMENS_XL.INC"
      PARAMETER (mxL=mxphi/2, mxhat=mxmu*(mxL+1), mxamp=2*mxhat)
C 
      COMMON /CRAMP0/ RAMP0Pa(mxamp),RAMP0Pz(mxamp,mxz),RAMP0Ma(mxamp)
      COMMON /CRAMP/ RAMPPa(mxamp),RAMPMa(mxamp),RAMPPz(mxamp,mxz),
     1               RAMPMz(mxamp,mxz) 
!      COMMON /CRthat/ that1(mxhat,mxhat),that2(mxhat,mxhat), 
!     1                rhat1(mxhat,mxhat),rhat2(mxhat,mxhat)
      COMMON /Crthat_MA/ that1(mxhat,mxhat),that2(mxhat,mxhat), 
     1                rhat1(mxhat,mxhat),rhat2(mxhat,mxhat)
      COMMON /CMISC/ imisc(30),fmisc(30) 
c
c     temporary local storage:
      dimension temp1(mxhat),temp2(mxhat),rhatp(mxhat,mxhat),
     1          thatp(mxhat,mxhat)
C 
      nhat = imisc(10) 
C 
c     p = 1 (cosine amplitudes) 
c 
      ip = 1
C 
  999 CONTINUE
      ipoffset = nhat*(ip-1)
C 
c     Load thatp(w,a) into that and load rhatp(a,w) into rhat 
C 
      if(ip.eq.1) then
      do j=1,nhat
         do i=1,nhat
            rhatp(i,j) = rhat1(i,j)
            thatp(i,j) = that1(i ,j)
         end do
      end do
      else
      do j=1,nhat
         do i=1,nhat
            rhatp(i,j) = rhat2(i,j)
            thatp(i,j) = that2(i,j)
         end do
      end do
      endif 
c 
C     Evaluate Eq. (8.107) and save the reflected direct beam, RAMP0Ma
c     (the specularly reflected incident sky radiance)
C 
c     first term:
c
      call matxmat(RAMPMz(1+ipoffset,1),thatp,1,nhat,nhat,1,mxhat,
     1  temp1,1)
c
c     second term:
      call matxmat(RAMP0Pa(1+ipoffset),rhatp,1,nhat,nhat,1,mxhat,
     1  temp2,1)
c
c     temp2 now contains the incident sky radiance amplitudes as reflected
c     upward by the air-water surface.  Save this for possible later use,
c     then compute the total (reflected + diffuse) water-leaving radiance
c     amplitudes by (8.107)
c   
      DO I=1,nhat
      RAMP0Ma(I+ipoffset) = temp2(I)
      RAMPMa(I+ipoffset) = temp1(I) + temp2(I)
      end do
C 
      if(ip.gt.1) return
c 
c     repeat for p = 2 (sine amplitudes)
      ip = 2
      GO TO 999 
c
      END 
