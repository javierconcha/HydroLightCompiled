C     Last change:  LKS   1 Nov 2007   12:57 pm
      SUBROUTINE SYNRAD(amp,rad,irow)
c 
c     core routine on file synrad.f
c 
c     called by RADANAL [MAIN->RADANAL->SYNRAD]
c
c     This routine synthesizes the quad-averaged radiances L(u,v) =
c     L(mu,phi) (for a given depth and wavelength) using (8.31) and (8.32). 
c
      INCLUDE "DIMENS_XL.INC"
      parameter(mxL=mxphi/2)
c 
      dimension amp(1),rad(irow,*)
      dimension coslp(0:mxL,mxphi),sinlp(0:mxL,mxphi) 
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /CMISC/  IMISC(30),FMISC(30) 
c 
      data kall/0/
c
      save
c 
      nmu = imisc(1)
      nphi = imisc(2) 
      nL = imisc(3)
c      idbug = imisc(9)
      nhat = imisc(10) 
c 
      if(kall.eq.0) then 
c     the first call computes a lookup table of sines and cosines
      do L=0,nL
      do j=1,nphi
         coslp(L,j) = cos(float(L)*phi(j))
         sinlp(L,j) = sin(float(L)*phi(j))
      end do
      end do
      kall = 1
      endif 
c 
c     loop over all mu and phi values 
c
c*****NOTE:  Radiances that in principle are very small or zero
c     may sometimes be slightly negative due to roundoff error (on some
c     computers) when computing the radiances from the amplitudes.  This
c     most often happens when there is a sun in a black sky:  when
c     reconstituting the downward direct-beam radiances from the amplitudes,
c     the zero values for the black quads may get non-zero - but very
c     small - positive and negative values.  Set any such negative
c     radiances to zero and print an informational message.
c
      rsmall = 1.0e30
      rlarge = -1.0e30
c 
c     Non-polar-cap quads:
c
      do i=1,nmu-1
      do j=1,nphi
c 
c     sum over L values, Eq. (8.31)
c 
      sum = 0.
      do L=0,nL
      sum = sum + amp(nmu*L+i)*coslp(L,j)
     1          + amp(nhat+nmu*L+i)*sinlp(L,j)
      end do
c
      if(sum.ge.0.0) then
         rad(i,j) = sum
      else
         if(sum.lt.rsmall) rsmall = sum
         if(sum.gt.rlarge) rlarge = sum
         rad(i,j) = 0.0
      endif
      end do   !  j loop
      end do   !  i loop
c 
c     polar cap term by Eq. (8.32):
c 
      rad(nmu,1) = amp(nmu) 
      do j=2,nphi
         rad(nmu,j) = 0.
      end do
c 
      idbug = 0
      if(idbug.ne.0 .and. rsmall.lt.0.0) write(10,300) rsmall,rlarge
      return
c
 300  format(2x,'NOTE: sub synrad: radiances between',1p,e12.4,
     1' and',e12.4,' set to zero')
      END
