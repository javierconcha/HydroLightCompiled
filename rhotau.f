C     Last change:  LKS  18 Nov 2007   10:18 am
      subroutine rhotau(L,zetanow,znow)
c 
c     core routine on file rhotau.f
c 
C     called by INFBOTM  [MAIN->RADAMPS->BOTMBC->INFBOTM->RHOTAU]
C     called by DRTDZS   [MAIN->RADAMPS->RICCATI------>DRTDZS->RHOTAU]
C     called by DRTDZS   [MAIN->RADAMPS->RICCATI->ODE->DRTDZS->RHOTAU]
c
c     This routine calls the "ab" routine
c
c     This routine computes the spectral local reflectances and
c     transmittances rhohat and tauhat (for the current L and zetanow)
c     using Eq. 8.42 in its various forms.  The needed betahat+/-
c     values are obtained from the quad-averaged betatilde+/- via
c     Eq. 8.34 (in its various forms shown on L&W p 396).
c
      INCLUDE "DIMENS_XL.INC"
      PARAMETER (mxbeta=mxmu*(mxphi/2 + 1))
c
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Crhotau/ rhohat(mxmu,mxmu),tauhat(mxmu,mxmu),
     1 betatP(mxmu,mxbeta,mxcomp),betatM(mxmu,mxbeta,mxcomp)
      COMMON /Cmisc/ imisc(30),fmisc(30) 
c
c     temporary local storage:
      dimension bcomp(mxcomp), acomp(mxcomp)
      dimension totlpp(mxmu,mxbeta),totlpm(mxmu,mxbeta),
     1  coslpv(mxphi)
C
      nmu = imisc(1)
      nphi = imisc(2) 
      nL = imisc(3)      
      iop = imisc(5)
      ncomp = imisc(6)
      wavelen = fmisc(13)
c
      ncol = nmu*(nphi/2 + 1)
C 
      DO iv=1,nphi
         coslpv(iv) = cos(float(L)*phi(iv))
      end do
C 
      IF(L.EQ.0 .OR. L.EQ.nL) THEN
         epsL = FLOAT(nphi)
      ELSE
         epsL = FLOAT(nL)
      ENDIF 
c
c-------------------------------------------------------------------
c
c     Get the total phase functions betatilde+ = totlpp and
c     betatilde- = totlpm, and the albedo, at optical depth zetanow
c
c     Get the component scattering coefficients
c
c     Call the abscat routine with either geometric (the usual case)
c     or optical depth, as is appropriate for the run.
c     NOTE:  the total a and b values computed in this abscat call
c     will be used in the Shat (source) routines (called by drtdzs)
c
      depth = znow
      if(iop.eq.1) depth = zetanow
c
c     insert the call to the desired "ab" routine:
      call abscat(depth,wavelen,ncomp,acomp,bcomp,atotal,btotal)
c
      fmisc(19) = atotal
      fmisc(20) = btotal
c
      albedo = btotal/(atotal + btotal)
c
c     Sum the component phase functions, weighted by the scattering
c     coefficients as in Eq. (3.13), to get the total quad-averaged
c     phase function at depth zetanow for use in Eq. (8.34)
c
      do j=1,ncol
      do i=1,nmu
      temppp = 0.
      temppm = 0.
      if(btotal.ge.1.0e-6) then
         do k=1,ncomp
         temppp = temppp + (bcomp(k)/btotal)*betatP(i,j,k)
         temppm = temppm + (bcomp(k)/btotal)*betatM(i,j,k)
         end do
      end if
      totlpp(i,j) = temppp
      totlpm(i,j) = temppm
      end do
      end do
c
c-----------------------------------------------------------------
c
c     Compute rhohat and tauhat at this depth
c
C     POLAR CAP OUTPUT, iu = nmu
C 
      IF(L.EQ.0) THEN 
C 
      fmu1 = 1.0/fmu(nmu) 
C     QUAD INPUT:  use Eq. 8.42c; betahat is given by 8.34(iii).
      DO ir=1,nmu-1
         rhohat(ir,nmu) = albedo*epsL*totlpm(ir,nmu)*fmu1 
         tauhat(ir,nmu) = albedo*epsL*totlpp(ir,nmu)*fmu1 
      end do
C 
C     POLAR CAP INPUT:  use Eq. 8.42d; betahat is given by 8.34(iv) 
      rhohat(nmu,nmu) = albedo*totlpm(nmu,nmu)*fmu1
      tauhat(nmu,nmu) = (albedo*totlpp(nmu,nmu) - 1.0)*fmu1
C 
      ELSE
C 
      DO ir=1,nmu
         rhohat(ir,nmu) = 0.
         tauhat(ir,nmu) = 0.
      end do
      ENDIF 
C 
C     QUAD (non-polar cap) OUTPUT 
C 
      DO iu=1,nmu-1
      fmu1 = 1.0/fmu(iu)
C 
C     POLAR CAP INPUT, ir = nmu.  Use 8.42b; betahat is given by 8.34(ii) 
C 
      IF(L.EQ.0) THEN 
         rhohat(nmu,iu) = albedo*totlpm(nmu,iu)*fmu1
         tauhat(nmu,iu) = albedo*totlpp(nmu,iu)*fmu1
      ELSE
         rhohat(nmu,iu) = 0.
         tauhat(nmu,iu) = 0.
      ENDIF 
C 
C     QUAD (non-polar cap) INPUT.  Use Eq. 8.42a; betahat must now be
c     computed by 8.34
C 
      DO ir=1,nmu-1
         sump = 0. 
         summ = 0. 
      DO iv=1,nphi
C 
C     Compute storage indices by Eq. (12.7) of Tech. Memo. ERL-PMEL-75.
c     This indexing takes advantage of the symmetries of Eq. (5.7). 
C 
      IF(iv.LE.nL+1) THEN 
         J = iu + nmu*(iv-1) 
      ELSE
         J = iu + nmu*(nL - MOD(iv-1,nL))
      ENDIF 
C 
      sump = sump + totlpp(ir,j)*coslpv(iv) 
      summ = summ + totlpm(ir,j)*coslpv(iv)
      end do
c 
      rhohat(ir,iu) = albedo*summ*fmu1 
      if(ir.eq.iu) then 
         delt = 1. 
      else
         delt = 0. 
      endif 
      tauhat(ir,iu) = (albedo*sump - delt)*fmu1
      end do   ! ir loop
      end do   ! iu loop
c
      RETURN
      END 

