C     Last change:  LKS   9 Dec 2011   10:44 am
      SUBROUTINE drtdzs(zetanow,RT,deriv)
c
c     core routine on file drtdzs.f 
c
c     called by ode, riccati
c
c     calls rhotau, sumshat (in shat.f), and eval1D (in dataintp.f)
c
c     This subroutine evaluates deriv = d(RT)/d(zeta) at zeta = zetanow
c     (the right hand side of 8.74-8.85) for use by the canned routine
c     (ode) that solves the Riccati equation system.  Internal source
c     terms are included if isource .ne. 0, and omitted if isource = 0.
c
c     For the downward integration sweep, arrays Rzw, etc are stored
c     in the linear array RT as follows (for a given zeta value): 
C 
c     Rzw(I,J)  IS RT(I + (J-1)*nmu) 
c     Twz(I,J)  IS RT(I + (J-1)*nmu + nmu*nmu)
c     S1ptwz(i) is RT(i + 2*nmu*nmu)
c     S2ptwz(i) is RT(i + 2*nmu*nmu + nmu)
C 
      INCLUDE "DIMENS_XL.INC"
      parameter(mxeqn=2*mxmu*mxmu + 2*mxmu, mxbeta=mxmu*(mxphi/2+1))
c
c     lookup table for z to zeta grid
      Common /Cztozeta/ nzvals, zetavals(mxnzvals),zvals(mxnzvals)
      integer nzvals
c
      dimension RT(mxeqn),deriv(mxeqn)
      COMMON /Crhotau/ rhohat(mxmu,mxmu),tauhat(mxmu,mxmu), 
     1                 betatP(mxmu,mxbeta,mxcomp),
     2                 betatM(mxmu,mxbeta,mxcomp)
      COMMON /CsourceH/ s1hatm(mxmu),s2hatm(mxmu),s1hatp(mxmu),
     1                  s2hatp(mxmu)
      Common /Csource0/ ibiolum,ichlfl,icdomfl,iraman, RamanEXP
      COMMON/CMISC/ imisc(30),fmisc(30)
c 
c     temporary local storage:
      dimension work(mxmu,mxmu)
c
      External yinterp 
c
!     iznow is the last known index into the z to zeta array
      integer iznow
      data iznow/1/
      save iznow
!*********************************************************************
c
      nmu = imisc(1)
      iop = imisc(5)
      isource = imisc(8)
      isweep = imisc(13) 
      L = imisc(14)
      nsq = nmu*nmu 
c
      if(iop.ne.1) then
c
c     If the ab routine is to be called with geometric depth, use the
c     splines determined in ztozeta to compute the geometric depth znow
c     corresponding to the current optical depth zetanow.  (znow is
c     not needed if the ab routine is to be called with optical depth.)
c     (depth calls are made in routines rhotau and sumshat)
c
c       call to linear interp routine that returns 
c       index and linear interp coef for znow
        znow = yinterp(iznow, nzvals, zetanow, zetavals, zvals)
        fmisc(18) = znow      
      end if
c
c----------------------------------------------------------------
c 
c     Determine rhohat and tauhat at the current L and zeta values
c     NOTE:  rhotau calls abscat; the total a and b values returned
c     from that call are then used in routine sumshat.
C
      call rhotau(L,zetanow,znow)
c
c     Determine the source terms at the current L and zeta values
c
      if(isource.ne.0) call sumshat(L,zetanow,znow)
c
c---------------------------------------------------------------
c
      if(isweep.eq.1) then
c
c     Compute dRT/dzeta as defined for the downward integration sweep
C 
c     Compute work = tauhat + rhohat*Rzw (used in 8.74, 8.75 and 8.78) 
      DO I=1,nmu
      DO J=1,nmu
      work(I,J) = tauhat(I,J) 
         DO K=1,nmu
         work(I,J) = work(I,J) + rhohat(I,K)*RT(K + (J-1)*nmu)
         end do
      end do
      end do
c
c     Compute d(Rzw)/dzeta BY EQ. 8.74 
C 
      DO I=1,nmu
      DO J=1,nmu
      temp1 = 0.
      temp2 = 0.
         DO K=1,nmu
         temp1 = temp1 + RT(I + (K-1)*nmu)*work(K,J)
         temp2 = temp2 + tauhat(I,K)*RT(K + (J-1)*nmu)
         end do
      deriv(I + (J-1)*nmu) = rhohat(I,J) + temp1 + temp2
      end do
      end do
C 
c     Compute d(Twz)/dzeta BY EQ. 8.75 
C 
      DO I=1,nmu
      DO J=1,nmu
      temp1 = 0.
         DO K=1,nmu
         temp1 = temp1 + RT(I + (K-1)*nmu + nsq)*work(K,J)
         end do
      deriv(I + (J-1)*nmu + nsq) = temp1
      end do
      end do
c      print*,' derivs(1-10) = ',(deriv(iii),iii=1,10)

C
      if(isource.ne.0) then
c     include source terms:
c     Compute d(S1ptwz)/dzeta by EQ. 8.78 with p = 1 (temp1 and temp2) and
c     compute d(s2ptwz)/dzeta by Eq. 8.78 with p = 2 (temp3 and temp4) 
C 
      DO J=1,nmu
      temp1 = 0.
      temp2 = 0.
      temp3 = 0.
      temp4 = 0.
         DO K=1,nmu
         temp1 = temp1 + RT(k + 2*nsq)*work(K,J)
         temp2 = temp2 + s1hatm(K)*RT(K + (J-1)*nmu)
         temp3 = temp3 + RT(k + 2*nsq + nmu)*work(k,j)
         temp4 = temp4 + s2hatm(k)*RT(k + (j-1)*nmu)
         end do
      deriv(j + 2*nsq) = s1hatp(J) + temp1 + temp2
      deriv(j + 2*nsq + nmu) = s2hatp(j) + temp3 + temp4
      end do
c      if(L.le.1) then
c      print*,'    source derivs 8.78 =',(deriv(j+2*nsq),j=1,2*nmu)
c      endif

      endif
C
      endif
C
c--------------------------------------------------------------------
c
      if(isweep.eq.2) then
c
c     upward integration sweep with p = 1: Eqs. 8.80 and 8.84 with p = 1
c
c     For the upward sweeps, arrays Rpzb and Spmtbz are stored in
c     the linear array RT as follows (for p = 1 or p = 2, and for a
c     given zeta value):
c
c     Rpzb(I,J) is RT(I + (J-1)*nmu)
c     Spmtbz(i) is RT(i + nmu*nmu)
C
c     Compute dRT/dzeta as defined for the upward integration sweep
C 
c     Compute work = tauhat + rhohat*Rzb (used in 8.80 and 8.84) 
      DO I=1,nmu
      DO J=1,nmu
      work(I,J) = tauhat(I,J) 
         DO K=1,nmu
         work(I,J) = work(I,J) + rhohat(I,K)*RT(K + (J-1)*nmu)
         end do
      end do
      end do
c
c     Compute d(Rzb)/dzeta BY EQ. 8.80 
C 
      DO I=1,nmu
      DO J=1,nmu
      temp1 = 0.
      temp2 = 0.
         DO K=1,nmu
         temp1 = temp1 + RT(I + (K-1)*nmu)*work(K,J)
         temp2 = temp2 + tauhat(I,K)*RT(K + (J-1)*nmu)
         end do
      deriv(I + (J-1)*nmu) = -rhohat(I,J) - temp1 - temp2
      end do
      end do
c
      if(isource.ne.0) then
c     Compute d(s1mtbz)/dzeta by 8.84
c
      do j=1,nmu
      temp1 = 0.
      temp2 = 0.
         do k=1,nmu
         temp1 = temp1 + RT(k+nsq)*work(k,j)
         temp2 = temp2 + s1hatp(k)*RT(k + (j-1)*nmu)
         end do
      deriv(j + nsq) = -s1hatm(j) - temp1 - temp2
      end do
      endif
c
      endif
c
c----------------------------------------------------------------
c
      if(isweep.eq.3) then
c
c     Upward integration sweep with p = 2: Eqs. 8.80 and 8.84 with p = 2
C
c     Compute dRT/dzeta as defined for the upward integration sweep
C 
c     Compute work = tauhat + rhohat*Rzb (used in 8.80 and 8.84) 
      DO I=1,nmu
      DO J=1,nmu
      work(I,J) = tauhat(I,J) 
         DO K=1,nmu
         work(I,J) = work(I,J) + rhohat(I,K)*RT(K + (J-1)*nmu)
         end do
      end do
      end do
c
c     Compute d(Rzb)/dzeta BY EQ. 8.80 
C 
      DO I=1,nmu
      DO J=1,nmu
      temp1 = 0.
      temp2 = 0.
         DO K=1,nmu
         temp1 = temp1 + RT(I + (K-1)*nmu)*work(K,J)
         temp2 = temp2 + tauhat(I,K)*RT(K + (J-1)*nmu)
         end do
      deriv(I + (J-1)*nmu) = -rhohat(I,J) - temp1 - temp2
      end do
      end do
c
      if(isource.ne.0) then
c     Compute d(s2mtbz)/dzeta by 8.84
c
      do j=1,nmu
      temp1 = 0.
      temp2 = 0.
         do k=1,nmu
         temp1 =  temp1 + RT(k+nsq)*work(k,j)
         temp2 = temp2 + s2hatp(k)*RT(k + (j-1)*nmu)
         end do
      deriv(j + nsq) = -s2hatm(j) - temp1 - temp2
      end do
c
      endif
c
      endif
c
c----------------------------------------------------------------
C 
      RETURN
      END 

