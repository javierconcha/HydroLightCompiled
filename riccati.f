C     Last change:  LKS  27 Feb 2008    2:16 pm
      SUBROUTINE riccati(L)
c 
c     core routine on file riccati.f
c
c     called by radamps [MAIN->RADAMPS->RICCATI]
c
c     calls ode and drtdzs
c 
c     This version uses the SODE package to solve the ODE system.
c
c     This routine solves for the standard operators Rzw = R(zeta,w;L),
c     Twz = T(w,zeta;L), S1ptwz = S1hat+t(w,zeta;L), etc by integrating
c     Eqs (8.74), (8.75), and (8.78) in a downward sweep with
c     initial values of R(w,w) = 0, T(w,w) = 0, S1hat+t(w,w) = 0, etc
c     as given by Eqs. (8.72).
c
c     Values of R1zb = R1(zeta,b), S1mtbz = S1hat-t(b,zeta), etc.
c     are obtained by integrating Eqs. (8.80) and (8.84) in an upward
c     sweep from zeta = m to zeta = w, with values for the lower
c     boundary S[m,b] built into the initial conditions as shown in Eq.
c     (8.94).
c
c     If isource.ne.0, this version of the Riccati integration includes
c     internal source terms (the sum of true sources and effective 
c     sources from inelastic scatter).  The source terms and the 
c     associated equations are omitted if isource = 0, in order to 
c     decrease the run time when no sources are present.
c
c     This version assumes that the lower boundary is either a matte
c     bottom or an infinitely deep layer of water, with no radiance
c     coming up through the lower boundary.  The lower boundary S[m,b]
c     is also assumed to be source free.  These lower boundary 
c     assumptions simplify the integration scheme outlined in Fig 8.2.
c
c     For the downward integration sweep, arrays Rzw, etc are stored
c     in the linear array RT as follows (for a given zeta value): 
c 
c     Rzw(I,J)  IS RT(I + (J-1)*nmu) 
c     Twz(I,J)  IS RT(I + (J-1)*nmu + nmu*nmu)
c     S1ptwz(i) is RT(i + 2*nmu*nmu)
c     S2ptwz(i) is RT(i + 2*nmu*nmu + nmu)
c
      INCLUDE "DIMENS_XL.INC"
      PARAMETER(mxeqn = 2*mxmu*mxmu + 2*mxmu)
c 
      COMMON /CRTS/ Rzw(mxmu,mxmu,mxz),Twz(mxmu,mxmu,mxz), 
     1  S1ptwz(mxmu,mxz),S2ptwz(mxmu,mxz),
     2  R1zb(mxmu,mxmu,mxz),R2zb(mxmu,mxmu,mxz),
     3  S1mtbz(mxmu,mxz),S2mtbz(mxmu,mxz)
      COMMON /CBOTBC/ rhatmb(mxmu,mxmu)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /CMISC/ imisc(30),fmisc(30)
c
      DIMENSION RT(mxeqn)
c     local storage for SODE:
      DIMENSION work(100+21*mxeqn),iwork(5)
C 
C     Subroutine drtdzs evaluates the rhs of Eqs. (8.74), (8.75), etc. 
c     for use by the ODE solver.
c 
      EXTERNAL drtdzs
C
      nmu = imisc(1)
      NL = imisc(3)
      nz = imisc(4)
      isource = imisc(8) 
c      IDBUG = imisc(9)

      IBOTM = imisc(12)
      relerrs = fmisc(6)
      abserrs = fmisc(7) 
      nmu2 = nmu*nmu 
C 
c     ------------------------------------------------------------
c
c     Begin integration of (8.74), (8.75) and (8.78) in a downward sweep from
c     zeta = w to zeta = m.
c
C     Initialize the arrays at zeta = w using (8.72) 
C 
      if(isource.eq.0) then
         neqns = 2*nmu2
      else
         neqns = 2*nmu2 + 2*nmu
      endif
c
      DO j=1,nmu
         S1ptwz(j,1) = 0.
         S2ptwz(j,1) = 0.
         RT(j + 2*nmu2) = 0.
         RT(j + 2*nmu2 + nmu) = 0.
            DO i=1,nmu
               Rzw(I,J,1) = 0.
               RT(I+(J-1)*nmu) = 0.
               delt = 0.
               IF(I.EQ.J) delt = 1.
               Twz(I,J,1) = delt
               RT(I+(J-1)*nmu+nmu2) = delt
           end do
      end do
C 
      zetastrt = zeta(1) 
c     set flag to "downward sweep" (= 1) for drtdzs:
      imisc(13) = 1
c
C     Integrate (8.74), etc to find R(zeta,w), etc at each zeta level 
C 
      DO iz=2,nz
!      zetaend = zetastrt + zeta(iz) - zeta(iz - 1) 
       idbug = 0
       IF(IDBUG.Gt.0) WRITE(10,3000) zetastrt,zetaend
 3000 FORMAT(2x,'zetastrt =',F8.4,5X,'zetaend =',F8.4)
c
clks  add extra evaluations iff zetas are too widely spaced
      dzeta = zeta(iz) - zeta(iz - 1) 
      if( dzeta  .lt. fmisc(26) ) then 
         nsteps = 1
      else
         nsteps = int(dzeta/fmisc(26)) +  1
         dzeta = dzeta / nsteps
      endif     
      Do istep = 1, nsteps
c       special case if last step; make sure zetaend is our output depth
        If(istep.eq.nsteps) then
          zetaend = zeta(iz) 
        Else
          zetaend = zetastrt + dzeta
        Endif

c
ccccccccccc  Call canned routine to solve the ODE system  ccccc
c
c     iflag is set to -1 to prevent ode (a predictor-corrector
c     algorithm) from extrapolating past the integration interval 
c     (e.g. to below the bottom on the downward sweep or to above 
c     the surface on the upward sweeps), which might cause problems 
c     in the calls to the abscat routine (even though the over-
c     extrapolation would be corrected to end up at the proper 
c     point).
c
      iflag = -1
      relerr = relerrs
      abserr = abserrs
*      write(10,*) 'Calling ode0', L, neqns, zetaend
*      write(6,*) 'Calling ode0', L, neqns,rt,zetastrt,zetaend,
*     1 relerr,abserr, iflag
      call ode(drtdzs,neqns,rt,zetastrt,zetaend,relerr,abserr,
     1 iflag,work,iwork)
c
      Enddo		!isteps
C
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C 
C     Save the solution at zeta = zetaend 
C 
         DO J=1,nmu
         S1ptwz(j,iz) = RT(j + 2*nmu2)
         S2ptwz(j,iz) = RT(j + 2*nmu2 + nmu)
            DO I=1,nmu
               Rzw(I,J,iz) = RT(I+(J-1)*nmu)
               Twz(I,J,iz) = RT(I + (J-1)*nmu + nmu2)
            end do   ! i loop
         end do      ! j loop
c      print*,' '
c      print*,' RT(1:10) after ode: iz = ',iz
c      write(10,fmt='(10e12.4)') (RT(iii),iii=1,10)

      end do         ! iz loop
c
c     ============= End of downward integration sweep ==================
c
c     Begin integration of (8.80) and (8.84) in two upward integration
c     sweeps (for p = 1 and p = 2) from zeta = m to zeta = w. 
c     The bottom boundary conditions are incorporated into the
c     initial values at zeta = m. 
c     Subroutine BOTMBC already has computed the needed value of
c     rhatmb = r1hat(m,b) for the current bottom type and L value
c
c     For the upward sweeps, arrays Rpzb and Spmtbz are stored in
c     the linear array RT as follows (for p =1 or p = 2, and for a
c     given zeta value):
c
C     Rpzb(I,J) IS RT(I + (J-1)*nmu)
c     Spmtbz(i) is RT(i + nmu*nmu)
c
c     Upward integration for p = 1 (cosine amplitudes) ---------
c
C     Integrate 8.80 from m to w to find R1(zeta,b) and integrate
c     8.84 to get S1hat-t(b,zeta), at each zeta level
C 
C     Initialize at zeta = m with R1(m,b) = R1hat(m,b), using 8.94 
C 
      DO J=1,nmu
      S1mtbz(j,nz) = 0.
      RT(j + nmu2) = 0.
         DO I=1,nmu
         R1zb(I,J,nz) = rhatmb(I,J)
         RT(I+(J-1)*nmu) = rhatmb(I,J)
         end do
      end do
C 
      zetastrt = zeta(nz)
c
c     set flag to "upward sweep with p = 1" for drtdzs:
      imisc(13) = 2
c
C     Integrate 
c
      DO iz=1,nz-1
      izrev = nz-iz
      zetaend = zetastrt - zeta(izrev+1) + zeta(izrev) 
c
ccccccccccc  Call canned routine to solve the ODE system  ccccc
c
      iflag = -1
      relerr = relerrs
      abserr = abserrs
      call ode(drtdzs,neqns,rt,zetastrt,zetaend,relerr,abserr,
     1 iflag,work,iwork)

c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
C     Save the solution at zetaend 
         DO J=1,nmu
         S1mtbz(j,izrev) = RT(j + nmu2)
            DO I=1,nmu
            R1zb(I,J,izrev) = RT(I+(J-1)*nmu)
            end do
         end do
      end do   ! iz loop
c
c     End of upward sweep for p = 1 -----------------------
c
c     Repeat upward sweep for p = 2 (sine amplitudes).  The value
c     of r2hat(m,b) is either 0 or equal to r1hat(m,b), depending on
c     the value of L and the bottom type.
c
      If (L.eq.0 .or. L.eq.NL) then
c
c     r2hat(m,b) = 0 for both matte and infinitely deep water
c
      DO J=1,nmu
      S2mtbz(j,nz) = 0.
      RT(j + nmu2) = 0.
          DO I=1,nmu
          R2zb(I,J,nz) = 0.0
          RT(I+(J-1)*nmu) = 0.0
          end do
      end do
c
      else
c
         if(ibotm.ne.0) then
c        r2hat = 0 for a matte bottom and any value of L
c
         DO J=1,nmu
         S2mtbz(j,nz) = 0.
         RT(j + nmu2) = 0.
            DO I=1,nmu
            R2zb(I,J,nz) = 0.0
            RT(I+(J-1)*nmu) = 0.0
            end do
         end do
c
         else
c        r2hat(m,b) = r1hat(m,b) for an infinitely deep bottom and
c        L = 1,...,NL-1
c
         DO J=1,nmu
         S2mtbz(j,nz) = 0.
         RT(j + nmu2) = 0.
            DO I=1,nmu
            R2zb(I,J,nz) = rhatmb(i,j)
            RT(I+(J-1)*nmu) = rhatmb(i,j)
            end do
         end do
         endif
      endif
c
c     perform the second (p = 2) upward integration sweep
C 
      zetastrt = zeta(nz)
c
c     set flag to "upward sweep with p = 2" for drtdzs
      imisc(13) = 3 
c
C     Integrate 
c
      DO iz=1,nz-1
      izrev = nz - iz
      zetaend = zetastrt - zeta(izrev+1) + zeta(izrev) 
c
ccccccccccc  Call canned routine to solve the ODE system  ccccc
c
      iflag = -1
      relerr = relerrs
      abserr = abserrs
      call ode(drtdzs,neqns,rt,zetastrt,zetaend,relerr,abserr,
     1 iflag,work,iwork)

c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Save the solution at zetaend 
         DO J=1,nmu
         s2mtbz(j,izrev) = rt(j + nmu2)
            DO I=1,nmu
            R2zb(I,J,izrev) = RT(I+(J-1)*nmu)
            end do   ! i loop
         end do   ! j
      end do   ! iz
c
c     ======== End of Riccati integrations for this L value =========
c
      RETURN
      END
