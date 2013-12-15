C     Last change:  LKS  26 Apr 2008    8:20 am
      subroutine SYNHL
!     called by Radanal, synthesize HL raidances and check their goodness
c     Compute the DIFFUSE radiances at depths a, w, ..., m

      INCLUDE "DIMENS_XL.INC"
      PARAMETER (mxL=mxphi/2, mxamp=2*mxmu*(mxL+1))
c
      COMMON /CMISC/ IMISC(30),FMISC(30) 
      COMMON /CRAMP0/ RAMP0Pa(mxamp),RAMP0Pz(mxamp,mxz),RAMP0Ma(mxamp) 
      COMMON /CRAMP/ RAMPPa(mxamp),RAMPMa(mxamp),RAMPPz(mxamp,mxz),
     1  RAMPMz(mxamp,mxz)
      COMMON /Cradif/ RADMa(mxmu,mxphi),RADMz(mxmu,mxphi,mxz),
     1  RADPa(mxmu,mxphi),RADPz(mxmu,mxphi,mxz)
      COMMON /Cradir/ RAD0Ma(mxmu,mxphi),RAD0Pa(mxmu,mxphi), 
     1  RAD0Pz(mxmu,mxphi,mxz) 
!     *****************************************************************

      nz = imisc(4)
      kcol = imisc(10)
      nhat = kcol
      namp = 2*nhat

c       Convert the downward TOTAL amplitudes to DIFFUSE amplitudes 
c       at zeta = a, w, ..., z,..., m 
c       convert the upward TOTAL amplitudes to DIFFUSE amplitudes at 
c       zeta = a.  The upward total = the upward diffuse for 
c       zeta = w, ..., m. 
        do i=1,namp
          RAMPPa(i) = RAMPPa(i) - RAMP0Pa(i) 
          RAMPMa(i) = RAMPMa(i) - RAMP0Ma(i) 
          do k=1,nz
            RAMPPz(i,k) = RAMPPz(i,k) - RAMP0Pz(i,k)
          end do
        end do
C*****  NOTE:  Now RAMPPa and RAMPPz contain the DIFFUSE amplitudes 
c       (same for RAMPMa and RAMPMz) 
!
c       Compute the DIFFUSE radiances at depths a, w, ..., m
        CALL SYNRAD(RAMPPa,RADPa,mxmu)
        do k=1,nz
           CALL SYNRAD(RAMPPz(1,k),RADPz(1,1,k),mxmu)
        end do
c
        CALL SYNRAD(RAMPMa,RADMa,mxmu)
        do k=1,nz
           CALL SYNRAD(RAMPMz(1,k),RADMz(1,1,k),mxmu)
        end do
c 
c       Compute the DOWNWARD DIRECT radiance at depths a, w, ..., m 
        CALL SYNRAD(RAMP0Pa,RAD0Pa,mxmu)
        do k=1,nz
           CALL SYNRAD(RAMP0Pz(1,k),RAD0Pz(1,1,k),mxmu)
        end do
c 
c       Compute the UPWARD DIRECT radiance at zeta = a. The upward direct
c       radiance is zero for zeta = w,..., m.
        CALL SYNRAD(RAMP0Ma,RAD0Ma,mxmu)
c
c     Note on storage.  Arrays now contain the following:
c     rad0pa = downward direct in air = incident sky radiance
c     radpa = downward diffuse in air = zero (all incident sky
c             radiance is by definition direct radiance)
c     rad0ma = upward direct in air = reflected sky radiance
c     radma = upward diffuse in air = water leaving radiance
c
c      Check that all radiances are non-negative
      call RADCHECK
c 
      return
      end subroutine
