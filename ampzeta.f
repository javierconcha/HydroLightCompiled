C     Last change:  LKS   7 Aug 2008    7:31 pm
      SUBROUTINE ampzeta 
C 
C     core routine on file ampzeta.f
C
c     called by RADAMPS [MAIN->RADAMPS->AMPZETA]
C
C     calls canned routines P2ARAY, SGEFS (in matinv.f), and MATXMAT
C 
c     This routine computes the radiance amplitudes Lhatp+(zeta;L) = RAMPPz
c     and Lhatp-(zeta;L) = RAMPMz at all INTERIOR depths w .lt. zeta .le. m,
c     using Eqs. (8.105) and (8.106).  These calculations are performed
c     L-value by L-value.  Source terms are omitted if isource = 0.
C 
      INCLUDE "DIMENS_XL.INC"
      PARAMETER (mxL=mxphi/2, mxamp=2*mxmu*(mxL+1))
C 
      COMMON /CRAMP/ RAMPPa(mxamp),RAMPMa(mxamp),RAMPPz(mxamp,mxz),
     1  RAMPMz(mxamp,mxz)
      COMMON /CRTS/ Rzw(mxmu,mxmu,mxz),Twz(mxmu,mxmu,mxz), 
     1  S1Ptwz(mxmu,mxz),S2Ptwz(mxmu,mxz),
     2  R1zb(mxmu,mxmu,mxz),R2zb(mxmu,mxmu,mxz),
     3  S1Mtbz(mxmu,mxz),S2Mtbz(mxmu,mxz)
      COMMON /CMISC/ imisc(30),fmisc(30)
c
c     temporary local storage:
      DIMENSION Twzb(mxmu,mxmu), Rpzb(mxmu,mxmu,mxz), SpMtbz(mxmu,mxz),
     1  SpPtwz(mxmu,mxz),temp1(mxmu,mxmu),temp2(mxmu,mxmu),temp3(mxmu),
     2  temp4(mxmu),temp5(mxmu)
c     work arrays for matrix inversion routine sgefs
      dimension work(mxmu),iwork(mxmu)
C 
      nmu = imisc(1)
      nL = imisc(3) 
      nz = imisc(4) 
      isource = imisc(8)
!      idbug = imisc(9)
      nhat = imisc(10)
      nuscr1 = imisc(18)
      nuscr2 = imisc(19)
C 
      REWIND nuscr1 
      REWIND nuscr2 
C 
      DO L=0,nL
      LOFSET = nmu*L
C 
C     READ IN Rzw = R(zeta,w;L), Twz = T(w,zeta;L), etc., for all zeta 
C     levels, for the current L value
C 
      DO iz=1,nz
      READ(nuscr1) ((Rzw(i,j,iz),i=1,nmu),j=1,nmu)
      READ(nuscr1) ((Twz(i,j,iz),i=1,nmu),j=1,nmu)
      READ(nuscr2) ((R1zb(i,j,iz),i=1,nmu),j=1,nmu) 
      READ(nuscr2) ((R2zb(i,j,iz),i=1,nmu),j=1,nmu) 
      if(isource.ne.0) read(nuscr2) (S1Mtbz(i,iz),i=1,nmu),
     1                 (S2Mtbz(i,iz),i=1,nmu),
     2                 (S1Ptwz(i,iz),i=1,nmu), (S2Ptwz(i,iz),i=1,nmu)
      end do
C
C     Initialize for cosine amplitudes (ip = 1)
      ip = 1
c
C     Load  R1zb, S1Mtbz and S1Ptwz 
      IOFSET = LOFSET 
      DO iz=1,nz
         DO j=1,nmu
           SpMtbz(j,iz) = S1Mtbz(j,iz)
           SpPtwz(j,iz) = S1Ptwz(j,iz)
             DO i=1,nmu
               Rpzb(i,j,iz) = R1zb(i,j,iz)
             end do
         end do
      end do
C 
  999 CONTINUE
C 
C     Compute the amplitudes at each interior zeta level, for the
c     current p value
C
      DO iz=2,nz
c
c     Begin evaluation of Eq. (8.105) ---------------------------------
c  
C     Compute Twzb = Tp(w,zeta,b;L) of Eq. (8.103) 
C 
C     Compute temp1 = I - Rp(z,b) * R(z,w)
      DO i=1,nmu
         DO j=1,nmu
           sum = 0.
             DO k=1,nmu
               sum = sum + Rpzb(i,k,iz)*Rzw(k,j,iz)
             end do
           delt = 0.
           IF(I.EQ.J) delt = 1.
           temp1(i,j) = delt - sum
         end do
      end do
c
CC      IF(idbug.EQ.2) then
c     prepare to check the inversion:  save temp1 in Twzb
CC      do j=1,nmu
CC         do i=1,nmu
CC         Twzb(i,j) = temp1(i,j)
CC         end do
CC      end do
CC      CALL P2ARAY(temp1,nmu,nmu,mxmu,2,' I - Rp(z,b) * R(z,w)')
CC      endif

c     Invert I - Rp(z,b) * R(z,w)
c
cccccccccc  Canned routine calls  ccccccccccccccccccccccc
c
      DO i = 1,nmu
        DO j = 1,nmu
          temp2(i,j) = 0.
        end do
        temp2(i,i) = 1.0
      end do
c
      itask = 1
      call sgefs(temp1,mxmu,nmu,temp2(1,1),itask,ind,work,iwork)
      if(ind.lt.0) 
     1   write(10,*)'Error on first call to SGEFS from AMPzeta',IND
c
      if(ind.eq.-10) then
        call HERR("AMPzeta","SGEFS returned ind = -10")  !stop run
      endif
c
      itask = 2
      do j = 2,nmu
        call sgefs(temp1,mxmu,nmu,temp2(1,j),itask,ind,work,iwork)
        if(ind.lt.0) 
     1    write(10,*)'Error on second call to SGEFS from AMPzeta',IND
      end do
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     temp2 now contains the inverse
c
c     Compute Tp(w,zeta,b;L) = T(w,zeta) * (Inverse)
      DO i=1,nmu
         DO j=1,nmu
         sum = 0.0
            DO k=1,nmu
            sum = sum + Twz(i,k,iz)*temp2(k,j)
            end do
         Twzb(i,j) = sum
         end do
      end do
c
c     first term in (8.105):
c
      call matxmat(RAMPPz(1+iofset,1),Twzb,1,nmu,nmu,1,mxmu,temp3,1)
c
      if(isource.ne.0) then
c
c     include the source terms:  evaluate the second term in (8.105):
c
      do j=1,nmu
        sum = SpPtwz(j,iz)
          do k=1,nmu
            sum = sum + SpMtbz(k,iz)*Rzw(k,j,iz)
          end do
        temp4(j) = sum
      end do
c
      call matxmat(temp4,temp2,1,nmu,nmu,1,mxmu,temp5,1)
      factor = 1.0
c
      else
c     no source terms:
      factor = 0.0
      endif
c
      do i=1,nmu
      RAMPPz(i+iofset,iz) = temp3(i) + factor*temp5(i)
      end do
c
c     Begin evaluation of Eq. (8.106) ---------------------------------
c
c     first term (using previously computed Twzb)
c
      do i=1,nmu
        do j=1,nmu
          sum = 0.0
             do k=1,nmu
               sum = sum + Twzb(i,k)*Rpzb(k,j,iz)
             end do
          temp2(i,j) = sum
        end do
      end do
c
      call matxmat(RAMPPz(1+iofset,1),temp2,1,nmu,nmu,1,mxmu,temp3,1)
c
      if(isource.ne.0) then
c
c     include internal sources:  evaluate the second term
c
c     compute temp1 = I - R(z,w) * Rp(z,b)
c
      do i=1,nmu
        do j=1,nmu
          sum = 0.0
            do k=1,nmu
              sum = sum + Rzw(i,k,iz)*Rpzb(k,j,iz)
            end do
          delt = 0.0
          if(i.eq.j) delt = 1.0
          temp1(i,j) = delt - sum
        end do
      end do
c
c     invert I - R(z,w) * Rp(z,b)
c
cccccccccc  Canned routine calls  ccccccccccccccccccccccc
c
      DO i = 1,nmu
        DO j = 1,nmu
          temp2(i,j) = 0.0
        end do
        temp2(i,i) = 1.0
      end do
c
      itask = 1
         call sgefs(temp1,mxmu,nmu,temp2(1,1),itask,ind,work,iwork)
c
      if(ind.lt.0) 
     1   write(10,*)'Error on third call to SGEFS from AMPzeta',IND

      itask = 2
      do j = 2,nmu
        call sgefs(temp1,mxmu,nmu,temp2(1,j),itask,ind,work,iwork)
        if(ind.lt.0) 
     1   write(10,*)'Error on fourth call to SGEFS from AMPzeta',IND
      end do
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     temp2 now contains the inverse
c
      do j=1,nmu
        sum = SpMtbz(j,iz)
        do k=1,nmu
          sum = sum + SpPtwz(k,iz)*Rpzb(k,j,iz)
        end do
        temp4(j) = sum
      end do
c
      call matxmat(temp4,temp2,1,nmu,nmu,1,mxmu,temp5,1)
      factor = 1.0
c
      else
c     no source terms:
      factor = 0.0
      endif
c
      do i=1,nmu
      RAMPMz(i+iofset,iz) = temp3(i) + factor*temp5(i)
      end do
C  
      END DO  ! end iz loop
C 
      if(ip.gt.1) go to 100
c
C     Repeat for the sine amplitudes: ip = 2
C 
      ip = 2
      IOFSET = LOFSET + nhat 
C     Load  R2zb, S2Mtzb and S2Ptwz 
      DO iz=1,nz
         DO j=1,nmu
            SpMtbz(j,iz) = S2Mtbz(j,iz)
            SpPtwz(j,iz) = S2Ptwz(j,iz)
            DO i=1,nmu
              Rpzb(i,j,iz) = R2zb(i,j,iz)
            end do
         end do
      end do
      GO TO 999 
C
  100 continue
      END DO   ! end L loop

      RETURN
      END
