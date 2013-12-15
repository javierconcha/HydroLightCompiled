C     Last change:  LKS  18 Nov 2007   10:16 am
       subroutine ampw 
c 
c     core routine on file ampw.f
c
c     called by RADAMPS [MAIN->RADAMPS->AMPW]
c
c     calls canned routines MATXMAT (in matxmat.f) and sgefs (in matinv.f)
c 
c     This routine computes the radiance amplitudes Lhatp+(w) = RAMPPw
c     and Lhatp-(w) = RAMPMw using Eqs (8.98) and (8.102), respectively. 
c     It is via these equations that the air-water surface boundary
c     effects are incorporated into the final solution.
c     Note that (8.98) and (8.102) incorporate all L values at once.
c     Source terms are omitted if isource = 0.
C 
C     Spectral air-water-surface storage arrays must be loaded with 
C        that1(a,w) in that1
C        that2(a,w) in that2
C        rhat1(w,a) in rhat1
C        rhat2(w,a) in rhat2
C       
      INCLUDE "DIMENS_XL.INC"
      PARAMETER(mxL=mxphi/2, mxhat=mxmu*(mxL+1), mxamp=2*mxhat)
C 
      COMMON /CRAMP/ RAMPPa(mxamp),RAMPMa(mxamp),RAMPPz(mxamp,mxz),
     1             RAMPMz(mxamp,mxz) 
      COMMON /CRAMP0/ RAMP0Pa(mxamp),RAMP0Pz(mxamp,mxz),RAMP0Ma(mxamp) 
      COMMON /Crthat/ that1(mxhat,mxhat),that2(mxhat,mxhat), 
     1                rhat1(mxhat,mxhat),rhat2(mxhat,mxhat)
      COMMON /CMISC/ imisc(30),fmisc(30)
c
c     temporary local storage:
      Dimension temp1(mxhat,mxhat),temp2(mxhat,mxhat),temp3(mxhat),
     1 temp4(mxhat),temp5(mxhat),
     2 rhat(mxhat,mxhat),that(mxhat,mxhat),R1wb(mxhat,mxhat),
     3 R2wb(mxhat,mxhat),Rwb(mxhat,mxhat),
     4 S1Mtbw(mxhat),S2Mtbw(mxhat),Shat(mxhat)
c     work arrays for matrix inversion routine sgefs
      dimension work(mxhat),iwork(mxhat)
c 
      nmu = imisc(1)
      nL = imisc(3) 
      nz = imisc(4)
      isource = imisc(8) 
      nhat = imisc(10) 
      nuscr2 = imisc(19)
C 
C     read R1(w,b;L), R2(w,b;L), S1-t(b,w;L) and S2-t(b,w;L)
c     from scratch file nuscr2.  These arrays were computed L value
c     by L value; now place the arrays in "full storage" form (i.e.
c     with blocks of zeros, as seen in Eq. 8.90). 
C
c     code change 22 Apr 98 by Mobley:
c     zero arrays for safety (problems on some machines)
c
      do j=1,nhat
      s1mtbw(j) = 0.
      s2mtbw(j) = 0.
      do i=1,nhat
      R1wb(i,j) = 0.
      R2wb(i,j) = 0.
      end do
      end do
c      
      REWIND nuscr2 
      DO L=0,nL
      Loffset = nmu*L
C 
C     Read the z = w (iz = 1) values
      read(nuscr2) ((R1wb(i+Loffset,j+Loffset),i=1,nmu),j=1,nmu) 
      read(nuscr2) ((R2wb(i+Loffset,j+Loffset),i=1,nmu),j=1,nmu)
      if(isource.ne.0) read(nuscr2) (S1Mtbw(i+Loffset),i=1,nmu), 
     1                              (S2Mtbw(i+Loffset),i=1,nmu) 
C 
C     Skip other zeta levels for this L value
         DO iz=2,nz
            read(nuscr2) dumrec
            read(nuscr2) dumrec
            if(isource.ne.0) read(nuscr2) dumrec
         end do
      end do
c 
c     Initialize for p = 1 (cosine amplitudes)
c 
      ip = 1
      ipoffset = 0
C     Load rhat1(w,a) into rhat, R1(w,b;L) into Rwb, etc  
      DO j=1,nhat
      Shat(j) = S1Mtbw(j)
         DO i=1,nhat
            Rwb(i,j) = R1wb(i,j)
            that(i,j) = that1(i,j)
            rhat(I,J) = rhat1(I,J) 
         end do
      end do
C 
  999 CONTINUE
c 
c     Begin evaluation of Eq. (8.98) -----------------------------
c
C     compute temp1 = Rp(w,b) * rhatp(w,a) 
C
      call matxmat(Rwb,rhat,nhat,nhat,nhat,mxhat,mxhat,temp1,mxhat)
C  
C     Compute the inverse of temp1 = I - Rp(w,b)*rhatp(w,a)
C
      do j=1,nhat
         do i=1,nhat
             if(i.eq.j) then
               delt = 1.0
             else
               delt = 0.0
             endif
             temp1(i,j) = delt - temp1(i,j)
         end do
      end do
c
cccccccccc  Canned routine calls for matrix inversion  ccccccccccccccc
c
c     The matrix temp1 is inverted by first obtaining an LU
c     decomposition, and the backsubstituting column by column
c     on the identity matrix
c 
      DO i = 1,nhat
         DO j = 1,nhat
            temp2(i,j) = 0.0
         end do
         temp2(i,i) = 1.0
      end do
c     temp 2 contains the identity matrix
c
c    obtain the LU decomposition of temp1 (itask = 1) and then 
c    backsubstitute for the first column vector
c
      itask = 1
      call sgefs(temp1,mxhat,nhat,temp2(1,1),itask,ind,work,iwork)
      if(ind.lt.0) 
     1   write(10,*)'Error on first call to SGEFS from AMPW',IND
c
c     temp1 now contains something related to the LU decomposition.
c     use the existing LU decomposition (itask = 2) and backsubstitute 
c     for the remaining column vectors to obtain the full inverse
c
      itask = 2
      do j = 2,nhat
         call sgefs(temp1,mxhat,nhat,temp2(1,j),itask,ind,work,iwork)
         if(ind.lt.0) 
     1   write(10,*)'Error on second call to SGEFS from AMPW',IND
      end do
c
c     temp2 now contains the inverse
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     compute temp3 = RAMPp+(a) * thatp(a,w)
c
      call matxmat(RAMP0Pa(1+ipoffset),that,1,nhat,nhat,1,mxhat,temp3,1)
c
      if(isource.ne.0) then
c
c     include internal sources:  compute temp4 = Shatp-t(b,w) * rhatp(w,a)
c
      call matxmat(Shat,rhat,1,nhat,nhat,1,mxhat,temp4,1)
      factor = 1.0
c
      else
c     no internal sources:
      factor = 0.0
      endif
c
      do i=1,nhat
      temp4(i) = temp3(i) + factor*temp4(i)
      end do
c
c     compute RAMPp+(w) by Eq. (8.98)
c
      call matxmat(temp4,temp2,1,nhat,nhat,1,mxhat,
     1             RAMPPz(1+ipoffset,1),1)
c
c     Begin evaluation of Eq. (8.102) -------------------------------
c
c     first term:
c
c     compute temp4 = RAMPp+(a)*thatp(a,w)*(inverse)
c
      call matxmat(temp3,temp2,1,nhat,nhat,1,mxhat,temp4,1)
c
      call matxmat(temp4,Rwb,1,nhat,nhat,1,mxhat,temp5,1)
c
      if(isource.ne.0) then
c     include internal sources:  
c
c     compute rhatp(w,a) * Rp(w,b)
c
      call matxmat(rhat,Rwb,nhat,nhat,nhat,mxhat,mxhat,temp1,mxhat)
c
C     Compute the inverse of temp1 = I - rhatp(w,a) * Rp(w,b)
C
      do j=1,nhat
         do i=1,nhat
            if(i.eq.j) then
               delt = 1.0
            else
               delt = 0.0
            endif
         temp1(i,j) = delt - temp1(i,j)
         end do
      end do
c
cccccccccc  Canned routine calls for matrix inversion  ccccccccccccccc
c
      DO i = 1,nhat
         DO j = 1,nhat
            temp2(i,j) = 0.0
         end do
      temp2(i,i) = 1.0
      end do
c
      itask = 1
      call sgefs(temp1,mxhat,nhat,temp2(1,1),itask,ind,work,iwork)
      if(ind.lt.0) 
     1   write(10,*)'Error on third call to SGEFS from AMPW',IND
c
      itask = 2
      do j = 2,nhat
         call sgefs(temp1,mxhat,nhat,temp2(1,j),itask,ind,work,iwork)
         if(ind.lt.0) 
     1   write(10,*)'Error on fourth call to SGEFS from AMPW',IND
      end do
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     second term:  Shatp-t(b,w) * (inverse)
c
      call matxmat(Shat,temp2,1,nhat,nhat,1,mxhat,temp4,1)
      factor = 1.0
c
      else
c     no internal sources:
      factor = 0.0
      endif
c
c     Compute RAMPp-(w) by Eq. (8.102)
c
      do i=1,nhat
      RAMPMz(i+ipoffset,1) = factor*temp4(i) + temp5(i)
      end do
      IF(IP.GT.1) return
C 
C     Repeat for p = 2 (sine amplitudes)
      ip = 2
      ipoffset = nhat
C     Load rhat2(w,a) into rhat, R2(w,b;L) into Rwb, etc  
      DO j=1,nhat
      Shat(j) = S2Mtbw(j)
         DO i=1,nhat
            Rwb(i,j) = R2wb(i,j)
            that(i,j) = that2(i,j)
            rhat(I,J) = rhat2(I,J)
         end do
      end do
      go to 999 
c
      END 

