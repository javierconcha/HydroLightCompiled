      SUBROUTINE PNTAMP(z,RAMPa,RAMPz,mxrow,izflag) 
c 
c     on part2/pntamp.f          (rewritten 7/94)    
C 
c     called by radamps and radanal
c
C     This routine prints the radiance amplitudes (for debugging purposes)
c     at all depths:  a, w,..., z,..., m.
c     A title giving the appropriate column headings should be written
c     before calling pntamp.
c     If only RAMPa is to be printed (the case of RAMPa = Lhat0-(a)), call
c     the routine with izflag = 0
C 
      DIMENSION z(*),RAMPa(*),RAMPz(mxrow,*) 
c
      COMMON /CMISC/  IMISC(30),FMISC(30) 
c 
      nmu = imisc(1)
      nz = imisc(4) 
      idbug = imisc(9)
      nhat = imisc(10) 
c
      il = 0
      if(izflag.eq.0) then
         write(10,1599) 
      else
         write(10,1600) (z(iz),iz=1,nz)
      endif 
c
      do 1602 i=1,2*nhat 
      if(i.eq.nhat+1) then 
         write(10,1610) 
         il = 0
      endif 
      imod = mod(i,nmu) 
      if(imod.eq.1) then
         write(10,1606) il
         il = il + 1 
         imu = 0 
      endif 
      imu = imu + 1
c
c     select full or partial printout 
      if(idbug.le.1 .and. il.gt.2) go to 1602
      if(izflag.eq.0) then
         write(10,1612) imu,RAMPa(i)  
      else
         write(10,1612) imu,RAMPa(i),(RAMPz(i,j),j=1,nz)
      endif 
 1602 continue
      return
c 
c     Format statements 
c 
 1599 format(/2x,'cosines')
 1600 format(/2x,'cosines',23x,5('z =',f7.3,5x)/32x,
     1     5('z =',f7.3,5x))
 1606 format(/'  l =',i3)
 1610 format(/2x,'sines')
 1612 format(10x,i2,1p,6e15.4/28x,5e15.4)
      end
 
