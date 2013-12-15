C     Last change:  CDM  22 Jan 2009
      subroutine writerad
c 
c     core routine on file writerad.f
c
c     called from radanal.f only if iOptRad = 1
c
c     This routine writes Lroot.txt files with the full radiance
c     distribution on a format designed for ease of use in plotting
c     with IDL (this file is read by IDL routine read_H_Lfile.pro v5.2).
c     The polar cap radiances are copied into all phi array elements
c     (not just to the phi = 0 element), and the phi = 0 values
c     are copied to create identical phi = 360 deg values.  This
c     expanded array makes it easy to "wrap around" plots on a phi
c     = 0 to 360 grid.

c     a depth of -1.0 labels values in air, just above the sea surface

c     Changes made Aug 2013 for v 5.2:
c     total radiances are also decomposed to L_sky, L_w, and L_sr in air,
c     and into L_direct and L_diffuse in water
c 
c     called by RADANAL [MAIN->RADANAL->WRITERAD]
c 
      INCLUDE "DIMENS_XL.INC"
c
      COMMON /CRADIF/ RADMa(mxmu,mxphi),RADMz(mxmu,mxphi,mxz),
     1          RADPa(mxmu,mxphi),RADPz(mxmu,mxphi,mxz)
      COMMON /CRADIR/ RAD0Ma(mxmu,mxphi),RAD0Pa(mxmu,mxphi), 
     1 RAD0Pz(mxmu,mxphi,mxz)
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)

      COMMON /Cpirrad/ ipirad,izirad(mxz)   !print on same grid as Irrads
      COMMON /Cmisc/ imisc(30),fmisc(30)
      COMMON /Ctitle/ ititle
      Character ititle*120
      common /Cradfile/ Lrootname!, Erootname
      character Lrootname*120 !, Erootname*120
c
      data kall/0/

c     on the first call, compute the directions of the quad
c     centers and write the file headers
c 
      dimension thedeg(mxmu),phideg(mxphi)
c 
      save
c
      nmu = imisc(1)
      nphi = imisc(2)
      nwave = imisc(7) 
      nuLfile = imisc(15)
      radeg = fmisc(3)

c
      if(kall.eq.0) then
c        get theta and phi in degrees
         thedeg(1) = 0.5*(90.0 + radeg*acos(bndmu(1))) 
         do i=2,nmu-1
            thedeg(i) = 0.5*radeg*(acos(bndmu(i-1)) + acos(bndmu(i)))
         end do
         thedeg(nmu) = 0.
         do j=1,nphi
            phideg(j) = radeg*phi(j)
         end do

c        open the output file and write headers
      open(nuLfile,file=Lrootname, status = 'unknown')
c
      write(nuLfile,fmt='(2a)') 'HYDROLIGHT Run Title: ',
     1                          ititle(1:lenstr(ititle))
      write(nuLfile,fmt='(a)') 'NOTE: This file is formatted for reading
     1 by IDL routine read_H_Lfile.pro v5.2'
      write(nuLfile,fmt='("Parameters: nz, ntheta, nphi, nlambda = ",
     1  4i5)')
     2  ipirad+1,2*nmu,nphi+1,nwave
      write(nuLfile,150)

         kall = 1
      endif

c-----End of initialization on first call.

c     Subsequent calls start here:

c     write radiances in the air (at z = -1.0)
      depth = -1.0
      wavel = fmisc(13)
c
c     downwelling sky radiances (theta = 0 to 90)
      do iu=nmu,1,-1
         do iv=1,nphi

            if(iu.eq.nmu) then
               rad = rad0pa(iu,1) ! = incident sky radiance
            else
               rad = rad0pa(iu,iv)
            endif

            write(nuLfile,152) depth,thedeg(iu),phideg(iv),wavel,rad,
     *rad,0.0,0.0
         enddo

c        duplicate phi = 0 value at 360 for convenient plotting
         write(nuLfile,152) depth,thedeg(iu),360.0,wavel,rad0pa(iu,1),
     * rad0pa(iu,1),0.0,0.0
      enddo

c     upwelling radiances (theta = 90 to 180)
      do iu=1,nmu
         do iv=1,nphi

            if(iu.eq.nmu) then
               rad = rad0ma(iu,1) + radma(iu,1) ! = surf-reflected + water-leaving
               radsr = rad0ma(iu,1)
               radwl = radma(iu,1)
            else
               rad = rad0ma(iu,iv) + radma(iu,iv)
               radsr = rad0ma(iu,iv)
               radwl = radma(iu,iv)
            endif

            write(nuLfile,152) depth,180.0-thedeg(iu),phideg(iv),
     1                         wavel,rad,0.0,radwl,radsr
         enddo

c     repeat phi=0 value at phi = 360:
         write(nuLfile,152) depth,180.0-thedeg(iu),360.0,wavel,
     1   rad0ma(iu,1) + radma(iu,1),0.0,radma(iu,1), rad0ma(iu,1)

      enddo

c     write radiances in the water (at z = user requested values)
c 
      do iiz=1,ipirad
        iz = izirad(iiz)
        depth = zgeo(iz)
c
c     downwelling radiances (theta = 0 to 90)
      do iu=nmu,1,-1
         do iv=1,nphi

            if(iu.eq.nmu) then
               rad = rad0pz(iu,1,iz) + radpz(iu,1,iz) ! = direct + diffuse
               raddir = rad0pz(iu,1,iz)
               raddif = radpz(iu,1,iz)
            else
               rad = rad0pz(iu,iv,iz) + radpz(iu,iv,iz)
               raddir = rad0pz(iu,iv,iz)
               raddif = radpz(iu,iv,iz)
            endif

            write(nuLfile,152) depth,thedeg(iu),phideg(iv),wavel,rad,
     * raddir,raddif
         enddo
         write(nuLfile,152) depth,thedeg(iu),360.0,wavel,
     1   rad0pz(iu,1,iz)+radpz(iu,1,iz),rad0pz(iu,1,iz),radpz(iu,1,iz)
      enddo

c     upwelling radiances (theta = 90 to 180)
      do iu=1,nmu
         do iv=1,nphi

            if(iu.eq.nmu) then
               rad = radmz(iu,1,iz) ! = diffuse only
            else
               rad = radmz(iu,iv,iz)
            endif

            write(nuLfile,152) depth,180.0-thedeg(iu),phideg(iv),
     1                         wavel,rad,0.0,rad
         enddo
         write(nuLfile,152) depth,180.0-thedeg(iu),360.0,wavel,
     1   radmz(iu,1,iz),0.0,radmz(iu,1,iz)
      enddo
c
      enddo     ! end izz loop
      return
c 
  150 format('Radiances L(z,theta,phi,lambda) are in W m^-2 sr^-1 nm^-1'
     */'theta and phi are the directions of photon travel:'/
     *'theta = 0 to 90 is downwelling; theta = 90 to 180 is upwelling; (
     *theta = 180 is nadir-viewing; theta = 0 is zenith-viewing)'/
     *'depth = -1.0 labels values in air (just above the sea surface)'/
     *'L_sky is incident sky radiance; theta = 0 to 90 deg only; in air
     *only'/
     *'L_w is water-leaving radiance; theta = 90 to 180 deg only; in air
     * only'/
     *'L_sr is surface-reflected radiance; theta = 90 to 180 deg only; i
     *n air only'/
     *'L_dir is in-water direct radiance; theta = 0 to 90 deg only; in w
     *ater only'/
     *'L_dif is in-water diffuse radiance; theta = 0 to 180 deg; in wate
     *r only'/
     *'   depth   theta    phi   lambda  total radiance L_sky or L_dir
     *L_w or L_dif    L_sr'/
     *'    [m]    [deg]   [deg]   [nm]   [W/(m^2 sr nm)]' )
  152 format(f8.2,3f8.1,4es15.5e3) !allows for 1.23456E-100
      END
