C     Last change:  LKS   1 Nov 2007   12:53 pm
      subroutine kradpath
c
c     core routine on file kradpath.f
c
c     called by RADANAL [MAIN->RADANAL->KRADPATH]
c 
c     This routine computes RADIANCE K-functions using L&W Eq. 3.22, 
c     for the selected directions.  The elastic-scattering path function
c     is also computed, using the source-free form of Eq. 5.23 and the 
c     same depth derivatives.
c 
c+++++WARNING:  The computed path function equals the elastic-scattering
c               path function of Eq. 5.23 ONLY if there is NO INELASTIC
c               SCATTERING and NO INTERNAL SOURCES.
c+++++WARNING:  a selected pair of depths z(iz) and z(iz+1) is used to
c               estimate derivatives of the radiance at the midpoint, but 
c               these estimates may be inaccurate if the z levels are
c               not closely spaced (e.g. 0.01 optical depths apart) 
c 
      INCLUDE "DIMENS_XL.INC"
c
      COMMON /Cgrid/ fmu(mxmu),bndmu(mxmu),omega(mxmu),deltmu(mxmu),
     1               zgeo(mxz),zeta(mxz)
      COMMON /CgridPhi/phi(mxphi),bndphi(mxphi)
      COMMON /Ciop/ acoef(mxz,0:mxcomp),bcoef(mxz,0:mxcomp),
     1		      atten(mxz),albedo(mxz), bbcoef(mxz,0:mxcomp)
      COMMON /Cradif/ RADMa(mxmu,mxphi),RADMz(mxmu,mxphi,mxz),
     1 RADPa(mxmu,mxphi),RADPz(mxmu,mxphi,mxz) 
      COMMON /Cradir/ RAD0Ma(mxmu,mxphi),RAD0Pa(mxmu,mxphi), 
     1 RAD0Pz(mxmu,mxphi,mxz)
      COMMON /Cpkfcn/ ipkfcn,izkfcn(mxz)
      COMMON /CKRAD/ ipkrad,istart,istop,istep,jstart,jstop,jstep 
      COMMON /Cmisc/ imisc(30),fmisc(30) 
c 
      dimension theta(mxmu)
      nmu = imisc(1)
      radeg = fmisc(3)
c
c     get exact theta values at cell centers for printout
      theta(1) = 0.5*(90.0 + radeg*acos(bndmu(1)))
      theta(nmu) = 0.
      do i=2,nmu-1
         theta(i) = 0.5*radeg*(acos(bndmu(i-1)) + acos(bndmu(i)))
      enddo

      write(10,300)
c 
      do j=jstart,jstop,jstep
      phideg = radeg*phi(j) 
c 
      do i=istart,istop,istep
      thedeg = theta(i)
c
      if(i.lt.nmu) then
c
c     non-polar quads
c 
      write(10,301)
         do iiz=1,ipkfcn
            iz = izkfcn(iiz)
            f1overdz = 1.0/(zgeo(iz+1) - zgeo(iz)) 
            zmid = 0.5*(zgeo(iz+1) + zgeo(iz))
            beamc = 0.5*(atten(iz+1) + atten(iz))
c 
c           radiances at zmid  
            RMmid = 0.5*(RADMz(i,j,iz+1) + RADMz(i,j,iz)) 
            RPmid = 0.5*(RADPz(i,j,iz+1) + RADPz(i,j,iz) + 
     1              RAD0Pz(i,j,iz+1) +  RAD0Pz(i,j,iz))
c
c           radiance derivatives at zmid 
            dLMdz = f1overdz*(RADMz(i,j,iz+1) - RADMz(i,j,iz)) 
            dLPdz = f1overdz*(RADPz(i,j,iz+1) - RADPz(i,j,iz) + 
     1              RAD0Pz(i,j,iz+1) - RAD0Pz(i,j,iz))
c 
c           path functions at zmid (note: in L&W Eq. 5.23, mu < 0 for 
c           upwelling directions, so use -fmu, since fmu > 0)
            pathfm = -fmu(i)*dLMdz + beamc*RMmid
            pathfp = fmu(i)*dLPdz + beamc*RPmid
c 
c           K-functions at zmid
            fkm = -dLMdz/RMmid
            fkp = -dLPdz/RPmid
c 
      write(10,302) i,j,thedeg,phideg,zgeo(iz),zgeo(iz+1),zmid,RPmid,
     1 RMmid,pathfp,pathfm,fkp,fkm
         end do      ! end iiz loop

      elseif(i.eq.nmu .and. j.eq.1) then
c
c     polar cap; same formulas as above
c 
      write(10,301)
      do iiz=1,ipkfcn
         iz = izkfcn(iiz)
         f1overdz = 1.0/(zgeo(iz+1) - zgeo(iz)) 
         zmid = 0.5*(zgeo(iz+1) + zgeo(iz))
         beamc = 0.5*(atten(iz+1) + atten(iz))
c
         RMmid = 0.5*(RADMz(nmu,1,iz+1) + RADMz(nmu,1,iz)) 
         RPmid = 0.5*(RADPz(nmu,1,iz+1) + RADPz(nmu,1,iz) +
     1           RAD0Pz(nmu,1,iz+1) + RAD0Pz(nmu,1,iz)) 
c 
         dLMdz = f1overdz*(RADMz(nmu,1,iz+1) - RADMz(nmu,1,iz)) 
         dLPdz = f1overdz*(RADPz(nmu,1,iz+1) - RADPz(nmu,1,iz) +
     1       RAD0Pz(nmu,1,iz+1) - RAD0Pz(nmu,1,iz))
c 
         pathfm = -fmu(i)*dLMdz + beamc*RMmid
         pathfp = fmu(i)*dLPdz + beamc*RPmid
c 
         fkm = -dLMdz/RMmid
         fkp = -dLPdz/RPmid
c 
      write(10,302) i,j,thedeg,phideg,zgeo(iz),zgeo(iz+1),zmid,RPmid,
     1 RMmid,pathfp,pathfm,fkp,fkm
      end do     ! end iiz loop

      endif

      end do      ! end i loop
      end do      ! end j loop
c 
      RETURN
C 
  300 FORMAT(//2x,'Radiances, Path Functions, and Radiance K-Functions'/
     1'     (Valid only for closely spaced depths)'/
     1'     (The path function equals the elastic-scatter path',
     1' function only if there were no internal sources or inelastic',
     1' scatter in the run)'/
     2'     ("+" indiciates theta,phi in the downward hemisphere; "-"',
     3' indicates theta,phi in the upward hemisphere)'//
     4'  I  J  Theta   Phi  zupper zlower    z      RAD+',
     5 7X,'RAD-      PATHF+     PATHF-       K+       K-'/ 
     6'        (deg)  (deg)  (m)    (m)     (m)      (W/m^2 sr nm)',
     79x,'(W/m^3 sr nm)       (1/m)    (1/m)')
  301 FORMAT(' ')
  302 FORMAT(2I3,F6.1,F7.1,2F7.3,F8.4,1P,4E11.3,0P,2F9.4,F9.3,2F9.4)
      END
