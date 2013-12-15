C     Last change:  LKS  21 Jan 2008    8:55 am
      function BRDFbotm(rmup,sphip,umu,vphi)
c
c     user routine on file ..\maincode\BRDFLamb.f
c
c     This BRDF is for a Lambertian bottom with reflectance R:
c
c     BRDF(mu',phi',mu,phi) = R/pi
c
c     where 
c     R is the "bottom reflectance" rflbot (= Eu/Ed for a Lambertian
c               surface) at the current wavelength
c
c*****NOTE:  This BRDF routine is the default for use with Hydrolight.
c     Users who wish to model non-Lambertian bottoms must replace this
c     subroutine with one of the same name, but which returns the desired
c     BRDF.  An example of a non-Lambertian BRDF is found on file
c     ..\examples\template\BRDFminn.f  
c
      COMMON /CMISC/ imisc(30),fmisc(30) 

      data kall/0/
c
      save
c
c     initialization on first call:
c
      if(kall.eq.0) then
         pi = fmisc(1)
         ibotm = imisc(12)
         kall = 1
      end if
c
c     subsequent calls start here (routine rbottom returns the
c     bottom irradiance reflectance R = Eu/Ed):
c
      wavel = fmisc(13)
      BRDFbotm = rbottom(ibotm,wavel)/pi
c
      return
      end

!-----------------------------------------------------------------------
      subroutine pntBRDFbotm
!     Prints the appropriate BRDF msg on initialization
c
      write(10,100)
      
  100 format(/5x,'The bottom bidirectional reflectance distribution',
     1' function (BRDF) is Lambertian:'/8x,
     2"BRDF(mu',phi',mu,phi) = R/pi")
      end     