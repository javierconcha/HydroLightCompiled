echo ***
echo *** Compiling the HydroLight code
 del *.obj
 copy ..\common\incfiles_default.for ..\common\incfiles_user.for
 lf95 -dbl -nco -nlst -nap -ndal -nchk -ntrace -inln -npca -nsav -stchk -o1 -nw -nwo -c ..\common\incfiles_user.for >> link_stnd.log
 lf95 -dbl -chk -nco -nlst -pca -nsav -stchk -ntrace -ml winapi -win -nvsw -nw -c ..\common\w*.f90 >> link_stnd.log
 lf95 -dbl -chk -nco -nlst -pca -nsav -stchk -ntrace -ml winapi -win -nvsw -nw -c ..\common\*.f90 >> link_stnd.log
 lf95 -dbl -nco -nlst -nap -ndal -nchk -ntrace -inln -npca -nsav -stchk -o1 -nw -nwo -c *.f -ml msvb >> link_stnd.log
 lf95 -dbl -co -nlst -nchk -pca -nsav -stchk -nw -nwo -c *.for -ml msvb >> link_stnd.log
echo ***
echo *** Linking the HydroLight code
 lf95 *.obj -stack 750000  -ml msvb -lib %WinDir%\system32\HE5info.lib -nomap -winconsole -out mainHL_stnd.exe >> link_stnd.log
echo ***
echo *** Moving the HydroLight executable
 move mainHL_stnd.exe ..
del *.obj
del *.mod
