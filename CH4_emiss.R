# Calculate the CH4 emissions

# We expect the CH4 concentrations to be stable over time
delta_ch4=0
M0=731.41
Tsoil=120 			# lifetime of soil sink (years) Myhre et al., 2013, doi: 10.1017/cbo9781107415324.018
Tstrat=150          # lifetime of tropospheric sink (years) Myhre et al., 2013, doi: 10.1017/cbo9781107415324.018
UC_CH4=2.78
TOH0=9.6		    # initial OH lifetime (years) Wigley et al. 2002



(M0 * (1/Tsoil + 1/Tstrat + 1/TOH0)) * UC_CH4

