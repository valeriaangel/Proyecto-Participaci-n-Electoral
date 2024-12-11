clear all
cd"/Users/valeria/Desktop/Hec Proyecto"

import excel "particip_vio.xlsx", firstrow
xtset codmpio ano

gen ataq_l_10k = ataq_l_100k/10
gen ataq_no_l_10k = ataq_no_l_100k/10
gen participacion_pp = participacion * 100



reghdfe participacion_pp ataq_l_10k ataq_no_l_10k y_total, a(codmpio ano) vce(cluster codmpio)
outreg2 using "reg.doc", word replace


reghdfe participacion_pp ataq_l_10k  y_total, a(codmpio ano) vce(cluster codmpio)
outreg2 using "reg.doc", word append


reghdfe participacion_pp ataq_no_l_10k y_total, a(codmpio ano) vce(cluster codmpio)
outreg2 using "reg.doc", word append

reg participacion_pp ataq_l_10k ataq_no_l_10k y_total
outreg2 using "reg.doc", word append


save "particip_vio.dta", replace


asdoc sum votos participacion ataq_l_10k ataq_no_l_10k y_total, save(estadisticas.doc)




