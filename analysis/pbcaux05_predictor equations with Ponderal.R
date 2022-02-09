x_1 = " reg_asia + iaea_ia_age + I(iaea_ia_age^2) + 
                iaea_ia_lgt + 
                 # iaea_ia_hc + iaea_ia_ac +
                iaea_ia_ts + iaea_ia_ss + 
                ponderal"

x_2 = " reg_asia + iaea_ia_age + iaea_ia_age9 + iaea_ia_age18 + 
                iaea_ia_lgt + 
                 # iaea_ia_hc + iaea_ia_ac +
                iaea_ia_ts + iaea_ia_ss + 
                ponderal"

x_3 = " reg_asia + ns(iaea_ia_age,df=4) + iaea_ia_lgt +
 # iaea_ia_hc + iaea_ia_ac +
          iaea_ia_ts + iaea_ia_ss + 
          ponderal"

x_4 = " reg_asia + ns(iaea_ia_age,df=4) + ns(iaea_ia_lgt,df=4) + 
 # ns(iaea_ia_hc,df=4) + ns(iaea_ia_ac,df=4) +
                ns(iaea_ia_ts,df=4) + ns(iaea_ia_ss,df=4) + 
                ns(ponderal,df=4)"