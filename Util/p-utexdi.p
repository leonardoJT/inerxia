/* se encarga de bajar la información de configuracion de cuentas de diferidos.
 luego de correr este programa correr el delta marcado con 1806981.df */

MESSAGE "El proceso baja la información de " SKIP
        "Configuración de Ctas de Diferidos, Luego " SKIP
        "de este proceso aplicar el delta marcado con 19980630.df"
        VIEW-AS ALERT-BOX TITLE "Utilitario Cfg_CtasDifer".

OUTPUT TO CfgCtadi.d.
Transaccion:
  DO TRANSACTION:
     FOR EACH Cfg_CtasDifer NO-LOCK:
       EXPORT Cfg_CtasDifer.Grupo Cfg_CtasDifer.Dif_ContraAju Cfg_CtasDifer.Dif_CostoMin
              Cfg_CtasDifer.Dif_CtaAjuste Cfg_CtasDifer.Dif_CtaFuente 
              Cfg_CtasDifer.Dif_CtaGastoAj Cfg_CtasDifer.Dif_CtaGtoDif 
              Cfg_CtasDifer.Dif_IdAjuste Cfg_CtasDifer.Dif_PlaMax.
     END.
        
    OUTPUT CLOSE.
    FOR EACH Cfg_CtasDifer:
      DELETE Cfg_CtasDifer.
    END.
  END.

OUTPUT CLOSE.
