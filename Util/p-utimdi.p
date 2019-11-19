INPUT FROM CfgCtadi.d.
transaccion:
DO TRANSACTION:
    REPEAT:
      CREATE Cfg_CtasDifer.
      IMPORT Cfg_CtasDifer.Clase Cfg_CtasDifer.Dif_ContraAju Cfg_CtasDifer.Dif_CostoMin
             Cfg_CtasDifer.Dif_CtaAjuste Cfg_CtasDifer.Dif_CtaFuente 
             Cfg_CtasDifer.Dif_CtaGastoAj Cfg_CtasDifer.Dif_CtaGtoDif 
             Cfg_CtasDifer.Dif_IdAjuste Cfg_CtasDifer.Dif_PlaMax.
      FIND FIRST Varios WHERE Varios.Tipo = "DIF" 
                          AND Varios.Clase = Cfg_CtasDifer.Clase NO-LOCK NO-ERROR.
      IF AVAILABLE(Varios) THEN
        Cfg_CtasDifer.Grupo = Varios.Codigo.
      ELSE DO:
        MESSAGE "No existes Codigos asociados a la clase en Tipos. Verifique.."
                SKIP "La clase es : " cfg_CtasDifer.clase skip
                "Se cancela el Proceso" VIEW-AS ALERT-BOX 
                TITLE "Subir Datos de Ctas Diferidos".
        UNDO transaccion, RETURN.
      END.
    END.
END.
        
INPUT CLOSE.


transacc1:
DO TRANSACTION:
   FOR EACH Diferido:
      FIND FIRST Varios WHERE Varios.Tipo = "DIF" 
                          AND Varios.Clase = Diferido.Clase NO-LOCK NO-ERROR.
      IF AVAILABLE(Varios) THEN
        Diferido.Grupo = Varios.Codigo.
      ELSE DO:
        MESSAGE "No existes Codigos asociados a la clase en Tipos. Verifique.."
                SKIP "La clase es : " Diferido.clase skip
                "Se cancela el Proceso" VIEW-AS ALERT-BOX 
                TITLE "Actualizacion de Grupo en Diferidos".
        UNDO transacc1, RETURN.
      END.
   END.
END.

transacc2:
DO TRANSACTION:
   FOR EACH Cfg_Activo:
      FIND FIRST Varios WHERE Varios.Tipo = "ACT" 
                          AND Varios.Clase = Cfg_Activo.Clase NO-LOCK NO-ERROR.
      IF AVAILABLE(Varios) THEN
        Cfg_Activo.Grupo = Varios.Codigo.
      ELSE DO:
        MESSAGE "No existes Codigos asociados a la clase en Tipos. Verifique.."
                SKIP "La clase es : " cfg_activo.clase skip
                "Se cancela el Proceso" VIEW-AS ALERT-BOX 
                TITLE "Actualizacion de Grupo en Cfg_Activo".
        UNDO transacc2, RETURN.
      END.
   END.
END.
