DEFINE VAR vNit AS CHARACTER INITIAL "8248316".
DEFINE VAR vCueAhorros AS CHARACTER INITIAL "8248316".
DEFINE VAR vValor AS DECIMAL INITIAL 180542.
    
FOR EACH mov_ahorros WHERE mov_ahorros.nit = vNit
                       AND mov_ahorros.cod_ahorro = 3
                       AND mov_ahorros.cue_ahorros = vCueAhorros
                       AND mov_ahorros.fecha >= 05/03/2018:
    mov_ahorros.sdo_disponible = mov_ahorros.sdo_disponible - vValor.
END.

FOR EACH rep_ahorros WHERE rep_ahorros.fecCorte >= 05/03/2018
                       AND rep_ahorros.nit = vNit
                       AND rep_ahorros.cod_ahorro = 3
                       AND rep_ahorros.cue_ahorros = vCueAhorros:
    rep_ahorros.sdo_disponible = rep_ahorros.sdo_disponible - vValor.
END.


FOR EACH ahorros WHERE ahorros.nit = vNit
                   AND ahorros.cod_ahorro = 3
                   AND ahorros.cue_ahorros = vCueAhorros:
    ahorros.sdo_disponible = ahorros.sdo_disponible - vValor.
END.


MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
