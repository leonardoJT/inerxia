DEFINE VAR vNit AS CHARACTER           INITIAL "51967904".
DEFINE VAR vCueAhorros AS CHARACTER    INITIAL "10000442".
DEFINE VAR vNewCueAhorros AS CHARACTER INITIAL "99000442".

FOR EACH ahorros WHERE ahorros.nit = vNit
                   AND ahorros.cue_ahorros = vCueAhorros:
    ahorro.cue_ahorros = vNewCueAhorros.
END.

FOR EACH rep_ahorros WHERE rep_ahorros.nit = vNit
                       AND rep_ahorros.cue_ahorros = vCueAhorros:
    rep_ahorro.cue_ahorros = vNewCueAhorros.
END.

FOR EACH mov_ahorros WHERE mov_ahorros.nit = vNit
                       AND mov_ahorros.cue_ahorros = vCueAhorros:
    mov_ahorro.cue_ahorros = vNewCueAhorros.
END.

MESSAGE "Fin" vNit
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
