DISABLE TRIGGERS FOR LOAD OF anexos.

DEFINE VAR cont AS INTEGER.

FOR EACH anexos WHERE anexos.sdo_inicial = 0
                  AND anexos.db[1] = 0
                  AND anexos.db[2] = 0
                  AND anexos.db[3] = 0
                  AND anexos.db[4] = 0
                  AND anexos.db[5] = 0
                  AND anexos.db[6] = 0
                  AND anexos.db[7] = 0
                  AND anexos.db[8] = 0
                  AND anexos.db[9] = 0
                  AND anexos.db[10] = 0
                  AND anexos.db[11] = 0
                  AND anexos.db[12] = 0
                  AND anexos.cr[1] = 0
                  AND anexos.cr[2] = 0
                  AND anexos.cr[3] = 0
                  AND anexos.cr[4] = 0
                  AND anexos.cr[5] = 0
                  AND anexos.cr[6] = 0
                  AND anexos.cr[7] = 0
                  AND anexos.cr[8] = 0
                  AND anexos.cr[9] = 0
                  AND anexos.cr[10] = 0
                  AND anexos.cr[11] = 0
                  AND anexos.cr[12] = 0:
    DELETE anexos.

    cont = cont + 1.
END.

MESSAGE cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

cont = 0.

FOR EACH anexos13 WHERE anexos13.sdo_inicial = 0
                  AND anexos13.db[1] = 0
                  AND anexos13.db[2] = 0
                  AND anexos13.db[3] = 0
                  AND anexos13.db[4] = 0
                  AND anexos13.db[5] = 0
                  AND anexos13.db[6] = 0
                  AND anexos13.db[7] = 0
                  AND anexos13.db[8] = 0
                  AND anexos13.db[9] = 0
                  AND anexos13.db[10] = 0
                  AND anexos13.db[11] = 0
                  AND anexos13.db[12] = 0
                  AND anexos13.cr[1] = 0
                  AND anexos13.cr[2] = 0
                  AND anexos13.cr[3] = 0
                  AND anexos13.cr[4] = 0
                  AND anexos13.cr[5] = 0
                  AND anexos13.cr[6] = 0
                  AND anexos13.cr[7] = 0
                  AND anexos13.cr[8] = 0
                  AND anexos13.cr[9] = 0
                  AND anexos13.cr[10] = 0
                  AND anexos13.cr[11] = 0
                  AND anexos13.cr[12] = 0:
    DELETE anexos13.

    cont = cont + 1.
END.

MESSAGE cont
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
