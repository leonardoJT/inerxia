DEFINE VAR vSaldo AS decimal.
DEFINE VAR cont AS INTEGER.

FOR EACH anexos13 WHERE anexos13.nit = "17033552"
                    AND (SUBSTRING(anexos13.cuenta,1,2) = "26" OR SUBSTRING(anexos13.cuenta,1,6) = "510551" OR SUBSTRING(anexos13.cuenta,1,8) = "51100203" OR
                         SUBSTRING(anexos13.cuenta,1,8) = "51100204" OR SUBSTRING(anexos13.cuenta,1,8) = "51100205" OR SUBSTRING(anexos13.cuenta,1,6) = "511008" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511010" OR SUBSTRING(anexos13.cuenta,1,6) = "511012" OR SUBSTRING(anexos13.cuenta,1,6) = "511016" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511018" OR SUBSTRING(anexos13.cuenta,1,6) = "511020" OR SUBSTRING(anexos13.cuenta,1,6) = "511022" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511024" OR SUBSTRING(anexos13.cuenta,1,6) = "511026" OR SUBSTRING(anexos13.cuenta,1,6) = "511028" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511030" OR SUBSTRING(anexos13.cuenta,1,6) = "511034" OR SUBSTRING(anexos13.cuenta,1,6) = "511036" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511038" OR SUBSTRING(anexos13.cuenta,1,6) = "511040" OR SUBSTRING(anexos13.cuenta,1,6) = "511042" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511044" OR SUBSTRING(anexos13.cuenta,1,6) = "511046" OR SUBSTRING(anexos13.cuenta,1,6) = "511048" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511050" OR SUBSTRING(anexos13.cuenta,1,8) = "51105201" OR SUBSTRING(anexos13.cuenta,1,8) = "51105202" OR
                         SUBSTRING(anexos13.cuenta,1,8) = "51105203" OR SUBSTRING(anexos13.cuenta,1,8) = "51105204" OR SUBSTRING(anexos13.cuenta,1,6) = "511060" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511062" OR SUBSTRING(anexos13.cuenta,1,6) = "511064" OR SUBSTRING(anexos13.cuenta,1,6) = "511095" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511515" OR SUBSTRING(anexos13.cuenta,1,6) = "511524" OR SUBSTRING(anexos13.cuenta,1,6) = "511530" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "514005" OR SUBSTRING(anexos13.cuenta,1,6) = "514015" OR SUBSTRING(anexos13.cuenta,1,6) = "514095" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "531520" OR SUBSTRING(anexos13.cuenta,1,6) = "539520" OR SUBSTRING(anexos13.cuenta,1,10) = "6140101108" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101110" OR SUBSTRING(anexos13.cuenta,1,10) = "6140101114" OR SUBSTRING(anexos13.cuenta,1,10) = "6140101116" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101118" OR SUBSTRING(anexos13.cuenta,1,10) = "6140101122"   OR SUBSTRING(anexos13.cuenta,1,10) = "6140101136"   OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101140" OR SUBSTRING(anexos13.cuenta,1,10) = "6140101195"   OR SUBSTRING(anexos13.cuenta,1,10) = "6140101251"   OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102108" OR SUBSTRING(anexos13.cuenta,1,10) = "6140102110"   OR SUBSTRING(anexos13.cuenta,1,10) = "6140102114"   OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102116" OR SUBSTRING(anexos13.cuenta,1,10) = "6140102118"   OR SUBSTRING(anexos13.cuenta,1,10) = "6140102122"   OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102136" OR SUBSTRING(anexos13.cuenta,1,10) = "6140102154"   OR SUBSTRING(anexos13.cuenta,1,10) = "6140102195"   OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102251" OR SUBSTRING(anexos13.cuenta,1,10) = "6170401108"   OR SUBSTRING(anexos13.cuenta,1,10) = "6170401110"   OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401116" OR SUBSTRING(anexos13.cuenta,1,10) = "6170401118"   OR SUBSTRING(anexos13.cuenta,1,10) = "6170401122"   OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401136" OR SUBSTRING(anexos13.cuenta,1,10) = "6170401195"   OR SUBSTRING(anexos13.cuenta,1,10) = "6170401251"   OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401260" OR SUBSTRING(anexos13.cuenta,1,8) = "61759501"      OR SUBSTRING(anexos13.cuenta,1,8) = "61759505"      OR
                         SUBSTRING(anexos13.cuenta,1,4) = "5125")
                    AND SUBSTRING(anexos13.cuenta,1,8) <> "51105211"
                    AND SUBSTRING(anexos13.cuenta,1,8) <> "51105212"
                    AND SUBSTRING(anexos13.cuenta,1,8) <> "51105213"
                    AND SUBSTRING(anexos13.cuenta,1,8) <> "51105214"
                    AND SUBSTRING(anexos13.cuenta,1,8) <> "53152001"
                    AND SUBSTRING(anexos13.cuenta,1,8) <> "53152002"
                    AND anexos13.ano = 2014 NO-LOCK BREAK BY anexos13.cuenta:
    IF FIRST-OF(anexos13.cuenta) THEN DO:
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos13.cuenta NO-LOCK NO-ERROR.
        vSaldo = 0.
    END.

    vSaldo = vSaldo + anexos13.sdo_inicial.

    MESSAGE sdo_inicial
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    DO cont = 1 TO 12:
        IF cuentas.naturaleza = "DB" THEN
            vSaldo = vSaldo + anexos13.db[cont] - anexos13.cr[cont].
        ELSE
            vSaldo = vSaldo + anexos13.cr[cont] - anexos13.db[cont].
    END.

    IF LAST-OF(anexos13.cuenta) THEN
        MESSAGE anexos13.cuenta vSaldo
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
