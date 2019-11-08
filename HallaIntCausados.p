  DEFINE INPUT  PARAMETER tnit LIKE creditos.nit.
  DEFINE INPUT  PARAMETER tnum_cre LIKE creditos.num_credito.
  DEFINE OUTPUT PARAMETER IntCausado AS DECIMAL     NO-UNDO.
  
  DEFINE VARIABLE dias_no_vencidos  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE Tint_CuotaActual AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE W_tasaDiaria      AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE WPer              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE W_Tasa2           LIKE creditos.tasa.
  DEFINE VARIABLE Periodo           AS INTEGER     NO-UNDO.
  DEFINE VARIABLE WDia              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE WMes              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE WnDias            AS INTEGER     NO-UNDO.
  DEFINE VARIABLE tFecha            AS DATE        NO-UNDO.
  
  FIND FIRST creditos WHERE creditos.nit EQ tnit AND creditos.num_credito EQ tnum_cre NO-LOCK NO-ERROR.

      /*** Halla el número de dias del mes ***/
    tfecha = ?.
    WDia = DAY(TODAY).
    WMes = MONTH(TODAY).
    /* Meses de 31 dias */
    IF  Wmes EQ 1   OR
        Wmes EQ 3   OR
        Wmes EQ 5   OR
        Wmes EQ 7   OR
        Wmes EQ 8   OR
        Wmes EQ 10  OR
        Wmes EQ 12 
        THEN DO:
        WnDias = 31.
    END.
    ELSE DO:
        IF Wmes EQ 2 THEN
            WnDias = 28.
        ELSE
            WnDias = 30.
    END.
    IF creditos.per_Pago EQ 3 THEN DO:
        WPer = 15.
        Periodo = 24.
        W_Tasa2 = creditos.tasa / 2400.
        W_tasaDiaria = W_Tasa2 / 15.

        IF day(creditos.fec_paganti) LE WnDias THEN
            tFecha = DATE(WMes , DAY(fec_paganti), YEAR(TODAY)).
        ELSE
            tFecha = DATE(WMes , WnDias, YEAR(TODAY)).
        IF tfecha - TODAY GT 15 THEN
            tfecha = tfecha - INTEGER(WnDias / 2).
        IF tfecha LT TODAY THEN tfecha = tfecha + INTEGER(WnDias / 2).
    END.
    ELSE IF creditos.per_Pago EQ 4 THEN DO:
        WPer = WnDias.
        W_Tasa2 = creditos.tasa / 1200.
        W_tasaDiaria = W_Tasa2 / 30.
        Periodo = 12.
        
        IF creditos.diapago NE 0 THEN DO:
            IF creditos.diapago LE WnDias THEN
                tfecha = DATE(WMes, creditos.diapago, YEAR(TODAY)).
            ELSE
                tfecha = DATE(WMes, WnDias, YEAR(TODAY)).
        END.
        ELSE DO:
            IF day(creditos.fec_paganti) LE WnDias THEN
                tfecha = DATE(WMes, day(creditos.fec_paganti), YEAR(TODAY)).
            ELSE
                tfecha = DATE(WMes, WnDias, YEAR(TODAY)).
        END.
        IF tfecha LT TODAY THEN tfecha = tfecha + INTEGER(WnDias).        
    END.

  /****************************************************/
  

    dias_no_vencidos = TODAY  - (tFecha - WPer).
    Tint_CuotaActual = sdo_proyectado * W_tasaDiaria * dias_no_vencidos.
    IF Tint_CuotaActual LT 0 THEN Tint_CuotaActual = 0.


    IF creditos.sistema EQ 2 OR (creditos.per_pago NE 3 AND creditos.per_pago NE 4) THEN DO:
      ASSIGN Tint_CuotaActual = 0.
    END.
    IF creditos.sdo_capital GT Sdo_Proyectado THEN DO:    /* Atrasado */
        IF Tint_CuotaActual GT creditos.Int_Corrientes THEN DO:
           /* antes asignaba 0 */
          ASSIGN Tint_CuotaActual = creditos.Int_Corrientes.
        END.
        IF tFecha - TODAY LE 3 THEN do:
/*             MESSAGE tFecha                         */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
          ASSIGN Tint_CuotaActual = 0.
        END.
    END.
    ELSE DO:  /* Al dia */
         ASSIGN Tint_CuotaActual = 0.
  /*       IF tFecha - TODAY LE 3 THEN do:                              */
  /*         ASSIGN Tint_CuotaActual = 0.                              */
  /*       END.                                                         */
  /*       ELSE DO:                                                     */
  /*           IF Tint_CuotaActual GT creditos.Int_Corrientes THEN DO: */
  /*             ASSIGN Tint_CuotaActual = 0.                          */
  /*           END.                                                     */
  /*       END.                                                         */
    END.

    ASSIGN IntCausado = Tint_CuotaActual.
/********************************************************************************************************************************/


