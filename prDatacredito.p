/*
    Exportar archivo a DataCredito
*/

{Incluido\VARIABLE.I "SHARED"}

    MESSAGE "Ojo si coje los cambios"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

DEFINE VARIABLE NumReg AS INTEGER     NO-UNDO.
DEFINE VARIABLE Prc AS INTEGER     NO-UNDO.
DEFINE VARIABLE TotNov AS INTEGER     NO-UNDO.
DEFINE VARIABLE Fec_Corte AS DATE        NO-UNDO.
DEFINE VARIABLE Z AS INTEGER     NO-UNDO.
DEFINE VAR C AS CHARACTER FORMAT "X(45)"  INITIAL "                               ".
DEFINE VAR i AS INTEGER.
DEFINE VAR NitAux LIKE Clientes.Nit.
DEFINE VAR VlrTmp AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VARIABLE Ced LIKE Creditos.Nit NO-UNDO.
/*1-REGISTRO CONTROL*/
DEFINE VAR CRegCon AS CHARACTER FORMAT "X(33)".
/*2-REGISTRO DE INFORMACION*/
DEFINE VAR IRegInf AS CHARACTER FORMAT "X".
/*REGISTRO DE FIN*/
DEFINE VAR CRegFIN AS CHARACTER FORMAT "X(42)".
DEFINE VARIABLE viPerFac AS INTEGER     NO-UNDO.


DEFINE TEMP-TABLE TmpDat
 FIELD INumCta AS CHARACTER FORMAT "X(18)"
 FIELD INumIde AS CHARACTER FORMAT "X(11)"
 FIELD INomCli AS CHARACTER FORMAT "X(45)"
 FIELD IFecNac AS CHARACTER FORMAT "X(6)"
 FIELD IFecApe AS DECIMAL   FORMAT "999999"
 FIELD IFecVen AS DECIMAL   FORMAT "999999"
 FIELD ICuoMes AS DECIMAL   FORMAT "9999999999"
 FIELD INoveda AS DECIMAL   FORMAT "99"
 FIELD IAdjeti AS DECIMAL   FORMAT "99"
 FIELD ITipIde AS DECIMAL   FORMAT "9"
 FIELD IValIni AS DECIMAL   FORMAT "9999999999"
 FIELD ISdoCre AS DECIMAL   FORMAT "9999999999"
 FIELD ISdoMor AS DECIMAL   FORMAT "9999999999"
 FIELD IValDis AS DECIMAL   FORMAT "9999999999"
 FIELD ITipMon AS DECIMAL   FORMAT "9"
 FIELD ITipCre AS DECIMAL   FORMAT "9"
 FIELD ITipGar AS DECIMAL   FORMAT "9"
 FIELD ICalifi AS CHARACTER FORMAT "X"
 FIELD ICiuRes AS CHARACTER FORMAT "X(15)"
 FIELD IDirRes AS CHARACTER FORMAT "X(30)"
 FIELD ITelRes AS CHARACTER FORMAT "X(10)"
 FIELD ICiuLab AS CHARACTER FORMAT "X(15)"
 FIELD ITelLab AS character FORMAT "X(10)"
 FIELD ICiuCor AS CHARACTER FORMAT "X(15)"
 FIELD IDirCor AS CHARACTER FORMAT "X(30)"
 FIELD ICiiu   AS DECIMAL   FORMAT "999999"
 FIELD ITotCuo AS DECIMAL   FORMAT "999"
 FIELD ICuoCan AS DECIMAL   FORMAT "999"
 FIELD ICuoMor AS DECIMAL   FORMAT "999"
 FIELD IFecPag AS DECIMAL   FORMAT "99999999"
 FIELD IOfiRad AS CHARACTER FORMAT "X(15)"
 FIELD ICiuRad AS CHARACTER FORMAT "X(15)"
 FIELD IForPag AS INTEGER   FORMAT "9"
 FIELD IPerPag AS INTEGER   FORMAT "9" INITIAL 1
 FIELD IEdaMor AS INTEGER   FORMAT "999"
 FIELD IFecAct AS DECIMAL   FORMAT "99999999"
 FIELD IReclam AS DECIMAL   FORMAT "9"
 FIELD IRespon AS DECIMAL   FORMAT "99"
 FIELD IEstrat AS INTEGER   FORMAT "9" INITIAL 3
 FIELD IFil    AS CHARACTER FORMAT "X(14)" INITIAL "00000000000000"
 FIELD Registro AS DECIMAL FORMAT "9999999"
    INDEX TmpDat INumIde INumCta.

DEFINE TEMP-TABLE TmpCod LIKE TmpDat.


ASSIGN Fec_Corte = DATE(04,30,2008).
                                                                    
/******************************************************************/
DEFINE VAR FecAuxi AS DATE.
DEFINE VAR XMul    AS INTEGER   FORMAT "99".
DEFINE VAR XDiaAtr AS INTEGER   FORMAT "9999".
DEFINE VAR WCli_nit LIKE Clientes.Cod_Anterior .
DEFINE VAR TmpCuota LIKE Creditos.Cuota.
DEFINE VAR Id_Guion AS LOGICAL INITIAL NO.
DEFINE VAR W_TSdoVdo  LIKE creditos.sdo_capital.
DEFINE VAR W_SdoDeuda LIKE creditos.sdo_capital.
DEFINE VAR Espacios AS CHARACTER FORMAT "X(15)" INITIAL "               ".

DEFINE VARIABLE viNumReg AS INTEGER  NO-UNDO. /*Cuenta registros a reportar a central de riesgo*/
DEFINE VARIABLE viSumNov AS INTEGER  NO-UNDO. /*Suma campo novedades, para control de Datacredito*/
DEFINE VARIABLE viCntErr AS INTEGER  NO-UNDO. /*Cuenta registros con error*/
                  
ASSIGN NumReg = 0
       Prc    = 0
       TotNov = 0.

/* DEBUGGER:INITIATE().  */
/* DEBUGGER:SET-BREAK(). */


FIND FIRST Entidad WHERE Entidad.Entidad EQ 01 NO-LOCK NO-ERROR.
CRegCon = "000000000000000000" + STRING(Entidad.Cod_Datacredito,"999999") + "27" + STRING(YEAR(Fec_Corte)) + STRING(MONTH(fec_corte),"99") + "M".
CRegCon = CRegCon + 
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "00000000000000000000000000000000000000000000000000" +
          "000000000000000000000000000".
OUTPUT TO VALUE(W_Pathspl + "\" + "DATACREDITO" + STRING(MONTH(Fec_Corte),"99") +
                 STRING(YEAR(Fec_Corte),"9999")  + ".TXT") NO-CONVERT.
PUT UNFORMATTED 
    CRegCon
    SKIP.

/* IF TmpDat.IEstrat EQ 0 THEN                               */
/*    TmpDat.IEstrat = 3. /* La generalidad en la empresa */ */

ASSIGN viNUmReg = 0
    viSumNov = 0.

FOR EACH Creditos WHERE /* creditos.agencia EQ 2 AND */
    ( Creditos.Fec_Desembolso LE Fec_Corte AND Creditos.Num_Credito NE 0 AND
                          /* Creditos.Sdo_Capital EQ 0 AND   Para Marzo y Abril */
                          Creditos.Estado EQ 2 OR 
                          (YEAR(Creditos.Fec_CanceTotal) EQ YEAR(Fec_Corte)    AND
                           MONTH(Creditos.Fec_CanceTotal) EQ MONTH(Fec_Corte) ) ) OR 
                           Creditos.Estado  EQ 5 /* Castigados a partir del 17 enero 2006 */
                   NO-LOCK BREAK BY Creditos.Nit BY Creditos.Num_Credito:
    IF  YEAR(Creditos.Fec_CanceTotal) EQ  YEAR(Fec_desembolso) AND
       MONTH(Creditos.Fec_CanceTotal) EQ MONTH(Fec_desembolso) AND 
        Creditos.sdo_capital EQ 0     THEN NEXT.

  /* Se reportan solo Mayores 
  FIND FIRST ahorros WHERE ahorros.nit EQ creditos.nit AND cod_ahorro = 5 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(ahorros) THEN NEXT.*/

  FIND FIRST Clientes WHERE Clientes.Nit = Creditos.Nit NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Clientes) THEN 
      NEXT.
  ELSE DO:
    /*IF Clientes.Tipo_Identificacion EQ "R.C" OR
       Clientes.Tipo_Identificacion EQ "T.I" OR
       Clientes.Tipo_Cliente EQ 2 THEN NEXT.*/
    CREATE TmpDat.
    NumReg = NumReg + 1.
    FIND FIRST Garantias WHERE
               Garantias.Cod_Credito EQ Creditos.Cod_Credito AND
               Garantias.Tip_Credito EQ Creditos.Tip_Credito AND
               Garantias.Num_Credito EQ Creditos.Num_Credito AND
               Garantias.Estado      EQ 1 NO-LOCK NO-ERROR.
    ASSIGN TmpDat.ITipGar = 2
           TmpDat.Registro = NumReg.
    IF AVAILABLE Garantias THEN TmpDat.ITipGar = 1.
    ASSIGN TmpDat.INomCli = trim(Clientes.Apellido1) + " " + trim(Clientes.Apellido2) + " " + trim(Clientes.Nombre)
           TmpDat.ITelLab = SUBSTR(TRIM(Clientes.Tel_Comercial),1,10) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN TmpDat.ITelLab = "".
    RUN Ciudades(INPUT Clientes.Lugar_Comercial, OUTPUT TmpDat.ICiuLab).
    RUN Tipo_Identificacion.
/*  END.
  FIND Clientes WHERE Clientes.Nit = Creditos.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN
   DO:*/

    RUN Ciudades(INPUT Clientes.Lugar_Residencia, OUTPUT TmpDat.ICiuRes).
    IF Clientes.Fec_Nacimiento EQ ? THEN
      TmpDat.IFecNac = "      ". 
    ELSE
      TmpDat.IFecNac = STRING(YEAR(Clientes.Fec_Nacimiento),"9999") + STRING(MONTH(Clientes.Fec_Nacimiento),"99").

    UPDATE TmpDat.IDirRes = SUBSTRING(Clientes.Dir_Residencia,1,30)
           TmpDat.IDirCor = SUBSTRING(Clientes.Dir_Residencia,1,30)
           TmpDat.ICiuCor = TmpDat.ICiuRes
           TmpDat.ITelRes = STRING(Clientes.Tel_Residencia)
           TmpDat.ICiiu   = DECIMAL(Clientes.Codigo_CIIU).
    UPDATE TmpDat.IEstrat = 3.

    IF Clientes.Codigo_CIIU EQ ? OR Clientes.Codigo_CIIU LE 0 THEN
       TmpDat.ICiiu = 0.

    IF  Clientes.Estrato GT 0 THEN
      TmpDat.IEstrat = Clientes.Estrato.
    ELSE
      TmpDat.IEstrat = 3. /* Promedio */
    IF LENGTH(TmpDat.ITelRes) LT 10 THEN
     ASSIGN Z = 10 - LENGTH(TmpDat.ITelRes)
            TmpDat.ITelRes = SUBSTRING(C,1,Z) + TmpDat.ITelRes
            TmpDat.ITelRes = REPLACE(TmpDat.ITelRes," ","0").
    ELSE
      ASSIGN TmpDat.ITelRes = SUBSTRING(TmpDat.ITelRes,1,10).

    IF LENGTH(TmpDat.IDirRes) LT 30 THEN
     ASSIGN Z = 30 - LENGTH(TmpDat.IDirRes)
            TmpDat.IDirRes = TmpDat.IDirRes + SUBSTRING(C,1,Z).
    ELSE
            TmpDat.IDirRes = SUBSTR(TmpDat.IDirRes,1,30).

    IF LENGTH(TmpDat.IDirCor) LT 30 THEN
     ASSIGN Z = 30 - LENGTH(TmpDat.IDirCor)
            TmpDat.IDirCor = TmpDat.IDirCor + SUBSTRING(C,1,Z).
    ELSE
            TmpDat.IDirCor = SUBSTR(TmpDat.IDirCor,1,30).

    /* 
        GIOCAM: Oct-04-07
        No se debe calcular la calificación. La tabla creditos.categoria ya lo tiene calificado
    */
    /*     RUN Calificacion. */ 


  END.
  /*Gcamacho*/
/*   ELSE DO:                                                      */
/*     ASSIGN TmpDat.ICiuRes = "               "                   */
/*            TmpDat.IFecNac = "      "                            */
/*            TmpDat.IDirRes = "               "                   */
/*            TmpDat.IDirCor = "               "                   */
/*            TmpDat.ICiuCor = "               "                   */
/*            TmpDat.ICiuRad = "               "                   */
/*            TmpDat.ITelRes = "0000000000"                        */
/*            TmpDat.ICiiu   = 0.                                  */
/*     IF LENGTH(TmpDat.IDirRes) LT 30 THEN                        */
/*      ASSIGN Z = 30 - LENGTH(TmpDat.IDirRes)                     */
/*             TmpDat.IDirRes = TmpDat.IDirRes + SUBSTRING(C,1,Z). */
/*     IF LENGTH(TmpDat.IDirCor) LT 30 THEN                        */
/*      ASSIGN Z = 30 - LENGTH(TmpDat.IDirCor)                     */
/*             TmpDat.IDirCor = TmpDat.IDirCor + SUBSTRING(C,1,Z). */
/*   END.                                                          */
   
   IF Creditos.per_pago = 4 THEN 
        TmpDat.IPerPag = 1.  /* Mensual */
   ELSE TmpDat.IPerPag = 9. /*  Otras  */
 
  /*calcula la fecha de vencimiento o busca en planpagos*/
    FIND LAST PlanPagos WHERE
             PlanPagos.Nit         EQ Creditos.Nit AND 
             PlanPagos.Num_Credito EQ Creditos.Num_Credito  AND
             planpagos.id_pdomes   LE 2                     AND
             PlanPagos.Nro_Cuota   EQ Creditos.Plazo NO-LOCK NO-ERROR.
    IF NOT AVAIL(planpagos) THEN
      FOR EACH planpagos WHERE  planpagos.nit         EQ creditos.nit         AND 
                                planpagos.num_credito EQ creditos.num_credito AND 
                                planpagos.id_pdomes   LE 2  NO-LOCK BY planpagos.nro_cuota:  
          ASSIGN IFecVen = DECIMAL(STRING(YEAR(PlanPagos.Fec_Vcto)) + STRING(MONTH(PlanPagos.Fec_Vcto),"99")).
      END.
    ELSE
      ASSIGN IFecVen = DECIMAL(STRING(YEAR(PlanPagos.Fec_Vcto)) + STRING(MONTH(PlanPagos.Fec_Vcto),"99")).

     /*
       FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
       IF AVAILABLE(Empresas) THEN
          RUN PeriodoEmpresa.
       ELSE 
          ASSIGN FecAuxi = Creditos.Fec_Desembolso + (Creditos.Plazo * 30)
                IFecVen = DECIMAL(STRING(YEAR(FecAuxi)) + STRING(MONTH(FecAuxi),"99")).  */
  /**/
   CASE Creditos.Agencia:
      WHEN  1 THEN TmpDat.ICiuRad = "Medellin       ".
      WHEN  2 THEN TmpDat.ICiuRad = "Medellin       ".
      WHEN  3 THEN TmpDat.ICiuRad = "Estrella       ".
      OTHERWISE 
            TmpDat.ICiuRad = "Medellin       ".
   END CASE.

   /*FIND Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
   IF AVAILABLE Agencias THEN
   DO:
      IF SUBSTRING(Agencias.ciudad,1,5) NE "05001" THEN
         FIND Ubicacion WHERE Ubicacion.Ubicacion = Ciudad NO-LOCK NO-ERROR.
         IF AVAILABLE(Ubicacion) THEN  TmpDat.ICiuRad = Ubicacion.Nombre.
         ELSE  TmpDat.ICiuRad = "Medellin       ".
         IF LENGTH(TmpDat.ICiuRad) LT 15 THEN DO:
         Z = 15 - LENGTH( TmpDat.ICiuRad).
         TmpDat.ICiuRad =  TmpDat.ICiuRad + SUBSTRING(espacios,1,Z).
      END.
      ELSE
          TmpDat.ICiuRad = "Medellin       ".
      /*RUN Ciudades(INPUT Agencias.Ciudad, OUTPUT TmpDat.ICiuRad).*/
   END.
   ELSE
      TmpDat.ICiuRad = 'Medellin       '.
   */

  /*RUN Calificacion. Coment.Feb.3/07 */

  WCli_Nit = Creditos.Nit.
  IF LENGTH(WCli_Nit) LT 7 THEN
     WCli_Nit = "00" + WCli_Nit.


  ASSIGN
        TmpDat.ICalifi = creditos.categoria /*GIOCAM*/
        NitAux = ""
         TmpDat.INumIde = "0000000000000".
    
  DO i = 1 TO 12 BY 1:
     IF SUBSTRING(Creditos.Nit,i,1) NE "-" AND NOT Id_Guion THEN
        NitAux = NitAux + SUBSTRING(Creditos.Nit,i,1).
     ELSE Id_Guion = YES.
  END.
  Id_Guion = NO.
  TmpDat.INumCta = "0" + SUBSTRING(STRING(creditos.agencia),1,1).
  TmpDat.INumIde = SUBSTRING(TmpDat.INumIde,1,11 - LENGTH(NitAux)) + NitAux.
/**/
    
  UPDATE 
   TmpDat.INumCta = STRING(Creditos.pagare,">>>>>>>>>>>>>99999")  
/*    TmpDat.INumCta = STRING(Creditos.pagare,"999999999999999999") */
   TmpDat.IFecApe = DECIMAL(STRING(YEAR(Creditos.Fec_Desembolso),"9999") + STRING(MONTH(Creditos.Fec_Desembolso),"99"))
   TmpDat.INoveda = 01
   TmpDat.IAdjeti = 00
   TmpDat.IValIni = Creditos.Monto
   TmpDat.ISdoCre = Creditos.Sdo_Capital
   TmpDat.IValDis = 0
   TmpDat.ITipMon = 1
   TmpDat.ITipCre = 2 /* se debe revisar cuando pase a cooperativa financiera*/
   TmpDat.ITotCuo = Creditos.Plazo
   TmpDat.ICuoCan = Creditos.Cuo_Pagadas
   /*TmpDat.ISdoMor = 0*/
   TmpDat.IReclam = 0.
   /*TmpDat.ICuoMor = Creditos.Cuo_Atraso.*/

  IF (MONTH(Creditos.Fec_CanceTotal) EQ MONTH(Fec_Corte))  AND 
     (YEAR(Creditos.Fec_CanceTotal) EQ YEAR(Fec_Corte))    AND sdo_capital LE 0 THEN 
       ASSIGN TmpDat.INoveda = 05 
        TmpDat.IEdaMor = 1 
        ICalifi = "A".
         
  IF Creditos.Abogado THEN   DO:
     ASSIGN TmpDat.IAdjeti = 11.
     IF creditos.sdo_capital EQ 0 THEN 
        ASSIGN TmpDat.INoveda = 14 
         TmpDat.IEdaMor = 1 
         TmpDat.ICalifi = "A" 
         TmpDat.ISdoMor = 0.
  END.
 
  IF Creditos.FOR_pago = 2  AND sdo_capital = 0 THEN
        UPDATE TmpDat.IAdjeti = 00
               TmpDat.ICuoMor = 0
               TmpDat.Inoveda = 05
               TmpDat.IEdaMor = 1
               TmpDat.ICalifi = "A".
    /* ELSE
         ASSIGN TmpDat.IAdjeti = 00
                TmpDat.ICuoMor = 0
                TmpDat.Inoveda = 01
                TmpDat.IEdaMor = 1
                TmpDat.ICalifi = "A".*/

  IF Clientes.Fec_Fallecido NE ? THEN TmpDat.IAdjeti = 16.

  CASE Creditos.Per_Pago:
      WHEN 1 THEN UPDATE TmpDat.ICuoMes = Creditos.Cuota * 4.
      WHEN 2 THEN UPDATE TmpDat.ICuoMes = Creditos.Cuota * 3.
      WHEN 3 THEN UPDATE TmpDat.ICuoMes = Creditos.Cuota * 2.
      WHEN 4 THEN UPDATE TmpDat.ICuoMes = Creditos.Cuota.
      WHEN 5 THEN UPDATE TmpDat.ICuoMes = Creditos.Cuota / 2.
      WHEN 6 THEN UPDATE TmpDat.ICuoMes = Creditos.Cuota / 3.
      WHEN 7 THEN UPDATE TmpDat.ICuoMes = Creditos.Cuota / 4.
      WHEN 8 THEN UPDATE TmpDat.ICuoMes = Creditos.Cuota / 6.
      WHEN 9 THEN UPDATE TmpDat.ICuoMes = Creditos.Cuota / 12.
  END CASE.

  IF TmpDat.IFecApe = 0 THEN
     UPDATE  TmpDat.IFecApe = DECIMAL(STRING(YEAR(Fec_Corte)) + STRING(MONTH(Fec_Corte),"99")).
  RUN NomAgencia (INPUT Creditos.agencia, OUTPUT TmpDat.IOfiRad).
  
  IF creditos.FOR_pago = 2 THEN /* Libranza */
  DO:
     IF Creditos.Sdo_Capital EQ 0 THEN
       IF Creditos.reestructurado = 1 THEN
          UPDATE TmpDat.IForPag   = 4. /* Reestructurado */ /* 9 sept jjmp */
       ELSE
          UPDATE TmpDat.IForPag   = 1. /* Voluntario */
     ELSE
       UPDATE TmpDat.IForPag   = 0. /* Vigente */
  END.
  ELSE
    IF Creditos.Sdo_Capital EQ 0 THEN DO:
      IF  Creditos.Abogado THEN
          UPDATE TmpDat.IForPag = 2. /* Proceso Ejecutivo */
      ELSE
          IF Creditos.reestructurado = 1 THEN
              UPDATE TmpDat.IForPag   = 4. /* Reestructurado */ /* 9 sept jjmp */
          ELSE
              UPDATE TmpDat.IForPag = 1. /* Vigente */

   /*  IF creditos.reestructurado = 1 THEN  /* se inactiva por los anteriores 9 sept jjmp */
        TmpDat.IForPag = 4. /*  Reestructurado */ */

    END.
    ELSE
        UPDATE TmpDat.IForPag = 0.
  
  UPDATE TmpDat.IFecAct = DECIMAL(STRING(YEAR(Fec_Corte),"9999") + STRING(MONTH(Fec_Corte),"99") + STRING(DAY(Fec_Corte),"99")).

  IF Creditos.Fec_UltPago NE ? THEN
     UPDATE TmpDat.IFecPag = DECIMAL(STRING(YEAR(Creditos.Fec_UltPago),"9999") +   
                              STRING(MONTH(Creditos.Fec_UltPago),"99") +    
                              STRING(DAY(Creditos.Fec_UltPago),"99")).      
  ELSE
     UPDATE TmpDat.IFecPag = 0. 

  /* ASSIGN TmpDat.ISdoMor = Creditos.Val_Atraso*/
  UPDATE TmpDat.IRespon = 0.
  ASSIGN VlrTmp         = 0.


  ASSIGN W_TSdoVdo = Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
                     Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob
        W_SdoDeuda = Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
                     Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
                     Creditos.Sdo_Capital    + Creditos.INT_DifCobro   - Creditos.Int_Anticipado.

  /*IF creditos.FOR_pago NE 2 THEN /* Control para libranzas  For_pago = nomina */
  DO:
    IF Creditos.Capital_Acum GT Creditos.Sdo_CapPag THEN
       W_TSdoVdo = W_TSdoVdo + (Creditos.Capital_Acum - Creditos.Sdo_CapPag).

    IF Creditos.INT_LiqAcum GT Creditos.Sdo_IntPag THEN                               
       W_TSdoVdo = W_TSdoVdo + (Creditos.INT_LiqAcum - Creditos.Sdo_IntPag).          

    IF W_TSdoVdo LE 0 THEN                                                            
       W_TSdoVdo = 0. 

    IF W_TSdoVdo GT W_SdoDeuda THEN                                                   
       W_TSdoVdo = W_SdoDeuda.

    TmpDat.ISdoMor = W_TSdoVdo.
  END.
  ELSE
    TmpDat.ISdoMor = 0.   Coment.Feb.3/07*/

  IF (Creditos.Fec_CanceTotal NE ? AND Creditos.Sdo_Capital EQ 0 ) OR 
     (MONTH(fec_desembolso) = MONTH(fec_corte) AND YEAR(fec_desembolso) = YEAR(fec_corte) ) THEN 
      TmpDat.ISdoMor = 0.

  IF INoveda = 05 THEN  /* cancelados */
     ASSIGN IEdaMor = 1 
            ICalifi = "A" 
            TmpDat.ISdoMor = 0.

  IF Inoveda = 05 OR Inoveda = 01 OR Inoveda = 14 THEN /* 9 sept-2005 jjmp */
     TmpDat.ICuoMor = 000.

  IF Inoveda = 14 THEN ASSIGN TmpDat.IAdjeti = 00.   /* 9 sept-2005 jjmp */

 /* IF Creditos.FOR_pago NE 2 THEN /* Nuevo a partir del 13 de febrero de 2006 jjmp*/
     RUN Data_ValCalif.    Coment.Feb.3/07*/

  /* Nuevo control para castigados 17 de enero de 2006 */
  IF creditos.estado EQ 5 THEN DO:
     UPDATE  TmpDat.INoveda = 13
            TmpDat.IAdjeti = 11
            TmpDat.ICalifi = "E"
            TmpDat.IFecPag = 000000
            TmpDat.IForPag = 2 /*  Proceso ejecutivo */
            TmpDat.IEdaMor = 360.
     FIND LAST planpagos WHERE planpagos.Nit         EQ creditos.nit         AND 
                               planpagos.Num_Credito EQ creditos.num_credito AND
                               Id_PdoMes             EQ 2                    AND 
                               Nro_Cuota             GT 0 NO-LOCK NO-ERROR.
     IF AVAILABLE(planpagos) THEN
         UPDATE  TmpDat.ICuoCan = planpagos.cuo_pagas
                TmpDat.ICuoMor = Creditos.plazo - planpagos.cuo_pagas
                TmpDat.IsdoCre = Planpagos.Monto_actual - Planpagos.Pagos_CapitalAcum
                TmpDat.ISdoMor = Planpagos.Monto_actual - Planpagos.Pagos_CapitalAcum.
     ELSE
     DO:
         UPDATE TmpDat.ICuoCan = Creditos.plazo - creditos.cuo_atraso
                TmpDat.ICuoMor = Creditos.cuo_atraso.
         IF MONTH(Creditos.Fec_CanceTotal) NE 1 THEN
            UPDATE TmpDat.IsdoCre = creditos.sdo_anuales[MONTH(Creditos.Fec_CanceTotal) - 1]
                   TmpDat.ISdoMor = creditos.sdo_anuales[MONTH(Creditos.Fec_CanceTotal) - 1].
         ELSE
            UPDATE TmpDat.IsdoCre = creditos.sdo_anuales[12]
                   TmpDat.IsdoMor = creditos.sdo_anuales[12].
     END.
  END.
  /* Termina  control para castigados al 17 de enero de  2006*/

  /* GCamacho - May/20/08 - Validacion Cupos rotativos */
/*     FIND LAST per_Facturacion WHERE per_Facturacion.estado EQ 1 USE-INDEX idx_Per NO-LOCK NO-ERROR. */
/*     ASSIGN viPerFac = per_Facturacion.Per_Factura - 1.                                              */
    IF creditos.cod_credito EQ 570 OR creditos.cod_credito EQ 870 THEN DO:
        UPDATE TmpDat.INoveda = 01 
            TmpDat.IAdjeti = 00
            ICuoMes = IF ICuoMes = 0 THEN 1 ELSE ICuoMes.

/*         UPDATE TmpDat.INoveda = 01                                                                  */
/*             TmpDat.IAdjeti = 00.                                                                    */
/*         FIND FIRST facturacion WHERE Facturacion.Per_Factura EQ viPerFac NO-LOCK NO-ERROR.          */
/*         IF AVAILABLE facturacion THEN                                                               */
/*             UPDATE ICuoMes = facturacion.cuota.                                                     */
    END.
  /* Termina Validacion cupos rotativos */

  IF tmpDat.ICuoMor LE 0 THEN
     UPDATE TmpDat.IsdoMor = 0.

  IF Creditos.Cuo_Atraso EQ ? OR Creditos.Cuo_Atraso LE 0 THEN
     UPDATE  TmpDat.ICuoMor = 0
            TmpDat.IsdoMor = 0.
  
  IF TmpDat.ICuoMes LT 0 OR
     TmpDat.IValIni LT 0 OR
     TmpDat.ISdoCre LT 0 OR
     TmpDat.ISdoMor LT 0 OR
     TmpDat.IValDis LT 0 THEN DO:
     UPDATE
         TmpDat.ICuoMes = IF TmpDat.ICuoMes LT 0 THEN 0 ELSE TmpDat.ICuoMes
         TmpDat.IValIni = IF TmpDat.IValIni LT 0 THEN 0 ELSE TmpDat.IValIni
         TmpDat.ISdoCre = IF TmpDat.ISdoCre LT 0 THEN 0 ELSE TmpDat.ISdoCre
         TmpDat.ISdoMor = IF TmpDat.ISdoMor LT 0 THEN 0 ELSE TmpDat.ISdoMor
         TmpDat.IValDis = IF TmpDat.IValDis LT 0 THEN 0 ELSE TmpDat.IValDis.
  END.

  ASSIGN Prc = Prc + 1
         Ced = Creditos.Nit.
/*   DISPLAY Prc Ced WITH FRAME FRM0. */
  ASSIGN TotNov = TotNov + TmpDat.INoveda.
  RUN CodeudoresDtCr.

  /* Impresion */

  ASSIGN viNumReg = viNumReg + 1
          viSumNov = viSumNov + TmpDat.INoveda.    

  PUT UNFORMATTED
/*    creditos.pagare   FORMAT "X(18)" */
   TmpDat.INumCta    FORMAT "X(18)"
   TmpDat.INumIde    FORMAT "X(11)"
   TmpDat.INomCli    FORMAT "X(45)"
   TmpDat.IFecNac    FORMAT "999999"
   TmpDat.IFecApe    FORMAT "999999"
   TmpDat.IFecVen    FORMAT "999999"
   TmpDat.ICuoMes    FORMAT "9999999999"
   TmpDat.INoveda    FORMAT "99"
   TmpDat.IAdjeti    FORMAT "99"
   TmpDat.ITipIde    FORMAT "9"
   TmpDat.IValIni    FORMAT "9999999999"
   TmpDat.ISdoCre    FORMAT "9999999999"
   TmpDat.ISdoMor    FORMAT "9999999999"
   TmpDat.IValDis    FORMAT "9999999999"
   TmpDat.ITipMon    FORMAT "9"
   TmpDat.ITipCre    FORMAT "9"
   TmpDat.ITipGar    FORMAT "9"
   TmpDat.ICalifi    FORMAT "X"
   TmpDat.ICiuRes    FORMAT "X(15)"
   TmpDat.IDirRes    FORMAT "X(30)"
   TmpDat.ITelRes    FORMAT "X(10)"
   TmpDat.ICiuLab    FORMAT "X(15)"
   TmpDat.ITelLab    FORMAT "X(10)"
   TmpDat.ICiuCor    FORMAT "X(15)"
   TmpDat.IDirCor    FORMAT "X(30)"
   TmpDat.ICiiu      FORMAT "999999"
   TmpDat.ITotCuo    FORMAT "999"
   TmpDat.ICuoCan    FORMAT "999"
   TmpDat.ICuoMor    FORMAT "999"
   TmpDat.IFecPag    FORMAT "99999999"
   TmpDat.IOfiRad    FORMAT "X(15)"
   TmpDat.ICiuRad    FORMAT "X(15)"
   TmpDat.IForPag    FORMAT "9"
   TmpDat.IPerPag    FORMAT "9"
   TmpDat.IEdaMor    FORMAT "999"
   TmpDat.IFecAct    FORMAT "99999999"
   TmpDat.IReclam    FORMAT "9"
   TmpDat.IRespon    FORMAT "99"
   TmpDat.IEstrat    FORMAT "9"
   TmpDat.IFil       FORMAT "X(14)"
      SKIP.

END.

CRegFin = "ZZZZZZZZZZZZZZZZZZ" + STRING(YEAR(Fec_Corte)) + STRING(MONTH(Fec_Corte),"99") + 
          STRING(DAY(Fec_Corte),"99") + STRING(viNumReg + 2,"99999999") + STRING(viSumNov,"99999999").

PUT CRegFin SKIP(0).
OUTPUT CLOSE.

OUTPUT TO VALUE(W_Pathspl + "\" + "DATACREDITO" + STRING(MONTH(Fec_Corte),"99") +
                 STRING(YEAR(Fec_Corte),"9999")  + "error.TXT") NO-CONVERT.

ASSIGN viCntErr = 0. /*Contador de registros con error*/

FOR EACH TmpDat WHERE 
    TmpDat.ICuoMes LT 0 OR
    TmpDat.IValIni LT 0 OR
    TmpDat.ISdoCre LT 0 OR
    TmpDat.ISdoMor LT 0 OR
    TmpDat.IValDis LT 0 NO-LOCK:

    FORM 
        Tmpdat.INumCta  COLUMN-LABEL "Num. Cta."
        TmpDat.INumIde  COLUMN-LABEL "Nit"
        TmpDat.INomCli  COLUMN-LABEL "Cliente"
        TmpDat.IValIni  COLUMN-LABEL "Val. Ini."    FORMAT "-999999999"
        TmpDat.ISdoCre  COLUMN-LABEL "Saldo Cred."  FORMAT "-999999999"
        TmpDat.ISdoMor  COLUMN-LABEL "Saldo Mora"   FORMAT "-999999999"
        TmpDat.IValDis  COLUMN-LABEL "Val. Dispon"  FORMAT "-999999999"
      WITH FRAME errores DOWN COLUMN 1 WIDTH 150
      NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.

    DISPLAY 
        TmpDat.INumCta  
        TmpDat.INumIde
        TmpDat.INomCli
        TmpDat.IValIni
        TmpDat.ISdoCre
        TmpDat.ISdoMor
        TmpDat.IValDis
        WITH FRAME errores. 
    DOWN WITH FRAME errores.
END. /*FOR EACH TmpDat*/

OUTPUT CLOSE.

IF viCntErr NE 0 THEN DO:
    MESSAGE "SE HA GENERADO ARCHIVO DE REGISTROS CON ERROR." SKIP
    "Saldos Negativos."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.



/***************** Procedimientos ********************************/

PROCEDURE Ciudades:
    DEFINE INPUT   PARAMETER Ciudad LIKE Clientes.Lugar_Residencia.
     DEFINE OUTPUT  PARAMETER CiuBak AS CHARACTER FORMAT "X(15)".

     IF SUBSTRING(ciudad,1,5) NE "05001" THEN DO:
       FIND FIRST Ubicacion WHERE SUBSTRING(Ubicacion.Ubicacion,1,5) = SUBSTRING(ciudad,1,5) NO-LOCK NO-ERROR.
       IF AVAILABLE(Ubicacion) THEN CiuBak = Ubicacion.Nombre.
       ELSE CiuBak = "               ".
       IF LENGTH(CiuBak) LT 15 THEN DO:
         Z = 15 - LENGTH(CiuBak).
         Ciubak = CiuBak + SUBSTRING(C,1,Z).
       END.
     END.
     ELSE
       Ciubak = "Medellin       ".
END PROCEDURE.

PROCEDURE Tipo_Identificacion:
    TmpDat.ITipIde = 0.
    CASE Clientes.Tipo_Identificacion:
      WHEN "C.C" THEN TmpDat.ITipIde = 1.
      WHEN "C.E" THEN TmpDat.ITipIde = 4.
      WHEN "NIT" THEN TmpDat.ITipIde = 2.
      WHEN "R.C" THEN TmpDat.ITipIde = 9.
     END CASE.
     IF TmpDat.ITipIde EQ 0 THEN TmpDat.ITipIde = 1.
END PROCEDURE.


PROCEDURE nomAgencia:
    DEFINE INPUT  PARAMETER agencia LIKE agencias.agencia.
     DEFINE OUTPUT PARAMETER OfiBak  AS CHARACTER FORMAT "X(15)".
     FIND FIRST agencias WHERE agencias.agencia EQ agencia NO-LOCK NO-ERROR.
     IF AVAILABLE(agencias) THEN
      OfiBak = agencias.Nombre.
     ELSE
      OfiBak = "MEDELLIN".
END PROCEDURE.

PROCEDURE CodeudoresDTCR:
    DEFINE VAR NCodeudor AS INTEGER FORMAT "9".
    DEFINE VAR WCli_Nit LIKE Clientes.Nit.
    DEFINE VAR Id_Guion AS LOGICAL INITIAL NO.
    DEFINE VAR NitAux LIKE Clientes.Nit.
    FOR EACH Relaciones WHERE
             Relaciones.Cod_relacion   EQ 11 AND
             Relaciones.Clase_Producto EQ 2 AND
             Relaciones.Cod_Producto   EQ Creditos.Cod_Credito AND
             Relaciones.Cuenta         EQ STRING(Creditos.Num_Credito) AND
             Relaciones.Nit            EQ Creditos.Nit /* AND
             Relaciones.Estado         EQ 1 */ NO-LOCK:
        FIND FIRST Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN DO:
           IF Clientes.Tipo_Identificacion EQ "R.C" OR
              Clientes.Tipo_Identificacion EQ "T.I" OR
              Clientes.Tipo_Cliente EQ 2 THEN NEXT.
           CREATE TmpCod.
           BUFFER-COPY TmpDat TO TmpCod.
           NCodeudor = NCodeudor + 1.
           ASSIGN TmpCod.INomCli = trim(Clientes.Apellido1) + " " + trim(Clientes.Apellido2) + " " + trim(Clientes.Nombre)
                  TmpCod.ITelLab = SUBSTR(TRIM(Clientes.Tel_Comercial),1,10) NO-ERROR. 
               /*DECIMAL(Clientes.Tel_Comercial) NO-ERROR.*/
           IF ERROR-STATUS:ERROR THEN TmpCod.ITelLab = "".
           RUN Ciudades(INPUT Clientes.Lugar_Comercial, OUTPUT TmpCod.ICiuLab).
           CASE Clientes.Tipo_Identificacion:
             WHEN "C.C" THEN TmpCod.ITipIde = 1.
             WHEN "C.E" THEN TmpCod.ITipIde = 4.
             WHEN "NIT" THEN TmpCod.ITipIde = 2.
             WHEN "R.C" THEN TmpCod.ITipIde = 9.
           END CASE.
           IF TmpCod.ITipIde EQ 0 THEN TmpCod.ITipIde = 1.
           /* RUN Tipo_Identificacion. */
           RUN Ciudades(INPUT Clientes.Lugar_Residencia, OUTPUT TmpCod.ICiuRes).
           ASSIGN TmpCod.IFecNac = STRING(YEAR(Clientes.Fec_Nacimiento),"9999") + STRING(MONTH(Clientes.Fec_Nacimiento),"99")
                  TmpCod.IDirRes = SUBSTRING(Clientes.Dir_Residencia,1,30)
                  TmpCod.IDirCor = SUBSTRING(Clientes.Dir_Residencia,1,30)
                  TmpCod.ICiuCor = TmpCod.ICiuRes
                  TmpCod.ITelRes = STRING(Clientes.Tel_Residencia)
                  TmpCod.ICiiu   = DECIMAL(Clientes.Codigo_CIIU)
                  TmpCod.IEstrat = Clientes.Estrato.
           IF TmpDat.IEstrat EQ 0 THEN
              TmpDat.IEstrat = 3. /* La generalidad en la empresa */

           IF LENGTH(TmpCod.ITelRes) LT 10 THEN
              ASSIGN Z = 10 - LENGTH(TmpCod.ITelRes)
                     TmpCod.ITelRes = SUBSTRING(C,1,Z) + TmpCod.ITelRes
                     TmpCod.ITelRes = REPLACE(TmpCod.ITelRes," ","0").
           ELSE
                     TmpCod.ITelRes = SUBSTR(TmpCod.ITelRes,1,10).

           IF LENGTH(TmpCod.IDirRes) LT 30 THEN
              ASSIGN Z = 30 - LENGTH(TmpCod.IDirRes)
                     TmpCod.IDirRes = TmpCod.IDirRes + SUBSTRING(C,1,Z).

           IF LENGTH(TmpCod.IDirCor) LT 30 THEN
              ASSIGN Z = 30 - LENGTH(TmpCod.IDirCor)
                     TmpCod.IDirCor = TmpCod.IDirCor + SUBSTRING(C,1,Z).

           IF TmpCod.IFecNac = ? THEN
               TmpCod.IFecNac = "      ". 
           /*   TmpCod.IFecNac = STRING(YEAR(Fec_Corte),"9999") + STRING(MONTH(Fec_Corte),"99").*/


           RUN NomAgencia (INPUT Creditos.agencia, OUTPUT TmpDat.ICiuRad).

/*             CASE Creditos.Agencia:                             */
/*               WHEN  1 THEN TmpDat.ICiuRad = "Medellin       ". */
/*               WHEN  2 THEN TmpDat.ICiuRad = "Medellin       ". */
/*               WHEN  3 THEN TmpDat.ICiuRad = "Sabaneta       ". */
/*               WHEN  4 THEN TmpDat.ICiuRad = "Caldas         ". */
/*               WHEN  5 THEN TmpDat.ICiuRad = "Envigado       ". */
/*               WHEN  6 THEN TmpDat.ICiuRad = "Itagui         ". */
/*               WHEN  7 THEN TmpDat.ICiuRad = "Medellin       ". */
/*               WHEN  8 THEN TmpDat.ICiuRad = "Rionegro       ". */
/*               WHEN  9 THEN TmpDat.ICiuRad = "Medellin       ". */
/*               WHEN 10 THEN TmpDat.ICiuRad = "Medellin       ". */
/*             END CASE.                                          */

           FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.

           ASSIGN  WCli_Nit = Creditos.Nit
                   TmpCod.INumIde = "00000000000"
                   NitAux = "".

           DO i = 1 TO 12 BY 1:
              IF SUBSTRING(Creditos.Nit,i,1) NE "-" AND NOT Id_Guion THEN
                 NitAux = NitAux + SUBSTRING(Creditos.Nit,i,1).
              ELSE Id_Guion = YES.
           END.

           TmpDat.INumCta = "0" + SUBSTRING(STRING(creditos.agencia),1,1).
           TmpDat.INumCta = STRING(Creditos.pagare,"999999") + TmpDat.INumCta + "00000000000". 

           ASSIGN TmpCod.INumCta = STRING(Creditos.pagare,"999999") + "0" + SUBSTRING(STRING(creditos.agencia),1,1)  + "C" + STRING(NCodeudor)
                  Id_Guion = NO  
                  NitAux = "".
           TmpCod.INumCta = TmpCod.INumCta + "00000000000000". 

           DO i = 1 TO 12 BY 1:
              IF SUBSTRING(Relaciones.Nit_Relacion,i,1) NE "-" AND NOT Id_Guion THEN
                 NitAux = NitAux + SUBSTRING(Relaciones.Nit_Relacion,i,1).
              ELSE
                 Id_Guion = YES.
           END.
           ASSIGN Id_Guion = NO
                  TmpCod.INumIde = SUBSTRING(TmpCod.INumIde,1,11 - LENGTH(NitAux)) + NitAux.


           UPDATE TmpDat.INoveda = 01.
           IF (MONTH(Creditos.Fec_CanceTotal) EQ MONTH(Fec_Corte))  AND 
              (YEAR(Creditos.Fec_CanceTotal) EQ YEAR(Fec_Corte))    AND sdo_capital LE 0 THEN 
                UPDATE TmpDat.INoveda = 05 
                        TmpDat.IEdaMor = 1 
                        TmpDat.ICalifi = "A".

           IF Creditos.Abogado THEN   DO:
              ASSIGN TmpDat.IAdjeti = 11.
              IF creditos.sdo_capital EQ 0 THEN 
                 ASSIGN TmpDat.INoveda = 14 TmpDat.IEdaMor = 1 TmpDat.ICalifi = "A" TmpDat.ISdoMor = 0.
           END.

           IF Creditos.FOR_pago = 2       THEN 
              IF sdo_capital = 0 THEN
                 ASSIGN TmpDat.IAdjeti = 00
                        TmpDat.ICuoMor = 0
                        TmpDat.Inoveda = 05
                        TmpDat.IEdaMor = 1
                        TmpDat.ICalifi = "A".
              ELSE
                  ASSIGN TmpDat.IAdjeti = 00
                         TmpDat.ICuoMor = 0
                         TmpDat.Inoveda = 01
                         TmpDat.IEdaMor = 1
                         TmpDat.ICalifi = "A".

           IF Clientes.Fec_Fallecido NE ? THEN TmpDat.IAdjeti = 16.

           IF creditos.FOR_pago = 2 THEN /* Libranza */
           DO:
              IF Creditos.Sdo_Capital EQ 0 THEN
                IF Creditos.reestructurado = 1 THEN
                   TmpDat.IForPag   = 4. /* Reestructurado */ /* 9 sept jjmp */
                ELSE
                   TmpDat.IForPag   = 1. /* Voluntario */
              ELSE
                TmpDat.IForPag   = 0. /* Vigente */
           END.
           ELSE
             IF Creditos.Sdo_Capital EQ 0 THEN DO:
               IF  Creditos.Abogado THEN
                 TmpDat.IForPag = 2. /* Proceso Ejecutivo */
               ELSE
                   IF Creditos.reestructurado = 1 THEN
                      TmpDat.IForPag   = 4. /* Reestructurado */ /* 9 sept jjmp */
                   ELSE
                      TmpDat.IForPag = 1. /* Vigente */
             END.

           IF TmpDat.Inoveda = 05 OR TmpDat.Inoveda = 01 OR TmpDat.Inoveda = 14 THEN /* 9 sept-2005 jjmp */
              TmpDat.ICuoMor = 000.

           IF TmpDat.Inoveda = 14 THEN ASSIGN TmpDat.IAdjeti = 00.   /* 9 sept-2005 jjmp */

            IF TmpCod.IFecApe = 0 THEN
               TmpCod.IFecApe = DECIMAL(STRING(YEAR(Fec_Corte)) + STRING(MONTH(Fec_Corte),"99")).
            ASSIGN TmpCod.IRespon = 01.


            /* Nuevo control para castigados 17 de enero de 2006 */
            IF creditos.estado EQ 5 THEN DO:
               ASSIGN TmpDat.INoveda = 13
                      TmpDat.IAdjeti = 11
                      TmpDat.ICalifi = "E"
                      TmpDat.IFecPag = 000000
                      TmpDat.IForPag = 2 /*  Proceso ejecutivo */
                      TmpDat.IEdaMor = 360.
               FIND LAST planpagos WHERE planpagos.Nit         EQ creditos.nit         AND 
                                         planpagos.Num_Credito EQ creditos.num_credito AND
                                         Id_PdoMes             EQ 2                    AND 
                                         Nro_Cuota             GT 0 NO-LOCK NO-ERROR.
               IF AVAILABLE(planpagos) THEN
                   ASSIGN TmpDat.ICuoCan = planpagos.cuo_pagas
                          TmpDat.ICuoMor = Creditos.plazo - planpagos.cuo_pagas
                          TmpDat.IsdoCre = Planpagos.Monto_actual - Planpagos.Pagos_CapitalAcum
                          TmpDat.ISdoMor = Planpagos.Monto_actual - Planpagos.Pagos_CapitalAcum.
               ELSE
               DO:
                   ASSIGN TmpDat.ICuoCan = Creditos.plazo - creditos.cuo_atraso
                          TmpDat.ICuoMor = Creditos.cuo_atraso.
                   IF MONTH(Creditos.Fec_CanceTotal) NE 1 THEN
                      ASSIGN TmpDat.IsdoCre = creditos.sdo_anuales[MONTH(Creditos.Fec_CanceTotal) - 1]
                             TmpDat.ISdoMor = creditos.sdo_anuales[MONTH(Creditos.Fec_CanceTotal) - 1].
                   ELSE
                      ASSIGN TmpDat.IsdoCre = creditos.sdo_anuales[12]
                             TmpDat.IsdoMor = creditos.sdo_anuales[12].

               END.
            END.
            ASSIGN Prc = Prc + 1
                   Ced = Creditos.Nit.
/*             DISPLAY Prc Ced WITH FRAME FRM0. */
            ASSIGN TotNov = TotNov + TmpCod.INoveda.
            /* Termina  control para castigados al 17 de enero de  2006*/
        END.
    END.
    FOR EACH TmpCod:
        NumReg = NumReg + 1.
        CREATE TmpDat.
        BUFFER-COPY TmpCod TO TmpDat.
        ASSIGN TmpDat.Registro = NumReg.
    END.
    FOR EACH TmpCod: DELETE TmpCod. END.
END PROCEDURE.


