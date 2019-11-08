DEFINE VAR wvlrRet    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrAboi   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrAboiM  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrabok   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrHon    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wVlrOtrosC AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTotOtrosC AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
/* 04-Mar-2008*/
DEFINE VAR wVlrOtrosCMan AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTotOtrosCMan AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
/************/
DEFINE VAR wzswDet    AS LOGICAL INITIAL FALSE.
DEFINE VAR wcupdis    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wsobcup    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wnomciu    AS CHARACTER FORMAT "X(60)"   INITIAL "".
DEFINE VAR wdircorres LIKE clientes.DIR_comercial   INITIAL "".
DEFINE VAR wcodciu    LIKE clientes.lugar_comercial INITIAL "".
DEFINE VAR wtel       LIKE clientes.Tel_comercial   INITIAL "".
DEFINE VAR wtotabok   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wintmor    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wpagtot    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wtotret    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wcuota     AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wpagomin   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wpagoSegC  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wdetaFact  AS CHARACTER FORMAT "X(45)".
DEFINE VAR wsdoant    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.    
DEFINE VAR wcant      AS INTEGER INITIAL 0.
DEFINE VAR wcantot    AS INTEGER INITIAL 0.
DEFINE VAR wcantage   AS INTEGER INITIAL 0.
DEFINE VAR wcantotage AS INTEGER INITIAL 0.
/* Fechas de Corte */
DEFINE VAR wfecIni    AS DATE.
DEFINE VAR wfecfin    AS DATE.
DEFINE VAR wfecplazo  AS DATE.
DEFINE VAR wTotAso     AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTotAsoExt  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTcuptot    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTcupDis    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTSobrecupo AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTsdo_ant   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTCargos    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTintCtes   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTIntMora   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTOtrosCar  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTPagos     AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTNvoSdo    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTSdoMora   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTPagoTotal AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTPagoMin   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0. 
DEFINE VAR wTPagoSegC  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0. 
DEFINE VAR wvlrCobSC   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0. 
DEFINE VAR wTCobroSC   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0. 
DEFINE VAR wnro_auditoria LIKE mov_contable.nro_auditoria.

DEFINE VAR wperiodo         AS INTEGER INITIAL 0.   
DEFINE VAR wperNuevo        AS INTEGER INITIAL 0.   
DEFINE VAR wfec_proxpago    AS DATE.                
DEFINE VAR wfec_inic        AS DATE.                
DEFINE VAR wfec_vcto        AS DATE.                
DEFINE VAR wfec_ANTproxpago AS DATE.
DEFINE VAR wfec_ANTinic     AS DATE.
DEFINE VAR wfec_ANTvcto     AS DATE.
DEFINE VAR wcontador        AS INTEGER INITIAL 0.
DEFINE VAR wsalsegcar       LIKE mov_contable.db        INITIAL 0  NO-UNDO.
DEFINE VAR wnatura          LIKE cuentas.naturaleza     INITIAL "" NO-UNDO.
DEFINE VAR Pagoinme         AS CHARACTER FORMAT "X(10)" INITIAL "" NO-UNDO.
DEFINE VAR wvlrAboSC        AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0. 

DEFINE TEMP-TABLE tmp_det_Extracto /* Detalle de Extracto */
    FIELD agencia      LIKE creditos.agencia
    FIELD Nit          LIKE clientes.nit
    FIELD num_credito  LIKE creditos.num_credito
    FIELD fec_trans    LIKE creditos.fec_pago
    FIELD descripcion  LIKE mov_creditos.descrip
    FIELD NumDocto     LIKE mov_contable.nro_auditoria
    FIELD CR           AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD DB           AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    INDEX idxagnit agencia nit.

DEFINE TEMP-TABLE TemMovCre
    FIELD agencia       LIKE mov_creditos.agencia
    FIELD nit           LIKE mov_creditos.nit
    FIELD num_credito   LIKE mov_creditos.num_credito
    FIELD cod_credito   LIKE mov_creditos.cod_credito
    FIELD cod_operacion LIKE mov_creditos.cod_operacion
    FIELD cpte          LIKE mov_creditos.cpte
    FIELD num_documento LIKE mov_creditos.num_documento
    FIELD fecha         LIKE mov_creditos.fecha
    FIELD descrip       LIKE mov_creditos.descrip
    FIELD detalle       AS CHARACTER FORMAT "X(2)"
    FIELD nro_auditoria LIKE mov_contable.nro_auditoria
    FIELD comentario    LIKE mov_contable.comentario
    FIELD val_efectivo  LIKE mov_creditos.val_efectivo
    FIELD val_cheque    LIKE mov_creditos.val_cheque
    INDEX idfedes fecha descrip.

DEFINE TEMP-TABLE TemCodigo
    FIELD cod_credito LIKE mov_creditos.cod_credito.

CREATE TemCodigo.
UPDATE TemCodigo.cod_credito = 570.
CREATE TemCodigo.
UPDATE TemCodigo.cod_credito = 870.


FIND FIRST per_facturacion WHERE estado = 1 NO-LOCK NO-ERROR.
IF AVAILABLE(per_facturacion) THEN   /* Facturacion Vigente */
    ASSIGN wfec_ANTproxpago  = per_facturacion.fec_limpago
           wfec_ANTinic      = per_facturacion.fec_inicial
           wfec_ANTvcto      = per_facturacion.fec_final
           wperiodo          = Per_Facturacion.Per_Factura.
   FIND FIRST per_facturacion WHERE per_factura = wperiodo + 1 NO-LOCK NO-ERROR.
   IF AVAILABLE(per_facturacion) THEN  /* Nueva Facturacion */ 
       ASSIGN wfec_proxpago  = per_facturacion.fec_limpago
              wfec_inic      = per_facturacion.fec_inicial
              wfec_vcto      = per_facturacion.fec_final
              wperNUEVO      = Per_Facturacion.Per_Factura.


MESSAGE "Desea Facturar con las siguientes Fechas" SKIP(1)
        "Periodo de Facturacion: " wperNUEVO SKIP
        "Fecha Inicial Corte   : " wfec_inic SKIP
        "Fecha Final de Corte  : " wfec_vcto SKIP 
        "Fecha Limite Plazo    : " wfec_proxpago VIEW-AS ALERT-BOX 
    QUESTION BUTTONS YES-NO UPDATE Ask AS LOGICAL.
IF NOT Ask THEN 
   RETURN.

/** Nuevo */
/* FIND FIRST cuentas WHERE                                                                          */
/*     cuentas.cuenta EQ "16609510" NO-LOCK NO-ERROR. /* Cuenta Cupo Rotativo - Seguro de Cartera */ */
/* IF AVAILABLE(cuentas) THEN                                                                        */
/*    IF cuentas.naturaleza = "DB" THEN                                                              */
/*       ASSIGN wnatura = "DB".                                                                      */
/*    ELSE                                                                                           */
/*       ASSIGN wnatura = "CR".                                                                      */
/***********/

FOR EACH TemCodigo,
    EACH  mov_creditos WHERE 
          mov_creditos.fecha       GE wfec_inic             AND
          mov_creditos.fecha       LE wfec_Vcto             AND
          mov_creditos.cod_credito EQ TemCodigo.cod_credito AND
          mov_creditos.num_credito = 244667 /*244211*/
 
          NO-LOCK:

    CREATE TemMovCre.
    UPDATE TemMovCre.agencia       = mov_creditos.agencia
           TemMovCre.nit           = mov_creditos.nit
           TemMovCre.num_credito   = mov_creditos.num_credito
           TemMovCre.cod_credito   = mov_creditos.cod_credito
           TemMovCre.cod_operacion = mov_creditos.cod_operacion
           TemMovCre.cpte          = mov_creditos.cpte
           TemMovCre.num_documento = mov_creditos.num_documento
           TemMovCre.fecha         = mov_creditos.fecha
           TemMovCre.descrip       = mov_creditos.descrip
           TemMovCre.nro_auditoria = mov_creditos.num_documento
           TemMovCre.val_efectivo  = mov_creditos.val_efectivo
           TemMovCre.val_cheque    = mov_creditos.val_cheque.

    FIND FIRST mov_contable WHERE
         mov_contable.agencia       EQ mov_creditos.agencia                AND
         mov_contable.comprobante   EQ mov_creditos.cpte                   AND
         mov_contable.num_documento EQ integer(mov_creditos.num_documento) AND
         mov_contable.nit           EQ mov_creditos.nit                    AND
         mov_contable.fec_contable  EQ mov_creditos.fecha                  AND
         (mov_contable.db + mov_contable.cr) EQ (mov_creditos.val_efectivo + mov_creditos.val_cheque)
        NO-LOCK NO-ERROR.
    IF AVAILABLE(mov_contable) AND mov_contable.nro_auditoria NE " " THEN DO:
       UPDATE TemMovCre.nro_auditoria = mov_contable.nro_auditoria
              TemMovCre.detalle       = "Si"
              TemMovCre.comentario    = mov_contable.comentario.
    END.
    ELSE
       UPDATE TemMovCre.detalle       = "No".
/*     DISPLAY TemMovCre.nit TemMovCre.num_credito TemMovCre.comentario TemMovCre.descrip. */
END.

/* FOR EACH TemMovCre NO-LOCK:                               */
/*    DISPLAY "Movi..." TemMovCre.nit TemMovCre.num_credito. */
/* END.                                                      */

FOR EACH creditos WHERE 
    (cod_credito EQ 570 OR cod_credito EQ 870) AND estado = 2 AND
    agencia EQ 20 AND creditos.num_credito = 244667 /*244211 /*244396*/  /* Credito = "244211" */*/
    NO-LOCK BREAK BY Creditos.agencia BY Creditos.nit:
   ASSIGN wvlrOtrosC = 0   wzswDet = FALSE  wcupdis     = 0   wsobcup    = 0
          wcupdis    = 0   wnomciu   = ""    wdircorres = ""
          wcodciu    = ""  wtel      = ""    wtotabok   = 0   wintmor    = 0
          wpagtot    = 0   wtotret   = 0     wcuota     = 0   wTotOtrosC = 0  wTotOtrosCMan = 0
          wpagomin   = 0   wpagoSegC = 0     wvlrCobSC  = 0.  
   wcantot = wcantot + 1.
   wcantage = wcantage + 1.
   ASSIGN wvlrCobSC = creditos.polizas.
   IF LAST-OF(creditos.agencia) THEN
    DO:
      ASSIGN wcantotage = wcantage wcantage = 0.
   END.
   ASSIGN wsalsegcar = 0.
   MESSAGE "Entre..." creditos.nit creditos.num_credito
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   /* Nuevo 8-Febrero-2008**/
   FOR EACH TemMovCre WHERE 
       TemMovCre.num_credito EQ creditos.num_credito AND
       TemMovCre.nit         EQ creditos.nit 
       NO-LOCK BREAK BY TemMovCre.Fecha BY TemMovCre.descrip:
   /**************/

/*    FOR EACH  mov_creditos WHERE                                                              */
/*              mov_creditos.fecha       GE wfec_inic            AND                            */
/*              mov_creditos.fecha       LE wfec_Vcto            AND                            */
/*             (mov_creditos.cod_credito EQ 570 OR mov_creditos.cod_credito EQ 870) AND /*Ojo*/ */
/* /*              mov_creditos.cod_credito EQ creditos.cod_credito AND */                      */
/*              mov_creditos.num_credito EQ creditos.num_credito AND                            */
/*              mov_creditos.nit         EQ creditos.nit     NO-LOCK                            */
/*              BREAK BY mov_creditos.Fecha BY mov_creditos.descrip :                           */
     ASSIGN wvlrRet = 0  wvlrAbok = 0  wvlrAboi = 0  wvlrAboim = 0  wvlrHon = 0 wvlrAboSC = 0.
     IF TemMovCre.cod_operacion = 030303001 THEN NEXT. /* Traslado entre agencias No tener en cuenta */
     /* Acumulo las comisiones por dia */
     IF FIRST-OF(TemMovCre.fecha) THEN /* Se da el total de las comisiones por día y asi no dar largos detalles */ 
        ASSIGN wvlrOtrosC = 0 wvlrOtrosCMan = 0.
      /* Ojo*/
     IF TemMovCre.cod_operacion = 20102001 AND TemMovCre.descrip BEGINS "RETIRO" THEN
        ASSIGN wvlrCobSC = ROUND(wvlrCobSC + (((TemMovCre.val_efectivo + TemMovCre.val_cheque) * 1 ) / 100 ),0).

     IF TemMovCre.cod_operacion = 20102001 AND TemMovCre.descrip BEGINS "COMISION" THEN
        ASSIGN wvlrOtrosC = wvlrOtrosC + ( (TemMovCre.val_efectivo + TemMovCre.val_cheque)  )
               wTotOtrosC = wTotOtrosC + ( (TemMovCre.val_efectivo + TemMovCre.val_cheque)  ).

/*      IF TemMovCre.cod_operacion = 10302001 AND TemMovCre.descrip BEGINS "COBRO SOBREGIRO" THEN DO:    */
/*         ASSIGN wvlrOtrosCMan = wvlrOtrosCMan + ( (TemMovCre.val_efectivo + TemMovCre.val_cheque)  )   */
/*                wTotOtrosCMan = wTotOtrosCMan + ( (TemMovCre.val_efectivo + TemMovCre.val_cheque)  ).  */
/*      END.                                                                                             */
     ELSE DO:
         CASE TemMovCre.cod_operacion :
             WHEN 20102001 THEN    /* Retiro / Desembolso de Credito  */
                  wvlrRet = (TemMovCre.val_efectivo + TemMovCre.val_cheque). 
             WHEN 20101001 THEN   /* Abono A Capital */
                  wvlrAbok = TemMovCre.val_efectivo + TemMovCre.val_cheque.
             WHEN 20101003 OR WHEN 20102006 THEN  /* Abono a Int. Corrientes y Cargo de Int. Corrientes  */
                  wvlrAboi  = TemMovCre.val_efectivo + TemMovCre.val_cheque.
             WHEN 20101007 THEN    /* Abono a Honorarios: Comisiones y Sobrecupo */
                  wvlrHon = (TemMovCre.val_efectivo + TemMovCre.val_cheque).
             WHEN 20101002 OR WHEN 20101004 THEN  /* Abono a Int. Mora  e Int. Dificil Cobro
                                                      para la fact 2 tenerlo  en cuenta 
                                                     al comparar con el pago minimo */
                  wvlrAboiM = TemMovCre.val_efectivo + TemMovCre.val_cheque.
             WHEN 20101006 THEN    /* Abono de Pólizas / Seguro de Cartera*/
                  wvlrAboSC =TemMovCre.val_efectivo + TemMovCre.val_cheque.

         END CASE.
         ASSIGN wtotabok = wtotabok + wvlrabok   
                wtotret  = wtotret + wvlrret.
     END.
     /* C U E R P O   D E L   I N F O R M E  */

     IF TemMovCre.cod_operacion = 10302001 AND TemMovCre.descrip BEGINS "COBRO SOBREGIRO" THEN DO:
        ASSIGN wvlrOtrosCMan = wvlrOtrosCMan + ( (TemMovCre.val_efectivo + TemMovCre.val_cheque)  )
               wTotOtrosCMan = wTotOtrosCMan + ( (TemMovCre.val_efectivo + TemMovCre.val_cheque)  ).
     END.
     IF  (wvlrRet + wvlrAbok + wvlrAboi + wvlrAboim + wvlrhon + wvlrAboSC) NE  0 THEN DO:
         /*  PUT TRIM(creditos.nit) ";" creditos.num_credito ";" mov_creditos.fecha  ";" mov_creditos.descrip ";" mov_creditos.num_documento    ";" vlrRet ";"     vlrAbo ";"  creditos.tasa   SKIP. */
          CREATE Tmp_Det_Extracto.
          ASSIGN wnro_auditoria = ""  wdetaFact = "".
/*           FIND FIRST mov_contable WHERE mov_contable.agencia       = TemMovCre.agencia                AND                    */
/*                                         mov_contable.comprobante   = TemMovCre.cpte                   AND                    */
/*                                         mov_contable.num_documento = INTEGER(TemMovCre.num_documento) AND                    */
/*                                         mov_contable.nit           = TemMovCre.nit    AND                                    */
/*                                         mov_contable.fec_contable  = TemMovCre.fecha  AND                                    */
/*                                        (mov_contable.db + mov_contable.cr) = (TemMovCre.val_efectivo + TemMovCre.val_cheque) */
/*               NO-LOCK NO-ERROR.                                                                                              */
/*           IF AVAILABLE(mov_contable) AND mov_contable.nro_auditoria NE " " THEN DO: */
/*               wnro_auditoria = mov_contable.nro_auditoria.                          */
/*               RUN detalle_factura.                                                  */
/*           END.                                                                      */
/*           ELSE wnro_auditoria = STRING(TemMovCre.num_documento). */
          IF TemMovCre.detalle = "Si" THEN
             RUN detalle_factura.
          ASSIGN wnro_auditoria = STRING(TemMovCre.nro_auditoria).

          ASSIGN Tmp_Det_Extracto.Nit           = TRIM(creditos.nit)
                 Tmp_Det_Extracto.num_credito   = creditos.num_credito
                 Tmp_Det_Extracto.fec_trans     = TemMovCre.fecha
                 Tmp_Det_Extracto.NumDocto      = wnro_auditoria
                 Tmp_Det_Extracto.CR            = wvlrRet 
                 Tmp_Det_Extracto.DB            = wvlrAbok + wvlrAboi + wvlrAboim + wvlrhon + wvlrAboSC.
                 /* Ojo*/
/*           DISPLAY TemMovCre.nit TemMovCre.num_credito            */
/*                   TemMovCre.fecha                                */
/*                   TemMovCre.num_documento                        */
/*                   TemMovCre.cod_operacion                        */
/*                   TemMovCre.val_efectivo + TemMovCre.val_cheque  */
/*                   TemMovCre.comentario TemMovCre.descrip.        */

          IF wdetafact NE " "  THEN DO:
             Tmp_Det_Extracto.descripcion   = wdetafact.
/*              IF TemMovCre.val_efectivo + TemMovCre.val_cheque = 600 OR    */
/*                 TemMovCre.val_efectivo + TemMovCre.val_cheque = 900 THEN  */
/*              DISPLAY TemMovCre.nit TemMovCre.num_credito            */
/*                      TemMovCre.fecha                                */
/*                      TemMovCre.num_documento                        */
/*                      TemMovCre.cod_operacion                        */
/*                      TemMovCre.val_efectivo + TemMovCre.val_cheque  */
/*                      TemMovCre.comentario TemMovCre.descrip.        */
          END.
          ELSE
             Tmp_Det_Extracto.descripcion   = TemMovCre.descrip.

          IF Tmp_Det_Extracto.descripcion = "Cgo.Capital X Trasl."  OR 
             Tmp_Det_Extracto.descripcion = "Cgo.Int.Ctes.X Trasl."  THEN
                  ASSIGN Tmp_Det_Extracto.descripcion = 'Traslado Cuenta Ahorros'.
          
          IF Tmp_Det_Extracto.descripcion = "Pagaré ActivadoCgo.Capital X Trasl." THEN
             ASSIGN Tmp_Det_Extracto.descripcion = "Traslado CupoRotat Cta Ahorros".
          /*Nuevo 12-Oct-2007*/
          IF Tmp_Det_Extracto.descripcion = "Abono para Honorarios-Efectivo" THEN
             ASSIGN Tmp_Det_Extracto.descripcion = "Abono para Comisión-Efectivo".
          IF Tmp_Det_Extracto.descripcion = "Abono para Honorarios-Cheque" THEN
             ASSIGN Tmp_Det_Extracto.descripcion = "Abono para Comisión-Cheque".
          /* Ojo*/
          IF Tmp_Det_Extracto.descripcion = "Abono para Polizas-Efectivo" THEN
             ASSIGN Tmp_Det_Extracto.descripcion = "Abono para Seg.Cartera-Efectivo".             
          IF Tmp_Det_Extracto.descripcion = "Abono para Polizas-Cheque" THEN
             ASSIGN Tmp_Det_Extracto.descripcion = "Abono para Seg.Cartera-Cheque".             
/*           IF Tmp_Det_Extracto.descripcion BEGINS "Mision " THEN           */
/*              ASSIGN Tmp_Det_Extracto.descripcion = "Utilizacion Cajero".  */
/*           IF Tmp_Det_Extracto.descripcion BEGINS "F asumido" THEN         */
/*              ASSIGN Tmp_Det_Extracto.descripcion = "GMF Asumido".         */
          /*******************/

          ASSIGN wzswDet = TRUE.
     END.
     /* Sumatoria de Comisiones por Dia */
     IF LAST-OF(TemMovCre.fecha) AND wvlrOtrosC > 0 THEN   DO:                                                                          /* Agregado  los  nuevos valores */
       /*  PUT TRIM(creditos.nit) ";" creditos.num_credito ";" mov_creditos.fecha  ";" "Cargos por Manejo" ";" mov_creditos.num_documento ";"  vlrOtrosC ";"  vlrAbo ";" creditos.tasa   SKIP. */
         CREATE Tmp_Det_Extracto.
         wnro_auditoria = "".
/*          FIND FIRST mov_contable WHERE mov_contable.agencia       = TemMovCre.agencia                AND                     */
/*                                        mov_contable.comprobante   = TemMovCre.cpte                   AND                     */
/*                                        mov_contable.num_documento = integer(TemMovCre.num_documento) AND                     */
/*                                        mov_contable.nit           = TemMovCre.nit                    AND                     */
/*                                        mov_contable.fec_contable  = TemMovCre.fecha                  AND                     */
/*                                        (mov_contable.db + mov_contable.cr) = (TemMovCre.val_efectivo + TemMovCre.val_cheque) */
/*              NO-LOCK NO-ERROR.                                                                                               */
/*          IF AVAILABLE(mov_contable) AND mov_contable.nro_auditoria NE " " THEN                                               */
/*             wnro_auditoria = mov_contable.nro_auditoria.                                                                     */
/*          ELSE wnro_auditoria = STRING(TemMovCre.num_documento).                                                              */

         ASSIGN wnro_auditoria = STRING(TemMovCre.nro_auditoria).
         ASSIGN Tmp_Det_Extracto.Nit           = TRIM(creditos.nit)
                Tmp_Det_Extracto.num_credito   = creditos.num_credito
                Tmp_Det_Extracto.fec_trans     = TemMovCre.fecha
                Tmp_Det_Extracto.descripcion   = "Utilizacion Cajero"
                Tmp_Det_Extracto.NumDocto      = wnro_auditoria
                Tmp_Det_Extracto.CR            = wvlrOtrosC 
                Tmp_Det_Extracto.DB            = wvlrAbok + wvlrAboi.
         ASSIGN wzswDet = TRUE.
     END.
     /* Nuevo 04-Mar-2008*/
     IF LAST-OF(TemMovCre.fecha) AND wvlrOtrosCMan > 0 THEN   DO:                                                                          /* Agregado  los  nuevos valores */
         CREATE Tmp_Det_Extracto.
         wnro_auditoria = "".
         ASSIGN wnro_auditoria = STRING(TemMovCre.nro_auditoria).
         ASSIGN Tmp_Det_Extracto.Nit           = TRIM(creditos.nit)
                Tmp_Det_Extracto.num_credito   = creditos.num_credito
                Tmp_Det_Extracto.fec_trans     = TemMovCre.fecha
                Tmp_Det_Extracto.descripcion   = "Cobro Sobregiro Manejo Tarjeta"
                Tmp_Det_Extracto.NumDocto      = wnro_auditoria
                Tmp_Det_Extracto.CR            = wvlrOtrosCMan 
                Tmp_Det_Extracto.DB            = wvlrAbok + wvlrAboi.
         ASSIGN wzswDet = TRUE.
     END.

     /* TERMINA EL CUERPO */
  END.
  IF wzswDet THEN DO:
      /* Termina Búsqueda de pagos pendientes  */
      IF creditos.monto GE creditos.sdo_capital THEN
        ASSIGN wcupdis = creditos.monto - creditos.sdo_capital   wsobcup = 0.
      ELSE 
        ASSIGN wcupdis = 0   wsobcup = creditos.sdo_capital - creditos.monto.
      /*correspondencia*/
      FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-ERROR.
      IF AVAILABLE Clientes AND DIR_correspondencia THEN 
         ASSIGN wdircorres = Clientes.DIR_comercial  wcodciu = clientes.lugar_comercial  wtel = clientes.tel_comercial.
      ELSE 
        IF AVAILABLE Clientes AND NOT DIR_correspondencia THEN
         ASSIGN wdircorres = Clientes.DIR_residencia wcodciu = clientes.lugar_residencia  wtel = clientes.tel_residencia.
      /*residencia*/
      FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ wcodciu  NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN ASSIGN Wnomciu  = trim(Ubicacion.Nombre).

      FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(wcodciu,1,5) NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN ASSIGN Wnomciu  = Wnomciu + " " + trim(Ubicacion.Nombre).
      
      FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(wcodciu,1,2) NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN ASSIGN Wnomciu  = Wnomciu + " " + trim(Ubicacion.Nombre).
      Wnomciu = LC(Wnomciu).

      IF wpagomin LE 0 THEN wpagomin = 0.
      /* Que hacer con interese anticipados */
      IF wpagtot LT 0 THEN  wpagtot = 0.
      /* Nuevo 10022008*/
      IF wcuota LT 0 THEN  wcuota = 0.
      /******************/
  END.
END.

PROCEDURE Detalle_factura:
   IF TemMovCre.comentario BEGINS "Retiro Red Exter" OR 
      TemMovCre.comentario BEGINS "Reversada Cajero" OR 
      TemMovCre.comentario BEGINS "AJUSTE" THEN
        wdetaFact = SUBSTRING(TemMovCre.comentario,3,17) + SUBSTRING(TemMovCre.comentario,39,30).
   ELSE
      IF TemMovCre.comentario BEGINS "Retiro Cajero  "  OR 
         TemMovCre.Comentario BEGINS "Retiro Datafono"  THEN
            wdetaFact = SUBSTRING(TemMovCre.comentario,3,17).
      ELSE
         wdetaFact = SUBSTRING(TemMovCre.comentario,3,17) + SUBSTRING(TemMovCre.comentario,39,30).
/* DISPLAY TemMovCre.nit TemMovCre.num_credito TemMovCre.comentario. */
END PROCEDURE.

FOR EACH Tmp_Det_Extracto : /*WHERE Tmp_Det_Extracto.CR = 900 OR Tmp_Det_Extracto.CR = 60 NO-LOCK:*/
    DISPLAY Tmp_Det_Extracto.Nit         
            Tmp_Det_Extracto.num_credito 
            Tmp_Det_Extracto.fec_trans   
            Tmp_Det_Extracto.NumDocto
            Tmp_Det_Extracto.descripcion 
            Tmp_Det_Extracto.CR          
            Tmp_Det_Extracto.DB.
END.
    
    
    
