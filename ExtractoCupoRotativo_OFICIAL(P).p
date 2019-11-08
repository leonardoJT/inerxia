/* ANTES DE REALIZAR ESTA OPERACION ES BUENO REALIZAR DUMP DE LAS TABLAS DE 
   * CLIENTES, MOV_CREDITOS, CREDITOS  FUERA DEL BACKUP DE LA BAES DE DATOS DBCENTRAL  */

/* controlar que todos los codigos de operacion se encuentren ok , de no encontrarlo, 
   verificar a que  pertenece e ingresarlo dentro del movimiento de creditos */
/* SELECT DISTINCT(cod_operacion)  FROM mov_creditos WHERE cod_credito = 570.  */

/*    NOTA : Verificar que los códigos aparezcan en este programa, analizar para cambiar el código o incluírlo */
/*    en el programa                                                                                           */
/* Definici¢n de Tablas de Trabajo */
DEFINE TEMP-TABLE tmp_enc_extracto /* Encabezado de Extracto */
    FIELD agencia   LIKE creditos.agencia
    FIELD nom_agen  AS CHARACTER FORMAT "X(30)"
    FIELD nit       LIKE mov_creditos.nit
    FIELD fecor     LIKE mov_creditos.Fecha
    FIELD nombre    AS CHAR FORMAT "X(60)"
    FIELD direccion LIKE clientes.Dir_comercial
    FIELD telefono  LIKE clientes.Tel_comercial
    FIELD ciudad    AS CHARACTER FORMAT "X(60)"
    FIELD numcre    LIKE mov_creditos.Num_Credito
    FIELD cuptot    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD cupDis    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD Sobrecupo AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD sdo_ant   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD Cargos    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD intCtes   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD IntMora   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD OtrosCar  AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD Pagos     AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD NvoSdo    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD SdoMora   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD PagoTotal AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD PagoMin   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0
    FIELD Tasa_IC   AS DECIMAL FORMAT "->9.99"                 INITIAL 0
    FIELD Tasa_EA   AS DECIMAL FORMAT "->9.99"                 INITIAL 0
    FIELD FecMora   AS DATE
    FIELD FecPlazo  AS DATE
    FIELD FacDesde  AS DATE
    FIELD FecHasta  AS DATE
    FIELD cuota     AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0 
    FIELD valatrak  AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0 
    FIELD seg_cartera AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99" INITIAL 0 
    INDEX idxAgenit agencia nit.

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

DEFINE TEMP-TABLE tmp_Tot_extracto /* Estadisticas */
    FIELD agencia    LIKE creditos.agencia
    FIELD Nombre     LIKE agencias.nombre
    FIELD TotAso     AS INTEGER
    FIELD TotAsoExt  AS INTEGER
    FIELD Tcuptot    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TcupDis    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TSobrecupo AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD Tsdo_ant   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TCargos    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TintCtes   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TIntMora   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TOtrosCar  AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TPagos     AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TNvoSdo    AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TSdoMora   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TPagoTotal AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TPagoMin   AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD TPagoSegC  AS DECIMAL FORMAT "->>,>>>,>>>,>>>,>>9.99"
    FIELD Tasa_IC    AS DECIMAL FORMAT "->9.99"
    FIELD Tasa_EA    AS DECIMAL FORMAT "->9.99"
    FIELD FecMora    AS DATE
    FIELD FecPlazo   AS DATE
    FIELD FacDesde   AS DATE
    FIELD FecHasta   AS DATE
    INDEX idxAge agencia.

/* TEMPORAL para control d seguro de cartera */
DEFINE TEMP-TABLE Temseguro
    FIELD TAgencia   LIKE mov_contable.agencia
    FIELD TNit       LIKE clientes.nit  
    FIELD TNumcre    LIKE creditos.num_credito
    FIELD TCuenta    LIKE mov_contable.cuenta     
    FIELD TSaldo     LIKE creditos.sdo_capital INITIAL 0
    FIELD Taplicar   AS LOGICAL INITIAL FALSE
    INDEX idxagenit TAgencia TNit TNumcre.

/* Definicion de Variables de Trabajo */
DEFINE VAR wvlrRet    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrAboi   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrAboiM  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrabok   AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wvlrHon    AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wVlrOtrosC AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
DEFINE VAR wTotOtrosC AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0.
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

ASSIGN wfecIni   = wfec_inic 
       wfecfin   = wfec_vcto
       wfecplazo = wfec_proxpago.
/** Validar que el saldo sea > 0 y el estado igual a 2 o que el saldo no sea menor que 0 */

/** Nuevo */
FIND FIRST cuentas WHERE 
    cuentas.cuenta EQ "16609510" NO-LOCK NO-ERROR. /* Cuenta Cupo Rotativo - Seguro de Cartera */
IF AVAILABLE(cuentas) THEN
   IF cuentas.naturaleza = "DB" THEN
      ASSIGN wnatura = "DB".
   ELSE
      ASSIGN wnatura = "CR".
/***********/

FOR EACH creditos WHERE 
    (cod_credito = 570 OR cod_credito = 870)  
    AND estado = 2 AND
    AGENCIA = 1 NO-LOCK /*num_credito = 124969 NO-LOCK*/
/*FOR EACH creditos WHERE nit = '12952142' AND  num_credito = 213793 AND cod_credito = 570 AND estado = 2 NO-LOCK */
   BREAK BY Creditos.agencia BY Creditos.nit:
/*    DISPLAY "Si". */
   ASSIGN wvlrOtrosC = 0   wzswDet = FALSE  wcupdis     = 0   wsobcup    = 0
          wcupdis    = 0   wnomciu   = ""    wdircorres = ""
          wcodciu    = ""  wtel      = ""    wtotabok   = 0   wintmor    = 0
          wpagtot    = 0   wtotret   = 0     wcuota     = 0   wTotOtrosC = 0
          wpagomin   = 0   wpagoSegC = 0     wvlrCobSC  = 0.  
   wcantot = wcantot + 1.
   wcantage = wcantage + 1.
   /* Control Estadistico */
   /* Ojo*/
   ASSIGN wvlrCobSC = creditos.polizas.
   IF LAST-OF(creditos.agencia) THEN
    DO:
      ASSIGN wcantotage = wcantage wcantage = 0.
      FIND FIRST agencias WHERE agencias.agencia = creditos.agencia NO-LOCK NO-ERROR.
      IF AVAILABLE(agencias) THEN
         DO:
          CREATE Tmp_Tot_Extracto.
          ASSIGN                                                   
          tmp_Tot_Extracto.agencia    =  agencias.agencia  
          tmp_Tot_Extracto.Nombre     =  agencias.nombre           
          tmp_Tot_Extracto.TotAso     =  wcantotage                   
          tmp_Tot_Extracto.TotAsoExt  =  0  
          tmp_Tot_Extracto.Tcuptot    =  0  
          tmp_Tot_Extracto.TcupDis    =  0  
          tmp_Tot_Extracto.TSobrecupo =  0  
          tmp_Tot_Extracto.Tsdo_ant   =  0  
          tmp_Tot_Extracto.TCargos    =  0  
          tmp_Tot_Extracto.TintCtes   =  0  
          tmp_Tot_Extracto.TIntMora   =  0  
          tmp_Tot_Extracto.TOtrosCar  =  0  
          tmp_Tot_Extracto.TPagos     =  0  
          tmp_Tot_Extracto.TNvoSdo    =  0  
          tmp_Tot_Extracto.TSdoMora   =  0  
          tmp_Tot_Extracto.TPagoTotal =  0  
          tmp_Tot_Extracto.TPagoMin   =  0  
          tmp_Tot_Extracto.TPagoSegC  =  0  
          tmp_Tot_Extracto.Tasa_IC    =  0  
          tmp_Tot_Extracto.Tasa_EA    =  0. 
         END.
   END.
   /** Nuevo */
   ASSIGN wsalsegcar = 0.
/*    RUN cobro_segucartera.                              */
/*    IF wsalsegcar LT 0 THEN wsalsegcar = 0.00.          */
/*    IF wsalsegcar GT 0 THEN DO: /* Verificar indice */  */
/*       FIND FIRST ahorros WHERE                         */
/*            ahorros.nit EQ creditos.nit AND             */
/*            ahorros.cod_ahorro EQ 3     AND             */
/*            ahorros.estado     EQ 1                     */
/*            NO-LOCK NO-ERROR.                           */
/*       CREATE Temseguro.                                */
/*       ASSIGN TAgencia  = creditos.Agencia              */
/*              TNit      = creditos.Nit                  */
/*              TNumcre   = creditos.num_credito          */
/*              TCuenta   = "16609510"                    */
/*              TSaldo    = wsalsegcar.                   */
/*       IF AVAILABLE(ahorros) THEN                       */
/*          ASSIGN Taplicar = TRUE.                       */
/*        ELSE                                            */
/*          ASSIGN Taplicar = FALSE.                      */
/*    END.                                                */
   /**************/

   /* Saldo inicial de Facturacion */
   FIND LAST mov_creditos WHERE mov_creditos.agencia     = creditos.agencia     AND
                                (mov_creditos.cod_credito = 570 OR mov_creditos.cod_credito = 870) AND
                                mov_creditos.Num_credito = creditos.Num_credito AND
                                mov_creditos.fecha       < wfecini /*NO Ojo debe ser wfecfin */
                                NO-LOCK NO-ERROR.
   IF AVAILABLE(Mov_creditos) THEN DO:
       ASSIGN wsdoant = Mov_Creditos.sdo_capital
              wzswDet = TRUE.
       /* Nuevo 14-Enero-2008*/
       IF wsdoant LT 0 THEN
          ASSIGN wsdoant = 0.
       /**********************/
       IF wsdoant LT 0 THEN
          ASSIGN wsdoant = 0.
   END.
   ELSE wsdoant = 0.
   FOR EACH  mov_creditos WHERE 
             mov_creditos.fecha       GE wfec_inic            AND
             mov_creditos.fecha       LE wfec_Vcto            AND
            (mov_creditos.cod_credito EQ 570 OR mov_creditos.cod_credito EQ 870) AND /*Ojo*/
/*              mov_creditos.cod_credito EQ creditos.cod_credito AND */
             mov_creditos.num_credito EQ creditos.num_credito AND
             mov_creditos.nit         EQ creditos.nit     NO-LOCK   
             BREAK BY mov_creditos.Fecha BY mov_creditos.descrip :
     ASSIGN wvlrRet = 0  wvlrAbok = 0  wvlrAboi = 0  wvlrAboim = 0  wvlrHon = 0 wvlrAboSC = 0.
     IF cod_operacion = 030303001 THEN NEXT. /* Traslado entre agencias No tener en cuenta */
     /* Acumulo las comisiones por dia */
     IF FIRST-OF(mov_creditos.fecha) THEN /* Se da el total de las comisiones por día y asi no dar largos detalles */ 
        wvlrOtrosC = 0.
      /* Ojo*/
     IF mov_creditos.cod_operacion = 20102001 AND descrip BEGINS "RETIRO" THEN
        ASSIGN wvlrCobSC = ROUND(wvlrCobSC + (((mov_creditos.val_efectivo + mov_creditos.val_cheque) * 1 ) / 100 ),0).

     IF mov_creditos.cod_operacion = 20102001 AND descrip BEGINS "COMISION" THEN 
        ASSIGN wvlrOtrosC = wvlrOtrosC + ( (mov_creditos.val_efectivo + mov_creditos.val_cheque)  )
               wTotOtrosC = wTotOtrosC + ( (mov_creditos.val_efectivo + mov_creditos.val_cheque)  ).
     ELSE DO:
         CASE mov_creditos.cod_operacion :
             WHEN 20102001 THEN    /* Retiro / Desembolso de Credito  */
                  wvlrRet = (mov_creditos.val_efectivo + mov_creditos.val_cheque). 
             WHEN 20101001 THEN   /* Abono A Capital */
                  wvlrAbok = mov_creditos.val_efectivo + mov_creditos.val_cheque.
             WHEN 20101003 OR WHEN 20102006 THEN  /* Abono a Int. Corrientes y Cargo de Int. Corrientes  */
                  wvlrAboi  = mov_creditos.val_efectivo + mov_creditos.val_cheque.
             WHEN 20101007 THEN    /* Abono a Honorarios: Comisiones y Sobrecupo */
                  wvlrHon = (mov_creditos.val_efectivo + mov_creditos.val_cheque).
             WHEN 20101002 OR WHEN 20101004 THEN  /* Abono a Int. Mora  e Int. Dificil Cobro
                                                      para la fact 2 tenerlo  en cuenta 
                                                     al comparar con el pago minimo */
                  wvlrAboiM = mov_creditos.val_efectivo + mov_creditos.val_cheque.
             WHEN 20101006 THEN    /* Abono de Pólizas / Seguro de Cartera*/
                  wvlrAboSC = mov_creditos.val_efectivo + mov_creditos.val_cheque.

         END CASE.
         ASSIGN wtotabok = wtotabok + wvlrabok   
                wtotret  = wtotret + wvlrret.
     END.
     /* C U E R P O   D E L   I N F O R M E  */
     IF  (wvlrRet + wvlrAbok + wvlrAboi + wvlrAboim + wvlrhon + wvlrAboSC) NE  0 THEN DO:
         /*  PUT TRIM(creditos.nit) ";" creditos.num_credito ";" mov_creditos.fecha  ";" mov_creditos.descrip ";" mov_creditos.num_documento    ";" vlrRet ";"     vlrAbo ";"  creditos.tasa   SKIP. */
          CREATE Tmp_Det_Extracto.
          ASSIGN wnro_auditoria = ""  wdetaFact = "".
          FIND FIRST mov_contable WHERE mov_contable.agencia       = mov_creditos.agencia                AND
                                        mov_contable.comprobante   = mov_creditos.cpte                   AND
                                        mov_contable.num_documento = INTEGER(mov_creditos.num_documento) AND 
                                        mov_contable.nit           = mov_creditos.nit    AND 
                                        mov_contable.fec_contable  = mov_creditos.fecha  AND 
                                       (mov_contable.db + mov_contable.cr) = (mov_creditos.val_efectivo + mov_creditos.val_cheque)
              NO-LOCK NO-ERROR.
          IF AVAILABLE(mov_contable) AND mov_contable.nro_auditoria NE " " THEN DO:
              wnro_auditoria = mov_contable.nro_auditoria.
              RUN detalle_factura.
          END.
          ELSE wnro_auditoria = STRING(mov_creditos.num_documento).

          ASSIGN Tmp_Det_Extracto.Nit           = TRIM(creditos.nit)
                 Tmp_Det_Extracto.num_credito   = creditos.num_credito
                 Tmp_Det_Extracto.fec_trans     = mov_creditos.fecha
                 Tmp_Det_Extracto.NumDocto      = wnro_auditoria
                 Tmp_Det_Extracto.CR            = wvlrRet 
                 Tmp_Det_Extracto.DB            = wvlrAbok + wvlrAboi + wvlrAboim + wvlrhon + wvlrAboSC.
                 /* Ojo*/
          IF wdetafact NE " "  THEN
             Tmp_Det_Extracto.descripcion   = wdetafact.
          ELSE
             Tmp_Det_Extracto.descripcion   = mov_creditos.descrip.

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
          /*******************/

          ASSIGN wzswDet = TRUE.
     END.
     /* Sumatoria de Comisiones por Dia */
     IF LAST-OF(mov_creditos.fecha) AND wvlrOtrosC > 0 THEN   DO:                                                                          /* Agregado  los  nuevos valores */
       /*  PUT TRIM(creditos.nit) ";" creditos.num_credito ";" mov_creditos.fecha  ";" "Cargos por Manejo" ";" mov_creditos.num_documento ";"  vlrOtrosC ";"  vlrAbo ";" creditos.tasa   SKIP. */
         CREATE Tmp_Det_Extracto.
         wnro_auditoria = "".
         FIND FIRST mov_contable WHERE mov_contable.agencia       = mov_creditos.agencia                AND
                                       mov_contable.comprobante   = mov_creditos.cpte                   AND
                                       mov_contable.num_documento = integer(mov_creditos.num_documento) AND 
                                       mov_contable.nit           = mov_creditos.nit                    AND 
                                       mov_contable.fec_contable  = mov_creditos.fecha                  AND 
                                       (mov_contable.db + mov_contable.cr) = (mov_creditos.val_efectivo + mov_creditos.val_cheque)
             NO-LOCK NO-ERROR.
         IF AVAILABLE(mov_contable) AND mov_contable.nro_auditoria NE " " THEN
            wnro_auditoria = mov_contable.nro_auditoria.
         ELSE wnro_auditoria = STRING(mov_creditos.num_documento).

         ASSIGN Tmp_Det_Extracto.Nit           = TRIM(creditos.nit)
                Tmp_Det_Extracto.num_credito   = creditos.num_credito
                Tmp_Det_Extracto.fec_trans     = mov_creditos.fecha
                Tmp_Det_Extracto.descripcion   = "Utilizacion Cajero"
                Tmp_Det_Extracto.NumDocto      = wnro_auditoria
                Tmp_Det_Extracto.CR            = wvlrOtrosC 
                Tmp_Det_Extracto.DB            = wvlrAbok + wvlrAboi.
         ASSIGN wzswDet = TRUE.
     END.
     /* TERMINA EL CUERPO */
  END.
  IF wzswDet THEN DO:
      /* búsqueda de Anterior Factura */
      CREATE Tmp_Enc_Extracto.
      ASSIGN Tmp_Enc_Extracto.seg_cartera = wvlrCobSC
             Tmp_Enc_Extracto.Otroscar = Creditos.honorarios.
      FIND FIRST facturacion WHERE facturacion.nit         = creditos.nit         AND 
                                   facturacion.num_credito = creditos.num_credito AND
                                   facturacion.estado      = 1 NO-ERROR.
      IF (AVAILABLE(facturacion) AND NOT facturacion.pagototal) OR 
         (AVAILABLE(facturacion) AND facturacion.pagototal AND creditos.honorarios GT 0) THEN DO:
          ASSIGN /* wfec_proxpago. */
                 Tmp_Enc_Extracto.valatrak = Facturacion.cuota - Facturacion.INT_Corrientes - 
                                             Facturacion.Rec_capital + Facturacion.val_atrasokpen
                                             /* Se agrego val_atrasokpen porque son valores anteriores */
                 Tmp_Enc_Extracto.Sdomora  = Tmp_Enc_Extracto.valatrak. 
                                             /* Facturacion.Pago_minimo - Facturacion.Val_recaudo */
          IF Tmp_Enc_Extracto.valatrak LT 0 THEN
             ASSIGN Tmp_Enc_Extracto.valatrak = 0
                    Tmp_Enc_Extracto.Sdomora  = 0.

      END.

      /* Termina Búsqueda de pagos pendientes  */
      FIND FIRST clientes WHERE clientes.nit = creditos.nit.
      IF creditos.monto GE creditos.sdo_capital THEN
        ASSIGN wcupdis = creditos.monto - creditos.sdo_capital   wsobcup = 0.
      ELSE 
        ASSIGN wcupdis = 0   wsobcup = creditos.sdo_capital - creditos.monto.
      IF DIR_correspondencia THEN DO:
         ASSIGN wdircorres = Clientes.DIR_comercial  wcodciu = clientes.lugar_comercial  wtel = clientes.tel_comercial.
      END.
      ELSE DO:
         ASSIGN wdircorres = Clientes.DIR_residencia wcodciu = clientes.lugar_residencia  wtel = clientes.tel_residencia.
      END.
      /*residencia*/
      FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ wcodciu  NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN ASSIGN Wnomciu  = trim(Ubicacion.Nombre).

      FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(wcodciu,1,5) NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN ASSIGN Wnomciu  = Wnomciu + " " + trim(Ubicacion.Nombre).
      
      FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(wcodciu,1,2) NO-LOCK NO-ERROR.
      IF AVAILABLE Ubicacion THEN ASSIGN Wnomciu  = Wnomciu + " " + trim(Ubicacion.Nombre).
      Wnomciu = LC(Wnomciu).

      ASSIGN wintMor  = Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar 
             wpagTot  = Creditos.Sdo_capital  + Creditos.INT_Corrientes + wintmor + wTotOtrosC +
                        Creditos.honorarios   + Creditos.costas + Tmp_Enc_Extracto.seg_cartera - Creditos.INT_anticipado.
      /* (wsdoant + wtotret - wtotAbok) +  (Creditos.INT_Corrientes + wintmor + wTotOtrosC). */ /* - (Creditos.Int_Anticipado + wtotAbok). */

      /* Calcular cuota */
      /* wpagomin = round( (wsdoant + wtotret -  wtotAbok)  / 36,0) + Creditos.INT_Corrientes + wintmor + wTotOtrosC + wsobcup.   */ /* La primer vez */ 
      wpagomin = ROUND( ( Creditos.sdo_capital -  wsobcup - Tmp_Enc_Extracto.valatrak  )  / 36,0) +  wsobcup 
               + Creditos.INT_Corrientes + wintmor + wTotOtrosC +  creditos.honorarios + Creditos.costas + Tmp_Enc_Extracto.seg_cartera + Tmp_Enc_Extracto.valatrak.
      wcuota   = ROUND( ( Creditos.sdo_capital -  wsobcup - Tmp_Enc_Extracto.valatrak  )  / 36,0)  + Creditos.INT_Corrientes.
      IF wpagomin LE 0 THEN wpagomin = 0.
      /* Que hacer con interese anticipados */
      IF wpagtot LT 0 THEN  wpagtot = 0.
      /* Nuevo 10022008*/
      IF wcuota LT 0 THEN  wcuota = 0.
      /******************/
      FIND FIRST agencias WHERE agencias.agencia = creditos.agencia NO-LOCK NO-ERROR.
      ASSIGN Tmp_Enc_Extracto.agencia    = creditos.agencia
             Tmp_Enc_Extracto.nom_agen   = STRING(creditos.agencia,"999") + " - " + TRIM(agencias.nombre)
             Tmp_Enc_Extracto.nit        = trim(Creditos.nit)
             Tmp_Enc_Extracto.fecor      = wfecfin
             Tmp_Enc_Extracto.nombre     = trim(clientes.nombre) + " " + trim(clientes.apellido1) + " " + trim(clientes.apellido2)
             Tmp_Enc_Extracto.direccion  = TRIM(wdircorres)
             Tmp_Enc_Extracto.telefono   = TRIM(wtel)
             Tmp_Enc_Extracto.ciudad     = TRIM(wnomciu)
             Tmp_Enc_Extracto.numcre     = Creditos.num_credito
             Tmp_Enc_Extracto.cuptot     = Creditos.Monto + Wsobcup
             Tmp_Enc_Extracto.cupDis     = wcupdis
             Tmp_Enc_Extracto.Sobrecupo  = wsobcup
             Tmp_Enc_Extracto.sdo_ant    = wsdoant  
             Tmp_Enc_Extracto.Cargos     = wtotret
             Tmp_Enc_Extracto.intCtes    = Creditos.INT_corrientes
             Tmp_Enc_Extracto.IntMora    = wintmor
             Tmp_Enc_Extracto.OtrosCar   = Tmp_Enc_Extracto.Otroscar + wTotOtrosC
             Tmp_Enc_Extracto.Pagos      = wtotAbok
             Tmp_Enc_Extracto.NvoSdo     = wpagtot
             Tmp_Enc_Extracto.PagoTotal  = wpagtot
             Tmp_Enc_Extracto.PagoMin    = wpagomin
             Tmp_Enc_Extracto.Tasa_IC    = ROUND(creditos.tasa / 12,3)
             Tmp_Enc_Extracto.Tasa_EA    = (EXP( 1 + ( creditos.tasa / 12 / 100 ) , 12) - 1) * 100
             Tmp_Enc_Extracto.FecPlazo   = wfecplazo
             Tmp_Enc_Extracto.FacDesde   = wfecini
             Tmp_Enc_Extracto.FecHasta   = wfecfin
             Tmp_Enc_Extracto.cuota      = wcuota.  /*  round( (wsdoant + wtotret -  wtotAbok)  / 36,0). */
             
             IF creditos.dias_atraso GT 0 THEN DO: /* Para Pago Inmediato */
                IF creditos.fec_pago EQ wfec_ANTproxpago THEN
                   ASSIGN Tmp_Enc_Extracto.FecPlazo = TODAY + 3 
                          Tmp_Enc_Extracto.FecMora  = creditos.Fec_pago + 1.
                ELSE
                    ASSIGN Tmp_Enc_Extracto.FecPlazo = TODAY + 3 
                           Tmp_Enc_Extracto.FecMora  = creditos.Fec_pago.
             END.
      
      wcant = wcant + 1.
      /* Nuevo Por Agregar 14-Enero-2008*/
      IF (wpagtot EQ 0) AND (wpagtot - wpagomin) LT 0 THEN DO:
         ASSIGN wcuota   = ROUND((Creditos.sdo_capital - wsobcup - 0) / 36,0) +
                                  Creditos.INT_Corrientes.
         IF wcuota LT 0 THEN wcuota = 0.
         UPDATE Tmp_Enc_Extracto.valatrak = 0
                Tmp_Enc_Extracto.sdomora  = 0
                Tmp_Enc_Extracto.PagoMin  = wpagtot         
                Tmp_Enc_Extracto.cuota    = wcuota.
      END.
      ELSE
         IF (wpagtot GT 0) AND (wpagtot - wpagomin) LT 0 THEN DO:
            ASSIGN wpagomin = ROUND((Creditos.sdo_capital -  wsobcup - 0) / 36,0) + wsobcup + Creditos.INT_Corrientes +
                              wintmor + wTotOtrosC + creditos.honorarios + Creditos.costas + Tmp_Enc_Extracto.seg_cartera + 0.
            ASSIGN wcuota   = ROUND((Creditos.sdo_capital - wsobcup - 0) / 36,0) +
                                     Creditos.INT_Corrientes.
            IF wpagomin LT 0 THEN wpagomin = 0.
            IF wcuota   LT 0 THEN wcuota = 0.

            UPDATE Tmp_Enc_Extracto.valatrak = 0
                   Tmp_Enc_Extracto.sdomora  = 0
                   Tmp_Enc_Extracto.PagoMin  = wpagomin
                   Tmp_Enc_Extracto.cuota    = wcuota.
         END.
      /************************************/
  END.
END.
/*RUN imp_segcartera.*/

/* OUTPUT TO "c:\Encabezado.txt". */
DEFINE VAR zarcEncabezado AS CHARACTER FORMAT "X(40)".
zarcEncabezado = "c:\Encabezado_" + STRING(DAY(TODAY),"99") +  STRING(MONTH(TODAY),"99") +  STRING(YEAR(TODAY),"9999") + ".TXT".
OUTPUT TO VALUE(zarcEncabezado).
PUT "nit;fecor;nombre;direccion;telefono;ciudad;numcre;cuptot;cupDis;Sobrecupo;sdo_ant;Cargos;intCtes;IntMora;OtrosCar;Pagos;NvoSdo;SdoMora;PagoTotal;PagoMin;Tasa_IC;Tasa_EA;FecMora;FecPagAnt;FacDesde;FecHasta;NomAgenc;S_Cartera" SKIP(0).
FOR EACH tmp_Enc_Extracto BREAK BY tmp_Enc_Extracto.agencia BY tmp_Enc_Extracto.nit:
 /* Nuevo */
  IF tmp_Enc_Extracto.FecMora NE ? THEN
   ASSIGN Pagoinme = "Inmediato". /*STRING(FecPlazo)*/
  ELSE
   /*ASSIGN Pagoinme = STRING(FecPlazo).*/
   ASSIGN Pagoinme = STRING(YEAR(FecPlazo),"9999") + "/" +
                     STRING(MONTH(FecPlazo),"99" ) + "/" +
                     STRING(DAY(FecPlazo),"99").
 /*********/

 IF FIRST-OF(tmp_Enc_Extracto.agencia)  THEN DO:
   /* Limpioar variables */
   ASSIGN wTotAso   = 0  wTotAsoExt = 0  wTcuptot   = 0 wTcupDis    = 0 wTSobrecupo = 0
          wTsdo_ant = 0  wTCargos   = 0  wTintCtes  = 0 wTIntMora   = 0 wTOtrosCar  = 0
          wTPagos   = 0  wTNvoSdo   = 0  wTSdoMora  = 0 wTPagoTotal = 0 wTPagoMin   = 0
          wcantage  = 0  wcantotage = 0  wTPagoSegC = 0.

   /*CREATE Tmp_Tot_Extracto.*/
 END.
 ASSIGN wcantage      =  wcantage      +  1
        wTcuptot      =  wTcuptot      +  tmp_Enc_Extracto.cuptot     
        wTcupDis      =  wTcupDis      +  tmp_Enc_Extracto.cupDis     
        wTSobrecupo   =  wTSobrecupo   +  tmp_Enc_Extracto.Sobrecupo  
        wTsdo_ant     =  wTsdo_ant     +  tmp_Enc_Extracto.sdo_ant    
        wTCargos      =  wTCargos      +  tmp_Enc_Extracto.Cargos     
        wTintCtes     =  wTintCtes     +  tmp_Enc_Extracto.intCtes 
        wTintMora     =  WTIntMora     +  tmp_Enc_Extracto.IntMora  
        wTOtrosCar    =  wTOtrosCar    +  tmp_Enc_Extracto.OtrosCar   
        wTPagos       =  wTPagos       +  tmp_Enc_Extracto.Pagos      
        wTNvoSdo      =  wTNvoSdo      +  tmp_Enc_Extracto.NvoSdo     
        wTSdoMora     =  wTSdoMora     +  tmp_Enc_Extracto.SdoMora    
        wTPagoTotal   =  wTPagoTotal   +  tmp_Enc_Extracto.PagoTotal  
        wTPagoMin     =  wTPagoMin     +  tmp_Enc_Extracto.PagoMin.  
        wTPagoSegC    =  wTPagoSegC    +  tmp_Enc_Extracto.Seg_Cartera.  

  PUT tmp_Enc_Extracto.nit        ";"      
      tmp_Enc_Extracto.fecor      ";"    
      tmp_Enc_Extracto.nombre     ";"
      tmp_Enc_Extracto.direccion  ";"
      tmp_Enc_Extracto.telefono   ";"
      tmp_Enc_Extracto.ciudad     ";"
      tmp_Enc_Extracto.numcre     ";"
      tmp_Enc_Extracto.cuptot     ";"
      tmp_Enc_Extracto.cupDis     ";"
      tmp_Enc_Extracto.Sobrecupo  ";"
      tmp_Enc_Extracto.sdo_ant    ";"
      tmp_Enc_Extracto.Cargos     ";"
      tmp_Enc_Extracto.intCtes    ";"
      tmp_Enc_Extracto.IntMora    ";"
      tmp_Enc_Extracto.OtrosCar   ";"
      tmp_Enc_Extracto.Pagos      ";"
      tmp_Enc_Extracto.NvoSdo     ";"
      tmp_Enc_Extracto.SdoMora    ";"
      tmp_Enc_Extracto.PagoTotal  ";"
      tmp_Enc_Extracto.PagoMin    ";"
      tmp_Enc_Extracto.Tasa_IC  FORMAT "->9.999"  ";"
      tmp_Enc_Extracto.Tasa_EA  FORMAT "->9.99"   ";"
      tmp_Enc_Extracto.FecMora    ";"
      Pagoinme                    ";"
      tmp_Enc_Extracto.FacDesde   ";"
      tmp_Enc_Extracto.FecHasta   ";"
      Tmp_Enc_Extracto.nom_agen   ";"
      Tmp_Enc_Extracto.seg_cartera SKIP(0). 

 IF LAST-OF(tmp_Enc_Extracto.agencia)  THEN DO: 
    FIND FIRST tmp_Tot_Extracto WHERE tmp_Tot_Extracto.agencia = tmp_Enc_Extracto.agencia NO-LOCK NO-ERROR.
    IF AVAILABLE(tmp_Tot_Extracto) THEN
       UPDATE 
            /*WHEN tmp_Tot_Extracto.agencia = tmp_Enc_Extracto.agencia*/
       tmp_Tot_Extracto.TotAsoExt  = wcantage                 
       tmp_Tot_Extracto.Tcuptot    = wTcuptot                 
       tmp_Tot_Extracto.TcupDis    = wTcupDis                 
       tmp_Tot_Extracto.TSobrecupo = wTSobrecupo              
       tmp_Tot_Extracto.Tsdo_ant   = wTsdo_ant                
       tmp_Tot_Extracto.TCargos    = wTCargos                 
       tmp_Tot_Extracto.TintCtes   = wTintCtes                
       tmp_Tot_Extracto.TIntMora   = wTintMora                
       tmp_Tot_Extracto.TOtrosCar  = wTOtrosCar               
       tmp_Tot_Extracto.TPagos     = wTPagos                  
       tmp_Tot_Extracto.TNvoSdo    = wTNvoSdo                 
       tmp_Tot_Extracto.TSdoMora   = wTSdoMora                
       tmp_Tot_Extracto.TPagoTotal = wTPagoTotal              
       tmp_Tot_Extracto.TPagoMin   = wTPagoMin
       tmp_Tot_Extracto.TPagoSegC  = wTPagoSegC
       tmp_Tot_Extracto.Tasa_IC    = tmp_Enc_Extracto.Tasa_IC 
       tmp_Tot_Extracto.Tasa_EA    = tmp_Enc_Extracto.Tasa_EA 
       NO-ERROR.
 END. 
END.
OUTPUT CLOSE.
DEFINE VAR zarcDetalle AS CHARACTER FORMAT "X(40)".
zarcDetalle = "c:\Detalle_" + STRING(DAY(TODAY),"99") +  STRING(MONTH(TODAY),"99") +  STRING(YEAR(TODAY),"9999") + ".TXT".
OUTPUT TO VALUE(zarcDetalle).
PUT "nit;Numcredito;fecha;descripcion;comprobante;vlr_Retiro;Abo_Capit" SKIP.
FOR EACH tmp_Det_Extracto BREAK BY tmp_Det_Extracto.agencia BY tmp_Det_Extracto.nit:
    PUT tmp_Det_Extracto.Nit          ";"
        tmp_Det_Extracto.num_credito  ";"
        tmp_Det_Extracto.fec_trans    ";"
        tmp_Det_Extracto.descripcion  ";"
        tmp_Det_Extracto.NumDocto     ";"
        tmp_Det_Extracto.CR           ";"
        tmp_Det_Extracto.DB SKIP(0).        
END.
OUTPUT CLOSE.

DEFINE VAR zarcesta AS CHARACTER FORMAT "X(40)".
zarcesta = "c:\Estadisticas_" + STRING(DAY(TODAY),"99") +  STRING(MONTH(TODAY),"99") +  STRING(YEAR(TODAY),"9999") + ".TXT".
OUTPUT TO VALUE(zarcesta).  /*"c:\Est" " .txt". */
PUT "agencia;Nombre;TotAso;TotAsoExT;Tcuptot;TcupDis;TSobrecupo;Tsdo_ant;TCargos;TintCtes;TIntMora;"
    "TOtrosCar;TPagos;TNvoSdo;TSdoMora;TPagoTotal;TPagoMin;TPagoSegC" SKIP(0).

FOR EACH tmp_Tot_Extracto BY agencia:
    PUT tmp_Tot_Extracto.agencia     ";"
        tmp_Tot_Extracto.Nombre      ";"
        tmp_Tot_Extracto.TotAso      ";"
        tmp_Tot_Extracto.TotAsoExt   ";"
        tmp_Tot_Extracto.Tcuptot     ";"
        tmp_Tot_Extracto.TcupDis     ";"
        tmp_Tot_Extracto.TSobrecupo  ";"
        tmp_Tot_Extracto.Tsdo_ant    ";"
        tmp_Tot_Extracto.TCargos     ";"
        tmp_Tot_Extracto.TintCtes    ";"
        tmp_Tot_Extracto.TIntMora    ";"
        tmp_Tot_Extracto.TOtrosCar   ";"
        tmp_Tot_Extracto.TPagos      ";"
        tmp_Tot_Extracto.TNvoSdo     ";"
        tmp_Tot_Extracto.TSdoMora    ";"
        tmp_Tot_Extracto.TPagoTotal  ";"
        tmp_Tot_Extracto.TPagoMin    ";"
        tmp_Tot_Extracto.TPagoSegC   SKIP(0).    
END.
OUTPUT CLOSE.

MESSAGE "Cant.de Extracto de Cupo Rotativo: " wcant SKIP(0)
        "Recuerde tener generado el backup de la base de datos" SKIP(0)
        "y realizar Dump de las Tablas Clientes, Creditos y Mov_creditos"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

 MESSAGE "Desea GRABAR LA NUEVA Facturacion con las siguientes Fechas" SKIP(1)
         "Periodo de Facturacion: " wperNUEVO SKIP
         "Fecha Inicial Corte   : " wfec_inic SKIP
         "Fecha Final de Corte  : " wfec_vcto SKIP 
         "Fecha Limite Plazo    : " wfec_proxpago VIEW-AS ALERT-BOX 
 QUESTION BUTTONS YES-NO UPDATE Ask2 AS LOGICAL.
 IF NOT Ask2 THEN 
    RETURN.

RUN grabar_newFact.

PROCEDURE Detalle_factura:
   IF mov_contable.comentario BEGINS "Retiro Red Exter" OR 
      mov_contable.comentario BEGINS "Reversada Cajero" OR 
      mov_contable.comentario BEGINS "AJUSTE" THEN
        wdetaFact = SUBSTRING(mov_contable.comentario,3,17) + SUBSTRING(mov_contable.comentario,39,30).
   ELSE
      IF mov_contable.comentario BEGINS "Retiro Cajero  "  OR 
         mov_contable.Comentario BEGINS "Retiro Datafono"  THEN
            wdetaFact = SUBSTRING(mov_contable.comentario,3,17).
      ELSE
         wdetaFact = SUBSTRING(mov_contable.comentario,3,17) + SUBSTRING(mov_contable.comentario,39,30).
   
END PROCEDURE.


PROCEDURE grabar_newFact:
  grabar:
  DO TRANSACTION ON ERROR UNDO grabar: 
/*       FOR EACH facturacion   WHERE facturacion.nit         = creditos.nit         AND  */
/*                                    facturacion.num_credito = creditos.num_credito AND  */
/*                                    facturacion.per_factura = wperiodo:                 */
/*           UPDATE facturacion.estado = 2.                                               */
/*       END.                                                                             */
      FOR EACH tmp_Enc_Extracto:
         wcontador = wcontador + 1.
         CREATE FACTURACION.
         ASSIGN Facturacion.Num_factura       = wcontador
                Facturacion.Per_Factura       = wperNUEVO    
                Facturacion.Nit               = tmp_enc_extracto.nit       
                Facturacion.fec_corte         = tmp_enc_extracto.fecor     
                Facturacion.nombre            = tmp_enc_extracto.nombre    
                Facturacion.direccion         = tmp_enc_extracto.direccion 
                Facturacion.Telefono          = tmp_enc_extracto.telefono  
                Facturacion.ciudad            = tmp_enc_extracto.ciudad    
                Facturacion.Num_Credito       = tmp_enc_extracto.numcre    
                Facturacion.CupoTotal         = tmp_enc_extracto.cuptot    
                Facturacion.CupoDisponible    = tmp_enc_extracto.cupDis    
                Facturacion.Sobrecupo         = tmp_enc_extracto.Sobrecupo 
                Facturacion.Sdo_Anterior      = tmp_enc_extracto.sdo_ant   
                Facturacion.Cargos            = tmp_enc_extracto.Cargos    
                Facturacion.Int_Corrientes    = tmp_enc_extracto.intCtes   
                Facturacion.Int_MorCobrar     = tmp_enc_extracto.IntMora   
                Facturacion.OtrosCargos       = tmp_enc_extracto.OtrosCar  
                Facturacion.Pagos             = tmp_enc_extracto.Pagos     
                Facturacion.Nuevo_Saldo       = tmp_enc_extracto.NvoSdo    
                Facturacion.Sdo_Mora          = tmp_enc_extracto.SdoMora   
                Facturacion.Pago_Total        = tmp_enc_extracto.PagoTotal 
                Facturacion.Pago_Minimo       = tmp_enc_extracto.PagoMin   
                Facturacion.Tasa_Ic           = tmp_enc_extracto.Tasa_IC   
                Facturacion.Tasa_Ea           = tmp_enc_extracto.Tasa_EA   
                Facturacion.Fec_Mora          = tmp_enc_extracto.FecMora   
                Facturacion.Nombre_Agencia    = tmp_enc_extracto.nom_agen 
                Facturacion.Agencia           = tmp_enc_extracto.agencia
                Facturacion.Fec_LimPago       = wfec_proxpago              
                Facturacion.Fec_Inicial       = wfec_inic    
                Facturacion.Fec_Final         = wfec_vcto
                Facturacion.Val_Recaudo       = 0                       
                Facturacion.Rec_Capital       = 0                       
                Facturacion.Rec_IntCorrientes = 0                       
                Facturacion.Rec_IntMora       = 0                       
                Facturacion.Rec_Honorarios    = 0
                Facturacion.Val_Atraso        = 0
                Facturacion.Cuota             = Tmp_Enc_Extracto.cuota
                Facturacion.PagoTotal         = NO
                Facturacion.Estado            = 1
                Facturacion.Val_atrasokpen    = Tmp_Enc_Extracto.valatrak
                Facturacion.Seg_Cartera       = Tmp_Enc_Extracto.seg_cartera.
         /* IF Facturacion.sdo_mora LE 0 THEN   /* Toda la facturacion con la nueva fecha, por intruccion de OMAR OSORIO 
                                                      AGOSTO 7 DE 2007 */
            ASSIGN Facturacion.Fec_LimPago       = wfec_proxpago 
                   Facturacion.Fec_Inicial       = wfec_inic     
                   Facturacion.Fec_Final         = wfec_vcto.     
         ELSE
            ASSIGN Facturacion.Fec_LimPago       = wfec_ANTproxpago 
                   Facturacion.Fec_Inicial       = wfec_ANTinic     
                   Facturacion.Fec_Final         = wfec_ANTvcto.      */ 

         FIND FIRST creditos  WHERE creditos.agencia     = Facturacion.agencia     AND 
                                    Creditos.Tip_credito = 1                       AND 
                                    (creditos.cod_credito = 570 OR creditos.cod_credito = 870)  AND
                                    Creditos.Num_credito = Facturacion.num_credito AND
                                    Creditos.nit         = Facturacion.nit NO-ERROR.
         IF AVAILABLE(creditos) THEN DO:
           ASSIGN creditos.cuota      = Facturacion.cuota
                  Creditos.Honorarios = /* Creditos.honorarios + */ Facturacion.OtrosCargos 
                  Creditos.Polizas    = Facturacion.Seg_Cartera
                  Creditos.Fec_pago   = Facturacion.Fec_limPago.
           IF Facturacion.Pago_Total GT 0 THEN DO:
               FIND FIRST planpagos WHERE   planpagos.agencia       =  creditos.agencia      AND
                                            planpagos.Tip_credito   =  Creditos.Tip_credito  AND
                                            planpagos.cod_credito   =  creditos.cod_credito  AND
                                            planpagos.Num_credito   =  Creditos.Num_credito  AND
                                            planpagos.nit           =  Creditos.nit          AND
                                            planpagos.id_pdo        = 1 NO-LOCK NO-ERROR.
               IF AVAILABLE(planpagos) THEN DO:
                 ASSIGN PlanPagos.Int_LiqAcum    = 0
                        PlanPagos.Int_LiqPdo     = 0                                                                        
                        PlanPagos.Int_MoraAcum   = 0                                                                         
                        PlanPagos.Int_MoraPdo    = 0                                                                         
                        PlanPagos.Monto_Actual   = 0                                                                         
                        PlanPagos.Fec_Vcto       = wfec_proxpago                                                             
                        PlanPagos.Fec_ProxPago   = wfec_inic                                                                 
                        PlanPagos.Fec_Inic       = wfec_vcto                                                                 
                        PlanPagos.Cuo_Pagas      = 0                                                                         
                        PlanPagos.Cuota          = Facturacion.cuota                                                         
                        PlanPagos.Cargos_Pdo     = Facturacion.Pago_minimo - (Facturacion.Cuota - Facturacion.INT_corrientes)                                                                         
                        PlanPagos.Cargos_Acum    = 0                                                                         
                        PlanPagos.Capital_Pdo    = Facturacion.Cuota - Facturacion.INT_corrientes 
                        PlanPagos.Capital_Acum   = 0.                                                                         
               END.
               ELSE DO:
                   MESSAGE "No fue encontrado PlanPagos para la actualizacion" SKIP
                           "de datos de la nueva factura par el Credito Nro.:" Facturacion.Num_Credito SKIP(1)
                           " Cedula  : " Factura.nit     SKIP
                           " Agencia : " Factura.agencia SKIP
                           "Comunique esta inconsistencia al Administrador!"
                   VIEW-AS ALERT-BOX.
                   RETURN ERROR.
               END.
           END.
         END.
         ELSE DO:
            MESSAGE "No fue encontrado el producto de créditos para la" SKIP
                    "actualizacion de datos de la nueva factura Credito Nro." Facturacion.Num_Credito SKIP(1)
                    " Cedula  : " Factura.nit     SKIP
                    " Agencia : " Factura.agencia SKIP
                    "Comunique esta inconsistencia al Administrador!"
            VIEW-AS ALERT-BOX.
             RETURN ERROR.
         END.
      END.
/*       FIND FIRST per_facturacion WHERE per_facturacion.estado = 1 AND                     */
/*                  per_facturacion.per_factura EQ wperiodo NO-ERROR.                        */
/*       IF AVAILABLE(per_facturacion) THEN   /* Facturacion Vigente */                      */
/*          ASSIGN per_facturacion.estado = 2.  /* Inactivo */                               */
/*                                                                                           */
/*       FIND FIRST per_facturacion WHERE per_facturacion.per_factura = wperNuevo NO-ERROR.  */
/*       IF AVAILABLE(per_facturacion) THEN  /* Nueva Facturacion */                         */
/*          ASSIGN per_facturacion.estado = 1.  /* Activo   */                               */
  END.
  MESSAGE "Revisar Tabla Facturacion, el estado 2 para la anterior facturacion" SKIP
          "y para la nueva el estado 1 " SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* Actualiza estados*/
FOR EACH facturacion   WHERE /*facturacion.nit         = creditos.nit         AND 
                             facturacion.num_credito = creditos.num_credito AND*/
                             facturacion.per_factura = wperiodo:
    UPDATE facturacion.estado = 2.
END.

FIND FIRST per_facturacion WHERE per_facturacion.estado = 1 AND 
           per_facturacion.per_factura EQ wperiodo NO-ERROR.
IF AVAILABLE(per_facturacion) THEN   /* Facturacion Vigente */
   ASSIGN per_facturacion.estado = 2.  /* Inactivo */

FIND FIRST per_facturacion WHERE per_facturacion.per_factura = wperNuevo NO-ERROR.
IF AVAILABLE(per_facturacion) THEN  /* Nueva Facturacion */ 
   ASSIGN per_facturacion.estado = 1.  /* Activo   */






/* PROCEDURE cobro_segucartera:                                                  */
/*   FOR EACH mov_contable WHERE                                                 */
/*     mov_contable.cuenta      EQ cuentas.cuenta  AND                           */
/*     mov_contable.nit         EQ creditos.nit    NO-LOCK: /*AND*/              */
/* /*     mov_contable.comprobante EQ 41              AND      */                */
/* /*     (mov_contable.usuario    EQ "998"           OR       */                */
/* /*     mov_contable.usuario     EQ "999")          NO-LOCK: */                */
/*     IF wnatura = "DB" THEN                                                    */
/*        ASSIGN wsalsegcar = wsalsegcar + (mov_contable.db - mov_contable.cr).  */
/*     ELSE                                                                      */
/*        ASSIGN wsalsegcar = wsalsegcar + (mov_contable.cr - mov_contable.db).  */
/*   END.                                                                        */
/* END PROCEDURE. */

/* PROCEDURE imp_segcartera:                                 */
/*   OUTPUT TO "c:\extracto\segcarterafacturacion.csv".      */
/*   PUT "Agencia;Nit;Credito;Cuenta;Saldo;Aplica" SKIP(0).  */
/*   FOR EACH Temseguro BY TAgencia BY TNit BY TNumcre:      */
/*       PUT TAgencia  ";"                                   */
/*           TNit      ";"                                   */
/*           TNumcre   ";"                                   */
/*           TCuenta   ";"                                   */
/*           TSaldo    FORMAT "->>>,>>>,>>>,>>9.99" ";"      */
/*           Taplica   SKIP(0).                              */
/*   END.                                                    */
/*   OUTPUT CLOSE.                                           */
/* END PROCEDURE.                                            */

/* Validacion de periodos inactivos*/

/* FOR EACH per_factura:                  */
/*    DISPLAY per_factura WITH 1 COLUMN.  */
/* END.                                   */

/* SELECT COUNT(distinct(num_credito)) FROM facturacion */
/*     WHERE per_factura = 3 AND estado = 1.            */

/* FOR EACH facturacion WHERE per_factura = 3 AND estado = 1:  */
/*     UPDATE estado = 2.                                      */
/* END.                                                        */
