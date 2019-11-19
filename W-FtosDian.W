&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W_FtosDian
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W_FtosDian 
CREATE WIDGET-POOL.

{Incluido/Variable.I "SHARED"}

ON RETURN TAB.

 /* oakley */

   DEFI TEMP-TABLE TRepor
        FIELD Ced  LIKE Mov_contable.Nit     
        FIELD Cpto AS INTEG FORM "9999"
        FIELD Base LIKE Mov_Contable.Db
        FIELD Acu  LIKE Mov_Contable.Db      
        FIELD Suc  LIKE Mov_Contable.Agencia.

   DEFI TEMP-TABLE TCed
     FIELD Ced  LIKE Clientes.Nit
     FIELD Cpto AS INTEG FORM "9999"
     FIELD Vlr  LIKE Mov_Contable.Db INIT 0.

   DEFI TEMP-TABLE TSinCC LIKE mov_contable.

   DEFI TEMP-TABLE CMenores
     FIELD Ced LIKE Creditos.Nit
     FIELD VlrK LIKE Creditos.Sdo_Capital
     FIELD VlrI LIKE Creditos.Sdo_Capital.

/* ---------- */
DEFINE TEMP-TABLE cedulas NO-UNDO
    FIELD cedula AS CHARACTER.
   
DEFINE TEMP-TABLE TT_1019 NO-UNDO
    FIELD registro AS INTEGER
    FIELD tdoc AS CHARACTER
    FIELD nid AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apl1 AS CHARACTER
    FIELD apl2 AS CHARACTER
    FIELD nom1 AS CHARACTER
    FIELD nom2 AS CHARACTER
    FIELD raz AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD dpto AS CHARACTER
    FIELD mun AS CHARACTER
    FIELD mov AS DECIMAL
    FIELD sal AS DECIMAL
    FIELD cta AS CHARACTER
    FIELD ntitsec AS INTEGER
    FIELD tipcta AS INTEGER
    FIELD codex AS INTEGER
    FIELD ntitSec_tdoc AS CHARACTER EXTENT 10
    FIELD ntitSec_nid AS CHARACTER EXTENT 10
    FIELD ntitSec_dv AS CHARACTER EXTENT 10
    FIELD ntitSec_apl1 AS CHARACTER EXTENT 10
    FIELD ntitSec_apl2 AS CHARACTER EXTENT 10
    FIELD ntitSec_nom1 AS CHARACTER EXTENT 10
    FIELD ntitSec_nom2 AS CHARACTER EXTENT 10
    FIELD ntitSec_raz AS CHARACTER EXTENT 10.
    
DEFINE TEMP-TABLE TT_1020 NO-UNDO
    FIELD registro AS INTEGER
    FIELD tdoc AS CHARACTER
    FIELD nid AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apl1 AS CHARACTER
    FIELD apl2 AS CHARACTER
    FIELD nom1 AS CHARACTER
    FIELD nom2 AS CHARACTER
    FIELD raz AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD dpto AS CHARACTER
    FIELD mun AS CHARACTER
    FIELD salIni AS DECIMAL
    FIELD inv AS DECIMAL
    FIELD ren AS DECIMAL
    FIELD saldic AS DECIMAL
    FIELD ntit AS CHARACTER
    FIELD numt AS INTEGER
    FIELD titSec_tdoc AS CHARACTER EXTENT 10
    FIELD titSec_nid AS CHARACTER EXTENT 10
    FIELD titSec_dv AS CHARACTER EXTENT 10
    FIELD titSec_apl1 AS CHARACTER EXTENT 10
    FIELD titSec_apl2 AS CHARACTER EXTENT 10
    FIELD titSec_nom1 AS CHARACTER EXTENT 10
    FIELD titSec_nom2 AS CHARACTER EXTENT 10
    FIELD titSec_raz AS CHARACTER EXTENT 10.

DEFINE TEMP-TABLE TT_documentos
    FIELD num_documento AS INTEGER
    FIELD cr AS DECIMAL
    FIELD db AS DECIMAL
    FIELD inversion AS DECIMAL.

DEFINE VAR consecutivo AS INTEGER.
/* ---------- */

DEFINE TEMP-TABLE F1001
    FIELD concepto AS CHARACTER
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD departamento AS CHARACTER
    FIELD municipio AS CHARACTER
    FIELD pais AS CHARACTER
    FIELD pagosCostoDeduccion AS DECIMAL
    FIELD pagosNoCostoNoDeduccion AS DECIMAL
    FIELD IvaMayorValorCostoDeducible AS DECIMAL
    FIELD IvaMayorValorCostoNoDeducible AS DECIMAL
    FIELD ReteFuenteRta AS DECIMAL
    FIELD ReteFuenteRtaAsumida AS DECIMAL
    FIELD ReteFuenteIvaPracticadaRegComun AS DECIMAL
    FIELD ReteFuenteIvaAsumidaRegSimpl AS DECIMAL
    FIELD ReteFuentePracticadaNoDomic AS DECIMAL
    FIELD ajuste AS CHARACTER.

DEFINE TEMP-TABLE F1003
    FIELD concepto AS CHARACTER
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD departamento AS CHARACTER
    FIELD municipio AS CHARACTER
    FIELD valorPagoAbonoTransaccion AS DECIMAL
    FIELD valorRetencion AS DECIMAL.

DEFINE TEMP-TABLE F1007
    FIELD concepto AS CHARACTER
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD pais AS CHARACTER
    FIELD valorIngresoBrutoOperaciones AS DECIMAL
    FIELD valorIngresoConsorcio AS DECIMAL
    FIELD valorIngresoContratos AS DECIMAL
    FIELD valorIngresosMineria AS DECIMAL
    FIELD valorIngresoFiducias AS DECIMAL
    FIELD valorIngresoTerceros AS DECIMAL
    FIELD valorDescuento AS DECIMAL.

DEFINE TEMP-TABLE F1009
    FIELD concepto AS CHARACTER
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD departamento AS CHARACTER
    FIELD municipio AS CHARACTER
    FIELD pais AS CHARACTER
    FIELD valorSaldoPasivo AS DECIMAL.

DEFINE TEMP-TABLE F1008
    FIELD concepto AS CHARACTER
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD departamento AS CHARACTER
    FIELD municipio AS CHARACTER
    FIELD pais AS CHARACTER
    FIELD valorSaldoCreditoActivo AS DECIMAL.

DEFINE TEMP-TABLE F1012
    FIELD concepto AS CHARACTER
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD pais AS CHARACTER
    FIELD valorSaldosPatrimoniales AS DECIMAL.

DEFINE TEMP-TABLE F1005
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD valorIva AS DECIMAL
    FIELD IvaResultante AS DECIMAL.


DEFINE TEMP-TABLE otros1001 LIKE F1001.
DEFINE TEMP-TABLE otros1003 LIKE F1003.
DEFINE TEMP-TABLE otros1005 LIKE F1005.
DEFINE TEMP-TABLE otros1007 LIKE F1007.
DEFINE TEMP-TABLE otros1008 LIKE F1008.
DEFINE TEMP-TABLE otros1009 LIKE F1009.
DEFINE TEMP-TABLE otros1012 LIKE F1012.

DEFINE TEMP-TABLE docs
    FIELD nit AS CHARACTER
    FIELD comprobante AS INTEGER
    FIELD agencia AS INTEGER
    FIELD num_documento AS INTEGER
    FIELD fec_contable AS DATE
    INDEX Idx NIT comprobante agencia num_documento fec_contable.

DEFINE TEMP-TABLE reportar
    FIELD nit AS CHARACTER.

DEFINE TEMP-TABLE SHD_ReteICA
    FIELD cuenta AS CHARACTER
    FIELD vigencia AS INTEGER
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD telefono AS CHARACTER
    FIELD email AS CHARACTER
    FIELD codMunicipio AS CHARACTER
    FIELD codDepartamento AS CHARACTER
    FIELD base AS DECIMAL
    FIELD tarifa AS DECIMAL FORMAT ">>9.9999"
    FIELD retencion AS DECIMAL.

DEFINE TEMP-TABLE SHD_Bienes&Servicios
    FIELD vigencia AS INTEGER
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD telefono AS CHARACTER
    FIELD email AS CHARACTER
    FIELD codMunicipio AS CHARACTER
    FIELD codDepartamento AS CHARACTER
    FIELD concepto AS INTEGER
    FIELD compras AS DECIMAL.

DEFINE TEMP-TABLE creditos12
    FIELD nit AS CHARACTER
    FIELD cod_credito AS INTEGER
    FIELD sdo_capital AS DECIMAL
    FIELD INT_corriente AS DECIMAL
    FIELD INT_morCobrar AS DECIMAL
    FIELD provision AS DECIMAL.

DEFINE TEMP-TABLE F1006
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD impuestoGenerado AS DECIMAL
    FIELD impuestoRecuperado AS DECIMAL.

DEFINE TEMP-TABLE otros1006 LIKE F1006.

DEFINE TEMP-TABLE F1010
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD departamento AS CHARACTER
    FIELD municipio AS CHARACTER
    FIELD pais AS CHARACTER
    FIELD valorPatrimonio AS DECIMAL
    FIELD porcentajeParticipacion AS DECIMAL
    FIELD decimales AS CHARACTER FORMAT "X(11)".

DEFINE TEMP-TABLE otros1010 LIKE F1010.

DEFINE TEMP-TABLE ttmov
    FIELD id AS ROWID.

DEFINE TEMP-TABLE F1026
    FIELD cod AS INTEGER
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD dv AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD razonSocial AS CHARACTER
    FIELD direccion AS CHARACTER
    FIELD departamento AS CHARACTER
    FIELD municipio AS CHARACTER
    FIELD valorPrestamos AS DECIMAL.


DEFINE TEMP-TABLE ttcreds
    FIELD nit AS CHARACTER
    FIELD sdoCreditos AS DECIMAL
    INDEX idx1 nit.

DEFINE TEMP-TABLE ttcredsDet
    FIELD nit AS CHARACTER
    FIELD tipo AS INTEGER
    FIELD sdoCreditos AS DECIMAL
    INDEX idx1 nit.


DEFINE TEMP-TABLE F2276
    FIELD tipoDoc AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD apellido1 AS CHARACTER
    FIELD apellido2 AS CHARACTER
    FIELD nombre1 AS CHARACTER
    FIELD nombre2 AS CHARACTER
    FIELD fec_ini AS CHARACTER
    FIELD fec_fin AS CHARACTER
    FIELD fec_expCert AS CHARACTER
    FIELD departamento AS CHARACTER
    FIELD municipio AS CHARACTER
    FIELD numEentidadesConsolidan AS INTEGER
    FIELD pagos AS DECIMAL
    FIELD cesantias AS DECIMAL
    FIELD cesantiasAnt AS DECIMAL
    FIELD gastosRepresentacion AS DECIMAL
    FIELD pensiones AS DECIMAL
    FIELD otrosIngresos AS DECIMAL
    FIELD totalIngresos AS DECIMAL
    FIELD aportesSalud AS DECIMAL
    FIELD aportesObligatoriosPensiones AS DECIMAL
    FIELD aportesVoluntariosPensiones AS DECIMAL
    FIELD reteFuente AS DECIMAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Ftos

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Ano_Corte Cmb_Ftos tgTope Btn_Proc 
&Scoped-Define DISPLAYED-OBJECTS Ano_Corte Cmb_Ftos tope tgTope 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W_FtosDian AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Proc 
     LABEL "Generar" 
     SIZE 19.43 BY 1.19 TOOLTIP "Procesa y Genera el Formato Seleccionado".

DEFINE VARIABLE Cmb_Ftos AS CHARACTER FORMAT "X(85)":U 
     LABEL "Formatos Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 24
     DROP-DOWN-LIST
     SIZE 60.43 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Ano_Corte AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Año Gravable" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tope AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Tope" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tgTope AS LOGICAL INITIAL no 
     LABEL "Generar con tope" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.14 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Ftos
     Ano_Corte AT ROW 1.5 COL 21.43 COLON-ALIGNED
     Cmb_Ftos AT ROW 2.65 COL 21.43 COLON-ALIGNED
     tope AT ROW 3.81 COL 21.43 COLON-ALIGNED WIDGET-ID 2
     tgTope AT ROW 3.96 COL 41.29 WIDGET-ID 4
     Btn_Proc AT ROW 5.58 COL 64
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 83.72 BY 6.08.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W_FtosDian ASSIGN
         HIDDEN             = YES
         TITLE              = "Formatos DIAN, Programa W_FtosDian.W"
         HEIGHT             = 6.08
         WIDTH              = 83.72
         MAX-HEIGHT         = 36.62
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 36.62
         VIRTUAL-WIDTH      = 182.86
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W_FtosDian
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Ftos
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN tope IN FRAME F_Ftos
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W_FtosDian)
THEN W_FtosDian:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Ftos
/* Query rebuild information for FRAME F_Ftos
     _Query            is NOT OPENED
*/  /* FRAME F_Ftos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W_FtosDian
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FtosDian W_FtosDian
ON END-ERROR OF W_FtosDian /* Formatos DIAN, Programa W_FtosDian.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FtosDian W_FtosDian
ON WINDOW-CLOSE OF W_FtosDian /* Formatos DIAN, Programa W_FtosDian.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ano_Corte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ano_Corte W_FtosDian
ON LEAVE OF Ano_Corte IN FRAME F_Ftos /* Año Gravable */
DO:
   ASSIGN Ano_Corte.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proc W_FtosDian
ON CHOOSE OF Btn_Proc IN FRAME F_Ftos /* Generar */
DO:
    DEFINE VAR pOut_1 AS DECIMAL.
    DEFINE VAR pOut_2 AS DECIMAL.

    SESSION:SET-WAIT-STATE("GENERAL").

    CASE Cmb_Ftos:SCREEN-VALUE:
        WHEN "1001_5001" THEN
            RUN VALUE("p-1001_5001_Salarios_" + STRING(ano_corte) + ".r") (INPUT 'E',
                                                                           INPUT '',
                                                                           INPUT DECIMAL(tope:SCREEN-VALUE),
                                                                           OUTPUT pOut_1,
                                                                           OUTPUT pOut_2).


        WHEN "1001_5002" THEN
            RUN VALUE("p-1001_5002_Honorarios_" + STRING(ano_corte) + ".r") (INPUT 'E',
                                                                             INPUT '',
                                                                             INPUT DECIMAL(tope:SCREEN-VALUE),
                                                                             OUTPUT pOut_1,
                                                                             OUTPUT pOut_2).

        
        WHEN "1001_5003" THEN
            RUN VALUE("p-1001_5003_Comisiones_" + STRING(ano_corte) + ".r") (INPUT 'E',
                                                                             INPUT '',
                                                                             INPUT DECIMAL(tope:SCREEN-VALUE),
                                                                             OUTPUT pOut_1,
                                                                             OUTPUT pOut_2).

        WHEN "1001_5004" THEN
            RUN VALUE("p-1001_5004_Servicios_" + STRING(ano_corte) + ".r") (INPUT 'E',
                                                                             INPUT '',
                                                                             INPUT DECIMAL(tope:SCREEN-VALUE),
                                                                             OUTPUT pOut_1,
                                                                             OUTPUT pOut_2).

        WHEN "1001_5005" THEN
            RUN VALUE("p-1001_5005_Arrendamientos_" + STRING(ano_corte) + ".r") (INPUT 'E',
                                                                                 INPUT '',
                                                                                 INPUT DECIMAL(tope:SCREEN-VALUE),
                                                                                 OUTPUT pOut_1,
                                                                                 OUTPUT pOut_2).

        WHEN "Formato_1001_5006" THEN RUN 1001_5006.
        WHEN "Formato_1001_5008" THEN RUN 1001_5008.
        WHEN "Formato_1001_5010" THEN RUN 1001_5010.
        WHEN "Formato_1001_5011" THEN RUN 1001_5011.
        WHEN "Formato_1001_5012" THEN RUN 1001_5012.
        WHEN "Formato_1001_5015" THEN RUN 1001_5015.
        WHEN "Formato_1001_5016" THEN RUN 1001_5016.
        WHEN "Formato_1001_5055" THEN RUN 1001_5055.
        WHEN "Formato_1001_5056" THEN RUN 1001_5056.
        WHEN "Formato_1001_5059" THEN RUN 1001_5059.
        WHEN "Formato_1003" THEN RUN 1003_1306.
        WHEN "Formato_1005" THEN RUN 1005.
        WHEN "Formato_1006" THEN RUN 1006.
        WHEN "Formato_1007" THEN RUN 1007_4001.
        WHEN "Formato_1008" THEN RUN 1008.
        WHEN "Formato_1009" THEN RUN 1009.
        WHEN "Formato_1010" THEN RUN 1010.
        WHEN "Formato_1012" THEN RUN 1012.
        WHEN "Formato_1026" THEN RUN 1026.
        WHEN "Formato_2276" THEN RUN 2276.
        WHEN "SHD_ReteICA" THEN RUN SHD_ReteICA.
        WHEN "SHD_ReteICA_Antes2017" THEN RUN SHD_ReteICA_Antes2017.
        WHEN "SHD_CompraBienes&Servicios" THEN RUN SHD_CompraBienes&Servicios.
        WHEN "SHD_CompraBienes&Servicios_Antes2017" THEN RUN SHD_CompraBienes&Servicios_Antes2017.
        WHEN "SHD_VentaBienes&Servicios" THEN RUN SHD_VentaBienes&Servicios.
        WHEN "SHD_VentaBienes&Servicios_Antes2017" THEN RUN SHD_VentaBienes&Servicios_Antes2017.
    END CASE.

    SESSION:SET-WAIT-STATE("").

    MESSAGE "El formato fue generado en la Carpeta C:\INFO_FODUN\"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Ftos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Ftos W_FtosDian
ON VALUE-CHANGED OF Cmb_Ftos IN FRAME F_Ftos /* Formatos Disponibles */
DO:
    tope:VISIBLE = TRUE.
    tope:LABEL = "Tope".
    
    CASE Cmb_Ftos:SCREEN-VALUE:
        WHEN "1001_5001" THEN tope:SCREEN-VALUE = STRING(500000).
        WHEN "Formato_1003" THEN tope:LABEL = "Tope 1306".
        WHEN "Formato_1007" THEN tope:LABEL = "Tope 4001".
        WHEN "SHD_ReteICA" THEN tope:HIDDEN = TRUE.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tope
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tope W_FtosDian
ON LEAVE OF tope IN FRAME F_Ftos /* Tope */
DO:
   ASSIGN tope.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W_FtosDian 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.

    ASSIGN Ano_Corte = YEAR(TODAY) - 1
           Ano_Corte:SCREEN-VALUE = STRING(Ano_Corte).

    RUN Llena_Formatos.
        
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1001_5002 W_FtosDian 
PROCEDURE 1001_5002 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR topeAux AS DECIMAL.

EMPTY TEMP-TABLE F1001.
EMPTY TEMP-TABLE otros1001.
EMPTY TEMP-TABLE docs.

FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,8) = "51100101" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51100102" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51100103" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51100106" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51102711" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51102712" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51102713" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51102714" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51102715" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511030")
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1001 WHERE F1001.nit = anexos.nit
                          AND F1001.concepto = "5002" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            ASSIGN F1001.nit = anexos.nit
                   F1001.concepto = "5002".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1001.tipoDoc = "11".
                    WHEN "T.I" THEN F1001.tipoDoc = "12".
                    WHEN "C.C" THEN F1001.tipoDoc = "13".
                    WHEN "T.E" THEN F1001.tipoDoc = "21".
                    WHEN "C.E" THEN F1001.tipoDoc = "22".
                    WHEN "NIT" THEN F1001.tipoDoc = "31".
                    WHEN "PPTE" THEN F1001.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1001.apellido1 = clientes.apellido1.
                    F1001.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1001.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1001.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1001.direccion = clientes.DIR_residencia.
                ELSE
                    F1001.direccion = clientes.DIR_comercial.

                IF F1001.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1001.direccion = agencias.direccion.
                END.
    
                IF clientes.lugar_residencia <> "" THEN DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1001.departamento = "" OR F1001.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1001.departamento = "05".
                            F1001.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1001.departamento = "17".
                            F1001.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1001.departamento = "76".
                            F1001.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.
                    END CASE.
                END.
    
                F1001.pais = "169".
            END.
    
            F1001.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1001.dv) NO-ERROR.
        END.

        EMPTY TEMP-TABLE docs.
    END.

    F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.db[cont] - anexos.cr[cont].
    END.

    IF LAST-OF(anexos.cuenta) THEN DO:
        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = ano_corte NO-LOCK BREAK:
            FIND FIRST docs WHERE docs.nit = mov_contable.nit
                              AND docs.comprobante = mov_contable.comprobante
                              AND docs.agencia = mov_contable.agencia
                              AND docs.num_documento = mov_contable.num_documento
                              AND docs.fec_contable = mov_contable.fec_contable NO-LOCK NO-ERROR.
            IF NOT AVAILABLE docs THEN DO:
                CREATE docs.
                docs.nit = mov_contable.nit.
                docs.agencia = mov_contable.agencia.
                docs.comprobante = mov_contable.comprobante.
                docs.num_documento = mov_contable.num_documento.
                docs.fec_contable = mov_contable.fec_contable.

                FOR EACH bfrMovContable WHERE bfrMovContable.agencia = docs.agencia
                                          AND bfrMovContable.comprobante = docs.comprobante
                                          AND bfrMovContable.num_documento = docs.num_documento
                                          AND bfrMovContable.nit = docs.nit
                                          AND bfrMovContable.fec_contable = docs.fec_contable
                                          AND (SUBSTRING(bfrMovContable.cuenta,1,6) = "243515" OR
                                               SUBSTRING(bfrMovContable.cuenta,1,6) = "243525") NO-LOCK:
                    F1001.ReteFuenteRta = F1001.ReteFuenteRta + bfrMovContable.cr - bfrMovContable.db.
                END.
            END.
        END.
    
        F1001.ReteFuenteIvaAsumidaRegSimpl = ROUND((F1001.ReteFuenteRta * 96.5) / 100,0).
    END.
END.

FOR EACH F1001 WHERE F1001.concepto = "5002" NO-LOCK:
    totalReporte = totalReporte + F1001.pagosCostoDeduccion.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,6) = "511001" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "51102711" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "51102712" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "51102713" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "51102714" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "51102715" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "511030")
                        AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable - totalReporte <> 0 THEN DO:
    CREATE otros1001.
    otros1001.concepto = "5002".
    otros1001.nit = "222222222".
    otros1001.tipoDoc = "43".
    otros1001.pagosCostoDeduccion = totalContable - totalReporte.
END.

OUTPUT TO C:\INFO_Fodun\F1001_5002.csv.
FOR EACH F1001 WHERE F1001.pagosCostoDeduccion <> 0 NO-LOCK:
    topeAux = DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos).
            
    IF F1001.pagosCostoDeduccion >= topeAux THEN
        EXPORT DELIMITER ";" F1001.
    ELSE DO:
        FIND FIRST otros1001 WHERE otros1001.concepto = F1001.concepto NO-ERROR.
        IF NOT AVAILABLE otros1001 THEN DO:
            CREATE otros1001.
            otros1001.concepto = F1001.concepto.
            otros1001.nit = "222222222".
            otros1001.tipoDoc = "43".
        END.

        otros1001.pagosCostoDeduccion = otros1001.pagosCostoDeduccion + F1001.pagosCostoDeduccion.
        otros1001.ReteFuenteRta = otros1001.ReteFuenteRta + F1001.ReteFuenteRta.
        otros1001.ReteFuenteIvaAsumidaRegSimpl = otros1001.ReteFuenteIvaAsumidaRegSimpl + F1001.ReteFuenteIvaAsumidaRegSimpl.
    END.
END.

IF AVAILABLE otros1001 THEN
    EXPORT DELIMITER ";" otros1001.

OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1001_5006 W_FtosDian 
PROCEDURE 1001_5006 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR topeAux AS DECIMAL.
DEFINE VAR ctaRete AS CHARACTER.

EMPTY TEMP-TABLE F1001.
EMPTY TEMP-TABLE otros1001.
EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,8) = "52100505" OR
                       SUBSTRING(anexos.cuenta,1,6) = "615005" OR
                       SUBSTRING(anexos.cuenta,1,6) = "615010" OR
                       SUBSTRING(anexos.cuenta,1,6) = "615020" OR
                       SUBSTRING(anexos.cuenta,1,6) = "615035")
                  /*AND anexos.nit = "19393345"*/
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1001 WHERE F1001.nit = anexos.nit
                          AND F1001.concepto = "5006" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            ASSIGN F1001.nit = anexos.nit
                   F1001.concepto = "5006".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1001.tipoDoc = "11".
                    WHEN "T.I" THEN F1001.tipoDoc = "12".
                    WHEN "C.C" THEN F1001.tipoDoc = "13".
                    WHEN "T.E" THEN F1001.tipoDoc = "21".
                    WHEN "C.E" THEN F1001.tipoDoc = "22".
                    WHEN "NIT" THEN F1001.tipoDoc = "31".
                    WHEN "PPTE" THEN F1001.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1001.apellido1 = clientes.apellido1.
                    F1001.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1001.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1001.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1001.direccion = clientes.DIR_residencia.
                ELSE
                    F1001.direccion = clientes.DIR_comercial.

                IF F1001.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1001.direccion = agencias.direccion.
                END.
    
                IF clientes.lugar_residencia <> "" THEN DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1001.departamento = "" OR F1001.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1001.departamento = "05".
                            F1001.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1001.departamento = "17".
                            F1001.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1001.departamento = "76".
                            F1001.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.
                    END CASE.
                END.
    
                F1001.pais = "169".
            END.
    
            F1001.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1001.dv) NO-ERROR.
        END.

        EMPTY TEMP-TABLE docs.
    END.

    F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.db[cont] - anexos.cr[cont].
    END.
    
    IF LAST-OF(anexos.cuenta) THEN DO:
        IF SUBSTRING(anexos.cuenta,1,8) = "52100505" THEN
            ctaRete = "243525".
        ELSE
            ctaRete = "243535".

        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = ano_corte NO-LOCK BREAK:
            FIND FIRST docs WHERE docs.nit = mov_contable.nit
                              AND docs.comprobante = mov_contable.comprobante
                              AND docs.agencia = mov_contable.agencia
                              AND docs.num_documento = mov_contable.num_documento
                              AND docs.fec_contable = mov_contable.fec_contable NO-LOCK NO-ERROR.
            IF NOT AVAILABLE docs THEN DO:
                CREATE docs.
                docs.nit = mov_contable.nit.
                docs.agencia = mov_contable.agencia.
                docs.comprobante = mov_contable.comprobante.
                docs.num_documento = mov_contable.num_documento.
                docs.fec_contable = mov_contable.fec_contable.

                FOR EACH bfrMovContable WHERE bfrMovContable.agencia = docs.agencia
                                          AND bfrMovContable.comprobante = docs.comprobante
                                          AND bfrMovContable.num_documento = docs.num_documento
                                          AND bfrMovContable.nit = docs.nit
                                          AND bfrMovContable.fec_contable = docs.fec_contable
                                          AND SUBSTRING(bfrMovContable.cuenta,1,6) = ctaRete NO-LOCK:
                    F1001.ReteFuenteRta = F1001.ReteFuenteRta + bfrMovContable.cr - bfrMovContable.db.
                END.
            END.
        END.
    END.
END.

FOR EACH F1001 WHERE F1001.concepto = "5006" NO-LOCK:
    totalReporte = totalReporte + F1001.pagosCostoDeduccion.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,8) = "52100505" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "615005" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "615010" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "615020" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "615035")
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable - totalReporte <> 0 THEN DO:
    CREATE otros1001.
    otros1001.concepto = "5006".
    otros1001.nit = "222222222".
    otros1001.tipoDoc = "43".
    otros1001.pagosCostoDeduccion = totalContable - totalReporte.
END.
    
OUTPUT TO C:\INFO_Fodun\F1001_5006.csv.
FOR EACH F1001 WHERE F1001.pagosCostoDeduccion <> 0 NO-LOCK:
    topeAux = DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos).
            
    IF F1001.pagosCostoDeduccion >= topeAux THEN
        EXPORT DELIMITER ";" F1001.
    ELSE DO:
        FIND FIRST otros1001 WHERE otros1001.concepto = F1001.concepto NO-ERROR.
        IF NOT AVAILABLE otros1001 THEN DO:
            CREATE otros1001.
            otros1001.concepto = F1001.concepto.
            otros1001.nit = "222222222".
            otros1001.tipoDoc = "43".
        END.

        otros1001.pagosCostoDeduccion = otros1001.pagosCostoDeduccion + F1001.pagosCostoDeduccion.
        otros1001.ReteFuenteRta = otros1001.ReteFuenteRta + F1001.ReteFuenteRta.
        otros1001.ReteFuenteIvaAsumidaRegSimpl = otros1001.ReteFuenteIvaAsumidaRegSimpl + F1001.ReteFuenteIvaAsumidaRegSimpl.
    END.
END.

IF AVAILABLE otros1001 THEN
    EXPORT DELIMITER ";" otros1001.

OUTPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1001_5008 W_FtosDian 
PROCEDURE 1001_5008 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR topeAux AS DECIMAL.

EMPTY TEMP-TABLE F1001.
EMPTY TEMP-TABLE otros1001.
EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH rep_activosFijos WHERE rep_activosFijos.fecCorte = DATE(12,31,ano_corte)
                            AND YEAR(rep_activosFijos.fechaCompra) = ano_corte
                            AND rep_activosFijos.nitProveedor <> ""
                            AND rep_activosFijos.contabilizado = YES NO-LOCK BREAK BY rep_activosFijos.nitProveedor:
    IF FIRST-OF(rep_activosFijos.nitProveedor) THEN DO:
        FIND FIRST F1001 WHERE F1001.nit = rep_activosFijos.nitProveedor
                           AND F1001.concepto = "5008" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            ASSIGN F1001.nit = rep_activosFijos.nitProveedor
                   F1001.concepto = "5008".
    
            FIND FIRST clientes WHERE clientes.nit = rep_activosFijos.nitProveedor NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1001.tipoDoc = "11".
                    WHEN "T.I" THEN F1001.tipoDoc = "12".
                    WHEN "C.C" THEN F1001.tipoDoc = "13".
                    WHEN "T.E" THEN F1001.tipoDoc = "21".
                    WHEN "C.E" THEN F1001.tipoDoc = "22".
                    WHEN "NIT" THEN F1001.tipoDoc = "31".
                    WHEN "PPTE" THEN F1001.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1001.apellido1 = clientes.apellido1.
                    F1001.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1001.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1001.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1001.direccion = clientes.DIR_residencia.
                ELSE
                    F1001.direccion = clientes.DIR_comercial.

                IF F1001.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1001.direccion = agencias.direccion.
                END.
    
                IF clientes.lugar_residencia <> "" THEN DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1001.departamento = "" OR F1001.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1001.departamento = "05".
                            F1001.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1001.departamento = "17".
                            F1001.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1001.departamento = "76".
                            F1001.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.
                    END CASE.
                END.
    
                F1001.pais = "169".
            END.
    
            F1001.nit = rep_activosFijos.nitProveedor.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1001.dv) NO-ERROR.
        END.
    END.

    F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + rep_activosFijos.valorCompra.
    
    IF LAST-OF(rep_activosFijos.nitProveedor) THEN DO:
        FOR EACH mov_contable WHERE mov_contable.nit = rep_activosFijos.nitProveedor
                                AND YEAR(mov_contable.fec_contable) = ano_corte
                                AND SUBSTRING(mov_contable.cuenta,1,6) = "243540" NO-LOCK:
            FIND FIRST F1001 WHERE F1001.nit = rep_activosFijos.nitProveedor
                               AND F1001.concepto = "5008" NO-ERROR.
            IF NOT AVAILABLE F1001 THEN DO:
                CREATE F1001.
                ASSIGN F1001.nit = rep_activosFijos.nitProveedor
                       F1001.concepto = "5008".
            END.
                
            FIND FIRST ttmov WHERE ttmov.Id = ROWID(mov_contable) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttmov THEN DO:
                F1001.ReteFuenteRta = F1001.ReteFuenteRta + mov_contable.cr - mov_contable.db.

                CREATE ttmov.
                ttmov.id = ROWID(mov_contable).
            END.
        END.
    END.
END.
    
FOR EACH F1001 WHERE F1001.concepto = "5008" NO-LOCK:
    totalReporte = totalReporte + F1001.pagosCostoDeduccion.
END.

OUTPUT TO C:\INFO_Fodun\F1001_5008.csv.
FOR EACH F1001 WHERE F1001.pagosCostoDeduccion <> 0 NO-LOCK:
    topeAux = DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos).
            
    IF F1001.pagosCostoDeduccion >= topeAux THEN
        EXPORT DELIMITER ";" F1001.
    ELSE DO:
        FIND FIRST otros1001 WHERE otros1001.concepto = F1001.concepto NO-ERROR.
        IF NOT AVAILABLE otros1001 THEN DO:
            CREATE otros1001.
            otros1001.concepto = F1001.concepto.
            otros1001.nit = "222222222".
            otros1001.tipoDoc = "43".
        END.

        otros1001.pagosCostoDeduccion = otros1001.pagosCostoDeduccion + F1001.pagosCostoDeduccion.
        otros1001.ReteFuenteRta = otros1001.ReteFuenteRta + F1001.ReteFuenteRta.
        otros1001.ReteFuenteIvaAsumidaRegSimpl = otros1001.ReteFuenteIvaAsumidaRegSimpl + F1001.ReteFuenteIvaAsumidaRegSimpl.
    END.
END.

IF AVAILABLE otros1001 THEN
    EXPORT DELIMITER ";" otros1001.

OUTPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1001_5010 W_FtosDian 
PROCEDURE 1001_5010 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR topeAux AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.

EMPTY TEMP-TABLE F1001.
EMPTY TEMP-TABLE otros1001.
EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,6) = "510526" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510527" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510528" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101272" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101275" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101278")
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:

    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1001 WHERE F1001.nit = anexos.nit
                          AND F1001.concepto = "5010" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            ASSIGN F1001.nit = anexos.nit
                   F1001.concepto = "5010".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1001.tipoDoc = "11".
                    WHEN "T.I" THEN F1001.tipoDoc = "12".
                    WHEN "C.C" THEN F1001.tipoDoc = "13".
                    WHEN "T.E" THEN F1001.tipoDoc = "21".
                    WHEN "C.E" THEN F1001.tipoDoc = "22".
                    WHEN "NIT" THEN F1001.tipoDoc = "31".
                    WHEN "PPTE" THEN F1001.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1001.apellido1 = clientes.apellido1.
                    F1001.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1001.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1001.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1001.direccion = clientes.DIR_residencia.
                ELSE
                    F1001.direccion = clientes.DIR_comercial.

                IF F1001.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1001.direccion = agencias.direccion.
                END.
                
                IF clientes.lugar_residencia <> "" THEN DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1001.departamento = "" OR F1001.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1001.departamento = "05".
                            F1001.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1001.departamento = "17".
                            F1001.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1001.departamento = "76".
                            F1001.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.
                    END CASE.
                END.

                F1001.pais = "169".
            END.
    
            F1001.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1001.dv) NO-ERROR.
        END.
    END.

    F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.sdo_inicial.
        
    DO cont = 1 TO 12:
        F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.db[cont] - anexos.cr[cont].
    END.
END.

FOR EACH F1001 WHERE F1001.concepto = "5010" NO-LOCK:
    totalReporte = totalReporte + F1001.pagosCostoDeduccion.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,6) = "510526" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "510527" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "510528" OR
                           SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101272" OR
                           SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101275" OR
                           SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101278")
                      AND sal_cuenta.ano = ano_corte NO-LOCK:               
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable - totalReporte <> 0 THEN DO:
    CREATE otros1001.
    otros1001.concepto = "5010".
    otros1001.nit = "222222222".
    otros1001.tipoDoc = "43".
    otros1001.pagosCostoDeduccion = totalContable - totalReporte.
END.

OUTPUT TO C:\INFO_Fodun\F1001_5010.csv.
FOR EACH F1001 WHERE F1001.pagosCostoDeduccion <> 0 NO-LOCK:
    topeAux = DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos).
            
    IF F1001.pagosCostoDeduccion >= topeAux THEN
        EXPORT DELIMITER ";" F1001.
    ELSE DO:
        FIND FIRST otros1001 WHERE otros1001.concepto = F1001.concepto NO-ERROR.
        IF NOT AVAILABLE otros1001 THEN DO:
            CREATE otros1001.
            otros1001.concepto = F1001.concepto.
            otros1001.nit = "222222222".
            otros1001.tipoDoc = "43".
        END.

        otros1001.pagosCostoDeduccion = otros1001.pagosCostoDeduccion + F1001.pagosCostoDeduccion.
        otros1001.ReteFuenteRta = otros1001.ReteFuenteRta + F1001.ReteFuenteRta.
        otros1001.ReteFuenteIvaAsumidaRegSimpl = otros1001.ReteFuenteIvaAsumidaRegSimpl + F1001.ReteFuenteIvaAsumidaRegSimpl.
    END.
END.

IF AVAILABLE otros1001 THEN
    EXPORT DELIMITER ";" otros1001.

OUTPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1001_5011 W_FtosDian 
PROCEDURE 1001_5011 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR topeAux AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.

EMPTY TEMP-TABLE F1001.
EMPTY TEMP-TABLE otros1001.
EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,6) = "510523" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510525" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101269" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101271")
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1001 WHERE F1001.nit = anexos.nit
                          AND F1001.concepto = "5011" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            ASSIGN F1001.nit = anexos.nit
                   F1001.concepto = "5011".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1001.tipoDoc = "11".
                    WHEN "T.I" THEN F1001.tipoDoc = "12".
                    WHEN "C.C" THEN F1001.tipoDoc = "13".
                    WHEN "T.E" THEN F1001.tipoDoc = "21".
                    WHEN "C.E" THEN F1001.tipoDoc = "22".
                    WHEN "NIT" THEN F1001.tipoDoc = "31".
                    WHEN "PPTE" THEN F1001.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1001.apellido1 = clientes.apellido1.
                    F1001.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1001.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1001.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1001.direccion = clientes.DIR_residencia.
                ELSE
                    F1001.direccion = clientes.DIR_comercial.

                IF F1001.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1001.direccion = agencias.direccion.
                END.
                
                IF clientes.lugar_residencia <> "" THEN DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1001.departamento = "" OR F1001.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1001.departamento = "05".
                            F1001.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1001.departamento = "17".
                            F1001.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1001.departamento = "76".
                            F1001.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.
                    END CASE.
                END.

                F1001.pais = "169".
            END.
    
            F1001.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1001.dv) NO-ERROR.
        END.
    END.

    F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.sdo_inicial.
        
    DO cont = 1 TO 12:
        F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.db[cont] - anexos.cr[cont].
    END.
END.

FOR EACH F1001 WHERE F1001.concepto = "5011" NO-LOCK:
    totalReporte = totalReporte + F1001.pagosCostoDeduccion.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,6) = "510523" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "510525" OR
                           SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101269" OR
                           SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101271")
                      AND sal_cuenta.ano = ano_corte NO-LOCK:               
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable - totalReporte <> 0 THEN DO:
    CREATE otros1001.
    otros1001.concepto = "5011".
    otros1001.nit = "222222222".
    otros1001.tipoDoc = "43".
    otros1001.pagosCostoDeduccion = totalContable - totalReporte.
END.



OUTPUT TO C:\INFO_Fodun\F1001_5011.csv.
FOR EACH F1001 WHERE F1001.pagosCostoDeduccion <> 0 NO-LOCK:
    topeAux = DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos).
            
    IF F1001.pagosCostoDeduccion >= topeAux THEN
        EXPORT DELIMITER ";" F1001.
    ELSE DO:
        FIND FIRST otros1001 WHERE otros1001.concepto = F1001.concepto NO-ERROR.
        IF NOT AVAILABLE otros1001 THEN DO:
            CREATE otros1001.
            otros1001.concepto = F1001.concepto.
            otros1001.nit = "222222222".
            otros1001.tipoDoc = "43".
        END.

        otros1001.pagosCostoDeduccion = otros1001.pagosCostoDeduccion + F1001.pagosCostoDeduccion.
        otros1001.ReteFuenteRta = otros1001.ReteFuenteRta + F1001.ReteFuenteRta.
        otros1001.ReteFuenteIvaAsumidaRegSimpl = otros1001.ReteFuenteIvaAsumidaRegSimpl + F1001.ReteFuenteIvaAsumidaRegSimpl.
    END.
END.

IF AVAILABLE otros1001 THEN
    EXPORT DELIMITER ";" otros1001.

OUTPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1001_5012 W_FtosDian 
PROCEDURE 1001_5012 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR topeAux AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.

EMPTY TEMP-TABLE F1001.
EMPTY TEMP-TABLE otros1001.
EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,6) = "510524" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101270")
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1001 WHERE F1001.nit = anexos.nit
                          AND F1001.concepto = "5012" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            ASSIGN F1001.nit = anexos.nit
                   F1001.concepto = "5012".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1001.tipoDoc = "11".
                    WHEN "T.I" THEN F1001.tipoDoc = "12".
                    WHEN "C.C" THEN F1001.tipoDoc = "13".
                    WHEN "T.E" THEN F1001.tipoDoc = "21".
                    WHEN "C.E" THEN F1001.tipoDoc = "22".
                    WHEN "NIT" THEN F1001.tipoDoc = "31".
                    WHEN "PPTE" THEN F1001.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1001.apellido1 = clientes.apellido1.
                    F1001.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1001.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1001.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1001.direccion = clientes.DIR_residencia.
                ELSE
                    F1001.direccion = clientes.DIR_comercial.

                IF F1001.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1001.direccion = agencias.direccion.
                END.
                
                IF clientes.lugar_residencia <> "" THEN DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1001.departamento = "" OR F1001.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1001.departamento = "05".
                            F1001.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1001.departamento = "17".
                            F1001.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1001.departamento = "76".
                            F1001.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.
                    END CASE.
                END.

                F1001.pais = "169".
            END.
    
            F1001.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1001.dv) NO-ERROR.
        END.
    END.

    F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.sdo_inicial.
        
    DO cont = 1 TO 12:
        F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.db[cont] - anexos.cr[cont].
    END.
END.

FOR EACH F1001 WHERE F1001.concepto = "5012" NO-LOCK:
    totalReporte = totalReporte + F1001.pagosCostoDeduccion.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,6) = "510524" OR
                           SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101270")
                      AND sal_cuenta.ano = ano_corte NO-LOCK:               
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable - totalReporte <> 0 THEN DO:
    CREATE otros1001.
    otros1001.concepto = "5012".
    otros1001.nit = "222222222".
    otros1001.tipoDoc = "43".
    otros1001.pagosCostoDeduccion = totalContable - totalReporte.
END.



OUTPUT TO C:\INFO_Fodun\F1001_5012.csv.
FOR EACH F1001 WHERE F1001.pagosCostoDeduccion <> 0 NO-LOCK:
    topeAux = DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos).
            
    IF F1001.pagosCostoDeduccion >= topeAux THEN
        EXPORT DELIMITER ";" F1001.
    ELSE DO:
        FIND FIRST otros1001 WHERE otros1001.concepto = F1001.concepto NO-ERROR.
        IF NOT AVAILABLE otros1001 THEN DO:
            CREATE otros1001.
            otros1001.concepto = F1001.concepto.
            otros1001.nit = "222222222".
            otros1001.tipoDoc = "43".
        END.

        otros1001.pagosCostoDeduccion = otros1001.pagosCostoDeduccion + F1001.pagosCostoDeduccion.
        otros1001.ReteFuenteRta = otros1001.ReteFuenteRta + F1001.ReteFuenteRta.
        otros1001.ReteFuenteIvaAsumidaRegSimpl = otros1001.ReteFuenteIvaAsumidaRegSimpl + F1001.ReteFuenteIvaAsumidaRegSimpl.
    END.
END.

IF AVAILABLE otros1001 THEN
    EXPORT DELIMITER ";" otros1001.

OUTPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1001_5015 W_FtosDian 
PROCEDURE 1001_5015 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR topeAux AS DECIMAL.

EMPTY TEMP-TABLE F1001.
EMPTY TEMP-TABLE otros1001.
EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,8) = "51100201" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51100202" OR
                       SUBSTRING(anexos.cuenta,1,8) = "52305001" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101102" OR
                       SUBSTRING(anexos.cuenta,1,8) = "61509501")
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        EMPTY TEMP-TABLE docs.

        FIND FIRST F1001 WHERE F1001.nit = anexos.nit
                          AND F1001.concepto = "5015" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            ASSIGN F1001.nit = anexos.nit
                   F1001.concepto = "5015".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1001.tipoDoc = "11".
                    WHEN "T.I" THEN F1001.tipoDoc = "12".
                    WHEN "C.C" THEN F1001.tipoDoc = "13".
                    WHEN "T.E" THEN F1001.tipoDoc = "21".
                    WHEN "C.E" THEN F1001.tipoDoc = "22".
                    WHEN "NIT" THEN F1001.tipoDoc = "31".
                    WHEN "PPTE" THEN F1001.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1001.apellido1 = clientes.apellido1.
                    F1001.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1001.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1001.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1001.direccion = clientes.DIR_residencia.
                ELSE
                    F1001.direccion = clientes.DIR_comercial.

                IF F1001.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1001.direccion = agencias.direccion.
                END.

                IF clientes.lugar_residencia <> "" THEN DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1001.departamento = "" OR F1001.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1001.departamento = "05".
                            F1001.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1001.departamento = "17".
                            F1001.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1001.departamento = "76".
                            F1001.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.
                    END CASE.
                END.

                F1001.pais = "169".
            END.
    
            F1001.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1001.dv) NO-ERROR.
        END.
    END.

    F1001.pagosNoCostoNoDeduccion = F1001.pagosNoCostoNoDeduccion + anexos.sdo_inicial.
    
    DO cont = 1 TO 12:
        F1001.pagosNoCostoNoDeduccion = F1001.pagosNoCostoNoDeduccion + anexos.db[cont] - anexos.cr[cont].
    END.

    IF LAST-OF(anexos.cuenta) THEN DO:
        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = ano_corte NO-LOCK BREAK:
            FIND FIRST docs WHERE docs.nit = mov_contable.nit
                              AND docs.comprobante = mov_contable.comprobante
                              AND docs.agencia = mov_contable.agencia
                              AND docs.num_documento = mov_contable.num_documento
                              AND docs.fec_contable = mov_contable.fec_contable NO-LOCK NO-ERROR.
            IF NOT AVAILABLE docs THEN DO:
                CREATE docs.
                docs.nit = mov_contable.nit.
                docs.agencia = mov_contable.agencia.
                docs.comprobante = mov_contable.comprobante.
                docs.num_documento = mov_contable.num_documento.
                docs.fec_contable = mov_contable.fec_contable.
            
                FOR EACH bfrMovContable WHERE bfrMovContable.agencia = docs.agencia
                                          AND bfrMovContable.comprobante = docs.comprobante
                                          AND bfrMovContable.num_documento = docs.num_documento
                                          AND bfrMovContable.nit = docs.nit
                                          AND bfrMovContable.fec_contable = docs.fec_contable
                                          AND SUBSTRING(bfrMovContable.cuenta,1,4) = "2435" NO-LOCK:
                    F1001.ReteFuenteRta = F1001.ReteFuenteRta + bfrMovContable.cr - bfrMovContable.db.
                END.
            END.
        END.
    
        F1001.ReteFuenteIvaAsumidaRegSimpl = ROUND((F1001.ReteFuenteRta * 96.5) / 100,0).
    END.
END.

FOR EACH F1001 WHERE F1001.concepto = "5015" NO-LOCK:
    totalReporte = totalReporte + F1001.pagosNoCostoNoDeduccion.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,8) = "51100201" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "51100202" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "52305001" OR
                           SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101102" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "61509501")
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1001.
    otros1001.concepto = "5015".
    otros1001.nit = "222222222".
    otros1001.tipoDoc = "43".
    otros1001.pagosNoCostoNoDeduccion = totalContable - totalReporte.
END.

OUTPUT TO C:\INFO_Fodun\F1001_5015.csv.
FOR EACH F1001 WHERE F1001.pagosNoCostoNoDeduccion <> 0 NO-LOCK:
    topeAux = DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos).
            
    IF F1001.pagosNoCostoNoDeduccion >= topeAux THEN
        EXPORT DELIMITER ";" F1001.
    ELSE DO:
        FIND FIRST otros1001 WHERE otros1001.concepto = F1001.concepto NO-ERROR.
        IF NOT AVAILABLE otros1001 THEN DO:
            CREATE otros1001.
            otros1001.concepto = F1001.concepto.
            otros1001.nit = "222222222".
            otros1001.tipoDoc = "43".
        END.

        otros1001.pagosNoCostoNoDeduccion = otros1001.pagosNoCostoNoDeduccion + F1001.pagosNoCostoNoDeduccion.
        otros1001.ReteFuenteRta = otros1001.ReteFuenteRta + F1001.ReteFuenteRta.
        otros1001.ReteFuenteIvaAsumidaRegSimpl = otros1001.ReteFuenteIvaAsumidaRegSimpl + F1001.ReteFuenteIvaAsumidaRegSimpl.
    END.
END.

IF AVAILABLE otros1001 THEN
    EXPORT DELIMITER ";" otros1001.

OUTPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1001_5016 W_FtosDian 
PROCEDURE 1001_5016 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR topeAux AS DECIMAL.
DEFINE VAR nitTemporal AS CHARACTER.
DEFINE VAR ctaRete1 AS CHARACTER.
DEFINE VAR ctaRete2 AS CHARACTER.
DEFINE VAR ctaRete3 AS CHARACTER.

EMPTY TEMP-TABLE F1001.
EMPTY TEMP-TABLE otros1001.
EMPTY TEMP-TABLE docs.

FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,4) = "2695" OR SUBSTRING(anexos.cuenta,1,6) = "262505" OR SUBSTRING(anexos.cuenta,1,6) = "510521" OR SUBSTRING(anexos.cuenta,1,6) = "510595" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511002" OR SUBSTRING(anexos.cuenta,1,6) = "511005" OR SUBSTRING(anexos.cuenta,1,6) = "511006" OR SUBSTRING(anexos.cuenta,1,6) = "511007" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511009" OR SUBSTRING(anexos.cuenta,1,6) = "511010" OR SUBSTRING(anexos.cuenta,1,6) = "511011" OR SUBSTRING(anexos.cuenta,1,6) = "511012" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511013" OR SUBSTRING(anexos.cuenta,1,6) = "511014" OR SUBSTRING(anexos.cuenta,1,6) = "511015" OR SUBSTRING(anexos.cuenta,1,6) = "511016" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511018" OR SUBSTRING(anexos.cuenta,1,6) = "511019" OR SUBSTRING(anexos.cuenta,1,6) = "511020" OR SUBSTRING(anexos.cuenta,1,6) = "511021" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511022" OR SUBSTRING(anexos.cuenta,1,6) = "511023" OR SUBSTRING(anexos.cuenta,1,6) = "511024" OR SUBSTRING(anexos.cuenta,1,6) = "511025" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511026" OR SUBSTRING(anexos.cuenta,1,8) = "51102701" OR SUBSTRING(anexos.cuenta,1,8) = "51102702" OR SUBSTRING(anexos.cuenta,1,8) = "51102703" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51102704" OR SUBSTRING(anexos.cuenta,1,8) = "51102705" OR SUBSTRING(anexos.cuenta,1,6) = "511031" OR SUBSTRING(anexos.cuenta,1,6) = "511032" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511095" OR SUBSTRING(anexos.cuenta,1,4) = "5115" OR SUBSTRING(anexos.cuenta,1,4) = "5125" OR SUBSTRING(anexos.cuenta,1,6) = "521005" OR
                       SUBSTRING(anexos.cuenta,1,6) = "521020" OR SUBSTRING(anexos.cuenta,1,6) = "523020" OR SUBSTRING(anexos.cuenta,1,6) = "523035" OR /*SUBSTRING(anexos.cuenta,1,6) = "523050" OR*/
                       SUBSTRING(anexos.cuenta,1,10) = "6140101102" OR SUBSTRING(anexos.cuenta,1,10) = "6140101108" OR SUBSTRING(anexos.cuenta,1,10) = "6140101110" OR SUBSTRING(anexos.cuenta,1,10) = "6140101114" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101116" OR SUBSTRING(anexos.cuenta,1,10) = "6140101118" OR SUBSTRING(anexos.cuenta,1,10) = "6140101122" OR SUBSTRING(anexos.cuenta,1,10) = "6140101136" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101140" OR SUBSTRING(anexos.cuenta,1,10) = "6140101195" OR SUBSTRING(anexos.cuenta,1,10) = "6140101251" OR SUBSTRING(anexos.cuenta,1,8) = "61509501" OR
                       SUBSTRING(anexos.cuenta,1,8) = "61509502" OR SUBSTRING(anexos.cuenta,1,8) = "61509512")
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    nitTemporal = anexos.nit.

    IF FIRST-OF(anexos.nit) THEN DO:
        EMPTY TEMP-TABLE docs.

        IF SUBSTRING(anexos.cuenta,1,4) = "5115" OR SUBSTRING(anexos.cuenta,1,4) = "5125" THEN
                nitTemporal = "800112808".
                
        FIND FIRST F1001 WHERE F1001.nit = nitTemporal
                           AND F1001.concepto = "5016" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            ASSIGN F1001.nit = nitTemporal
                   F1001.concepto = "5016".
    
            FIND FIRST clientes WHERE clientes.nit = nitTemporal NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1001.tipoDoc = "11".
                    WHEN "T.I" THEN F1001.tipoDoc = "12".
                    WHEN "C.C" THEN F1001.tipoDoc = "13".
                    WHEN "T.E" THEN F1001.tipoDoc = "21".
                    WHEN "C.E" THEN F1001.tipoDoc = "22".
                    WHEN "NIT" THEN F1001.tipoDoc = "31".
                    WHEN "PPTE" THEN F1001.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1001.apellido1 = clientes.apellido1.
                    F1001.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1001.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1001.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1001.direccion = clientes.DIR_residencia.
                ELSE
                    F1001.direccion = clientes.DIR_comercial.

                IF F1001.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1001.direccion = agencias.direccion.
                END.

                IF clientes.lugar_residencia <> "" AND clientes.lugar_residencia <> ? THEN DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1001.departamento = "" OR F1001.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1001.departamento = "05".
                            F1001.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1001.departamento = "17".
                            F1001.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1001.departamento = "76".
                            F1001.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.
                    END CASE.
                END.

                F1001.pais = "169".
            END.
    
            F1001.nit = nitTemporal.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1001.dv) NO-ERROR.
        END.
    END.

    IF SUBSTRING(anexos.cuenta,1,2) <> "26" THEN
        F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.sdo_inicial.
        
    DO cont = 1 TO 12:
        IF SUBSTRING(anexos.cuenta,1,2) <> "26" THEN
            F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.db[cont] - anexos.cr[cont].
        ELSE DO:
            F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.db[cont].

            FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                    AND mov_contable.nit = anexos.nit
                                    AND YEAR(mov_contable.fec_contable) = ano_corte
                                    AND MONTH(mov_contable.fec_contable) = cont
                                    AND (mov_contable.comprobante = 9 OR INDEX(mov_contable.comentario,"rev") > 0 OR INDEX(mov_contable.comentario,"reclasif") > 0)
                                    AND mov_contable.cen_costos = anexos.cen_costos
                                    AND mov_contable.cr > 0 NO-LOCK:
                F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion - mov_contable.cr.
            END.

        END.
    END.

    IF LAST-OF(anexos.cuenta) THEN DO:
        IF SUBSTRING(anexos.cuenta,1,6) = "511005" OR SUBSTRING(anexos.cuenta,1,6) = "511013" OR SUBSTRING(anexos.cuenta,1,6) = "511014" OR SUBSTRING(anexos.cuenta,1,6) = "511016" OR SUBSTRING(anexos.cuenta,1,6) = "511019" OR
           SUBSTRING(anexos.cuenta,1,6) = "511024" OR SUBSTRING(anexos.cuenta,1,6) = "511025" OR SUBSTRING(anexos.cuenta,1,8) = "51102701" OR SUBSTRING(anexos.cuenta,1,8) = "51102702" OR SUBSTRING(anexos.cuenta,1,8) = "51102703" OR
           SUBSTRING(anexos.cuenta,1,8) = "51102704" OR SUBSTRING(anexos.cuenta,1,8) = "51102705" OR SUBSTRING(anexos.cuenta,1,6) = "511031" OR SUBSTRING(anexos.cuenta,1,6) = "511032" OR SUBSTRING(anexos.cuenta,1,8) = "52100501" OR
           SUBSTRING(anexos.cuenta,1,8) = "52100502" OR SUBSTRING(anexos.cuenta,1,8) = "52100504" OR SUBSTRING(anexos.cuenta,1,8) = "52100510" OR SUBSTRING(anexos.cuenta,1,6) = "521020" OR
           SUBSTRING(anexos.cuenta,1,10) = "6140101108" OR SUBSTRING(anexos.cuenta,1,10) = "6140101114" OR SUBSTRING(anexos.cuenta,1,10) = "6140101136" OR SUBSTRING(anexos.cuenta,1,10) = "6140101140" OR
            SUBSTRING(anexos.cuenta,1,8) = "61509502" OR SUBSTRING(anexos.cuenta,1,8) = "61509512" THEN
            ASSIGN ctaRete1 = "243525"
                   ctaRete2 = "243525"
                   ctaRete3 = "243525".

        IF SUBSTRING(anexos.cuenta,1,6) = "510521" OR SUBSTRING(anexos.cuenta,1,6) = "511011" OR SUBSTRING(anexos.cuenta,1,6) = "511015" OR SUBSTRING(anexos.cuenta,1,10) = "6140101195" OR
            SUBSTRING(anexos.cuenta,1,10) = "6140101251" THEN
            ASSIGN ctaRete1 = "243540"
                   ctaRete2 = "243540"
                   ctaRete3 = "243540".

        IF SUBSTRING(anexos.cuenta,1,4) = "2695" THEN
            ASSIGN ctaRete1 = "243585"
                   ctaRete2 = "243585"
                   ctaRete3 = "243585".

        IF SUBSTRING(anexos.cuenta,1,6) = "511006" OR SUBSTRING(anexos.cuenta,1,6) = "511007" OR SUBSTRING(anexos.cuenta,1,6) = "511009" OR SUBSTRING(anexos.cuenta,1,6) = "511010" OR SUBSTRING(anexos.cuenta,1,6) = "511018" OR
           SUBSTRING(anexos.cuenta,1,6) = "511020" OR SUBSTRING(anexos.cuenta,1,6) = "511021" OR SUBSTRING(anexos.cuenta,1,6) = "511022" OR SUBSTRING(anexos.cuenta,1,6) = "511023" OR SUBSTRING(anexos.cuenta,1,6) = "511026" OR
           SUBSTRING(anexos.cuenta,1,6) = "511095" OR SUBSTRING(anexos.cuenta,1,10) = "6140101110" OR SUBSTRING(anexos.cuenta,1,10) = "6140101116" OR SUBSTRING(anexos.cuenta,1,10) = "6140101118" THEN
            ASSIGN ctaRete1 = "243525"
                   ctaRete2 = "243540"
                   ctaRete3 = "243540".

        IF SUBSTRING(anexos.cuenta,1,6) = "262505" THEN
            ASSIGN ctaRete1 = "243585"
                   ctaRete2 = "243525"
                   ctaRete3 = "243540".


        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND YEAR(mov_contable.fec_contable) = ano_corte
                                AND mov_contable.nit = anexos.nit NO-LOCK BREAK:
            FIND FIRST docs WHERE docs.nit = mov_contable.nit
                              AND docs.comprobante = mov_contable.comprobante
                              AND docs.agencia = mov_contable.agencia
                              AND docs.num_documento = mov_contable.num_documento
                              AND docs.fec_contable = mov_contable.fec_contable NO-LOCK NO-ERROR.
            IF NOT AVAILABLE docs THEN DO:
                CREATE docs.
                docs.nit = mov_contable.nit.
                docs.agencia = mov_contable.agencia.
                docs.comprobante = mov_contable.comprobante.
                docs.num_documento = mov_contable.num_documento.
                docs.fec_contable = mov_contable.fec_contable.
            
                FOR EACH bfrMovContable WHERE bfrMovContable.agencia = docs.agencia
                                          AND bfrMovContable.comprobante = docs.comprobante
                                          AND bfrMovContable.num_documento = docs.num_documento
                                          AND bfrMovContable.nit = docs.nit
                                          AND bfrMovContable.fec_contable = docs.fec_contable
                                          AND (SUBSTRING(bfrMovContable.cuenta,1,6) = ctaRete1 OR
                                               SUBSTRING(bfrMovContable.cuenta,1,6) = ctaRete2 OR
                                               SUBSTRING(bfrMovContable.cuenta,1,6) = ctaRete3) NO-LOCK:
                    IF bfrMovContable.nit <> "800197268" THEN
                        F1001.ReteFuenteRta = F1001.ReteFuenteRta + bfrMovContable.cr - bfrMovContable.db.
                    ELSE
                        F1001.ReteFuenteRta = F1001.ReteFuenteRta + bfrMovContable.cr.
                END.
            END.
        END.
    
        F1001.ReteFuenteIvaAsumidaRegSimpl = ROUND((F1001.ReteFuenteRta * 96.5) / 100,0).
    END.
END.

FOR EACH F1001 WHERE F1001.concepto = "5016" NO-LOCK:
    totalReporte = totalReporte + F1001.pagosCostoDeduccion.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "2695" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "262505" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "510521" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "510595" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "511002" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511005" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511006" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511007" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "511009" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511010" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511011" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511012" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "511013" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511014" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511015" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511016" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "511018" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511019" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511020" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511021" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "511022" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511023" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511024" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511025" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "511026" OR SUBSTRING(sal_cuenta.cuenta,1,8) = "51102701" OR SUBSTRING(sal_cuenta.cuenta,1,8) = "51102702" OR SUBSTRING(sal_cuenta.cuenta,1,8) = "51102703" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "51102704" OR SUBSTRING(sal_cuenta.cuenta,1,8) = "51102705" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511031" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "511032" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "511095" OR SUBSTRING(sal_cuenta.cuenta,1,4) = "5115" OR SUBSTRING(sal_cuenta.cuenta,1,4) = "5125" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "521005" OR
                           SUBSTRING(sal_cuenta.cuenta,1,6) = "521020" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "523020" OR SUBSTRING(sal_cuenta.cuenta,1,6) = "523035" OR /*SUBSTRING(sal_cuenta.cuenta,1,6) = "523050" OR*/
                           SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101102" OR SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101108" OR SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101110" OR SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101114" OR
                           SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101116" OR SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101118" OR SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101122" OR SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101136" OR
                           SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101140" OR SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101195" OR SUBSTRING(sal_cuenta.cuenta,1,10) = "6140101251" OR SUBSTRING(sal_cuenta.cuenta,1,8) = "61509501" OR
                           SUBSTRING(sal_cuenta.cuenta,1,8) = "61509502" OR SUBSTRING(sal_cuenta.cuenta,1,8) = "61509512")
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    IF SUBSTRING(sal_cuenta.cuenta,1,2) <> "26" THEN
        totalContable = totalContable + sal_cuenta.sal_inicial.
        
    DO cont = 1 TO 12:
        IF SUBSTRING(sal_cuenta.cuenta,1,2) <> "26" THEN
            totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
        ELSE DO:
            totalContable = totalContable + sal_cuenta.db[cont].
            
            FOR EACH mov_contable WHERE mov_contable.cuenta = sal_cuenta.cuenta
                                    AND YEAR(mov_contable.fec_contable) = ano_corte
                                    AND MONTH(mov_contable.fec_contable) = cont
                                    AND (mov_contable.comprobante = 9 OR INDEX(mov_contable.comentario,"rev") > 0)
                                    AND mov_contable.cen_costos = sal_cuenta.cen_costos
                                    AND mov_contable.cr > 0 NO-LOCK:
                totalContable = totalContable - mov_contable.cr.
            END.
        END.
    END.
END.

IF totalContable <> totalReporte THEN DO:
    CREATE otros1001.
    otros1001.concepto = "5016".
    otros1001.nit = "222222222".
    otros1001.tipoDoc = "43".
    otros1001.pagosCostoDeduccion = totalContable - totalReporte.
END.

OUTPUT TO C:\INFO_Fodun\F1001_5016.csv.
FOR EACH F1001 WHERE F1001.pagosCostoDeduccion <> 0 NO-LOCK:
    topeAux = DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos).
            
    IF F1001.pagosCostoDeduccion >= topeAux THEN
        EXPORT DELIMITER ";" F1001.
    ELSE DO:
        FIND FIRST otros1001 WHERE otros1001.concepto = F1001.concepto NO-ERROR.
        IF NOT AVAILABLE otros1001 THEN DO:
            CREATE otros1001.
            otros1001.concepto = F1001.concepto.
            otros1001.nit = "222222222".
            otros1001.tipoDoc = "43".
        END.

        otros1001.pagosCostoDeduccion = otros1001.pagosCostoDeduccion + F1001.pagosCostoDeduccion.
        otros1001.ReteFuenteRta = otros1001.ReteFuenteRta + F1001.ReteFuenteRta.
        otros1001.ReteFuenteIvaAsumidaRegSimpl = otros1001.ReteFuenteIvaAsumidaRegSimpl + F1001.ReteFuenteIvaAsumidaRegSimpl.
    END.
END.


IF AVAILABLE otros1001 THEN
    EXPORT DELIMITER ";" otros1001.

OUTPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1001_5055 W_FtosDian 
PROCEDURE 1001_5055 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR topeAux AS DECIMAL.

EMPTY TEMP-TABLE F1001.
EMPTY TEMP-TABLE otros1001.
EMPTY TEMP-TABLE docs.

FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,6) = "510507"
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1001 WHERE F1001.nit = anexos.nit
                          AND F1001.concepto = "5055" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            ASSIGN F1001.nit = anexos.nit
                   F1001.concepto = "5055".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1001.tipoDoc = "11".
                    WHEN "T.I" THEN F1001.tipoDoc = "12".
                    WHEN "C.C" THEN F1001.tipoDoc = "13".
                    WHEN "T.E" THEN F1001.tipoDoc = "21".
                    WHEN "C.E" THEN F1001.tipoDoc = "22".
                    WHEN "NIT" THEN F1001.tipoDoc = "31".
                    WHEN "PPTE" THEN F1001.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1001.apellido1 = clientes.apellido1.
                    F1001.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1001.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1001.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1001.direccion = clientes.DIR_residencia.
                ELSE
                    F1001.direccion = clientes.DIR_comercial.
                
                IF F1001.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1001.direccion = agencias.direccion.
                END.
    
                IF clientes.lugar_residencia <> "" THEN DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1001.departamento = "" OR F1001.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1001.departamento = "05".
                            F1001.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1001.departamento = "17".
                            F1001.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1001.departamento = "76".
                            F1001.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.
                    END CASE.
                END.

                F1001.pais = "169".
            END.
    
            F1001.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1001.dv) NO-ERROR.
        END.
    END.

    F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.db[cont] - anexos.cr[cont].
    END.
END.

FOR EACH F1001 WHERE F1001.concepto = "5055" NO-LOCK:
    totalReporte = totalReporte + F1001.pagosCostoDeduccion.
END.

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,6) = "510507"
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1001.
    otros1001.concepto = "5055".
    otros1001.nit = "222222222".
    otros1001.tipoDoc = "43".
    otros1001.pagosCostoDeduccion = totalContable - totalReporte.
END.
    
OUTPUT TO C:\INFO_Fodun\F1001_5055.csv.
FOR EACH F1001 WHERE F1001.pagosCostoDeduccion <> 0 NO-LOCK:
    topeAux = DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos).
            
    IF F1001.pagosCostoDeduccion >= topeAux THEN
        EXPORT DELIMITER ";" F1001.
    ELSE DO:
        FIND FIRST otros1001 WHERE otros1001.concepto = F1001.concepto NO-ERROR.
        IF NOT AVAILABLE otros1001 THEN DO:
            CREATE otros1001.
            otros1001.concepto = F1001.concepto.
            otros1001.nit = "222222222".
            otros1001.tipoDoc = "43".
        END.

        otros1001.pagosCostoDeduccion = otros1001.pagosCostoDeduccion + F1001.pagosCostoDeduccion.
        otros1001.ReteFuenteRta = otros1001.ReteFuenteRta + F1001.ReteFuenteRta.
        otros1001.ReteFuenteIvaAsumidaRegSimpl = otros1001.ReteFuenteIvaAsumidaRegSimpl + F1001.ReteFuenteIvaAsumidaRegSimpl.
    END.
END.

IF AVAILABLE otros1001 THEN
    EXPORT DELIMITER ";" otros1001.

OUTPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1001_5056 W_FtosDian 
PROCEDURE 1001_5056 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR topeAux AS DECIMAL.

EMPTY TEMP-TABLE F1001.
EMPTY TEMP-TABLE otros1001.
EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,8) = "51102601"
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1001 WHERE F1001.nit = anexos.nit
                           AND F1001.concepto = "5056" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            ASSIGN F1001.nit = anexos.nit
                   F1001.concepto = "5056".

            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1001.tipoDoc = "11".
                    WHEN "T.I" THEN F1001.tipoDoc = "12".
                    WHEN "C.C" THEN F1001.tipoDoc = "13".
                    WHEN "T.E" THEN F1001.tipoDoc = "21".
                    WHEN "C.E" THEN F1001.tipoDoc = "22".
                    WHEN "NIT" THEN F1001.tipoDoc = "31".
                    WHEN "PPTE" THEN F1001.tipoDoc = "41".
                END CASE.

                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1001.apellido1 = clientes.apellido1.
                    F1001.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1001.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1001.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1001.direccion = clientes.DIR_residencia.
                ELSE
                    F1001.direccion = clientes.DIR_comercial.
    
                IF clientes.lugar_residencia <> "" THEN DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.
    
                F1001.pais = "169".
            END.
    
            F1001.nit = anexos.nit.

            IF INDEX(anexos.nit,"-") > 0 THEN DO:
                F1001.dv = SUBSTRING(anexos.nit,INDEX(anexos.nit,"-") + 1,1).
                F1001.nit = SUBSTRING(anexos.nit,1,INDEX(anexos.nit,"-") - 1).
            END.
        END.
    END.

    F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.sdo_inicial.
        
    DO cont = 1 TO 12:
        F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + anexos.db[cont] - anexos.cr[cont].
    END.

    IF LAST-OF(anexos.cuenta) THEN DO:
        EMPTY TEMP-TABLE docs.
    
        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = ano_corte NO-LOCK:
            FIND FIRST docs WHERE docs.nit = mov_contable.nit
                              AND docs.comprobante = mov_contable.comprobante
                              AND docs.agencia = mov_contable.agencia
                              AND docs.num_documento = mov_contable.num_documento NO-LOCK NO-ERROR.
            IF NOT AVAILABLE docs THEN DO:
                CREATE docs.
                docs.nit = mov_contable.nit.
                docs.agencia = mov_contable.agencia.
                docs.comprobante = mov_contable.comprobante.
                docs.num_documento = mov_contable.num_documento.
            END.
        END.
    
        FOR EACH docs NO-LOCK:
            FOR EACH mov_contable WHERE mov_contable.agencia = docs.agencia
                                    AND mov_contable.comprobante = docs.comprobante
                                    AND mov_contable.num_documento = docs.num_documento
                                    AND mov_contable.nit = docs.nit
                                    AND YEAR(mov_contable.fec_contable) = ano_corte
                                    AND SUBSTRING(mov_contable.cuenta,1,4) = "2435"
                                    AND SUBSTRING(mov_contable.cuenta,1,6) <> "243535" NO-LOCK:
                FIND FIRST ttmov WHERE ttmov.Id = ROWID(mov_contable) NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttmov THEN DO:
                    F1001.ReteFuenteRta = F1001.ReteFuenteRta + mov_contable.cr - mov_contable.db.

                    CREATE ttmov.
                    ttmov.id = ROWID(mov_contable).
                END.
            END.
        END.
    
        F1001.ReteFuenteIvaAsumidaRegSimpl = ROUND((F1001.ReteFuenteIvaAsumidaRegSimpl * 96.5) / 100,0).
    END.
END.

FOR EACH F1001 WHERE F1001.concepto = "5056" NO-LOCK:
    totalReporte = totalReporte + F1001.pagosCostoDeduccion.
END.

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,8) = "51102601"
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable - totalReporte <> 0 THEN DO:
    CREATE otros1001.
    otros1001.concepto = "5056".
    otros1001.nit = "222222222".
    otros1001.tipoDoc = "43".
    otros1001.pagosCostoDeduccion = totalContable - totalReporte.
END.

OUTPUT TO C:\INFO_Fodun\F1001_5056.csv.
FOR EACH F1001 WHERE F1001.pagosCostoDeduccion <> 0 NO-LOCK:
    topeAux = DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos).
            
    IF F1001.pagosCostoDeduccion >= topeAux THEN
        EXPORT DELIMITER ";" F1001.
    ELSE DO:
        FIND FIRST otros1001 WHERE otros1001.concepto = F1001.concepto NO-ERROR.
        IF NOT AVAILABLE otros1001 THEN DO:
            CREATE otros1001.
            otros1001.concepto = F1001.concepto.
            otros1001.nit = "222222222".
            otros1001.tipoDoc = "43".
        END.

        otros1001.pagosCostoDeduccion = otros1001.pagosCostoDeduccion + F1001.pagosCostoDeduccion.
        otros1001.ReteFuenteRta = otros1001.ReteFuenteRta + F1001.ReteFuenteRta.
        otros1001.ReteFuenteIvaAsumidaRegSimpl = otros1001.ReteFuenteIvaAsumidaRegSimpl + F1001.ReteFuenteIvaAsumidaRegSimpl.
    END.
END.

EXPORT DELIMITER ";" otros1001.
OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1001_5059 W_FtosDian 
PROCEDURE 1001_5059 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR topeAux AS DECIMAL.

EMPTY TEMP-TABLE F1001.
EMPTY TEMP-TABLE otros1001.
EMPTY TEMP-TABLE docs.

FOR EACH mov_contable WHERE SUBSTRING(mov_contable.cuenta,1,8) = "33101001"
                        AND YEAR(mov_contable.fec_contable) = ano_corte
                        AND INDEX(mov_contable.comentario,"Revaloriza") = 1 NO-LOCK BREAK BY mov_contable.num_documento
                                                                                          BY mov_contable.cuenta:
    IF FIRST-OF(mov_contable.num_documento) THEN DO:
        FIND FIRST docs WHERE docs.comprobante = mov_contable.comprobante
                          AND docs.agencia = mov_contable.agencia
                          AND docs.num_documento = mov_contable.num_documento
                          AND docs.fec_contable = mov_contable.fec_contable NO-LOCK NO-ERROR.
        IF NOT AVAILABLE docs THEN DO:
            CREATE docs.
            docs.agencia = mov_contable.agencia.
            docs.comprobante = mov_contable.comprobante.
            docs.num_documento = mov_contable.num_documento.
            docs.fec_contable = mov_contable.fec_contable.

            FOR EACH bfrMovContable WHERE bfrMovContable.agencia = docs.agencia
                                      AND bfrMovContable.comprobante = docs.comprobante
                                      AND bfrMovContable.num_documento = docs.num_documento
                                      AND bfrMovContable.fec_contable = docs.fec_contable
                                      AND (SUBSTRING(bfrMovContable.cuenta,1,8) = "31050501" OR
                                           SUBSTRING(bfrMovContable.cuenta,1,8) = "24650501") NO-LOCK BREAK BY bfrMovContable.nit:
                IF FIRST-OF(bfrMovContable.nit) THEN DO:
                    FIND FIRST F1001 WHERE F1001.nit = bfrMovContable.nit
                                      AND F1001.concepto = "5059" NO-ERROR.
                    IF NOT AVAILABLE F1001 THEN DO:
                        CREATE F1001.
                        ASSIGN F1001.nit = bfrMovContable.nit
                               F1001.concepto = "5059".

                        FIND FIRST clientes WHERE clientes.nit = bfrMovContable.nit NO-LOCK NO-ERROR.
                        IF AVAILABLE clientes THEN DO:
                            CASE clientes.tipo_identificacion:
                                WHEN "R.C" THEN F1001.tipoDoc = "11".
                                WHEN "T.I" THEN F1001.tipoDoc = "12".
                                WHEN "C.C" THEN F1001.tipoDoc = "13".
                                WHEN "T.E" THEN F1001.tipoDoc = "21".
                                WHEN "C.E" THEN F1001.tipoDoc = "22".
                                WHEN "NIT" THEN F1001.tipoDoc = "31".
                                WHEN "PPTE" THEN F1001.tipoDoc = "41".
                            END CASE.

                            IF clientes.tipo_identificacion <> "NIT" THEN DO:
                                F1001.apellido1 = clientes.apellido1.
                                F1001.apellido2 = clientes.apellido2.

                                IF INDEX(clientes.nombre," ") > 0 THEN DO:
                                    F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                                    F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                                END.
                                ELSE
                                    F1001.nombre1 = clientes.nombre.
                            END.
                            ELSE
                                F1001.razonSocial = clientes.nombre.

                            IF clientes.DIR_residencia <> "" THEN
                                F1001.direccion = clientes.DIR_residencia.
                            ELSE
                                F1001.direccion = clientes.DIR_comercial.

                            IF F1001.direccion = "" THEN DO:
                                FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                                IF AVAILABLE agencias THEN
                                    F1001.direccion = agencias.direccion.
                            END.

                            IF clientes.lugar_residencia <> "" THEN DO:
                                F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                                F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                            END.
                            ELSE DO:
                                F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                                F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                            END.

                            IF F1001.departamento = "" OR F1001.departamento = ? THEN DO:
                                CASE clientes.agencia:
                                    WHEN 1 THEN DO:
                                        F1001.departamento = "11".
                                        F1001.municipio = "001".
                                    END.

                                    WHEN 2 THEN DO:
                                        F1001.departamento = "05".
                                        F1001.municipio = "001".
                                    END.

                                    WHEN 3 THEN DO:
                                        F1001.departamento = "17".
                                        F1001.municipio = "001".
                                    END.

                                    WHEN 4 THEN DO:
                                        F1001.departamento = "76".
                                        F1001.municipio = "250".
                                    END.

                                    OTHERWISE DO:
                                        F1001.departamento = "11".
                                        F1001.municipio = "001".
                                    END.
                                END CASE.
                            END.

                            F1001.pais = "169".
                        END.

                        F1001.nit = bfrMovContable.nit.

                        IF clientes.tipo_identificacion = "NIT" THEN
                            RUN digitoVerificacion.r(INPUT clientes.nit,
                                                     OUTPUT F1001.dv) NO-ERROR.
                    END.
                END.

                F1001.pagosCostoDeduccion = F1001.pagosCostoDeduccion + bfrMovContable.cr - bfrMovContable.db.
            END.
        END.
    END.
END.


OUTPUT TO C:\INFO_Fodun\F1001_5059.csv.
FOR EACH F1001 WHERE F1001.pagosCostoDeduccion <> 0 NO-LOCK:
    topeAux = DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos).
            
    IF F1001.pagosCostoDeduccion >= topeAux THEN
        EXPORT DELIMITER ";" F1001.
    ELSE DO:
        FIND FIRST otros1001 WHERE otros1001.concepto = F1001.concepto NO-ERROR.
        IF NOT AVAILABLE otros1001 THEN DO:
            CREATE otros1001.
            otros1001.concepto = F1001.concepto.
            otros1001.nit = "222222222".
            otros1001.tipoDoc = "43".
        END.

        otros1001.pagosCostoDeduccion = otros1001.pagosCostoDeduccion + F1001.pagosCostoDeduccion.
        otros1001.ReteFuenteRta = otros1001.ReteFuenteRta + F1001.ReteFuenteRta.
        otros1001.ReteFuenteIvaAsumidaRegSimpl = otros1001.ReteFuenteIvaAsumidaRegSimpl + F1001.ReteFuenteIvaAsumidaRegSimpl.
    END.
END.

IF AVAILABLE otros1001 THEN
    EXPORT DELIMITER ";" otros1001.

OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1001_AjusteRet W_FtosDian 
PROCEDURE 1001_AjusteRet :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR topeAux AS DECIMAL.

FOR EACH mov_contable WHERE YEAR(mov_contable.fec_contable) = ano_corte
                        AND SUBSTRING(mov_contable.cuenta,1,4) = "2435"
                        AND mov_contable.cr > 0 NO-LOCK:
    IF SUBSTRING(mov_contable.cuenta,1,6) = "243515" THEN DO:
        FIND FIRST bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                    AND bfrMovContable.comprobante = mov_contable.comprobante
                                    AND bfrMovContable.num_documento = mov_contable.num_documento
                                    AND bfrMovContable.nit = mov_contable.nit
                                    AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                    AND (SUBSTRING(bfrMovContable.cuenta,1,6) = "511001" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102711" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102712" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102713" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51102714" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511030" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "61509512") NO-LOCK NO-ERROR.
        IF AVAILABLE bfrMovContable THEN
            NEXT.
    END.

    IF SUBSTRING(mov_contable.cuenta,1,6) = "243525" THEN DO:
        FIND FIRST bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                    AND bfrMovContable.comprobante = mov_contable.comprobante
                                    AND bfrMovContable.num_documento = mov_contable.num_documento
                                    AND bfrMovContable.nit = mov_contable.nit
                                    AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                    AND (SUBSTRING(bfrMovContable.cuenta,1,6) = "511028" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101154" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101155") NO-LOCK NO-ERROR.
        IF AVAILABLE bfrMovContable THEN
            NEXT.
    END.
    
    IF SUBSTRING(mov_contable.cuenta,1,6) = "243535" THEN DO:
        FIND FIRST bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                    AND bfrMovContable.comprobante = mov_contable.comprobante
                                    AND bfrMovContable.num_documento = mov_contable.num_documento
                                    AND bfrMovContable.nit = mov_contable.nit
                                    AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                    AND (SUBSTRING(bfrMovContable.cuenta,1,8) = "52100502" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "52100505" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "615005" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "615010" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "615020" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "615030" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "616035" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "61509504" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "61509511" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "510526" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "510527" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "510528" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101272" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101275" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101278") NO-LOCK NO-ERROR.
        IF AVAILABLE bfrMovContable THEN
            NEXT.
    END.

    IF SUBSTRING(mov_contable.cuenta,1,6) = "243540" THEN DO:
        FIND FIRST bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                    AND bfrMovContable.comprobante = mov_contable.comprobante
                                    AND bfrMovContable.num_documento = mov_contable.num_documento
                                    AND bfrMovContable.nit = mov_contable.nit
                                    AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                    AND (SUBSTRING(bfrMovContable.cuenta,1,8) = "52100502" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "52100505" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "615005" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "615010" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "615020" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "615030" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "615035" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "61509504" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "61509511") NO-LOCK NO-ERROR.
        IF AVAILABLE bfrMovContable THEN
            NEXT.
    END.

    IF SUBSTRING(mov_contable.cuenta,1,4) = "2435" AND SUBSTRING(mov_contable.cuenta,1,6) <> "243515" THEN DO:
        FIND FIRST bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                    AND bfrMovContable.comprobante = mov_contable.comprobante
                                    AND bfrMovContable.num_documento = mov_contable.num_documento
                                    AND bfrMovContable.nit = mov_contable.nit
                                    AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                    AND (SUBSTRING(bfrMovContable.cuenta,1,2) = "26"            OR SUBSTRING(bfrMovContable.cuenta,1,6) = "510551"      OR SUBSTRING(bfrMovContable.cuenta,1,8) = "51100203"    OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51100204"      OR SUBSTRING(bfrMovContable.cuenta,1,8) = "51100205"    OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511008"      OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511010"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511012"      OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511016"      OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511018"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511020"      OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511022"      OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511024"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511026"      OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511028"      OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511030"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511034"      OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511036"      OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511038"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511040"      OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511042"      OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511044"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511046"      OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511048"      OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511050"        OR SUBSTRING(bfrMovContable.cuenta,1,8) = "51105201"    OR SUBSTRING(bfrMovContable.cuenta,1,8) = "51105202"    OR
                                         SUBSTRING(bfrMovContable.cuenta,1,8) = "51105203"      OR SUBSTRING(bfrMovContable.cuenta,1,8) = "51105204"    OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511060"      OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511062"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511064"      OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511095"      OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "511515"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511524"      OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511530"      OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "514005"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "514015"      OR SUBSTRING(bfrMovContable.cuenta,1,6) = "514095"      OR
                                         SUBSTRING(bfrMovContable.cuenta,1,6) = "531520"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "539520"      OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101108" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101110"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101114" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101116" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101118"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101122" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101136" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101140"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101195" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101251" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102108"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102110" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102114" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102116"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102118" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102122" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102136"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102154" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102195" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102251"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401108" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401110" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401116"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401118" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401122" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401136"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401195" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401251" OR
                                         SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401260"   OR SUBSTRING(bfrMovContable.cuenta,1,8) = "61759501"    OR SUBSTRING(bfrMovContable.cuenta,1,8) = "61759505"    OR
                                         SUBSTRING(bfrMovContable.cuenta,1,4) = "5125")
                                    AND SUBSTRING(bfrMovContable.cuenta,1,8) <> "51105211"
                                    AND SUBSTRING(bfrMovContable.cuenta,1,8) <> "51105212"
                                    AND SUBSTRING(bfrMovContable.cuenta,1,8) <> "51105213"
                                    AND SUBSTRING(bfrMovContable.cuenta,1,8) <> "51105214"
                                    AND bfrMovContable.comentario <> "Cierre Anexos Anual" NO-LOCK NO-ERROR.
        IF AVAILABLE bfrMovContable THEN
            NEXT.
    END.


    FIND FIRST bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                AND bfrMovContable.comprobante = mov_contable.comprobante
                                AND bfrMovContable.num_documento = mov_contable.num_documento
                                AND bfrMovContable.nit = mov_contable.nit
                                AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                AND (SUBSTRING(bfrMovContable.cuenta,1,6) = "271015"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "510506"      OR SUBSTRING(bfrMovContable.cuenta,1,6) = "510515"      OR
                                     SUBSTRING(bfrMovContable.cuenta,1,6) = "510521"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "510527"      OR SUBSTRING(bfrMovContable.cuenta,1,6) = "510536"      OR
                                     SUBSTRING(bfrMovContable.cuenta,1,6) = "510539"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "510548"      OR SUBSTRING(bfrMovContable.cuenta,1,6) = "510560"      OR
                                     SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101206"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101215" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101227" OR
                                     SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101236"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101239" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101248" OR
                                     SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101260"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102206" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102215" OR
                                     SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102236"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102239" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401206" OR
                                     SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401215"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401227" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401236" OR
                                     SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401239"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401248" OR SUBSTRING(bfrMovContable.cuenta,1,6) = "511004"      OR
                                     SUBSTRING(bfrMovContable.cuenta,1,6) = "510569"        OR SUBSTRING(bfrMovContable.cuenta,1,6) = "510571"      OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101271" OR
                                     SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102269"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102271" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401269" OR
                                     SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401271"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101269" OR SUBSTRING(bfrMovContable.cuenta,1,6) = "510570"      OR
                                     SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101270"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102270" OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401270" OR
                                     SUBSTRING(bfrMovContable.cuenta,1,8) = "51100201"      OR SUBSTRING(bfrMovContable.cuenta,1,8) = "51100202"    OR SUBSTRING(bfrMovContable.cuenta,1,6) = "530550"      OR
                                     SUBSTRING(bfrMovContable.cuenta,1,10) = "6140101102"   OR SUBSTRING(bfrMovContable.cuenta,1,10) = "6140102102" OR SUBSTRING(bfrMovContable.cuenta,1,6) = "617535"      OR
                                     SUBSTRING(bfrMovContable.cuenta,1,10) = "6170401102")
                                AND bfrMovContable.comentario <> "Cierre Anexos Anual" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bfrMovContable THEN DO:
        FIND FIRST F1001 WHERE F1001.nit = mov_contable.nit
                           AND F1001.concepto = "5016" NO-ERROR.
        IF NOT AVAILABLE F1001 THEN DO:
            CREATE F1001.
            ASSIGN F1001.nit = mov_contable.nit
                   F1001.concepto = "5016"
                   F1001.ajuste = "X".

            FIND FIRST clientes WHERE clientes.nit = mov_contable.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1001.tipoDoc = "11".
                    WHEN "T.I" THEN F1001.tipoDoc = "12".
                    WHEN "C.C" THEN F1001.tipoDoc = "13".
                    WHEN "T.E" THEN F1001.tipoDoc = "21".
                    WHEN "C.E" THEN F1001.tipoDoc = "22".
                    WHEN "NIT" THEN F1001.tipoDoc = "31".
                    WHEN "PPTE" THEN F1001.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1001.apellido1 = clientes.apellido1.
                    F1001.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1001.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1001.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1001.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1001.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1001.direccion = clientes.DIR_residencia.
                ELSE
                    F1001.direccion = clientes.DIR_comercial.
                
                IF F1001.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1001.direccion = agencias.direccion.
                END.
    
                IF clientes.lugar_residencia <> "" THEN DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1001.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1001.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1001.departamento = "" OR F1001.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1001.departamento = "05".
                            F1001.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1001.departamento = "17".
                            F1001.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1001.departamento = "76".
                            F1001.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1001.departamento = "11".
                            F1001.municipio = "001".
                        END.
                    END CASE.
                END.

                F1001.pais = "169".
            END.
    
            F1001.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1001.dv) NO-ERROR.
        END.

        F1001.ReteFuenteRta = F1001.ReteFuenteRta + mov_contable.cr - mov_contable.db.
    END.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1003_1306 W_FtosDian 
PROCEDURE 1003_1306 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.

EMPTY TEMP-TABLE F1003.
EMPTY TEMP-TABLE otros1003.

FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,6) = "164010"
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1003 WHERE F1003.nit = anexos.nit
                           AND F1003.concepto = "1306" NO-ERROR.
        IF NOT AVAILABLE F1003 THEN DO:
            CREATE F1003.
            ASSIGN F1003.nit = anexos.nit
                   F1003.concepto = "1306".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1003.tipoDoc = "11".
                    WHEN "T.I" THEN F1003.tipoDoc = "12".
                    WHEN "C.C" THEN F1003.tipoDoc = "13".
                    WHEN "T.E" THEN F1003.tipoDoc = "21".
                    WHEN "C.E" THEN F1003.tipoDoc = "22".
                    WHEN "NIT" THEN F1003.tipoDoc = "31".
                    WHEN "PPTE" THEN F1003.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1003.apellido1 = clientes.apellido1.
                    F1003.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1003.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1003.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1003.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1003.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1003.direccion = clientes.DIR_residencia.
                ELSE
                    F1003.direccion = clientes.DIR_comercial.

                IF F1003.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1003.direccion = agencias.direccion.
                END.

                IF clientes.lugar_residencia <> "" THEN DO:
                    F1003.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1003.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1003.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1003.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1003.departamento = "" OR F1003.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1003.departamento = "11".
                            F1003.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1003.departamento = "05".
                            F1003.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1003.departamento = "17".
                            F1003.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1003.departamento = "76".
                            F1003.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1003.departamento = "11".
                            F1003.municipio = "001".
                        END.
                    END CASE.
                END.
            END.
    
            F1003.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1003.dv).
        END.
    END.

    DO cont = 1 TO 12:
        F1003.valorRetencion = F1003.valorRetencion + anexos.db[cont] /*- anexos.cr[cont]*/.
        F1003.valorPagoAbonoTransaccion = F1003.valorPagoAbonoTransaccion + anexos.db[cont] /*- anexos.cr[cont]*/.
    END.
END.

FOR EACH F1003 WHERE F1003.concepto = "1306" NO-LOCK:
    totalReporte = totalReporte + F1003.valorRetencion.
END.

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,6) = "164010"
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] /*- sal_cuenta.cr[cont]*/.
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1003.
    otros1003.concepto = "1306".
    otros1003.nit = "222222222".
    otros1003.tipoDoc = "43".
    otros1003.valorRetencion = totalContable - totalReporte.
END.

OUTPUT TO C:\INFO_Fodun\F1003.csv.
FOR EACH F1003 WHERE F1003.valorRetencion > 0 NO-LOCK:
    F1003.valorPagoAbonoTransaccion = ROUND((F1003.valorRetencion * 100) / 7,0).

    IF F1003.valorRetencion >= DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) THEN
        EXPORT DELIMITER ";" F1003.
    ELSE DO:
        FIND FIRST otros1003 WHERE otros1003.concepto = F1003.concepto NO-ERROR.
        IF NOT AVAILABLE otros1003 THEN DO:
            CREATE otros1003.
            otros1003.concepto = F1003.concepto.
            otros1003.nit = "222222222".
            otros1003.tipoDoc = "43".
        END.

        otros1003.valorPagoAbonoTransaccion = otros1003.valorPagoAbonoTransaccion + F1003.valorPagoAbonoTransaccion.
        otros1003.valorRetencion = otros1003.valorRetencion + F1003.valorRetencion.
    END.
END.

FOR EACH otros1003:
    EXPORT DELIMITER ";" otros1003.
END.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1005 W_FtosDian 
PROCEDURE 1005 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR nitAux AS CHARACTER.

EMPTY TEMP-TABLE F1005.

FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,6) = "251002" /* Se reporta con el Nit de Fodun */
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    nitAux = anexos.nit.

    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1005 WHERE F1005.nit = nitAux NO-ERROR.
        IF NOT AVAILABLE F1005 THEN DO:
            CREATE F1005.
            ASSIGN F1005.nit = nitAux.
    
            FIND FIRST clientes WHERE clientes.nit = nitAux NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1005.tipoDoc = "11".
                    WHEN "T.I" THEN F1005.tipoDoc = "12".
                    WHEN "C.C" THEN F1005.tipoDoc = "13".
                    WHEN "T.E" THEN F1005.tipoDoc = "21".
                    WHEN "C.E" THEN F1005.tipoDoc = "22".
                    WHEN "NIT" THEN F1005.tipoDoc = "31".
                    WHEN "PPTE" THEN F1005.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1005.apellido1 = clientes.apellido1.
                    F1005.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1005.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1005.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1005.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1005.razonSocial = clientes.nombre.
            END.
    
            F1005.nit = nitAux.
    
            IF INDEX(nitAux,"-") > 0 THEN DO:
                F1005.dv = SUBSTRING(nitAux,INDEX(nitAux,"-") + 1,1).
                F1005.nit = SUBSTRING(nitAux,1,INDEX(nitAux,"-") - 1).
            END.
        END.
    END.

    IF LAST-OF(anexos.cuenta) THEN DO:
        EMPTY TEMP-TABLE docs.
    
        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = ano_corte NO-LOCK BREAK:
            FIND FIRST docs WHERE docs.nit = mov_contable.nit
                              AND docs.comprobante = mov_contable.comprobante
                              AND docs.agencia = mov_contable.agencia
                              AND docs.num_documento = mov_contable.num_documento NO-LOCK NO-ERROR.
            IF NOT AVAILABLE docs THEN DO:
                CREATE docs.
                docs.nit = mov_contable.nit.
                docs.agencia = mov_contable.agencia.
                docs.comprobante = mov_contable.comprobante.
                docs.num_documento = mov_contable.num_documento.
            END.
        END.
    
        FOR EACH docs NO-LOCK:
            FOR EACH mov_contable WHERE mov_contable.agencia = docs.agencia
                                    AND mov_contable.comprobante = docs.comprobante
                                    AND mov_contable.num_documento = docs.num_documento
                                    AND mov_contable.nit = docs.nit
                                    AND YEAR(mov_contable.fec_contable) = ano_corte
                                    AND SUBSTRING(mov_contable.cuenta,1,4) = "2510" NO-LOCK:
                F1005.valorIva = F1005.valorIva + mov_contable.cr - mov_contable.db.
            END.
        END.
    
        F1005.valorIva = ROUND((F1005.valorIva * 3.5) / 96.5,0).
    END.
END.

EMPTY TEMP-TABLE otros1005.

OUTPUT TO C:\INFO_Fodun\F1005.csv.

FOR EACH F1005 WHERE F1005.valorIva <> 0 NO-LOCK:
    IF F1005.valorIva >= DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) OR DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) = 0 THEN
        EXPORT DELIMITER ";" F1005.
    ELSE DO:
        FIND FIRST otros1005 NO-ERROR.
        IF NOT AVAILABLE otros1005 THEN DO:
            CREATE otros1005.
            otros1005.nit = "222222222".
            otros1005.tipoDoc = "43".
        END.

        otros1005.valorIva = otros1005.valorIva + F1005.valorIva.
    END.
END.

FOR EACH otros1005:
    EXPORT DELIMITER ";" otros1005.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1006 W_FtosDian 
PROCEDURE 1006 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR nitTemporal AS CHARACTER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.

EMPTY TEMP-TABLE F1006.

FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,6) = "244010"
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    nitTemporal = anexos.nit.

    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1006 WHERE F1006.nit = nitTemporal NO-ERROR.
        IF NOT AVAILABLE F1006 THEN DO:
            CREATE F1006.
            ASSIGN F1006.nit = nitTemporal.

            FIND FIRST clientes WHERE clientes.nit = nitTemporal NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1006.tipoDoc = "11".
                    WHEN "T.I" THEN F1006.tipoDoc = "12".
                    WHEN "C.C" THEN F1006.tipoDoc = "13".
                    WHEN "T.E" THEN F1006.tipoDoc = "21".
                    WHEN "C.E" THEN F1006.tipoDoc = "22".
                    WHEN "NIT" THEN F1006.tipoDoc = "31".
                    WHEN "PPTE" THEN F1006.tipoDoc = "41".
                END CASE.

                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1006.apellido1 = clientes.apellido1.
                    F1006.apellido2 = clientes.apellido2.

                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1006.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1006.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1006.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1006.razonSocial = clientes.nombre.
            END.

            F1006.nit = nitTemporal.

            IF INDEX(nitTemporal,"-") > 0 THEN DO:
                F1006.dv = SUBSTRING(nitTemporal,INDEX(nitTemporal,"-") + 1,1).
                F1006.nit = SUBSTRING(nitTemporal,1,INDEX(nitTemporal,"-") - 1).
            END.
        END.
    END.

    DO cont = 1 TO 12:
        F1006.impuestoGenerado = F1006.impuestoGenerado + anexos.cr[cont] - anexos.db[cont].
    END.
END.

EMPTY TEMP-TABLE otros1006.

FOR EACH F1006 NO-LOCK:
    totalReporte = totalReporte + F1006.impuestoGenerado.
END.

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,6) = "244010"
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.
END.

IF totalContable > totalReporte  THEN DO:
    CREATE otros1006.
    otros1006.nit = "222222222".
    otros1006.tipoDoc = "43".
    otros1006.impuestoGenerado = totalContable - totalReporte.
END.


OUTPUT TO C:\INFO_Fodun\F1006.csv.

FOR EACH F1006 WHERE F1006.impuestoGenerado <> 0 NO-LOCK:
    IF F1006.impuestoGenerado >= DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) OR DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) = 0 THEN
        EXPORT DELIMITER ";" F1006.
    ELSE DO:
        FIND FIRST otros1006 NO-ERROR.
        IF NOT AVAILABLE otros1006 THEN DO:
            CREATE otros1006.
            otros1006.nit = "222222222".
            otros1006.tipoDoc = "43".
        END.

        otros1006.impuestoGenerado = otros1006.impuestoGenerado + F1006.impuestoGenerado.
    END.
END.

FOR EACH otros1006:
    EXPORT DELIMITER ";" otros1006.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1007_4001 W_FtosDian 
PROCEDURE 1007_4001 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR nitTemporal AS CHARACTER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.

EMPTY TEMP-TABLE F1007.

FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,2) = "41" OR
                       SUBSTRING(anexos.cuenta,1,4) = "42")
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    nitTemporal = anexos.nit.

    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1007 WHERE F1007.nit = nitTemporal
                           AND F1007.concepto = "4001" NO-ERROR.
        IF NOT AVAILABLE F1007 THEN DO:
            CREATE F1007.
            ASSIGN F1007.nit = nitTemporal
                   F1007.concepto = "4001".
    
            FIND FIRST clientes WHERE clientes.nit = nitTemporal NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1007.tipoDoc = "11".
                    WHEN "T.I" THEN F1007.tipoDoc = "12".
                    WHEN "C.C" THEN F1007.tipoDoc = "13".
                    WHEN "T.E" THEN F1007.tipoDoc = "21".
                    WHEN "C.E" THEN F1007.tipoDoc = "22".
                    WHEN "NIT" THEN F1007.tipoDoc = "31".
                    WHEN "PPTE" THEN F1007.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1007.apellido1 = clientes.apellido1.
                    F1007.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1007.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1007.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1007.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1007.razonSocial = clientes.nombre.
            END.

            F1007.pais = "169".
            F1007.nit = nitTemporal.
    
            IF INDEX(nitTemporal,"-") > 0 THEN DO:
                F1007.dv = SUBSTRING(nitTemporal,INDEX(nitTemporal,"-") + 1,1).
                F1007.nit = SUBSTRING(nitTemporal,1,INDEX(nitTemporal,"-") - 1).
            END.
        END.
    END.

    /*F1007.valorIngresoBrutoOperaciones = F1007.valorIngresoBrutoOperaciones + anexos.sdo_inicial.*/

    DO cont = 1 TO 12:
        F1007.valorIngresoBrutoOperaciones = F1007.valorIngresoBrutoOperaciones + anexos.cr[cont] - anexos.db[cont].
    END.
END.

/*FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "4180" OR
                           SUBSTRING(anexos.cuenta,1,6) = "425060")
                      AND sal_cuenta.ano = ano_corte NO-LOCK BREAK BY sal_cuenta.cuenta:
    nitTemporal = "800112808-7".

    IF FIRST-OF(sal_cuenta.cuenta) THEN DO:
        FIND FIRST F1007 WHERE F1007.nit = nitTemporal
                           AND F1007.concepto = "4001" NO-ERROR.
        IF NOT AVAILABLE F1007 THEN DO:
            CREATE F1007.
            ASSIGN F1007.nit = nitTemporal
                   F1007.concepto = "4001".
    
            FIND FIRST clientes WHERE clientes.nit = nitTemporal NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1007.tipoDoc = "11".
                    WHEN "T.I" THEN F1007.tipoDoc = "12".
                    WHEN "C.C" THEN F1007.tipoDoc = "13".
                    WHEN "T.E" THEN F1007.tipoDoc = "21".
                    WHEN "C.E" THEN F1007.tipoDoc = "22".
                    WHEN "NIT" THEN F1007.tipoDoc = "31".
                    WHEN "PPTE" THEN F1007.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1007.apellido1 = clientes.apellido1.
                    F1007.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1007.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1007.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1007.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1007.razonSocial = clientes.nombre.
            END.

            F1007.pais = "169".
            F1007.nit = nitTemporal.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1007.dv).
        END.
    END.

    F1007.valorIngresoBrutoOperaciones = F1007.valorIngresoBrutoOperaciones + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        F1007.valorIngresoBrutoOperaciones = F1007.valorIngresoBrutoOperaciones + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.
END.*/

EMPTY TEMP-TABLE otros1007.

FOR EACH F1007 WHERE F1007.concepto = "4001" NO-LOCK:
    totalReporte = totalReporte + F1007.valorIngresoBrutoOperaciones.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,2) = "41" OR
                           SUBSTRING(sal_cuenta.cuenta,1,2) = "42")
                        AND sal_cuenta.ano = ano_corte NO-LOCK:
    /*totalContable = totalContable + sal_cuenta.sal_inicial.*/

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.
END.

IF totalContable - totalReporte <> 0 THEN DO:
    CREATE otros1007.
    otros1007.concepto = "4001".
    otros1007.nit = "222222222".
    otros1007.tipoDoc = "43".
    otros1007.valorIngresoBrutoOperaciones = totalContable - totalReporte.
END.


OUTPUT TO C:\INFO_Fodun\F1007.csv.

FOR EACH F1007 WHERE F1007.valorIngresoBrutoOperaciones <> 0 NO-LOCK:
    IF F1007.valorIngresoBrutoOperaciones >= DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) OR DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) = 0 THEN
        EXPORT DELIMITER ";" F1007.
    ELSE DO:
        FIND FIRST otros1007 WHERE otros1007.concepto = F1007.concepto NO-ERROR.
        IF NOT AVAILABLE otros1007 THEN DO:
            CREATE otros1007.
            otros1007.concepto = F1007.concepto.
            otros1007.nit = "222222222".
            otros1007.tipoDoc = "43".
        END.

        otros1007.valorIngresoBrutoOperaciones = otros1007.valorIngresoBrutoOperaciones + F1007.valorIngresoBrutoOperaciones.
    END.
END.

FOR EACH otros1007:
    EXPORT DELIMITER ";" otros1007.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1008 W_FtosDian 
PROCEDURE 1008 :
DEFINE VAR suma AS DECIMAL.

EMPTY TEMP-TABLE otros1008. /* Unificar con el 1009 */

RUN 1008_1316.
RUN 1008_1317.
RUN 1008_1318.

EMPTY TEMP-TABLE reportar.

OUTPUT TO C:\INFO_Fodun\F1008.csv.

FOR EACH F1008 WHERE F1008.valorSaldoCreditoActivo > 0 NO-LOCK:
    IF F1008.valorSaldoCreditoActivo >= DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) THEN
        EXPORT DELIMITER ";" F1008.
    ELSE DO:
        FIND FIRST otros1008 WHERE otros1008.concepto = F1008.concepto NO-ERROR.
        IF NOT AVAILABLE otros1008 THEN DO:
            CREATE otros1008.
            otros1008.concepto = F1008.concepto.
            otros1008.nit = "222222222".
            otros1008.tipoDoc = "43".
        END.

        otros1008.valorSaldoCreditoActivo = otros1008.valorSaldoCreditoActivo + F1008.valorSaldoCreditoActivo.
    END.
END.

FOR EACH otros1008:
    EXPORT DELIMITER ";" otros1008.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1008_1316 W_FtosDian 
PROCEDURE 1008_1316 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR suma AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.

EMPTY TEMP-TABLE F1008.

/* 1441 - 1442 - 1655 excepto empleados */

/* OK */
FOR EACH rep_creditos WHERE rep_creditos.cod_credito <> 62
                        AND rep_creditos.sdo_capital <> 0
                        AND fecCorte = DATE(12,31,ano_corte) NO-LOCK BREAK BY rep_creditos.nit:
    suma = suma + rep_creditos.sdo_capital + rep_creditos.int_corriente + rep_creditos.int_morCobrar.

    IF LAST-OF(rep_creditos.nit) THEN DO:
        CREATE F1008.
        ASSIGN F1008.nit = rep_creditos.nit
               F1008.concepto = "1316".

        FIND FIRST clientes WHERE clientes.nit = rep_creditos.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN F1008.tipoDoc = "11".
                WHEN "T.I" THEN F1008.tipoDoc = "12".
                WHEN "C.C" THEN F1008.tipoDoc = "13".
                WHEN "T.E" THEN F1008.tipoDoc = "21".
                WHEN "C.E" THEN F1008.tipoDoc = "22".
                WHEN "NIT" THEN F1008.tipoDoc = "31".
                WHEN "PPTE" THEN F1008.tipoDoc = "41".
            END CASE.

            IF clientes.tipo_identificacion <> "NIT" THEN DO:
                F1008.apellido1 = clientes.apellido1.
                F1008.apellido2 = clientes.apellido2.

                IF INDEX(clientes.nombre," ") > 0 THEN DO:
                    F1008.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                    F1008.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                END.
                ELSE
                    F1008.nombre1 = clientes.nombre.
            END.
            ELSE
                F1008.razonSocial = clientes.nombre.

            IF clientes.DIR_residencia <> "" THEN
                F1008.direccion = clientes.DIR_residencia.
            ELSE
                F1008.direccion = clientes.DIR_comercial.

            IF F1008.direccion = "" THEN DO:
                FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                IF AVAILABLE agencias THEN
                    F1008.direccion = agencias.direccion.
            END.

            IF clientes.lugar_residencia <> "" THEN DO:
                F1008.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                F1008.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
            END.
            ELSE DO:
                F1008.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                F1008.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
            END.

            IF F1008.departamento = "" OR F1008.departamento = ? THEN DO:
                CASE clientes.agencia:
                    WHEN 1 THEN DO:
                        F1008.departamento = "11".
                        F1008.municipio = "001".
                    END.

                    WHEN 2 THEN DO:
                        F1008.departamento = "05".
                        F1008.municipio = "001".
                    END.

                    WHEN 3 THEN DO:
                        F1008.departamento = "17".
                        F1008.municipio = "001".
                    END.

                    WHEN 4 THEN DO:
                        F1008.departamento = "76".
                        F1008.municipio = "250".
                    END.

                    OTHERWISE DO:
                        F1008.departamento = "11".
                        F1008.municipio = "001".
                    END.
                END CASE.
            END.

            F1008.pais = "169".
        END.

        F1008.nit = rep_creditos.nit.

        IF clientes.tipo_identificacion = "NIT" THEN
            RUN digitoVerificacion.r(INPUT clientes.nit,
                                     OUTPUT F1008.dv).

        F1008.valorSaldoCreditoActivo = suma.

        suma = 0.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1008_1317 W_FtosDian 
PROCEDURE 1008_1317 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR suma AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.

/* OK */
FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,4) = "1620" OR
                         SUBSTRING(anexos.cuenta,1,4) = "1625" OR
                         SUBSTRING(anexos.cuenta,1,4) = "1635" OR
                         /*SUBSTRING(anexos.cuenta,1,4) = "1640" OR*/
                         SUBSTRING(anexos.cuenta,1,4) = "1650" OR
                         SUBSTRING(anexos.cuenta,1,4) = "1660" OR
                         SUBSTRING(anexos.cuenta,1,4) = "1675" OR
                         SUBSTRING(anexos.cuenta,1,4) = "1690")
                    /*AND SUBSTRING(anexos.cuenta,1,6) <> "167530"*/
                    AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                               BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1008 WHERE F1008.nit = anexos.nit
                           AND F1008.concepto = "1317" NO-ERROR.
        IF NOT AVAILABLE F1008 THEN DO:
            IF INDEX(anexos.nit,"-") > 0 THEN
                FIND FIRST F1008 WHERE F1008.nit = SUBSTRING(anexos.nit,1,INDEX(anexos.nit,"-") - 1) NO-LOCK NO-ERROR.

            IF NOT AVAILABLE F1008 THEN DO:
                CREATE F1008.
                ASSIGN F1008.nit = anexos.nit
                       F1008.concepto = "1317".
            
                FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
                IF AVAILABLE clientes THEN DO:
                    CASE clientes.tipo_identificacion:
                        WHEN "R.C" THEN F1008.tipoDoc = "11".
                        WHEN "T.I" THEN F1008.tipoDoc = "12".
                        WHEN "C.C" THEN F1008.tipoDoc = "13".
                        WHEN "T.E" THEN F1008.tipoDoc = "21".
                        WHEN "C.E" THEN F1008.tipoDoc = "22".
                        WHEN "NIT" THEN F1008.tipoDoc = "31".
                        WHEN "PPTE" THEN F1008.tipoDoc = "41".
                    END CASE.
    
                    IF clientes.tipo_identificacion <> "NIT" THEN DO:
                        F1008.apellido1 = clientes.apellido1.
                        F1008.apellido2 = clientes.apellido2.
        
                        IF INDEX(clientes.nombre," ") > 0 THEN DO:
                            F1008.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                            F1008.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                        END.
                        ELSE
                            F1008.nombre1 = clientes.nombre.
                    END.
                    ELSE
                        F1008.razonSocial = clientes.nombre.
    
                    IF clientes.DIR_residencia <> "" THEN
                        F1008.direccion = clientes.DIR_residencia.
                    ELSE
                        F1008.direccion = clientes.DIR_comercial.

                    IF F1008.direccion = "" THEN DO:
                        FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                        IF AVAILABLE agencias THEN
                            F1008.direccion = agencias.direccion.
                    END.

    
                    IF clientes.lugar_residencia <> "" THEN DO:
                        F1008.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                        F1008.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                    END.
                    ELSE DO:
                        F1008.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                        F1008.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                    END
                        .
                    IF F1008.departamento = "" OR F1008.departamento = ? THEN DO:
                        CASE clientes.agencia:
                            WHEN 1 THEN DO:
                                F1008.departamento = "11".
                                F1008.municipio = "001".
                            END.

                            WHEN 2 THEN DO:
                                F1008.departamento = "05".
                                F1008.municipio = "001".
                            END.

                            WHEN 3 THEN DO:
                                F1008.departamento = "17".
                                F1008.municipio = "001".
                            END.

                            WHEN 4 THEN DO:
                                F1008.departamento = "76".
                                F1008.municipio = "250".
                            END.

                            OTHERWISE DO:
                                F1008.departamento = "11".
                                F1008.municipio = "001".
                            END.
                        END CASE.
                    END.
    
                    F1008.pais = "169".
                END.
    
                F1008.nit = anexos.nit.
    
                IF INDEX(anexos.nit,"-") > 0 THEN DO:
                    F1008.dv = SUBSTRING(anexos.nit,INDEX(anexos.nit,"-") + 1,1).
                    F1008.nit = SUBSTRING(anexos.nit,1,INDEX(anexos.nit,"-") - 1).
                END.
            END.
        END.
    END.

    F1008.valorSaldoCreditoActivo = F1008.valorSaldoCreditoActivo + anexos.sdo_inicial.
        
    DO cont = 1 TO 12:
        F1008.valorSaldoCreditoActivo = F1008.valorSaldoCreditoActivo + anexos.db[cont] - anexos.cr[cont].
    END.

    F1008.valorSaldoCreditoActivo = ROUND(F1008.valorSaldoCreditoActivo,0).
END.


/* 1640 - Créditos de empleados */
FOR EACH rep_creditos WHERE rep_creditos.fecCorte = DATE(12, 31, ano_corte)
                        AND rep_creditos.cod_credito = 62 NO-LOCK BREAK BY rep_creditos.nit:
    suma = suma + rep_creditos.sdo_capital + rep_creditos.int_corriente + rep_creditos.int_morCobrar.

    IF LAST-OF(rep_creditos.nit) THEN DO:
        FIND FIRST F1008 WHERE F1008.nit = rep_creditos.nit
                           AND F1008.concepto = "1317" NO-LOCK NO-ERROR.
        IF NOT AVAILABLE F1008 THEN DO:
            CREATE F1008.
            ASSIGN F1008.nit = rep_creditos.nit
                   F1008.concepto = "1317".

            FIND FIRST clientes WHERE clientes.nit = rep_creditos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1008.tipoDoc = "11".
                    WHEN "T.I" THEN F1008.tipoDoc = "12".
                    WHEN "C.C" THEN F1008.tipoDoc = "13".
                    WHEN "T.E" THEN F1008.tipoDoc = "21".
                    WHEN "C.E" THEN F1008.tipoDoc = "22".
                    WHEN "NIT" THEN F1008.tipoDoc = "31".
                    WHEN "PPTE" THEN F1008.tipoDoc = "41".
                END CASE.

                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1008.apellido1 = clientes.apellido1.
                    F1008.apellido2 = clientes.apellido2.

                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1008.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1008.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1008.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1008.razonSocial = clientes.nombre.

                IF clientes.DIR_residencia <> "" THEN
                    F1008.direccion = clientes.DIR_residencia.
                ELSE
                    F1008.direccion = clientes.DIR_comercial.

                IF clientes.lugar_residencia <> "" THEN DO:
                    F1008.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1008.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1008.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1008.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                F1008.pais = "169".
            END.

            F1008.nit = rep_creditos.nit.

            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1008.dv).
        END.

        F1008.valorSaldoCreditoActivo = F1008.valorSaldoCreditoActivo + ROUND(suma,0).

        suma = 0.
    END.
END.

FOR EACH F1008 WHERE F1008.concepto = "1317" NO-LOCK:
    totalReporte = totalReporte + F1008.valorSaldoCreditoActivo.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "1620" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1625" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1635" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1640" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1650" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1660" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1675" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1690")
                        /*AND SUBSTRING(sal_cuenta.cuenta,1,6) <> "167530"*/
                        AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1008.
    otros1008.concepto = "1317".
    otros1008.nit = "222222222".
    otros1008.tipoDoc = "43".
    otros1008.valorSaldoCreditoActivo = totalContable - totalReporte.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1008_1318 W_FtosDian 
PROCEDURE 1008_1318 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR suma AS DECIMAL.
DEFINE VAR nitAux AS CHARACTER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.

/* 1491 - 1696 */
/* OK */
FOR EACH rep_creditos WHERE rep_creditos.sdo_capital <> 0
                        AND fecCorte = DATE(12,31,ano_corte) NO-LOCK BREAK BY rep_creditos.nit:
    suma = suma + rep_creditos.provision + rep_creditos.provision_interes.

    IF LAST-OF(rep_creditos.nit) THEN DO:
        FIND FIRST F1008 WHERE F1008.nit = rep_creditos.nit
                           AND F1008.concepto = "1318" NO-LOCK NO-ERROR.
        IF NOT AVAILABLE F1008 THEN DO:
            CREATE F1008.
            ASSIGN F1008.nit = rep_creditos.nit
                   F1008.concepto = "1318".

            FIND FIRST clientes WHERE clientes.nit = rep_creditos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1008.tipoDoc = "11".
                    WHEN "T.I" THEN F1008.tipoDoc = "12".
                    WHEN "C.C" THEN F1008.tipoDoc = "13".
                    WHEN "T.E" THEN F1008.tipoDoc = "21".
                    WHEN "C.E" THEN F1008.tipoDoc = "22".
                    WHEN "NIT" THEN F1008.tipoDoc = "31".
                    WHEN "PPTE" THEN F1008.tipoDoc = "41".
                END CASE.

                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1008.apellido1 = clientes.apellido1.
                    F1008.apellido2 = clientes.apellido2.

                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1008.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1008.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1008.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1008.razonSocial = clientes.nombre.

                IF clientes.DIR_residencia <> "" THEN
                    F1008.direccion = clientes.DIR_residencia.
                ELSE
                    F1008.direccion = clientes.DIR_comercial.

                IF F1008.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1008.direccion = agencias.direccion.
                END.

                IF clientes.lugar_residencia <> "" THEN DO:
                    F1008.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1008.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1008.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1008.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1008.departamento = "" OR F1008.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1008.departamento = "11".
                            F1008.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1008.departamento = "05".
                            F1008.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1008.departamento = "17".
                            F1008.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1008.departamento = "76".
                            F1008.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1008.departamento = "11".
                            F1008.municipio = "001".
                        END.
                    END CASE.
                END.

                F1008.pais = "169".
            END.

            F1008.nit = rep_creditos.nit.

            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1008.dv).
        END.

        F1008.valorSaldoCreditoActivo = ROUND(suma,0).

        suma = 0.
    END.
END.

/* 1498 */
/*nitAux = "800112808".

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "1498"
                        AND sal_cuenta.ano = ano_corte NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF FIRST-OF(sal_cuenta.cuenta) THEN DO:
        FIND FIRST F1008 WHERE F1008.nit = nitAux
                           AND F1008.concepto = "1318" NO-ERROR.
        IF NOT AVAILABLE F1008 THEN DO:
            CREATE F1008.
            ASSIGN F1008.nit = nitAux
                   F1008.concepto = "1318".
    
            FIND FIRST clientes WHERE clientes.nit = nitAux NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1008.tipoDoc = "11".
                    WHEN "T.I" THEN F1008.tipoDoc = "12".
                    WHEN "C.C" THEN F1008.tipoDoc = "13".
                    WHEN "T.E" THEN F1008.tipoDoc = "21".
                    WHEN "C.E" THEN F1008.tipoDoc = "22".
                    WHEN "NIT" THEN F1008.tipoDoc = "31".
                    WHEN "PPTE" THEN F1008.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1008.apellido1 = clientes.apellido1.
                    F1008.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1008.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1008.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1008.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1008.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1008.direccion = clientes.DIR_residencia.
                ELSE
                    F1008.direccion = clientes.DIR_comercial.
    
                IF clientes.lugar_residencia <> "" THEN DO:
                    F1008.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1008.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1008.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1008.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.
    
                F1008.pais = "169".
            END.
    
            F1008.nit = nitAux.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1008.dv).
        END.
    END.

    FIND CURRENT F1008 NO-ERROR.

    F1008.valorSaldoCreditoActivo = F1008.valorSaldoCreditoActivo + (sal_cuenta.sal_inicial * -1).

    DO cont = 1 TO 12:
        F1008.valorSaldoCreditoActivo = F1008.valorSaldoCreditoActivo + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.

    F1008.valorSaldoCreditoActivo = ROUND(F1008.valorSaldoCreditoActivo,0).
END.*/

FOR EACH F1008 WHERE F1008.concepto = "1318" NO-LOCK:
    totalReporte = totalReporte + F1008.valorSaldoCreditoActivo.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "1491" OR
                             /*SUBSTRING(sal_cuenta.cuenta,1,4) = "1498" OR*/
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1696")
                        AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + (sal_cuenta.sal_inicial * -1).

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1008.
    otros1008.concepto = "1318".
    otros1008.nit = "222222222".
    otros1008.tipoDoc = "43".
    otros1008.valorSaldoCreditoActivo = totalContable - totalReporte.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1009 W_FtosDian 
PROCEDURE 1009 :
DEFINE VAR suma AS DECIMAL.
    
RUN 1009_2201.
RUN 1009_2202.
RUN 1009_2203.
RUN 1009_2204.
RUN 1009_2205.
RUN 1009_2206.

OUTPUT TO C:\INFO_Fodun\F1009.csv.

FOR EACH F1009 WHERE F1009.valorSaldoPasivo > 0 NO-LOCK:
    IF F1009.valorSaldoPasivo >= DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) OR DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) = 0 THEN
        EXPORT DELIMITER ";" F1009.
    ELSE DO:
        FIND FIRST otros1009 WHERE otros1009.concepto = F1009.concepto NO-ERROR.
        IF NOT AVAILABLE otros1009 THEN DO:
            CREATE otros1009.
            otros1009.concepto = F1009.concepto.
            otros1009.nit = "222222222".
            otros1009.tipoDoc = "43".
        END.

        otros1009.valorSaldoPasivo = otros1009.valorSaldoPasivo + F1009.valorSaldoPasivo.
    END.
END.

IF DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) > 0 THEN DO:
    FOR EACH otros1009:
        EXPORT DELIMITER ";" otros1009.
    END.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1009_2201 W_FtosDian 
PROCEDURE 1009_2201 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.

FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,4) = "2410"
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1009 WHERE F1009.nit = anexos.nit
                           AND F1009.concepto = "2201" NO-ERROR.
        IF NOT AVAILABLE F1009 THEN DO:
            CREATE F1009.
            ASSIGN F1009.nit = anexos.nit
                   F1009.concepto = "2201".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1009.tipoDoc = "11".
                    WHEN "T.I" THEN F1009.tipoDoc = "12".
                    WHEN "C.C" THEN F1009.tipoDoc = "13".
                    WHEN "T.E" THEN F1009.tipoDoc = "21".
                    WHEN "C.E" THEN F1009.tipoDoc = "22".
                    WHEN "NIT" THEN F1009.tipoDoc = "31".
                    WHEN "PPTE" THEN F1009.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1009.apellido1 = clientes.apellido1.
                    F1009.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1009.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1009.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1009.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1009.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1009.direccion = clientes.DIR_residencia.
                ELSE
                    F1009.direccion = clientes.DIR_comercial.

                IF F1009.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1009.direccion = agencias.direccion.
                END.

                IF clientes.lugar_residencia <> "" THEN DO:
                    F1009.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1009.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1009.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1009.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1009.departamento = "" OR F1009.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1009.departamento = "11".
                            F1009.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1009.departamento = "05".
                            F1009.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1009.departamento = "17".
                            F1009.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1009.departamento = "76".
                            F1009.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1009.departamento = "11".
                            F1009.municipio = "001".
                        END.
                    END CASE.
                END.
    
                F1009.pais = "169".
            END.
    
            F1009.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1009.dv).
        END.
    END.

    DO cont = 1 TO 12:
        F1009.valorSaldoPasivo = F1009.valorSaldoPasivo + anexos.cr[cont] - anexos.db[cont].
    END.
END.

FOR EACH F1009 WHERE F1009.concepto = "2201" NO-LOCK:
    totalReporte = totalReporte + F1009.valorSaldoPasivo.
END.

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "2410"
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1009.
    otros1009.concepto = "2201".
    otros1009.nit = "222222222".
    otros1009.tipoDoc = "43".
    otros1009.valorSaldoPasivo = totalContable - totalReporte.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1009_2202 W_FtosDian 
PROCEDURE 1009_2202 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR suma AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.

EMPTY TEMP-TABLE F1009.

/* 21 */
/* OK */
FOR EACH rep_ahorros WHERE rep_ahorros.tip_ahorro <> 4
                       AND rep_ahorros.sdo_disponible <> 0
                       AND rep_ahorros.fecCorte = DATE(12, 31, ano_corte) NO-LOCK BREAK BY rep_ahorros.nit:
    suma = suma + rep_ahorros.sdo_disponible + rep_ahorros.INT_causado + rep_ahorros.INT_pagar.

    IF LAST-OF(rep_ahorros.nit) THEN DO:
        CREATE F1009.
        ASSIGN F1009.nit = rep_ahorros.nit
               F1009.concepto = "2202".

        FIND FIRST clientes WHERE clientes.nit = rep_ahorros.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN F1009.tipoDoc = "11".
                WHEN "T.I" THEN F1009.tipoDoc = "12".
                WHEN "C.C" THEN F1009.tipoDoc = "13".
                WHEN "T.E" THEN F1009.tipoDoc = "21".
                WHEN "C.E" THEN F1009.tipoDoc = "22".
                WHEN "NIT" THEN F1009.tipoDoc = "31".
                WHEN "PPTE" THEN F1009.tipoDoc = "41".
            END CASE.

            IF clientes.tipo_identificacion <> "NIT" THEN DO:
                F1009.apellido1 = clientes.apellido1.
                F1009.apellido2 = clientes.apellido2.

                IF INDEX(clientes.nombre," ") > 0 THEN DO:
                    F1009.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                    F1009.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                END.
                ELSE
                    F1009.nombre1 = clientes.nombre.
            END.
            ELSE
                F1009.razonSocial = clientes.nombre.

            IF clientes.DIR_residencia <> "" THEN
                F1009.direccion = clientes.DIR_residencia.
            ELSE
                F1009.direccion = clientes.DIR_comercial.

            IF F1009.direccion = "" THEN DO:
                FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                IF AVAILABLE agencias THEN
                    F1009.direccion = agencias.direccion.
            END.

            IF clientes.lugar_residencia <> "" THEN DO:
                F1009.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                F1009.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
            END.
            ELSE DO:
                F1009.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                F1009.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
            END.

            IF F1009.departamento = "" OR F1009.departamento = ? THEN DO:
                CASE clientes.agencia:
                    WHEN 1 THEN DO:
                        F1009.departamento = "11".
                        F1009.municipio = "001".
                    END.

                    WHEN 2 THEN DO:
                        F1009.departamento = "05".
                        F1009.municipio = "001".
                    END.

                    WHEN 3 THEN DO:
                        F1009.departamento = "17".
                        F1009.municipio = "001".
                    END.

                    WHEN 4 THEN DO:
                        F1009.departamento = "76".
                        F1009.municipio = "250".
                    END.

                    OTHERWISE DO:
                        F1009.departamento = "11".
                        F1009.municipio = "001".
                    END.
                END CASE.
            END.

            F1009.pais = "169".
        END.

        F1009.nit = rep_ahorros.nit.

        IF clientes.tipo_identificacion = "NIT" THEN
            RUN digitoVerificacion.r(INPUT clientes.nit,
                                     OUTPUT F1009.dv).

        F1009.valorSaldoPasivo = suma.

        suma = 0.
    END.
END.

/*FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,4) = "2305" OR
                         SUBSTRING(anexos.cuenta,1,4) = "2465")
                    AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                               BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1009 WHERE F1009.nit = anexos.nit
                           AND F1009.concepto = "2202" NO-ERROR.
        IF NOT AVAILABLE F1009 THEN DO:
            CREATE F1009.
            ASSIGN F1009.nit = anexos.nit
                   F1009.concepto = "2202".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1009.tipoDoc = "11".
                    WHEN "T.I" THEN F1009.tipoDoc = "12".
                    WHEN "C.C" THEN F1009.tipoDoc = "13".
                    WHEN "T.E" THEN F1009.tipoDoc = "21".
                    WHEN "C.E" THEN F1009.tipoDoc = "22".
                    WHEN "NIT" THEN F1009.tipoDoc = "31".
                    WHEN "PPTE" THEN F1009.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1009.apellido1 = clientes.apellido1.
                    F1009.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1009.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1009.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1009.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1009.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1009.direccion = clientes.DIR_residencia.
                ELSE
                    F1009.direccion = clientes.DIR_comercial.

                IF clientes.lugar_residencia <> "" THEN DO:
                    F1009.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1009.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1009.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1009.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1009.departamento = "" OR F1009.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1009.departamento = "11".
                            F1009.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1009.departamento = "05".
                            F1009.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1009.departamento = "17".
                            F1009.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1009.departamento = "76".
                            F1009.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1009.departamento = "11".
                            F1009.municipio = "001".
                        END.
                    END CASE.
                END.

                F1009.pais = "169".
            END.
    
            F1009.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1009.dv).
        END.
    END.

    F1009.valorSaldoPasivo = F1009.valorSaldoPasivo + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        F1009.valorSaldoPasivo = F1009.valorSaldoPasivo + anexos.cr[cont] - anexos.db[cont].
    END.
END.*/

FOR EACH F1009 WHERE F1009.concepto = "2202" NO-LOCK:
    totalReporte = totalReporte + F1009.valorSaldoPasivo.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "2105" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "2110" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "2130" OR
                             SUBSTRING(sal_cuenta.cuenta,1,6) = "249530")
                        AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + (sal_cuenta.sal_inicial).

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1009.
    otros1009.concepto = "2202".
    otros1009.nit = "222222222".
    otros1009.tipoDoc = "43".
    otros1009.valorSaldoPasivo = totalContable - totalReporte.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1009_2203 W_FtosDian 
PROCEDURE 1009_2203 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.

FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,4) = "2305"
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1009 WHERE F1009.nit = anexos.nit
                           AND F1009.concepto = "2203" NO-ERROR.
        IF NOT AVAILABLE F1009 THEN DO:
            CREATE F1009.
            ASSIGN F1009.nit = anexos.nit
                   F1009.concepto = "2203".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1009.tipoDoc = "11".
                    WHEN "T.I" THEN F1009.tipoDoc = "12".
                    WHEN "C.C" THEN F1009.tipoDoc = "13".
                    WHEN "T.E" THEN F1009.tipoDoc = "21".
                    WHEN "C.E" THEN F1009.tipoDoc = "22".
                    WHEN "NIT" THEN F1009.tipoDoc = "31".
                    WHEN "PPTE" THEN F1009.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1009.apellido1 = clientes.apellido1.
                    F1009.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1009.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1009.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1009.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1009.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1009.direccion = clientes.DIR_residencia.
                ELSE
                    F1009.direccion = clientes.DIR_comercial.

                IF F1009.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1009.direccion = agencias.direccion.
                END.

                IF clientes.lugar_residencia <> "" THEN DO:
                    F1009.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1009.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1009.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1009.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1009.departamento = "" OR F1009.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1009.departamento = "11".
                            F1009.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1009.departamento = "05".
                            F1009.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1009.departamento = "17".
                            F1009.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1009.departamento = "76".
                            F1009.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1009.departamento = "11".
                            F1009.municipio = "001".
                        END.
                    END CASE.
                END.
    
                F1009.pais = "169".
            END.
    
            F1009.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1009.dv).
        END.
    END.

    DO cont = 1 TO 12:
        F1009.valorSaldoPasivo = F1009.valorSaldoPasivo + anexos.cr[cont] - anexos.db[cont].
    END.
END.

FOR EACH F1009 WHERE F1009.concepto = "2203" NO-LOCK:
    totalReporte = totalReporte + F1009.valorSaldoPasivo.
END.

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "2305"
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1009.
    otros1009.concepto = "2203".
    otros1009.nit = "222222222".
    otros1009.tipoDoc = "43".
    otros1009.valorSaldoPasivo = totalContable - totalReporte.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1009_2204 W_FtosDian 
PROCEDURE 1009_2204 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR nitAux AS CHARACTER.
DEFINE VAR dvAux AS CHARACTER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "2430" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "2435" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "2440")
                      AND sal_cuenta.ano = ano_corte NO-LOCK BREAK BY sal_cuenta.cuenta:
    IF SUBSTRING(sal_cuenta.cuenta,1,6) = "244010" THEN
        ASSIGN nitAux = "800197268"
               dvAux = "4".
    ELSE
        ASSIGN nitAux = "899999112"
               dvAux = "0".
    
    IF FIRST-OF(sal_cuenta.cuenta) THEN DO:
        FIND FIRST F1009 WHERE F1009.nit = nitAux
                           AND F1009.concepto = "2204" NO-ERROR.
        IF NOT AVAILABLE F1009 THEN DO:
            CREATE F1009.
            ASSIGN F1009.nit = nitAux
                   F1009.dv = dvAux.
                   F1009.concepto = "2204".
    
            FIND FIRST clientes WHERE clientes.nit = nitAux NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1009.tipoDoc = "11".
                    WHEN "T.I" THEN F1009.tipoDoc = "12".
                    WHEN "C.C" THEN F1009.tipoDoc = "13".
                    WHEN "T.E" THEN F1009.tipoDoc = "21".
                    WHEN "C.E" THEN F1009.tipoDoc = "22".
                    WHEN "NIT" THEN F1009.tipoDoc = "31".
                    WHEN "PPTE" THEN F1009.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1009.apellido1 = clientes.apellido1.
                    F1009.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1009.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1009.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1009.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1009.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1009.direccion = clientes.DIR_residencia.
                ELSE
                    F1009.direccion = clientes.DIR_comercial.

                IF clientes.lugar_residencia <> "" THEN DO:
                    F1009.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1009.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1009.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1009.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1009.departamento = "" OR F1009.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1009.departamento = "11".
                            F1009.municipio = "001".
                        END.

                        
                        WHEN 2 THEN DO:
                            F1009.departamento = "05".
                            F1009.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1009.departamento = "17".
                            F1009.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1009.departamento = "76".
                            F1009.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1009.departamento = "11".
                            F1009.municipio = "001".
                        END.
                    END CASE.
                END.
    
                F1009.pais = "169".
            END.
    
            F1009.nit = nitAux.
        END.
    END.

    F1009.valorSaldoPasivo = F1009.valorSaldoPasivo + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        F1009.valorSaldoPasivo = F1009.valorSaldoPasivo + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1009_2205 W_FtosDian 
PROCEDURE 1009_2205 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.

FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,6) = "271005"
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1009 WHERE F1009.nit = anexos.nit
                           AND F1009.concepto = "2205" NO-ERROR.
        IF NOT AVAILABLE F1009 THEN DO:
            CREATE F1009.
            ASSIGN F1009.nit = anexos.nit
                   F1009.concepto = "2205".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1009.tipoDoc = "11".
                    WHEN "T.I" THEN F1009.tipoDoc = "12".
                    WHEN "C.C" THEN F1009.tipoDoc = "13".
                    WHEN "T.E" THEN F1009.tipoDoc = "21".
                    WHEN "C.E" THEN F1009.tipoDoc = "22".
                    WHEN "NIT" THEN F1009.tipoDoc = "31".
                    WHEN "PPTE" THEN F1009.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1009.apellido1 = clientes.apellido1.
                    F1009.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1009.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1009.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1009.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1009.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1009.direccion = clientes.DIR_residencia.
                ELSE
                    F1009.direccion = clientes.DIR_comercial.

                IF F1009.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1009.direccion = agencias.direccion.
                END.

                IF clientes.lugar_residencia <> "" THEN DO:
                    F1009.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1009.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1009.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1009.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1009.departamento = "" OR F1009.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1009.departamento = "11".
                            F1009.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1009.departamento = "05".
                            F1009.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1009.departamento = "17".
                            F1009.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1009.departamento = "76".
                            F1009.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1009.departamento = "11".
                            F1009.municipio = "001".
                        END.
                    END CASE.
                END.
    
                F1009.pais = "169".
            END.
    
            F1009.nit = anexos.nit.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1009.dv).
        END.
    END.

    F1009.valorSaldoPasivo = F1009.valorSaldoPasivo + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        F1009.valorSaldoPasivo = F1009.valorSaldoPasivo + anexos.cr[cont] - anexos.db[cont].
    END.
END.

FOR EACH F1009 WHERE F1009.concepto = "2205" NO-LOCK:
    totalReporte = totalReporte + F1009.valorSaldoPasivo.
END.

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,6) = "271005"
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1009.
    otros1009.concepto = "2205".
    otros1009.nit = "222222222".
    otros1009.tipoDoc = "43".
    otros1009.valorSaldoPasivo = totalContable - totalReporte.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1009_2206 W_FtosDian 
PROCEDURE 1009_2206 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR nitAux AS CHARACTER.

FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,4) = "2445" OR
                       SUBSTRING(anexos.cuenta,1,4) = "2450" OR
                       SUBSTRING(anexos.cuenta,1,4) = "2465" OR
                       SUBSTRING(anexos.cuenta,1,4) = "2720" OR
                       SUBSTRING(anexos.cuenta,1,4) = "2820")
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    nitAux = anexos.nit.

    IF SUBSTRING(anexos.cuenta,1,4) = "2445" THEN
        nitAux = "800197268".

    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1009 WHERE F1009.nit = nitAux
                           AND F1009.concepto = "2206" NO-ERROR.
        IF NOT AVAILABLE F1009 THEN DO:
            CREATE F1009.
            ASSIGN F1009.nit = nitAux
                   F1009.concepto = "2206".
    
            FIND FIRST clientes WHERE clientes.nit = nitAux NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1009.tipoDoc = "11".
                    WHEN "T.I" THEN F1009.tipoDoc = "12".
                    WHEN "C.C" THEN F1009.tipoDoc = "13".
                    WHEN "T.E" THEN F1009.tipoDoc = "21".
                    WHEN "C.E" THEN F1009.tipoDoc = "22".
                    WHEN "NIT" THEN F1009.tipoDoc = "31".
                    WHEN "PPTE" THEN F1009.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1009.apellido1 = clientes.apellido1.
                    F1009.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1009.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1009.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1009.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1009.razonSocial = clientes.nombre.
    
                IF clientes.DIR_residencia <> "" THEN
                    F1009.direccion = clientes.DIR_residencia.
                ELSE
                    F1009.direccion = clientes.DIR_comercial.

                IF F1009.direccion = "" THEN DO:
                    FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                    IF AVAILABLE agencias THEN
                        F1009.direccion = agencias.direccion.
                END.

                IF clientes.lugar_residencia <> "" THEN DO:
                    F1009.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F1009.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F1009.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F1009.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F1009.departamento = "" OR F1009.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F1009.departamento = "11".
                            F1009.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F1009.departamento = "05".
                            F1009.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F1009.departamento = "17".
                            F1009.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F1009.departamento = "76".
                            F1009.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F1009.departamento = "11".
                            F1009.municipio = "001".
                        END.
                    END CASE.
                END.

                F1009.pais = "169".
            END.
    
            F1009.nit = nitAux.
    
            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1009.dv).
        END.
    END.

    F1009.valorSaldoPasivo = F1009.valorSaldoPasivo + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        F1009.valorSaldoPasivo = F1009.valorSaldoPasivo + anexos.cr[cont] - anexos.db[cont].
    END.
END.

FOR EACH F1009 WHERE F1009.concepto = "2206" NO-LOCK:
    totalReporte = totalReporte + F1009.valorSaldoPasivo.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "2445" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "2450" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "2465" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "2720" OR
                           SUBSTRING(sal_cuenta.cuenta,1,4) = "2820")
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1009.
    otros1009.concepto = "2206".
    otros1009.nit = "222222222".
    otros1009.tipoDoc = "43".
    otros1009.valorSaldoPasivo = totalContable - totalReporte.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1010 W_FtosDian 
PROCEDURE 1010 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR suma AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalAportes AS DECIMAL.
DEFINE VAR vDecimales AS DECIMAL.

EMPTY TEMP-TABLE F1010.


FOR EACH rep_ahorros WHERE rep_ahorros.fecCorte = DATE(12, 31, ano_corte)
                       AND rep_ahorros.tip_ahorro = 4 NO-LOCK BREAK BY rep_ahorros.nit:
    totalAportes = totalAportes + rep_ahorros.sdo_disponible + rep_ahorros.sdo_canje.
END.

FOR EACH rep_ahorros WHERE rep_ahorros.fecCorte = DATE(12, 31, ano_corte)
                       AND rep_ahorros.tip_ahorro = 4 NO-LOCK BREAK BY rep_ahorros.nit:
    suma = suma + rep_ahorros.sdo_disponible + rep_ahorros.sdo_canje.

    IF LAST-OF(rep_ahorros.nit) THEN DO:
        CREATE F1010.
        ASSIGN F1010.nit = rep_ahorros.nit.

        FIND FIRST clientes WHERE clientes.nit = rep_ahorros.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN F1010.tipoDoc = "11".
                WHEN "T.I" THEN F1010.tipoDoc = "12".
                WHEN "C.C" THEN F1010.tipoDoc = "13".
                WHEN "T.E" THEN F1010.tipoDoc = "21".
                WHEN "C.E" THEN F1010.tipoDoc = "22".
                WHEN "NIT" THEN F1010.tipoDoc = "31".
                WHEN "PPTE" THEN F1010.tipoDoc = "41".
            END CASE.

            IF clientes.tipo_identificacion <> "NIT" THEN DO:
                F1010.apellido1 = clientes.apellido1.
                F1010.apellido2 = clientes.apellido2.

                IF INDEX(clientes.nombre," ") > 0 THEN DO:
                    F1010.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                    F1010.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                END.
                ELSE
                    F1010.nombre1 = clientes.nombre.
            END.
            ELSE
                F1010.razonSocial = clientes.nombre.

            IF clientes.DIR_residencia <> "" THEN
                F1010.direccion = clientes.DIR_residencia.
            ELSE
                F1010.direccion = clientes.DIR_comercial.

            IF F1010.direccion = "" THEN DO:
                FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                IF AVAILABLE agencias THEN
                    F1010.direccion = agencias.direccion.
            END.

            IF clientes.lugar_residencia <> "" AND clientes.lugar_residencia <> ? THEN DO:
                F1010.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                F1010.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
            END.
            ELSE DO:
                F1010.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                F1010.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
            END.

            IF F1010.departamento = "" OR F1010.departamento = ? THEN DO:
                CASE clientes.agencia:
                    WHEN 1 THEN DO:
                        F1010.departamento = "11".
                        F1010.municipio = "001".
                    END.

                    WHEN 2 THEN DO:
                        F1010.departamento = "05".
                        F1010.municipio = "001".
                    END.

                    WHEN 3 THEN DO:
                        F1010.departamento = "17".
                        F1010.municipio = "001".
                    END.

                    WHEN 4 THEN DO:
                        F1010.departamento = "76".
                        F1010.municipio = "250".
                    END.

                    OTHERWISE DO:
                        F1010.departamento = "11".
                        F1010.municipio = "001".
                    END.
                END CASE.
            END.

            F1010.pais = "169".
        END.

        F1010.nit = rep_ahorros.nit.

        IF clientes.tipo_identificacion = "NIT" THEN
            RUN digitoVerificacion.r(INPUT clientes.nit,
                                     OUTPUT F1010.dv).

        F1010.valorPatrimonio = suma.
        F1010.porcentajeParticipacion = TRUNCATE((suma * 100) / totalAportes,0).
        vDecimales = ((suma * 100) / totalAportes) - TRUNCATE((suma * 100) / totalAportes,0).

        F1010.decimales = STRING(vDecimales).
        
        suma = 0.
    END.
END.

EMPTY TEMP-TABLE otros1010.

OUTPUT TO C:\INFO_Fodun\F1010.csv.

FOR EACH F1010 NO-LOCK:
    IF F1010.valorPatrimonio >= DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) OR DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) = 0 THEN
        EXPORT DELIMITER ";" F1010.
    ELSE DO:
        FIND FIRST otros1010 NO-ERROR.
        IF NOT AVAILABLE otros1010 THEN DO:
            CREATE otros1010.
            otros1010.nit = "222222222".
            otros1010.tipoDoc = "43".
        END.

        otros1010.valorPatrimonio = otros1010.valorPatrimonio + F1010.valorPatrimonio.
    END.
END.

FOR EACH otros1010:
    EXPORT DELIMITER ";" otros1010.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1012 W_FtosDian 
PROCEDURE 1012 :
DEFINE VAR suma AS DECIMAL.
    
RUN 1012_1110.
RUN 1012_1201.
RUN 1012_1203.
RUN 1012_1204.

EMPTY TEMP-TABLE otros1012.
EMPTY TEMP-TABLE reportar.

OUTPUT TO C:\INFO_Fodun\F1012.csv.

FOR EACH F1012 NO-LOCK:
    IF F1012.valorSaldosPatrimoniales >= DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) OR DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) = 0 THEN
        EXPORT DELIMITER ";" F1012.
    ELSE DO:
        FIND FIRST otros1012 WHERE otros1012.concepto = F1012.concepto NO-ERROR.
        IF NOT AVAILABLE otros1012 THEN DO:
            CREATE otros1012.
            otros1012.concepto = F1012.concepto.
            otros1012.nit = "222222222".
            otros1012.tipoDoc = "43".
        END.

        otros1012.ValorSaldosPatrimoniales = otros1012.ValorSaldosPatrimoniales + F1012.ValorSaldosPatrimoniales.
    END.
END.

FOR EACH otros1012:
    EXPORT DELIMITER ";" otros1012.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1012_1110 W_FtosDian 
PROCEDURE 1012_1110 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.

/* OK */
FOR EACH anexos WHERE (SUBSTRING(anexos.cuenta,1,4) = "1110" /*OR
                       SUBSTRING(anexos.cuenta,1,4) = "1120" OR
                       SUBSTRING(anexos.cuenta,1,4) = "1203" OR
                       SUBSTRING(anexos.cuenta,1,4) = "1204" OR
                       SUBSTRING(anexos.cuenta,1,4) = "1205" OR
                       SUBSTRING(anexos.cuenta,1,4) = "1206" OR
                       SUBSTRING(anexos.cuenta,1,4) = "1208" OR
                       SUBSTRING(anexos.cuenta,1,4) = "1213"*/)
                    AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                             BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1012 WHERE F1012.nit = anexos.nit
                           AND F1012.concepto = "1110" NO-ERROR.
        IF NOT AVAILABLE F1012 THEN DO:
            CREATE F1012.
            ASSIGN F1012.nit = anexos.nit
                   F1012.concepto = "1110".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1012.tipoDoc = "11".
                    WHEN "T.I" THEN F1012.tipoDoc = "12".
                    WHEN "C.C" THEN F1012.tipoDoc = "13".
                    WHEN "T.E" THEN F1012.tipoDoc = "21".
                    WHEN "C.E" THEN F1012.tipoDoc = "22".
                    WHEN "NIT" THEN F1012.tipoDoc = "31".
                    WHEN "PPTE" THEN F1012.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1012.apellido1 = clientes.apellido1.
                    F1012.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1012.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1012.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1012.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1012.razonSocial = clientes.nombre.
    
                F1012.pais = "169".
            END.
    
            F1012.nit = anexos.nit.

            IF F1012.tipoDoc = "31" THEN
                RUN digitoVerificacion.r(INPUT anexos.nit,
                                         OUTPUT F1012.dv).
        END.
    END.

    F1012.valorSaldosPatrimoniales = F1012.valorSaldosPatrimoniales + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        F1012.valorSaldosPatrimoniales = F1012.valorSaldosPatrimoniales + anexos.db[cont] - anexos.cr[cont].
    END.
END.

/* Ajuste */
FOR EACH F1012 WHERE F1012.concepto = "1110" NO-LOCK:
    totalReporte = totalReporte + F1012.valorSaldosPatrimoniales.
END.

FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,4) = "1110" /*OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1120" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1203" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1204" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1205" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1206" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1208" OR
                             SUBSTRING(sal_cuenta.cuenta,1,4) = "1213"*/)
                        AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1012.
    otros1012.concepto = "1110".
    otros1012.nit = "222222222".
    otros1012.tipoDoc = "43".
    otros1012.valorSaldosPatrimoniales = totalContable - totalReporte.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1012_1201 W_FtosDian 
PROCEDURE 1012_1201 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalContable AS DECIMAL.
DEFINE VAR totalReporte AS DECIMAL.

FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,4) = "1203"
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1012 WHERE F1012.nit = anexos.nit
                           AND F1012.concepto = "1201" NO-ERROR.
        IF NOT AVAILABLE F1012 THEN DO:
            CREATE F1012.
            ASSIGN F1012.nit = anexos.nit
                   F1012.concepto = "1201".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1012.tipoDoc = "11".
                    WHEN "T.I" THEN F1012.tipoDoc = "12".
                    WHEN "C.C" THEN F1012.tipoDoc = "13".
                    WHEN "T.E" THEN F1012.tipoDoc = "21".
                    WHEN "C.E" THEN F1012.tipoDoc = "22".
                    WHEN "NIT" THEN F1012.tipoDoc = "31".
                    WHEN "PPTE" THEN F1012.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1012.apellido1 = clientes.apellido1.
                    F1012.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1012.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1012.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1012.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1012.razonSocial = clientes.nombre.
    
                F1012.pais = "169".
            END.
    
            F1012.nit = anexos.nit.
    
            IF F1012.tipoDoc = "31" THEN
                RUN digitoVerificacion.r(INPUT anexos.nit,
                                         OUTPUT F1012.dv).
        END.
    END.

    F1012.valorSaldosPatrimoniales = F1012.valorSaldosPatrimoniales + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        F1012.valorSaldosPatrimoniales = F1012.valorSaldosPatrimoniales + anexos.db[cont] - anexos.cr[cont].
    END.
END.

/* Ajuste */
FOR EACH F1012 WHERE F1012.concepto = "1201" NO-LOCK:
    totalReporte = totalReporte + F1012.valorSaldosPatrimoniales.
END.

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "1203"
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable - totalReporte <> 0 THEN DO:
    CREATE otros1012.
    otros1012.concepto = "1201".
    otros1012.nit = "222222222".
    otros1012.tipoDoc = "43".
    otros1012.valorSaldosPatrimoniales = totalContable - totalReporte.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1012_1203 W_FtosDian 
PROCEDURE 1012_1203 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.

/* OK */
FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,4) = "1115"
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1012 WHERE F1012.nit = anexos.nit
                           AND F1012.concepto = "1203" NO-ERROR.
        IF NOT AVAILABLE F1012 THEN DO:
            CREATE F1012.
            ASSIGN F1012.nit = anexos.nit
                   F1012.concepto = "1203".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1012.tipoDoc = "11".
                    WHEN "T.I" THEN F1012.tipoDoc = "12".
                    WHEN "C.C" THEN F1012.tipoDoc = "13".
                    WHEN "T.E" THEN F1012.tipoDoc = "21".
                    WHEN "C.E" THEN F1012.tipoDoc = "22".
                    WHEN "NIT" THEN F1012.tipoDoc = "31".
                    WHEN "PPTE" THEN F1012.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1012.apellido1 = clientes.apellido1.
                    F1012.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1012.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1012.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1012.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1012.razonSocial = clientes.nombre.
    
                F1012.pais = "169".

                IF clientes.tipo_identificacion = "NIT" THEN
                    RUN digitoVerificacion.r(INPUT clientes.nit,
                                             OUTPUT F1012.dv).
            END.
        END.
    END.

    F1012.valorSaldosPatrimoniales = F1012.valorSaldosPatrimoniales + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        F1012.valorSaldosPatrimoniales = F1012.valorSaldosPatrimoniales + anexos.db[cont] - anexos.cr[cont].
    END.
END.

/* Ajuste */
FOR EACH F1012 WHERE F1012.concepto = "1203" NO-LOCK:
    totalReporte = totalReporte + F1012.valorSaldosPatrimoniales.
END.

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "1115"
                      AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1012.
    otros1012.concepto = "1203".
    otros1012.nit = "222222222".
    otros1012.tipoDoc = "43".
    otros1012.valorSaldosPatrimoniales = totalContable - totalReporte.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1012_1204 W_FtosDian 
PROCEDURE 1012_1204 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR totalReporte AS DECIMAL.
DEFINE VAR totalContable AS DECIMAL.

/* OK */
FOR EACH anexos WHERE SUBSTRING(anexos.cuenta,1,4) = "1226"
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F1012 WHERE F1012.nit = anexos.nit
                           AND F1012.concepto = "1204" NO-ERROR.
        IF NOT AVAILABLE F1012 THEN DO:
            CREATE F1012.
            ASSIGN F1012.nit = anexos.nit
                   F1012.concepto = "1204".
    
            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                CASE clientes.tipo_identificacion:
                    WHEN "R.C" THEN F1012.tipoDoc = "11".
                    WHEN "T.I" THEN F1012.tipoDoc = "12".
                    WHEN "C.C" THEN F1012.tipoDoc = "13".
                    WHEN "T.E" THEN F1012.tipoDoc = "21".
                    WHEN "C.E" THEN F1012.tipoDoc = "22".
                    WHEN "NIT" THEN F1012.tipoDoc = "31".
                    WHEN "PPTE" THEN F1012.tipoDoc = "41".
                END CASE.
    
                IF clientes.tipo_identificacion <> "NIT" THEN DO:
                    F1012.apellido1 = clientes.apellido1.
                    F1012.apellido2 = clientes.apellido2.
    
                    IF INDEX(clientes.nombre," ") > 0 THEN DO:
                        F1012.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                        F1012.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                    END.
                    ELSE
                        F1012.nombre1 = clientes.nombre.
                END.
                ELSE
                    F1012.razonSocial = clientes.nombre.
    
                F1012.pais = "169".

                IF clientes.tipo_identificacion = "NIT" THEN
                    RUN digitoVerificacion.r(INPUT clientes.nit,
                                             OUTPUT F1012.dv).
            END.
        END.
    END.

    F1012.valorSaldosPatrimoniales = F1012.valorSaldosPatrimoniales + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        F1012.valorSaldosPatrimoniales = F1012.valorSaldosPatrimoniales + anexos.db[cont] - anexos.cr[cont].
    END.
END.

/* Ajuste */
FOR EACH F1012 WHERE F1012.concepto = "1204" NO-LOCK:
    totalReporte = totalReporte + F1012.valorSaldosPatrimoniales.
END.

FOR EACH sal_cuenta WHERE SUBSTRING(sal_cuenta.cuenta,1,4) = "1226"
                        AND sal_cuenta.ano = ano_corte NO-LOCK:
    totalContable = totalContable + sal_cuenta.sal_inicial.

    DO cont = 1 TO 12:
        totalContable = totalContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
    END.
END.

IF totalContable > totalReporte THEN DO:
    CREATE otros1012.
    otros1012.concepto = "1204".
    otros1012.nit = "222222222".
    otros1012.tipoDoc = "43".
    otros1012.valorSaldosPatrimoniales = totalContable - totalReporte.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 1026 W_FtosDian 
PROCEDURE 1026 :
EMPTY TEMP-TABLE F1026.
    
FOR EACH mov_creditos WHERE YEAR(mov_creditos.fecha) = ano_corte
                        AND (mov_creditos.cod_operacion = 020102001 OR
                             mov_creditos.cod_operacion = 020102003) NO-LOCK:
    FIND FIRST ttcreds WHERE ttcreds.nit = mov_creditos.nit NO-ERROR.
    IF NOT AVAILABLE ttcreds THEN DO:
        CREATE ttcreds.
        ttcreds.nit = mov_creditos.nit.
    END.

    ttcreds.sdoCreditos = ttcreds.sdoCreditos + mov_creditos.val_efectivo + mov_creditos.val_cheque.

    FIND FIRST creditos WHERE creditos.nit = mov_creditos.nit AND creditos.num_credito = mov_creditos.num_credito NO-LOCK NO-ERROR.

    CREATE ttcredsDet.
    ttcredsDet.nit = mov_creditos.nit.

    CASE creditos.tip_credito:
        WHEN 1 THEN ttcredsDet.tipo = 2.
        WHEN 2 THEN ttcredsDet.tipo = 1.
        WHEN 3 THEN ttcredsDet.tipo = 3.
        OTHERWISE ttcredsDet.tipo = 4.
     END CASE.

    ttcredsDet.tipo = creditos.tip_credito.
    ttcredsDet.sdoCreditos = mov_creditos.val_efectivo + mov_creditos.val_cheque.
END.

FOR EACH ttcreds WHERE ttcreds.sdoCreditos > DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) NO-LOCK:
    FOR EACH ttcredsDet WHERE ttcredsDet.nit = ttcreds.nit NO-LOCK:
        FIND FIRST F1026 WHERE F1026.nit = ttcredsDet.nit AND F1026.cod = ttcredsDet.tipo NO-ERROR.
        IF NOT AVAILABLE F1026 THEN DO:
            CREATE F1026.
            F1026.cod = ttcredsDet.tipo.

            FIND FIRST clientes WHERE clientes.nit = ttcreds.nit NO-LOCK NO-ERROR.

            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN F1026.tipoDoc = "11".
                WHEN "T.I" THEN F1026.tipoDoc = "12".
                WHEN "C.C" THEN F1026.tipoDoc = "13".
                WHEN "T.E" THEN F1026.tipoDoc = "21".
                WHEN "C.E" THEN F1026.tipoDoc = "22".
                WHEN "NIT" THEN F1026.tipoDoc = "31".
                WHEN "PPTE" THEN F1026.tipoDoc = "41".
            END CASE.

            F1026.nit = ttcredsDet.nit.

            IF clientes.tipo_identificacion = "NIT" THEN
                RUN digitoVerificacion.r(INPUT clientes.nit,
                                         OUTPUT F1026.dv) NO-ERROR.

            IF clientes.tipo_identificacion <> "NIT" THEN DO:
                F1026.apellido1 = clientes.apellido1.
                F1026.apellido2 = clientes.apellido2.

                IF INDEX(clientes.nombre," ") > 0 THEN DO:
                    F1026.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                    F1026.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                END.
                ELSE
                    F1026.nombre1 = clientes.nombre.
            END.
            ELSE
                F1026.razonSocial = clientes.nombre.

            IF clientes.DIR_residencia <> "" THEN
                F1026.direccion = clientes.DIR_residencia.
            ELSE
                F1026.direccion = clientes.DIR_comercial.

            IF F1026.direccion = "" THEN DO:
                FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
                IF AVAILABLE agencias THEN
                    F1026.direccion = agencias.direccion.
            END.

            IF clientes.lugar_residencia <> "" THEN DO:
                F1026.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                F1026.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
            END.
            ELSE DO:
                F1026.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                F1026.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
            END.

            IF F1026.departamento = "" OR F1026.departamento = ? THEN DO:
                CASE clientes.agencia:
                    WHEN 1 THEN DO:
                        F1026.departamento = "11".
                        F1026.municipio = "001".
                    END.
            
                    WHEN 2 THEN DO:
                        F1026.departamento = "05".
                        F1026.municipio = "001".
                    END.

                    WHEN 3 THEN DO:
                        F1026.departamento = "17".
                        F1026.municipio = "001".
                    END.

                    WHEN 4 THEN DO:
                        F1026.departamento = "76".
                        F1026.municipio = "250".
                    END.

                    OTHERWISE DO:
                        F1026.departamento = "11".
                        F1026.municipio = "001".
                    END.
                END CASE.
            END.
        END.
        
        F1026.valorPrestamos = F1026.valorPrestamos + ttcredsDet.sdoCreditos.
    END.
END.

OUTPUT TO C:\INFO_Fodun\F1026.csv.
FOR EACH F1026 NO-LOCK:
    EXPORT DELIMITER ";" F1026.
END.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE 2276 W_FtosDian 
PROCEDURE 2276 :
DEFINE VAR cont AS INTEGER.
DEFINE VAR vSmmlv AS INTEGER.

FIND FIRST entidad NO-LOCK NO-ERROR.
FIND FIRST indicadores WHERE indicadores.indicador = entidad.ind_SMLV NO-LOCK NO-ERROR.
IF AVAILABLE indicadores THEN
    vSmmlv = indicadores.valor.

EMPTY TEMP-TABLE F2276.

FOR EACH anexos WHERE ((SUBSTRING(anexos.cuenta,1,8) = "51050101" OR     /* Pagos */
                        SUBSTRING(anexos.cuenta,1,8) = "51050301" OR
                        SUBSTRING(anexos.cuenta,1,8) = "51050501" OR
                        SUBSTRING(anexos.cuenta,1,8) = "51050901" OR
                        SUBSTRING(anexos.cuenta,1,8) = "51051201" OR
                        SUBSTRING(anexos.cuenta,1,8) = "51051601" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101206" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101215" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101227" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101236" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101239" OR

                        SUBSTRING(anexos.cuenta,1,8) = "51051001" OR     /* Cesantías */
                        SUBSTRING(anexos.cuenta,1,8) = "51051101" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101230" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101233" OR
          
                        SUBSTRING(anexos.cuenta,1,8) = "51051901" OR     /* Otros  ingresos */
                        SUBSTRING(anexos.cuenta,1,8) = "51052001" OR
                        SUBSTRING(anexos.cuenta,1,8) = "51102714" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101248" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101260" OR

                        SUBSTRING(anexos.cuenta,1,8) = "51050301" OR     /* Aportes Salud */
                        SUBSTRING(anexos.cuenta,1,8) = "51050501" OR
                        SUBSTRING(anexos.cuenta,1,8) = "51051601" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101206" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101215" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101239" OR

                        SUBSTRING(anexos.cuenta,1,8) = "51050301" OR     /* Aportes Obligatorios Pension */
                        SUBSTRING(anexos.cuenta,1,8) = "51050501" OR
                        SUBSTRING(anexos.cuenta,1,8) = "51051601" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101206" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101215" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101239" OR
                        SUBSTRING(anexos.cuenta,1,8) = "24501003" OR

                        SUBSTRING(anexos.cuenta,1,8) = "24350501"        /* Retención en la fuente */)
                       AND anexos.ano = ano_corte)
                      OR
                      ((SUBSTRING(anexos.cuenta,1,8) = "51051001" OR     /* Cesantías */
                        SUBSTRING(anexos.cuenta,1,8) = "51051101" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101230" OR
                        SUBSTRING(anexos.cuenta,1,10) = "6140101233")
                       AND anexos.ano = ano_corte - 1) NO-LOCK BREAK BY anexos.nit
                                                                     BY anexos.cuenta:

    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST F2276 WHERE F2276.nit = anexos.nit NO-ERROR.
        IF NOT AVAILABLE F2276 THEN DO:
            CREATE F2276.

            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.

            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN F2276.tipoDoc = "11".
                WHEN "T.I" THEN F2276.tipoDoc = "12".
                WHEN "C.C" THEN F2276.tipoDoc = "13".
                WHEN "T.E" THEN F2276.tipoDoc = "21".
                WHEN "C.E" THEN F2276.tipoDoc = "22".
                WHEN "NIT" THEN F2276.tipoDoc = "31".
                WHEN "PPTE" THEN F2276.tipoDoc = "41".
            END CASE.

            F2276.nit = anexos.nit.

            FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN DO:
                F2276.apellido1 = clientes.apellido1.
                F2276.apellido2 = clientes.apellido2.

                IF INDEX(clientes.nombre," ") > 0 THEN DO:
                    F2276.nombre1 = SUBSTRING(clientes.nombre,1,INDEX(clientes.nombre," ") - 1).
                    F2276.nombre2 = SUBSTRING(clientes.nombre,INDEX(clientes.nombre," ") + 1).
                END.
                ELSE
                    F2276.nombre1 = clientes.nombre.
                
                IF clientes.lugar_residencia <> "" THEN DO:
                    F2276.departamento = SUBSTRING(clientes.lugar_residencia,1,2).
                    F2276.municipio = SUBSTRING(clientes.lugar_residencia,3,3).
                END.
                ELSE DO:
                    F2276.departamento = SUBSTRING(clientes.lugar_comercial,1,2).
                    F2276.municipio = SUBSTRING(clientes.lugar_comercial,3,3).
                END.

                IF F2276.departamento = "" OR F2276.departamento = ? THEN DO:
                    CASE clientes.agencia:
                        WHEN 1 THEN DO:
                            F2276.departamento = "11".
                            F2276.municipio = "001".
                        END.

                        WHEN 2 THEN DO:
                            F2276.departamento = "05".
                            F2276.municipio = "001".
                        END.

                        WHEN 3 THEN DO:
                            F2276.departamento = "17".
                            F2276.municipio = "001".
                        END.

                        WHEN 4 THEN DO:
                            F2276.departamento = "76".
                            F2276.municipio = "250".
                        END.

                        OTHERWISE DO:
                            F2276.departamento = "11".
                            F2276.municipio = "001".
                        END.
                    END CASE.
                END.
            END.

            F2276.fec_ini = STRING(DATE(1, 1, ano_corte),"99/99/9999").
            F2276.fec_fin = STRING(DATE(12, 31, ano_corte),"99/99/9999").
            F2276.fec_expCert = STRING(DATE(3, 31, ano_corte + 1),"99/99/9999").
        END.
    END.

    /* Pagos */
    IF (SUBSTRING(anexos.cuenta,1,8) = "51050101" OR     /* Pagos */
        SUBSTRING(anexos.cuenta,1,8) = "51050301" OR
        SUBSTRING(anexos.cuenta,1,8) = "51050501" OR
        SUBSTRING(anexos.cuenta,1,8) = "51050901" OR
        SUBSTRING(anexos.cuenta,1,8) = "51051201" OR
        SUBSTRING(anexos.cuenta,1,8) = "51051601" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101206" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101215" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101227" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101236" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101239") THEN DO:
        DO cont = 1 TO 12:
            F2276.pagos = F2276.pagos + anexos.db[cont] - anexos.cr[cont].
        END.
    END.

    /* Cesantías */
    IF (SUBSTRING(anexos.cuenta,1,8) = "51051001" OR     /* Cesantías */
        SUBSTRING(anexos.cuenta,1,8) = "51051101" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101230" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101233") AND anexos.ano = ano_corte THEN DO:
        DO cont = 1 TO 12:
            F2276.cesantias = F2276.cesantias + anexos.db[cont] - anexos.cr[cont].
        END.
    END.

    /* Cesantías Ant */
    IF (SUBSTRING(anexos.cuenta,1,8) = "51051001" OR     /* Cesantías */
        SUBSTRING(anexos.cuenta,1,8) = "51051101" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101230" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101233") AND anexos.ano = ano_corte - 1 THEN DO:
        DO cont = 1 TO 12:
            F2276.cesantiasAnt = F2276.cesantiasAnt + anexos.db[cont] - anexos.cr[cont].
        END.
    END.

    /* Otros ingresos */
    IF (SUBSTRING(anexos.cuenta,1,8) = "51051901" OR     /* Otros  ingresos */
        SUBSTRING(anexos.cuenta,1,8) = "51052001" OR
        SUBSTRING(anexos.cuenta,1,8) = "51102714" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101248" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101260") THEN DO:
        DO cont = 1 TO 12:
            F2276.otrosIngresos = F2276.otrosIngresos + anexos.db[cont] - anexos.cr[cont].
        END.
    END.

    /* Aportes Salud */
    IF (SUBSTRING(anexos.cuenta,1,8) = "51050301" OR     /* Aportes Salud */
        SUBSTRING(anexos.cuenta,1,8) = "51050501" OR
        SUBSTRING(anexos.cuenta,1,8) = "51051601" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101206" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101215" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101239") THEN DO:
        DO cont = 1 TO 12:
            F2276.aportesSalud = F2276.aportesSalud + anexos.db[cont] - anexos.cr[cont].
        END.
    END.

    /* Aportes Obligatorios Pensión */
    IF (SUBSTRING(anexos.cuenta,1,8) = "51050301" OR     /* Aportes Obligatorios Pension */
        SUBSTRING(anexos.cuenta,1,8) = "51050501" OR
        SUBSTRING(anexos.cuenta,1,8) = "51051601" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101206" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101215" OR
        SUBSTRING(anexos.cuenta,1,10) = "6140101239" OR
        SUBSTRING(anexos.cuenta,1,8) = "24501003") THEN DO:
        DO cont = 1 TO 12:
            IF SUBSTRING(anexos.cuenta,1,8) <> "24501003" THEN
                F2276.aportesObligatoriosPension = F2276.aportesObligatoriosPension + anexos.db[cont] - anexos.cr[cont].
            ELSE
                F2276.aportesObligatoriosPension = F2276.aportesObligatoriosPension + anexos.cr[cont] - anexos.cr[cont].
        END.
    END.

    /* Retención en la fuente */
    IF SUBSTRING(anexos.cuenta,1,8) = "24350501" THEN DO:
        DO cont = 1 TO 12:
            F2276.retefuente = F2276.retefuente + anexos.db[cont] - anexos.cr[cont].
        END.
    END.
END.

FOR EACH F2276:
    F2276.totalIngresos = F2276.pagos + F2276.otrosIngresos.
    F2276.aportesSalud = ROUND((F2276.aportesSalud / 100) * 4,0).

    IF F2276.aportesObligatoriosPension <= vSmmlv * 4 THEN
        F2276.aportesObligatoriosPension = ROUND((F2276.aportesObligatoriosPension / 100) * 4,0).
    ELSE
        F2276.aportesObligatoriosPension = ROUND((F2276.aportesObligatoriosPension / 100) * 5,0).
END.


OUTPUT TO C:\INFO_Fodun\F2276.csv.
FOR EACH F2276 NO-LOCK:
    EXPORT DELIMITER ";" F2276.
END.
OUTPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W_FtosDian  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W_FtosDian)
  THEN DELETE WIDGET W_FtosDian.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W_FtosDian  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY Ano_Corte Cmb_Ftos tope tgTope 
      WITH FRAME F_Ftos IN WINDOW W_FtosDian.
  ENABLE Ano_Corte Cmb_Ftos tgTope Btn_Proc 
      WITH FRAME F_Ftos IN WINDOW W_FtosDian.
  {&OPEN-BROWSERS-IN-QUERY-F_Ftos}
  VIEW W_FtosDian.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerarArchivo W_FtosDian 
PROCEDURE GenerarArchivo :
DEFINE INPUT PARAMETER formato AS INTEGER.
DEFINE INPUT PARAMETER cantidadRegistros AS INTEGER.
DEFINE INPUT PARAMETER vVersion AS INTEGER.

DEFINE VAR nombreArchivo AS CHARACTER.
DEFINE VAR cabecera AS CHARACTER.
DEFINE VAR cont AS INTEGER INITIAL 1.
DEFINE VAR contRegistros AS INTEGER.
DEFINE VAR cantidadArchivos AS INTEGER.
DEFINE VAR totalCabecera AS DECIMAL.

cantidadArchivos = TRUNCATE(cantidadRegistros / 5000,0) + 1.

DO cont = 1 TO cantidadArchivos:
    nombreArchivo = "C:\Info_Cooprudea\Dmuisca_01" + STRING(formato,"99999") + STRING(vVersion,"99") + STRING(ano_corte,"9999") + STRING(cont,"99999999") + ".txt".
    
    CASE formato:
        WHEN 1019 THEN DO:
            FOR EACH TT_1019 WHERE TT_1019.registro <= cont * 5000 NO-LOCK:
                contRegistros = contRegistros + 1.
                totalCabecera = totalCabecera + TT_1019.mov.
            END.
        END.

        WHEN 1020 THEN DO:
            FOR EACH TT_1020 WHERE TT_1020.registro <= cont * 5000 NO-LOCK:
                contRegistros = contRegistros + 1.
                totalCabecera = totalCabecera + TT_1020.inv.
            END.
        END.
    END CASE.

    cabecera = STRING(ano_corte,"9999") +
               "01" +
               STRING(formato,"99999") +
               STRING(vVersion,"99") +
               STRING(cont,"99999999") +
               STRING(YEAR(TODAY),"9999") + "-" + STRING(MONTH(TODAY),"99") + "-" + STRING(DAY(TODAY),"99") + STRING(TIME,"HH:MM:SS") +
               STRING(ano_corte,"9999") + "-01-01" +
               STRING(ano_corte,"9999") + "-12-31" +
               STRING(totalCabecera) +
               STRING(contRegistros).

    OUTPUT TO VALUE(nombreArchivo).
        PUT UNFORMATTED cabecera FORMAT "X(100)".

        FOR EACH TT_1020 WHERE TT_1020.registro <= cont * 5000 NO-LOCK:
            PUT UNFORMATTED
                TT_1020.tdoc FORMAT "X(2)" AT 1
                TT_1020.nid FORMAT "X(20)" AT 4
                TT_1020.dv FORMAT "X(1)" AT 25
                TT_1020.apl1 FORMAT "X(60)" AT 27
                TT_1020.apl2 FORMAT "X(60)" AT 88
                TT_1020.nom1 FORMAT "X(60)" AT 149
                TT_1020.nom2 FORMAT "X(60)" AT 210
                TT_1020.raz FORMAT "X(120)" AT 271
                TT_1020.direccion FORMAT "X(120)" AT 392
                TT_1020.dpto FORMAT "X(2)" AT 513
                TT_1020.mun FORMAT "X(3)" AT 516
                TT_1020.salini FORMAT "99999999999999999999" AT 520
                TT_1020.inv FORMAT "99999999999999999999" AT 541
                TT_1020.ren FORMAT "99999999999999999999" AT 562
                TT_1020.saldic FORMAT "99999999999999999999" AT 583
                TT_1020.ntit FORMAT "X(20)" AT 604
                TT_1020.numt FORMAT "999" AT 625.

            DO cont = 1 TO TT_1020.numt:
                PUT UNFORMATTED
                    " " TT_1020.titSec_tdoc[cont] FORMAT "X(2)"
                    " " TT_1020.titSec_nid[cont] FORMAT "X(20)"
                    " " TT_1020.titSec_dv[cont] FORMAT "X(1)"
                    " " TT_1020.titSec_apl1[cont] FORMAT "X(60)"
                    " " TT_1020.titSec_apl2[cont] FORMAT "X(60)"
                    " " TT_1020.titSec_nom1[cont] FORMAT "X(60)"
                    " " TT_1020.titSec_nom2[cont] FORMAT "X(60)"
                    " " TT_1020.titSec_raz[cont] FORMAT "X(120)".
            END.
            
            DELETE TT_1020.
        END.
    OUTPUT CLOSE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llena_Formatos W_FtosDian 
PROCEDURE Llena_Formatos :
DO WITH FRAME F_Ftos:
    Cmb_Ftos:ADD-LAST("1001_5001").
    Cmb_Ftos:ADD-LAST("1001_5002").
    Cmb_Ftos:ADD-LAST("1001_5003").
    Cmb_Ftos:ADD-LAST("1001_5004").
    Cmb_Ftos:ADD-LAST("1001_5005").
    Cmb_Ftos:ADD-LAST("Formato_1001_5006").
    Cmb_Ftos:ADD-LAST("Formato_1001_5008").
    Cmb_Ftos:ADD-LAST("Formato_1001_5010").
    Cmb_Ftos:ADD-LAST("Formato_1001_5011").
    Cmb_Ftos:ADD-LAST("Formato_1001_5012").
    Cmb_Ftos:ADD-LAST("Formato_1001_5015").
    Cmb_Ftos:ADD-LAST("Formato_1001_5016").
    Cmb_Ftos:ADD-LAST("Formato_1001_5055").
    Cmb_Ftos:ADD-LAST("Formato_1001_5056").
    Cmb_Ftos:ADD-LAST("Formato_1001_5059").
    Cmb_Ftos:ADD-LAST("Formato_1003").
    Cmb_Ftos:ADD-LAST("Formato_1006").
    Cmb_Ftos:ADD-LAST("Formato_1007").
    Cmb_Ftos:ADD-LAST("Formato_1008").
    Cmb_Ftos:ADD-LAST("Formato_1009").
    Cmb_Ftos:ADD-LAST("Formato_1010").
    Cmb_Ftos:ADD-LAST("Formato_1012").
    Cmb_Ftos:ADD-LAST("Formato_1026").
    Cmb_Ftos:ADD-LAST("Formato_2276").
    Cmb_Ftos:ADD-LAST("SHD_ReteICA").
    Cmb_Ftos:ADD-LAST("SHD_ReteICA_Antes2017").
    Cmb_Ftos:ADD-LAST("SHD_CompraBienes&Servicios").
    Cmb_Ftos:ADD-LAST("SHD_CompraBienes&Servicios_Antes2017").
    Cmb_Ftos:ADD-LAST("SHD_VentaBienes&Servicios").
    Cmb_Ftos:ADD-LAST("SHD_VentaBienes&Servicios_Antes2017").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SHD_CompraBienes&Servicios W_FtosDian 
PROCEDURE SHD_CompraBienes&Servicios :
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE SHD_Bienes&Servicios.

FOR EACH anexos WHERE anexos.agencia = 1
                    AND (SUBSTRING(anexos.cuenta,1,6) = "511001" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51102711" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51102712" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51102713" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51102714" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511030" OR
                         SUBSTRING(anexos.cuenta,1,8) = "61509512" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511028" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101154" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511003" OR
                         SUBSTRING(anexos.cuenta,1,2) = "26" OR
                         SUBSTRING(anexos.cuenta,1,6) = "510521" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51100203" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51100204" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51100205" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511005" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511006" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511007" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511009" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511010" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511011" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511012" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511013" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511014" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511015" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511016" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511018" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511019" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511020" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511021" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511022" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511023" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511024" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511025" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511026" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511027" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511031" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511032" OR
                         SUBSTRING(anexos.cuenta,1,6) = "523035" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511095" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511517" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511529" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511518" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511530" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51154501" OR
                         SUBSTRING(anexos.cuenta,1,8) = "52100501" OR
                         SUBSTRING(anexos.cuenta,1,8) = "52100504" OR
                         SUBSTRING(anexos.cuenta,1,6) = "521020" OR
                         SUBSTRING(anexos.cuenta,1,8) = "52100510" OR
                         SUBSTRING(anexos.cuenta,1,8) = "52100515" OR
                         SUBSTRING(anexos.cuenta,1,6) = "523020" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101102" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101108" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101110" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101114" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101116" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101118" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101122" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101136" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101140" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101195" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101251" OR
                         SUBSTRING(anexos.cuenta,1,8) = "61509502" OR
                         SUBSTRING(anexos.cuenta,1,8) = "61509511" OR
                         SUBSTRING(anexos.cuenta,1,10) = "6140101155")
                    AND LENGTH(anexos.nit) >= 5
                    AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                               BY anexos.cuenta:
    FIND FIRST SHD_Bienes&Servicios WHERE INDEX(anexos.nit,SHD_Bienes&Servicios.nit) = 1 NO-ERROR.
    IF NOT AVAILABLE SHD_Bienes&Servicios THEN DO:
        CREATE SHD_Bienes&Servicios.
        ASSIGN SHD_Bienes&Servicios.nit = anexos.nit
               SHD_Bienes&Servicios.concepto = 1.

        SHD_Bienes&Servicios.vigencia = ano_corte.

        FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN SHD_Bienes&Servicios.tipoDoc = "RC".
                WHEN "T.I" THEN SHD_Bienes&Servicios.tipoDoc = "TI".
                WHEN "C.C" THEN SHD_Bienes&Servicios.tipoDoc = "CC".
                WHEN "T.E" THEN SHD_Bienes&Servicios.tipoDoc = "CC".
                WHEN "C.E" THEN SHD_Bienes&Servicios.tipoDoc = "CE".
                WHEN "NIT" THEN SHD_Bienes&Servicios.tipoDoc = "NIT".
                WHEN "PPTE" THEN SHD_Bienes&Servicios.tipoDoc = "PA".
            END CASE.

            SHD_Bienes&Servicios.nombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.

            IF clientes.DIR_residencia <> "" THEN
                SHD_Bienes&Servicios.direccion = clientes.DIR_residencia.
            ELSE
                SHD_Bienes&Servicios.direccion = clientes.DIR_comercial.

            IF clientes.tel_residencia <> "" THEN
                SHD_Bienes&Servicios.telefono = clientes.tel_residencia.
            ELSE
                SHD_Bienes&Servicios.telefono = clientes.tel_comercial.

            SHD_Bienes&Servicios.email = clientes.email.

            IF clientes.lugar_residencia <> "" THEN DO:
                SHD_Bienes&Servicios.codMunicipio = SUBSTRING(clientes.lugar_residencia,1,5).
                SHD_Bienes&Servicios.codDepartamento = SUBSTRING(clientes.lugar_residencia,1,2).
            END.
            ELSE DO:
                SHD_Bienes&Servicios.codMunicipio = SUBSTRING(clientes.lugar_comercial,1,5).
                SHD_Bienes&Servicios.codDepartamento = SUBSTRING(clientes.lugar_comercial,1,2).
            END.
                
            IF SHD_Bienes&Servicios.codMunicipio = ? THEN
                SHD_Bienes&Servicios.codMunicipio = "11001".
                
            IF SHD_Bienes&Servicios.codDepartamento = ? THEN
                SHD_Bienes&Servicios.codDepartamento = "11".
        END.
    END.

    DO cont = 1 TO 12:
        SHD_Bienes&Servicios.compras = SHD_Bienes&Servicios.compras + anexos.db[cont] - anexos.cr[cont].
    END.
END.

FOR EACH rep_activosFijos WHERE rep_activosFijos.fecCorte = DATE(12,31,ano_corte)
                            AND rep_activosFijos.agencia = 1
                            AND year(rep_activosFijos.fechaCompra) = ano_corte
                            AND rep_activosFijos.nitProveedor <> ""
                            AND rep_activosFijos.contabilizado = YES NO-LOCK BREAK BY rep_activosFijos.nitProveedor:
    FIND FIRST SHD_Bienes&Servicios WHERE INDEX(rep_activosFijos.nitProveedor,SHD_Bienes&Servicios.nit) = 1 NO-ERROR.
    IF NOT AVAILABLE SHD_Bienes&Servicios THEN DO:
        CREATE SHD_Bienes&Servicios.
        ASSIGN SHD_Bienes&Servicios.nit = rep_activosFijos.nitProveedor
               SHD_Bienes&Servicios.concepto = 1.

        SHD_Bienes&Servicios.vigencia = ano_corte.

        FIND FIRST clientes WHERE clientes.nit = activosFijos.nitProveedor NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN SHD_Bienes&Servicios.tipoDoc = "RC".
                WHEN "T.I" THEN SHD_Bienes&Servicios.tipoDoc = "TI".
                WHEN "C.C" THEN SHD_Bienes&Servicios.tipoDoc = "CC".
                WHEN "T.E" THEN SHD_Bienes&Servicios.tipoDoc = "CC".
                WHEN "C.E" THEN SHD_Bienes&Servicios.tipoDoc = "CE".
                WHEN "NIT" THEN SHD_Bienes&Servicios.tipoDoc = "NIT".
                WHEN "PPTE" THEN SHD_Bienes&Servicios.tipoDoc = "PA".
            END CASE.

            SHD_Bienes&Servicios.nombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.

            IF clientes.DIR_residencia <> "" THEN
                SHD_Bienes&Servicios.direccion = clientes.DIR_residencia.
            ELSE
                SHD_Bienes&Servicios.direccion = clientes.DIR_comercial.

            IF clientes.tel_residencia <> "" THEN
                SHD_Bienes&Servicios.telefono = clientes.tel_residencia.
            ELSE
                SHD_Bienes&Servicios.telefono = clientes.tel_comercial.

            SHD_Bienes&Servicios.email = clientes.email.
            
            IF clientes.lugar_residencia <> "" THEN DO:
                SHD_Bienes&Servicios.codMunicipio = SUBSTRING(clientes.lugar_residencia,1,5).
                SHD_Bienes&Servicios.codDepartamento = SUBSTRING(clientes.lugar_residencia,1,2).
            END.
            ELSE DO:
                SHD_Bienes&Servicios.codMunicipio = SUBSTRING(clientes.lugar_comercial,1,5).
                SHD_Bienes&Servicios.codDepartamento = SUBSTRING(clientes.lugar_comercial,1,2).
            END.

            IF SHD_Bienes&Servicios.codMunicipio = ? THEN
                SHD_Bienes&Servicios.codMunicipio = "11001".
            
            IF SHD_Bienes&Servicios.codDepartamento = ? THEN
                SHD_Bienes&Servicios.codDepartamento = "11".
        END.
    END.
        
    SHD_Bienes&Servicios.compras = SHD_Bienes&Servicios.compras + rep_activosFijos.valorCompra.
END.


OUTPUT TO C:\INFO_Fodun\SHD_CompraBienes_yServicios.csv.
    
EXPORT DELIMITER ";" "VIGENCIA" "TIPO_DOC" "DOCUMENTO" "NOMBRE" "DIRECCIÓN" "TELÉFONO" "EMAIL" "MUNICIPIO" "DEPARTAMENTO" "CONCEPTO" "COMPRAS".

FOR EACH SHD_Bienes&Servicios WHERE SHD_Bienes&Servicios.compras >= DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) NO-LOCK:
    EXPORT DELIMITER ";" SHD_Bienes&Servicios.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SHD_CompraBienes&Servicios_Antes2017 W_FtosDian 
PROCEDURE SHD_CompraBienes&Servicios_Antes2017 :
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE SHD_Bienes&Servicios.

FOR EACH anexos13 WHERE anexos13.agencia = 1
                    AND (SUBSTRING(anexos13.cuenta,1,6) = "511001" OR
                         SUBSTRING(anexos13.cuenta,1,8) = "51105211" OR
                         SUBSTRING(anexos13.cuenta,1,8) = "51105212" OR
                         SUBSTRING(anexos13.cuenta,1,8) = "51105213" OR
                         SUBSTRING(anexos13.cuenta,1,8) = "51105214" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511058" OR
                         SUBSTRING(anexos13.cuenta,1,8) = "61759512" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511054" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511056" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401154" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101154" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511004" OR
                         /*SUBSTRING(anexos13.cuenta,1,4) = "1705" OR
                         SUBSTRING(anexos13.cuenta,1,4) = "1710" OR
                         SUBSTRING(anexos13.cuenta,1,4) = "1715" OR
                         SUBSTRING(anexos13.cuenta,1,4) = "1720" OR
                         SUBSTRING(anexos13.cuenta,1,4) = "1725" OR
                         SUBSTRING(anexos13.cuenta,1,4) = "1730" OR*/
                         SUBSTRING(anexos13.cuenta,1,2) = "26" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "510551" OR
                         SUBSTRING(anexos13.cuenta,1,8) = "51100203" OR
                         SUBSTRING(anexos13.cuenta,1,8) = "51100204" OR
                         SUBSTRING(anexos13.cuenta,1,8) = "51100205" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511008" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511010" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511012" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511016" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511018" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511020" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511022" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511024" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511026" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511028" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511030" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511034" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511036" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511038" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511040" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511042" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511044" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511046" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511048" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511050" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511052" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511060" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511062" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511064" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511095" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511515" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511524" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "511530" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "514005" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "514015" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "514095" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "539520" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101102" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101108" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101110" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101114" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101116" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101118" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101122" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101136" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101140" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101195" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140101251" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102108" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102110" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102114" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102116" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102118" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102122" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102136" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102154" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102195" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6140102251" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401108" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401110" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401116" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401118" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401122" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401136" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401195" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401251" OR
                         SUBSTRING(anexos13.cuenta,1,10) = "6170401260" OR
                         SUBSTRING(anexos13.cuenta,1,8) = "61759501")
                    AND LENGTH(anexos13.nit) >= 5
                    AND anexos13.ano = ano_corte NO-LOCK BREAK BY anexos13.nit
                                                               BY anexos13.cuenta:
    FIND FIRST SHD_Bienes&Servicios WHERE INDEX(anexos13.nit,SHD_Bienes&Servicios.nit) = 1 NO-ERROR.
    IF NOT AVAILABLE SHD_Bienes&Servicios THEN DO:
        CREATE SHD_Bienes&Servicios.
        ASSIGN SHD_Bienes&Servicios.nit = anexos13.nit
               SHD_Bienes&Servicios.concepto = 1.

        SHD_Bienes&Servicios.vigencia = ano_corte.

        FIND FIRST clientes WHERE clientes.nit = anexos13.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN SHD_Bienes&Servicios.tipoDoc = "RC".
                WHEN "T.I" THEN SHD_Bienes&Servicios.tipoDoc = "TI".
                WHEN "C.C" THEN SHD_Bienes&Servicios.tipoDoc = "CC".
                WHEN "T.E" THEN SHD_Bienes&Servicios.tipoDoc = "CC".
                WHEN "C.E" THEN SHD_Bienes&Servicios.tipoDoc = "CE".
                WHEN "NIT" THEN SHD_Bienes&Servicios.tipoDoc = "NIT".
                WHEN "PPTE" THEN SHD_Bienes&Servicios.tipoDoc = "PA".
            END CASE.

            SHD_Bienes&Servicios.nombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.

            IF clientes.DIR_residencia <> "" THEN
                SHD_Bienes&Servicios.direccion = clientes.DIR_residencia.
            ELSE
                SHD_Bienes&Servicios.direccion = clientes.DIR_comercial.

            IF clientes.tel_residencia <> "" THEN
                SHD_Bienes&Servicios.telefono = clientes.tel_residencia.
            ELSE
                SHD_Bienes&Servicios.telefono = clientes.tel_comercial.

            SHD_Bienes&Servicios.email = clientes.email.

            IF clientes.lugar_residencia <> "" THEN DO:
                SHD_Bienes&Servicios.codMunicipio = SUBSTRING(clientes.lugar_residencia,1,5).
                SHD_Bienes&Servicios.codDepartamento = SUBSTRING(clientes.lugar_residencia,1,2).
            END.
            ELSE DO:
                SHD_Bienes&Servicios.codMunicipio = SUBSTRING(clientes.lugar_comercial,1,5).
                SHD_Bienes&Servicios.codDepartamento = SUBSTRING(clientes.lugar_comercial,1,2).
            END.
                
            IF SHD_Bienes&Servicios.codMunicipio = ? THEN
                SHD_Bienes&Servicios.codMunicipio = "11001".
                
            IF SHD_Bienes&Servicios.codDepartamento = ? THEN
                SHD_Bienes&Servicios.codDepartamento = "11".
        END.
    END.

    DO cont = 1 TO 12:
        SHD_Bienes&Servicios.compras = SHD_Bienes&Servicios.compras + anexos13.db[cont] - anexos13.cr[cont].
    END.
END.

FOR EACH activosFijos WHERE activosFijos.agencia = 1
                        AND year(activosFijos.fechaCompra) = ano_corte
                        AND activosFijos.contabilizado = YES NO-LOCK:
    FIND FIRST SHD_Bienes&Servicios WHERE INDEX(activosFijos.nitProveedor,SHD_Bienes&Servicios.nit) = 1 NO-ERROR.
    IF NOT AVAILABLE SHD_Bienes&Servicios THEN DO:
        CREATE SHD_Bienes&Servicios.
        ASSIGN SHD_Bienes&Servicios.nit = activosFijos.nitProveedor
               SHD_Bienes&Servicios.concepto = 1.

        SHD_Bienes&Servicios.vigencia = ano_corte.

        FIND FIRST clientes WHERE clientes.nit = activosFijos.nitProveedor NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN SHD_Bienes&Servicios.tipoDoc = "RC".
                WHEN "T.I" THEN SHD_Bienes&Servicios.tipoDoc = "TI".
                WHEN "C.C" THEN SHD_Bienes&Servicios.tipoDoc = "CC".
                WHEN "T.E" THEN SHD_Bienes&Servicios.tipoDoc = "CC".
                WHEN "C.E" THEN SHD_Bienes&Servicios.tipoDoc = "CE".
                WHEN "NIT" THEN SHD_Bienes&Servicios.tipoDoc = "NIT".
                WHEN "PPTE" THEN SHD_Bienes&Servicios.tipoDoc = "PA".
            END CASE.

            SHD_Bienes&Servicios.nombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.

            IF clientes.DIR_residencia <> "" THEN
                SHD_Bienes&Servicios.direccion = clientes.DIR_residencia.
            ELSE
                SHD_Bienes&Servicios.direccion = clientes.DIR_comercial.

            IF clientes.tel_residencia <> "" THEN
                SHD_Bienes&Servicios.telefono = clientes.tel_residencia.
            ELSE
                SHD_Bienes&Servicios.telefono = clientes.tel_comercial.

            SHD_Bienes&Servicios.email = clientes.email.

            IF clientes.lugar_residencia <> "" THEN DO:
                SHD_Bienes&Servicios.codMunicipio = SUBSTRING(clientes.lugar_residencia,1,5).
                SHD_Bienes&Servicios.codDepartamento = SUBSTRING(clientes.lugar_residencia,1,2).
            END.
            ELSE DO:
                SHD_Bienes&Servicios.codMunicipio = SUBSTRING(clientes.lugar_comercial,1,5).
                SHD_Bienes&Servicios.codDepartamento = SUBSTRING(clientes.lugar_comercial,1,2).
            END.
                
            IF SHD_Bienes&Servicios.codMunicipio = ? THEN
                SHD_Bienes&Servicios.codMunicipio = "11001".
                
            IF SHD_Bienes&Servicios.codDepartamento = ? THEN
                SHD_Bienes&Servicios.codDepartamento = "11".
        END.
    END.

    SHD_Bienes&Servicios.compras = SHD_Bienes&Servicios.compras + activosFijos.valorCompra.
END.


OUTPUT TO VALUE("C:\INFO_Fodun\SHD_CompraBienes_yServicios_" + STRING(ano_corte,"9999") + ".csv").
    
EXPORT DELIMITER ";" "VIGENCIA" "TIPO_DOC" "DOCUMENTO" "NOMBRE" "DIRECCIÓN" "TELÉFONO" "EMAIL" "MUNICIPIO" "DEPARTAMENTO" "CONCEPTO" "COMPRAS".

FOR EACH SHD_Bienes&Servicios WHERE SHD_Bienes&Servicios.compras >= DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) NO-LOCK:
    EXPORT DELIMITER ";" SHD_Bienes&Servicios.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SHD_ReteICA W_FtosDian 
PROCEDURE SHD_ReteICA :
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE SHD_ReteICA.

FOR EACH anexos WHERE anexos.agencia = 1
                  AND SUBSTRING(anexos.cuenta,1,6) = "243575"
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    FIND FIRST SHD_ReteICA WHERE INDEX(anexos.nit,SHD_ReteICA.nit) = 1
                             AND SHD_ReteICA.cuenta = anexos.cuenta NO-ERROR.
    IF NOT AVAILABLE SHD_ReteICA THEN DO:
        CREATE SHD_ReteICA.
        ASSIGN SHD_ReteICA.nit = anexos.nit
               SHD_ReteICA.cuenta = anexos.cuenta.

        CASE anexos.cuenta:
            WHEN "24357501" THEN SHD_ReteICA.tarifa = 13.8.
            WHEN "24357503" THEN SHD_ReteICA.tarifa = 0.966.
            WHEN "24357504" THEN SHD_ReteICA.tarifa = 11.04.
            WHEN "24357505" THEN SHD_ReteICA.tarifa = 6.9.
            WHEN "24357506" THEN SHD_ReteICA.tarifa = 4.14.
        END CASE.

        SHD_ReteICA.vigencia = ano_corte.

        FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN SHD_ReteICA.tipoDoc = "RC".
                WHEN "T.I" THEN SHD_ReteICA.tipoDoc = "TI".
                WHEN "C.C" THEN SHD_ReteICA.tipoDoc = "CC".
                WHEN "T.E" THEN SHD_ReteICA.tipoDoc = "CC".
                WHEN "C.E" THEN SHD_ReteICA.tipoDoc = "CE".
                WHEN "NIT" THEN SHD_ReteICA.tipoDoc = "NIT".
                WHEN "PPTE" THEN SHD_ReteICA.tipoDoc = "PA".
            END CASE.

            SHD_ReteICA.nombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.

            IF clientes.DIR_residencia <> "" THEN
                SHD_ReteICA.direccion = clientes.DIR_residencia.
            ELSE
                SHD_ReteICA.direccion = clientes.DIR_comercial.

            IF clientes.tel_residencia <> "" THEN
                SHD_ReteICA.telefono = clientes.tel_residencia.
            ELSE
                SHD_ReteICA.telefono = clientes.tel_comercial.

            SHD_ReteICA.email = clientes.email.

            IF clientes.lugar_residencia <> "" THEN DO:
                SHD_ReteICA.codMunicipio = SUBSTRING(clientes.lugar_residencia,1,5).
                SHD_ReteICA.codDepartamento = SUBSTRING(clientes.lugar_residencia,1,2).
            END.
            ELSE DO:
                SHD_ReteICA.codMunicipio = SUBSTRING(clientes.lugar_comercial,1,5).
                SHD_ReteICA.codDepartamento = SUBSTRING(clientes.lugar_comercial,1,2).
            END.
                
            IF SHD_ReteICA.codMunicipio = ? THEN
                SHD_ReteICA.codMunicipio = "11001".
                
            IF SHD_ReteICA.codDepartamento = ? THEN
                SHD_ReteICA.codDepartamento = "11".
        END.
    END.

    DO cont = 1 TO 12:
        SHD_ReteICA.retencion = SHD_ReteICA.retencion + anexos.cr[cont] - anexos.db[cont].
    END.
END.

FOR EACH SHD_ReteICA:
    FOR EACH anexos WHERE anexos.nit = SHD_ReteICA.nit
                        AND (SUBSTRING(anexos.cuenta,1,4) = "5110" OR
                             SUBSTRING(anexos.cuenta,1,8) = "52303501" OR
                             SUBSTRING(anexos.cuenta,1,8) = "61401011")
                        AND anexos.ano = ano_corte NO-LOCK:
        DO cont = 1 TO 12:
            SHD_ReteICA.base = SHD_ReteICA.base + anexos.db[cont] - anexos.cr[cont].
        END.
    END.
END.

OUTPUT TO C:\INFO_Fodun\SHD_ReteICA.csv.
    
EXPORT DELIMITER ";" "VIGENCIA" "TIPO_DOC" "DOCUMENTO" "NOMBRE" "DIRECCIÓN" "TELÉFONO" "EMAIL" "MUNICIPIO" "DEPARTAMENTO" "BASE" "TARIFA" "RETENCIÓN".

FOR EACH SHD_ReteICA NO-LOCK:

    /*SHD_ReteICA.base = ROUND((SHD_ReteICA.Retencion * 100) / SHD_ReteICA.tarifa,0).*/

    EXPORT DELIMITER ";" SHD_ReteICA EXCEPT SHD_ReteICA.cuenta.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SHD_ReteICA_Antes2017 W_FtosDian 
PROCEDURE SHD_ReteICA_Antes2017 :
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE SHD_ReteICA.

FOR EACH anexos13 WHERE anexos13.agencia = 1
                    AND SUBSTRING(anexos13.cuenta,1,6) = "244805"
                    AND anexos13.ano = ano_corte NO-LOCK BREAK BY anexos13.nit
                                                               BY anexos13.cuenta:
    FIND FIRST SHD_ReteICA WHERE INDEX(anexos13.nit,SHD_ReteICA.nit) = 1
                             AND SHD_ReteICA.cuenta = anexos13.cuenta NO-ERROR.
    IF NOT AVAILABLE SHD_ReteICA THEN DO:
        CREATE SHD_ReteICA.
        ASSIGN SHD_ReteICA.nit = anexos13.nit
               SHD_ReteICA.cuenta = anexos13.cuenta.

        CASE anexos13.cuenta:
            WHEN "24480501" THEN SHD_ReteICA.tarifa = 13.8.
            WHEN "24480503" THEN SHD_ReteICA.tarifa = 0.966.
            WHEN "24480504" THEN SHD_ReteICA.tarifa = 11.04.
            WHEN "24480505" THEN SHD_ReteICA.tarifa = 6.9.
            WHEN "24480506" THEN SHD_ReteICA.tarifa = 4.14.
        END CASE.

        SHD_ReteICA.vigencia = ano_corte.

        FIND FIRST clientes WHERE clientes.nit = anexos13.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN SHD_ReteICA.tipoDoc = "RC".
                WHEN "T.I" THEN SHD_ReteICA.tipoDoc = "TI".
                WHEN "C.C" THEN SHD_ReteICA.tipoDoc = "CC".
                WHEN "T.E" THEN SHD_ReteICA.tipoDoc = "CC".
                WHEN "C.E" THEN SHD_ReteICA.tipoDoc = "CE".
                WHEN "NIT" THEN SHD_ReteICA.tipoDoc = "NIT".
                WHEN "PPTE" THEN SHD_ReteICA.tipoDoc = "PA".
            END CASE.

            SHD_ReteICA.nombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.

            IF clientes.DIR_residencia <> "" THEN
                SHD_ReteICA.direccion = clientes.DIR_residencia.
            ELSE
                SHD_ReteICA.direccion = clientes.DIR_comercial.

            IF clientes.tel_residencia <> "" THEN
                SHD_ReteICA.telefono = clientes.tel_residencia.
            ELSE
                SHD_ReteICA.telefono = clientes.tel_comercial.

            SHD_ReteICA.email = clientes.email.

            IF clientes.lugar_residencia <> "" THEN DO:
                SHD_ReteICA.codMunicipio = SUBSTRING(clientes.lugar_residencia,1,5).
                SHD_ReteICA.codDepartamento = SUBSTRING(clientes.lugar_residencia,1,2).
            END.
            ELSE DO:
                SHD_ReteICA.codMunicipio = SUBSTRING(clientes.lugar_comercial,1,5).
                SHD_ReteICA.codDepartamento = SUBSTRING(clientes.lugar_comercial,1,2).
            END.
                
            IF SHD_ReteICA.codMunicipio = ? THEN
                SHD_ReteICA.codMunicipio = "11001".
                
            IF SHD_ReteICA.codDepartamento = ? THEN
                SHD_ReteICA.codDepartamento = "11".
        END.
    END.

    DO cont = 1 TO 12:
        SHD_ReteICA.retencion = SHD_ReteICA.retencion + anexos13.cr[cont] - anexos13.db[cont].
    END.
END.

FOR EACH SHD_ReteICA:
    FOR EACH anexos13 WHERE anexos13.nit = SHD_ReteICA.nit
                        AND (SUBSTRING(anexos13.cuenta,1,4) = "5110" OR
                             SUBSTRING(anexos13.cuenta,1,8) = "61401011" OR
                             SUBSTRING(anexos13.cuenta,1,8) = "61704011")
                        AND anexos13.ano = ano_corte NO-LOCK:
        DO cont = 1 TO 12:
            SHD_ReteICA.base = SHD_ReteICA.base + anexos13.db[cont] - anexos13.cr[cont].
        END.
    END.
END.

OUTPUT TO VALUE("C:\INFO_Fodun\SHD_ReteICA_" + STRING(ano_corte,"9999") + ".csv").
    
EXPORT DELIMITER ";" "VIGENCIA" "TIPO_DOC" "DOCUMENTO" "NOMBRE" "DIRECCIÓN" "TELÉFONO" "EMAIL" "MUNICIPIO" "DEPARTAMENTO" "BASE" "TARIFA" "RETENCIÓN".

FOR EACH SHD_ReteICA NO-LOCK:

    /*SHD_ReteICA.base = ROUND((SHD_ReteICA.Retencion * 100) / SHD_ReteICA.tarifa,0).*/

    EXPORT DELIMITER ";" SHD_ReteICA EXCEPT SHD_ReteICA.cuenta.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SHD_VentaBienes&Servicios W_FtosDian 
PROCEDURE SHD_VentaBienes&Servicios :
/* Compra de Bienes y Servicios - Art. 2 */

DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE SHD_Bienes&Servicios.

FOR EACH anexos WHERE anexos.agencia = 1
                  AND (SUBSTRING(anexos.cuenta,1,6) = "414010" OR
                       SUBSTRING(anexos.cuenta,1,4) = "4170" OR
                       SUBSTRING(anexos.cuenta,1,8) = "42201005" OR
                       SUBSTRING(anexos.cuenta,1,8) = "42201001" OR
                       SUBSTRING(anexos.cuenta,1,8) = "42201005" OR
                       SUBSTRING(anexos.cuenta,1,8) = "42201010" OR
                       SUBSTRING(anexos.cuenta,1,6) = "420560")
                  AND LENGTH(anexos.nit) >= 5
                  AND anexos.ano = ano_corte NO-LOCK BREAK BY anexos.nit
                                                           BY anexos.cuenta:
    FIND FIRST SHD_Bienes&Servicios WHERE INDEX(anexos.nit,SHD_Bienes&Servicios.nit) = 1 NO-ERROR.
    IF NOT AVAILABLE SHD_Bienes&Servicios THEN DO:
        CREATE SHD_Bienes&Servicios.
        ASSIGN SHD_Bienes&Servicios.nit = anexos.nit
               SHD_Bienes&Servicios.concepto = 1.

        SHD_Bienes&Servicios.vigencia = ano_corte.

        FIND FIRST clientes WHERE clientes.nit = anexos.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN SHD_Bienes&Servicios.tipoDoc = "RC".
                WHEN "T.I" THEN SHD_Bienes&Servicios.tipoDoc = "TI".
                WHEN "C.C" THEN SHD_Bienes&Servicios.tipoDoc = "CC".
                WHEN "T.E" THEN SHD_Bienes&Servicios.tipoDoc = "CC".
                WHEN "C.E" THEN SHD_Bienes&Servicios.tipoDoc = "CE".
                WHEN "NIT" THEN SHD_Bienes&Servicios.tipoDoc = "NIT".
                WHEN "PPTE" THEN SHD_Bienes&Servicios.tipoDoc = "PA".
            END CASE.

            SHD_Bienes&Servicios.nombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.

            IF clientes.DIR_residencia <> "" THEN
                SHD_Bienes&Servicios.direccion = clientes.DIR_residencia.
            ELSE
                SHD_Bienes&Servicios.direccion = clientes.DIR_comercial.

            IF clientes.tel_residencia <> "" THEN
                SHD_Bienes&Servicios.telefono = clientes.tel_residencia.
            ELSE
                SHD_Bienes&Servicios.telefono = clientes.tel_comercial.

            SHD_Bienes&Servicios.email = clientes.email.

            IF clientes.lugar_residencia <> "" THEN DO:
                SHD_Bienes&Servicios.codMunicipio = SUBSTRING(clientes.lugar_residencia,1,5).
                SHD_Bienes&Servicios.codDepartamento = SUBSTRING(clientes.lugar_residencia,1,2).
            END.
            ELSE DO:
                SHD_Bienes&Servicios.codMunicipio = SUBSTRING(clientes.lugar_comercial,1,5).
                SHD_Bienes&Servicios.codDepartamento = SUBSTRING(clientes.lugar_comercial,1,2).
            END.
                
            IF SHD_Bienes&Servicios.codMunicipio = ? THEN
                SHD_Bienes&Servicios.codMunicipio = "11001".
                
            IF SHD_Bienes&Servicios.codDepartamento = ? THEN
                SHD_Bienes&Servicios.codDepartamento = "11".
        END.
    END.

    DO cont = 1 TO 12:
        SHD_Bienes&Servicios.compras = SHD_Bienes&Servicios.compras + anexos.cr[cont] - anexos.db[cont].
    END.
END.

OUTPUT TO C:\INFO_Fodun\SHD_VentaBienes_yServicios.csv.
    
EXPORT DELIMITER ";" "VIGENCIA" "TIPO_DOC" "DOCUMENTO" "NOMBRE" "DIRECCIÓN" "TELÉFONO" "EMAIL" "MUNICIPIO" "DEPARTAMENTO" "CONCEPTO" "COMPRAS".

FOR EACH SHD_Bienes&Servicios WHERE SHD_Bienes&Servicios.compras >= DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) NO-LOCK:
    EXPORT DELIMITER ";" SHD_Bienes&Servicios.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SHD_VentaBienes&Servicios_Antes2017 W_FtosDian 
PROCEDURE SHD_VentaBienes&Servicios_Antes2017 :
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE SHD_Bienes&Servicios.

FOR EACH anexos13 WHERE anexos13.agencia = 1
                    AND (SUBSTRING(anexos13.cuenta,1,4) = "4140" OR
                         SUBSTRING(anexos13.cuenta,1,4) = "4170" OR
                         SUBSTRING(anexos13.cuenta,1,8) = "41854502" OR
                         SUBSTRING(anexos13.cuenta,1,6) = "418598" OR
                         SUBSTRING(anexos13.cuenta,1,4) = "4245")
                    AND LENGTH(anexos13.nit) >= 5
                    AND anexos13.ano = ano_corte NO-LOCK BREAK BY anexos13.nit
                                                               BY anexos13.cuenta:
    FIND FIRST SHD_Bienes&Servicios WHERE INDEX(anexos13.nit,SHD_Bienes&Servicios.nit) = 1 NO-ERROR.
    IF NOT AVAILABLE SHD_Bienes&Servicios THEN DO:
        CREATE SHD_Bienes&Servicios.
        ASSIGN SHD_Bienes&Servicios.nit = anexos13.nit
               SHD_Bienes&Servicios.concepto = 1.

        SHD_Bienes&Servicios.vigencia = ano_corte.

        FIND FIRST clientes WHERE clientes.nit = anexos13.nit NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            CASE clientes.tipo_identificacion:
                WHEN "R.C" THEN SHD_Bienes&Servicios.tipoDoc = "RC".
                WHEN "T.I" THEN SHD_Bienes&Servicios.tipoDoc = "TI".
                WHEN "C.C" THEN SHD_Bienes&Servicios.tipoDoc = "CC".
                WHEN "T.E" THEN SHD_Bienes&Servicios.tipoDoc = "CC".
                WHEN "C.E" THEN SHD_Bienes&Servicios.tipoDoc = "CE".
                WHEN "NIT" THEN SHD_Bienes&Servicios.tipoDoc = "NIT".
                WHEN "PPTE" THEN SHD_Bienes&Servicios.tipoDoc = "PA".
            END CASE.

            SHD_Bienes&Servicios.nombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.

            IF clientes.DIR_residencia <> "" THEN
                SHD_Bienes&Servicios.direccion = clientes.DIR_residencia.
            ELSE
                SHD_Bienes&Servicios.direccion = clientes.DIR_comercial.

            IF clientes.tel_residencia <> "" THEN
                SHD_Bienes&Servicios.telefono = clientes.tel_residencia.
            ELSE
                SHD_Bienes&Servicios.telefono = clientes.tel_comercial.

            SHD_Bienes&Servicios.email = clientes.email.

            IF clientes.lugar_residencia <> "" THEN DO:
                SHD_Bienes&Servicios.codMunicipio = SUBSTRING(clientes.lugar_residencia,1,5).
                SHD_Bienes&Servicios.codDepartamento = SUBSTRING(clientes.lugar_residencia,1,2).
            END.
            ELSE DO:
                SHD_Bienes&Servicios.codMunicipio = SUBSTRING(clientes.lugar_comercial,1,5).
                SHD_Bienes&Servicios.codDepartamento = SUBSTRING(clientes.lugar_comercial,1,2).
            END.
                
            IF SHD_Bienes&Servicios.codMunicipio = ? THEN
                SHD_Bienes&Servicios.codMunicipio = "11001".
                
            IF SHD_Bienes&Servicios.codDepartamento = ? THEN
                SHD_Bienes&Servicios.codDepartamento = "11".
        END.
    END.

    DO cont = 1 TO 12:
        SHD_Bienes&Servicios.compras = SHD_Bienes&Servicios.compras + anexos13.cr[cont] - anexos13.db[cont].
    END.
END.

OUTPUT TO VALUE("C:\INFO_Fodun\SHD_VentaBienes_yServicios_" + STRING(ano_corte,"9999") + ".csv").
    
EXPORT DELIMITER ";" "VIGENCIA" "TIPO_DOC" "DOCUMENTO" "NOMBRE" "DIRECCIÓN" "TELÉFONO" "EMAIL" "MUNICIPIO" "DEPARTAMENTO" "CONCEPTO" "COMPRAS".

FOR EACH SHD_Bienes&Servicios WHERE SHD_Bienes&Servicios.compras >= DECIMAL(tope:SCREEN-VALUE IN FRAME F_Ftos) NO-LOCK:
    EXPORT DELIMITER ";" SHD_Bienes&Servicios.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

