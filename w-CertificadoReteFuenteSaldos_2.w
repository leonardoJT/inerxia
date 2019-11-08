&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

DEFINE VAR W_Ok AS LOGICAL.
DEFI VAR WSdo AS LOG INIT FALSE.

DEFINE TEMP-TABLE TCer
    FIELD Nit LIKE Clientes.Nit
    FIELD Cta LIKE Cuentas.Cuenta

    /* oakley */

  FIELD Nom LIKE Cuentas.Nombre
  FIELD Bas AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
  FIELD CBa LIKE Cuentas.Cuenta
  FIELD NBa LIKE Cuentas.Nombre
  FIELD Ret AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
  FIELD Por AS DEC FORM "->9.99".

  DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
  DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
  DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
  DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.

  DEFINE VARIABLE NomCtaBas  LIKE Cuentas.Nombre.
 DEFINE VAR j AS DECIMAL.
 DEFINE VAR k AS INTEGER.

 DEFINE VAR Puntero AS ROWID.


 DEFINE VARIABLE viCRrf LIKE Creditos.Sdo_Capital INITIAL 0 NO-UNDO. /*Rendimientos Financieros*/
 DEFINE VARIABLE viCRra LIKE Creditos.Sdo_Capital INITIAL 0 NO-UNDO. /*Revalorizacion de Aportes*/
 DEFINE VARIABLE viCRot LIKE Creditos.Sdo_Capital INITIAL 0 NO-UNDO. /*Saldo Cupo Rotativo*/
 DEFINE VARIABLE viBand AS INTEGER INITIAL 0 NO-UNDO.

DEFINE TEMP-TABLE docs
    FIELD nit AS CHARACTER
    FIELD agencia AS INTEGER
    FIELD comprobante AS INTEGER
    FIELD num_documento AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD fec_contable AS DATE.

DEFINE TEMP-TABLE saldos
    FIELD cedula AS CHARACTER FORMAT "X(11)"
    FIELD nombre AS CHARACTER
    FIELD totalAportes AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD totalRevalorizacionAportes AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD totalAhorros AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD totalCDAT as DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD totalCreditos AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD totalIntereses AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    INDEX idx1 cedula.

DEFINE TEMP-TABLE ttmov
    FIELD id AS ROWID.

DEFINE TEMP-TABLE F1001
    FIELD concepto AS INTEGER
    FIELD nit AS CHARACTER
    FIELD base AS DECIMAL
    FIELD retefuente AS DECIMAL.

/* PDF */
/*{Incluido\pdf_inc.i "SHARED"}*/

DEFINE VAR l-file AS CHARACTER.

DEFINE TEMP-TABLE emails
    FIELD nit AS CHARACTER
    FIELD email AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FCer

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 WAno WNit btnMasivoAsociados BtnDone 
&Scoped-Define DISPLAYED-OBJECTS WAno WNit WNomNit 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 20 BY 1.35
     BGCOLOR 8 .

DEFINE BUTTON btnMasivoAsociados 
     LABEL "Certificados masivos" 
     SIZE 20 BY 1.35.

DEFINE BUTTON btnPdf 
     LABEL "Certificado Individual" 
     SIZE 20 BY 1.35.

DEFINE VARIABLE WAno AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "Periodo Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNit AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNomNit AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 64 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 2.69.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FCer
     WAno AT ROW 1.81 COL 79 COLON-ALIGNED
     WNit AT ROW 2.88 COL 8 COLON-ALIGNED
     WNomNit AT ROW 2.88 COL 26 COLON-ALIGNED NO-LABEL
     btnMasivoAsociados AT ROW 4.42 COL 54.14 WIDGET-ID 18
     btnPdf AT ROW 4.42 COL 74.72 WIDGET-ID 16
     BtnDone AT ROW 5.92 COL 74.72
     RECT-1 AT ROW 1.54 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.86 BY 6.54
         FONT 5
         DEFAULT-BUTTON BtnDone.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Certificados de Retención y Otros, Prog.W-Rp_CertificadosAnuales.W"
         HEIGHT             = 6.54
         WIDTH              = 94.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FCer
   FRAME-NAME                                                           */
ASSIGN 
       btnMasivoAsociados:HIDDEN IN FRAME FCer           = TRUE.

/* SETTINGS FOR BUTTON btnPdf IN FRAME FCer
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WNomNit IN FRAME FCer
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Certificados de Retención y Otros, Prog.W-Rp_CertificadosAnuales.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Certificados de Retención y Otros, Prog.W-Rp_CertificadosAnuales.W */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME FCer /* Salir */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMasivoAsociados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMasivoAsociados wWin
ON CHOOSE OF btnMasivoAsociados IN FRAME FCer /* Certificados masivos */
DO:
    DEFINE VAR choice AS LOGICAL.

    MESSAGE "Está seguro que desea generar todos los certificados?" SKIP
            "Este proceso puede tardar varias horas..."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.

    IF choice = TRUE THEN DO:
        EMPTY TEMP-TABLE TCer.
        EMPTY TEMP-TABLE saldos.
        EMPTY TEMP-TABLE emails.

        FOR EACH clientes NO-LOCK BY clientes.nit:
            FIND FIRST rep_creditos WHERE /*rep_creditos.fecCorte = DATE(12,31,WAno)*/
                                          YEAR(rep_creditos.fecCorte) = wAno
                                      AND rep_creditos.nit = clientes.nit NO-LOCK NO-ERROR.
            IF NOT AVAILABLE rep_creditos THEN DO:
                FIND FIRST rep_ahorros WHERE rep_ahorros.fecCorte = DATE(12,31,WAno)
                                         AND rep_ahorros.nit = clientes.nit
                                         AND rep_ahorros.tip_ahorro = 4 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE rep_ahorros THEN
                    NEXT.
            END.

            CREATE emails.
            emails.nit = clientes.nit.
            emails.email = clientes.email.

            RUN certificadoReteFuente.
            RUN certificadoDeSaldos.
            RUN A_Archivo.

            RUN Rp_CertificadoReteFuente.r(INPUT clientes.nit,
                                           INPUT WAno,
                                           INPUT 2) NO-ERROR.
        END.

        OUTPUT TO "Reportes\CertificadosRetencion\Masivos\correos.csv".
        FOR EACH emails NO-LOCK:
            EXPORT DELIMITER ";" emails.
        END.
        OUTPUT CLOSE.

        MESSAGE "La generación de certificados finalizó con éxito"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPdf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPdf wWin
ON CHOOSE OF btnPdf IN FRAME FCer /* Certificado Individual */
DO:
    RUN CertificadoReteFuente.
    RUN certificadoDeSaldos.
    RUN a_Archivo.

    RUN Rp_CertificadoReteFuente.r(INPUT clientes.nit,
                                   INPUT WAno,
                                   INPUT 1) NO-ERROR.

    l-file = "Reportes\CertificadosRetencion\" + clientes.nit + ".pdf".

    ASSIGN FRAME FCer:SENSITIVE = FALSE.

    RUN visorPDF(INPUT l-file) NO-ERROR.

    ASSIGN FRAME FCer:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME WAno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WAno wWin
ON LEAVE OF WAno IN FRAME FCer /* Periodo Fiscal */
DO:
    ASSIGN FRAME FCer WAno.

    IF WNit:SCREEN-VALUE <> "" THEN
        RUN certificadoReteFuente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME WNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WNit wWin
ON LEAVE OF WNit IN FRAME FCer /* Nit */
DO:
    DEFINE VAR choice AS LOGICAL.

    ASSIGN WNit.

    WNomNit:SCREEN-VALUE = "".

    IF WNit:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST Clientes WHERE Clientes.Nit = SELF:SCREEN-VALUE NO-ERROR.
        IF AVAILABLE Clientes THEN DO:
            WNomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
            btnPDF:SENSITIVE = TRUE.
        END.
    END.
    ELSE
        btnPDF:SENSITIVE = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Arrendamientos wWin 
PROCEDURE Arrendamientos :
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE anexos.nit = clientes.nit
                    AND SUBSTRING(anexos.cuenta,1,6) = "511003"
                    AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                          BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE TCer.nit = clientes.nit
                      AND TCer.CBa = "ARRENDAMIENTOS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = clientes.nit
               TCer.CBa = "ARRENDAMIENTOS".
    END.

    TCer.Bas = TCer.Bas + anexos.sdo_inicial.

    DO cont = 1 TO 12:
         TCer.Bas = TCer.Bas + anexos.db[cont] - anexos.cr[cont].
    END.

    IF LAST-OF(anexos.cuenta) THEN DO:
        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = wAno NO-LOCK BREAK:
            FIND FIRST docs WHERE docs.nit = mov_contable.nit
                              AND docs.agencia = mov_contable.agencia
                              AND docs.comprobante = mov_contable.comprobante
                              AND docs.num_documento = mov_contable.num_documento
                              AND docs.fec_contable = mov_contable.fec_contable NO-LOCK NO-ERROR.
            IF NOT AVAILABLE docs THEN DO:
                CREATE docs.
                docs.nit = mov_contable.nit.
                docs.agencia = mov_contable.agencia.
                docs.comprobante = mov_contable.comprobante.
                docs.num_documento = mov_contable.num_documento.
                docs.fec_contable = mov_contable.fec_contable.
            END.
        END.

        FOR EACH docs NO-LOCK:
            FOR EACH mov_contable WHERE mov_contable.agencia = docs.agencia
                                    AND mov_contable.comprobante = docs.comprobante
                                    AND mov_contable.num_documento = docs.num_documento
                                    AND mov_contable.nit = docs.nit
                                    AND YEAR(mov_contable.fec_contable) = wAno
                                    AND SUBSTRING(mov_contable.cuenta,1,6) = "243530" NO-LOCK:
                FIND FIRST ttmov WHERE ttmov.Id = ROWID(mov_contable) NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttmov THEN DO:
                    TCer.Ret = TCer.Ret + mov_contable.cr - mov_contable.db.

                    CREATE ttmov.
                    ttmov.id = ROWID(mov_contable).
                END.
            END.
        END.
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE A_archivo wWin 
PROCEDURE A_archivo :
{INCLUIDO\RepEncabezado.I}

DEFINE VAR TotalRet AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR WKEnt AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKDoc AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKCer AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKHem AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKPie AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKCta AS CHARACTER FORMAT "X(50)".
DEFINE VAR totalBase AS DECIMAL.
DEFINE VAR pLineas AS CHARACTER EXTENT 50.
DEFINE VAR WAAno AS INTEG FORM "9999".

ASSIGN FRAME FCer Wano.

FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.

OUTPUT TO VALUE("Reportes\CertificadosRetencion\" + clientes.nit + ".txt").
DISPLAY SKIP(15)
        "                                  CERTIFICADO DE RETENCION EN LA FUENTE" SKIP
        "                                            AÑO GRAVABLE" STRING(WAno,"9999") SKIP(3)                
        "                En  cumplimiento  de  las  disposiciones  fiscales  vigentes  (artículo 381 del" SKIP
        "                estatuto tributario), el Fondo de Empleados Docentes de la Universidad Nacional" SKIP
        "                de  Colombia  -  FODUN,  con  NIT 800112808-7,  practicó Retención en la Fuente" SKIP
        "                durante el año " TRIM(STRING(WAno,"9999")) FORMAT "X(4)" "a:" CAPS(clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2) FORMAT "X(40)" ", identificado" SKIP
        "                con documento #" TRIM(clientes.Nit) FORMAT "X(12)" ", valores que fueron consignados en BOGOTÁ:" SKIP(2)
        "       -------------------------------------------------------------------------------" AT 10
        /*         1         2         3         4         5         6         7         8*/
        /*12346578901234657890123465789012346578901234657890123465789012346578901234657890*/
        "       CONCEPTO                                             BASE        VALOR RETENIDO" AT 10
        "       -------------------------------------------------------------------------------" AT 10
    WITH WIDTH 132 FRAME FEnca /*PAGE-TOP*/ USE-TEXT NO-BOX STREAM-IO NO-LABELS.

totalRet = 0.
totalBase = 0.

FOR EACH TCer WHERE TCer.Nit = clientes.Nit
                AND TCer.Cba = "GRAVAMEN A LOS MOVIMIENTOS FINANCIEROS" NO-LOCK BREAK BY TCer.nit:
    WKCta = "       GRAVAMEN A LOS MOVIMIENTOS FINANCIEROS".
    TotalRet = TotalRet + TCer.Ret.
    totalBase = totalBase + TCer.Bas.

    IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
        DISPLAY WKCta                                    AT 10  FORM "X(50)"
                totalBase FORM "->>,>>>,>>>,>>9"
                " " totalRet
            WITH WIDTH 132 FRAME F-Base_Ret1 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END.

totalRet = 0.
totalBase = 0.

FOR EACH TCer WHERE TCer.Nit = clientes.Nit
                AND TCer.Cba = "ICA"
                AND TCer.Ret > 0 NO-LOCK BREAK BY TCer.nit:
    WKCta = "       ICA".
    TotalRet = TotalRet + TCer.Ret.
    totalBase = totalBase + TCer.Bas.

    IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
        DISPLAY WKCta                                    AT 10  FORM "X(50)"
                totalBase FORM "->>,>>>,>>>,>>9"
                " " totalRet
           WITH WIDTH 132 FRAME F-Base_Ret2 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END.

totalRet = 0.
totalBase = 0.

DISPLAY skip(2)
    WITH WIDTH 132 FRAME F-inicia_varios USE-TEXT STREAM-IO NO-LABELS NO-BOX.

/* ----------------------- */

FOR EACH TCer WHERE TCer.Nit = clientes.Nit
                AND TCer.Cba = "SALARIOS" NO-LOCK BREAK BY TCer.nit:
    WKCta = "       SALARIOS".
    totalBase = totalBase + TCer.Bas.
    TotalRet = TotalRet + TCer.Ret.
    
    IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
       DISPLAY WKCta                                    AT 10  FORM "X(50)"
               totalBase FORM "->>,>>>,>>>,>>9"
        WITH WIDTH 132 FRAME F-Salarios USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END.

/*totalRet = 0.*/
totalBase = 0.


FOR EACH TCer WHERE TCer.Nit = clientes.Nit
                AND TCer.Cba = "GASTOS DE REPRESENTACIÓN" NO-LOCK BREAK BY TCer.nit:
    WKCta = "       GASTOS DE REPRESENTACIÓN".
    totalBase = totalBase + TCer.Bas.
    TotalRet = TotalRet + TCer.Ret.
    
    IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
       DISPLAY WKCta                                    AT 10  FORM "X(50)"
               totalBase FORM "->>,>>>,>>>,>>9"
        WITH WIDTH 132 FRAME F-GastosDeRepresentacion USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END.

/*totalRet = 0.*/
totalBase = 0.

FOR EACH TCer WHERE TCer.Nit = clientes.Nit
                AND TCer.Cba = "HONORARIOS" NO-LOCK BREAK BY TCer.nit:
    WKCta = "       HONORARIOS".
    TotalRet = TotalRet + TCer.Ret.
    totalBase = totalBase + TCer.Bas.

    IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
        DISPLAY WKCta                                    AT 10  FORM "X(50)"
                totalBase FORM "->>,>>>,>>>,>>9"
            WITH WIDTH 132 FRAME F-Base_Ret4 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END.

/*totalRet = 0.*/
totalBase = 0.

FOR EACH TCer WHERE TCer.Nit = clientes.Nit
                AND TCer.Cba = "COMISIONES" NO-LOCK BREAK BY TCer.nit:
    WKCta = "       COMISIONES".
    totalBase = totalBase + TCer.Bas.
    TotalRet = TotalRet + TCer.Ret.
    
    IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
       DISPLAY WKCta                                    AT 10  FORM "X(50)"
               totalBase FORM "->>,>>>,>>>,>>9"
        WITH WIDTH 132 FRAME F-Comisiones USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END.

/*totalRet = 0.*/
totalBase = 0.

FOR EACH TCer WHERE TCer.Nit = clientes.Nit
                AND TCer.Cba = "SERVICIOS" NO-LOCK BREAK BY TCer.nit:
    WKCta = "       SERVICIOS".
    TotalRet = TotalRet + TCer.Ret.
    totalBase = totalBase + TCer.Bas.

    IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
        DISPLAY WKCta                                    AT 10  FORM "X(50)"
                totalBase FORM "->>,>>>,>>>,>>9"
            WITH WIDTH 132 FRAME F-Base_Ret5 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END.

/*totalRet = 0.*/
totalBase = 0.

FOR EACH TCer WHERE TCer.Nit = clientes.Nit
                AND TCer.Cba = "ARRENDAMIENTOS" NO-LOCK BREAK BY TCer.nit:
    WKCta = "       ARRENDAMIENTOS".
    TotalRet = TotalRet + TCer.Ret.
    totalBase = totalBase + TCer.Bas.

    IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
        DISPLAY WKCta                                    AT 10  FORM "X(50)"
                totalBase FORM "->>,>>>,>>>,>>9"
            WITH WIDTH 132 FRAME F-Base_Ret8 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END.

/*totalRet = 0.*/
totalBase = 0.

FOR EACH TCer WHERE TCer.Nit = clientes.Nit
                AND TCer.Cba = "RENDIMIENTOS FINANCIEROS" NO-LOCK BREAK BY TCer.nit:
    WKCta = "       RENDIMIENTOS FINANCIEROS".
    TotalRet = TotalRet + TCer.Ret.
    totalBase = totalBase + TCer.Bas.

    IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
        DISPLAY WKCta                                    AT 10  FORM "X(50)"
                totalBase FORM "->>,>>>,>>>,>>9"
            WITH WIDTH 132 FRAME F-RendimientosFinancieros USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END.

/*totalRet = 0.*/
totalBase = 0.

FOR EACH TCer WHERE TCer.Nit = clientes.Nit
                AND TCer.Cba = "COMPRA DE ACTIVOS FIJOS" NO-LOCK BREAK BY TCer.nit:
    WKCta = "       COMPRA DE ACTIVOS FIJOS".
    TotalRet = TotalRet + TCer.Ret.
    totalBase = totalBase + TCer.Bas.

    IF LAST-OF(TCer.Nit) THEN
        DISPLAY WKCta                                    AT 10  FORM "X(50)"
                totalBase FORM "->>,>>>,>>>,>>9"
            WITH WIDTH 132 FRAME F-CompraDeActivosFijos USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END.

/*totalRet = 0.*/
totalBase = 0.

FOR EACH TCer WHERE TCer.Nit = clientes.Nit
                AND TCer.Cba = "DEMAS COSTOS Y DEDUCCIONES" NO-LOCK BREAK BY TCer.nit:
    WKCta = "       OTROS".
    TotalRet = TotalRet + TCer.Ret.
    totalBase = totalBase + TCer.Bas.

    IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
       DISPLAY WKCta                                    AT 10  FORM "X(50)"
                totalBase FORM "->>,>>>,>>>,>>9"
        WITH WIDTH 132 FRAME F-Base_Ret7 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END.


FOR EACH TCer WHERE TCer.Nit = clientes.Nit
                AND TCer.Cba = "RETENCIÓN EN LA FUENTE"  NO-LOCK BREAK BY TCer.nit:
    WKCta = "       TOTAL RETENCIÓN EN LA FUENTE - RENTA".
    TotalRet = /*TotalRet +*/ TCer.Ret.
    totalBase = totalBase + TCer.Bas.

    IF LAST-OF(TCer.Nit) THEN
        DISPLAY "       -------------------------------------------------------------------------------" AT 10
                WKCta                                    AT 10  FORM "X(65)"
                " " totalRet
            WITH WIDTH 132 FRAME F-Base_Ret9 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END.


/*DISPLAY SKIP(2)
        SKIP(3)
        /*"                 El  76.46% de los Rendimientos Financieros percibidos durante el año gravable"*/ SKIP
        /*"                " STRING(WAno,"9999") ", no constituye Renta ni Ganancia Ocasional."*/ SKIP
        "" SKIP
        /*"                 De conformidad con el Decreto 0652 de abril 05 de 2013, no constituye Renta ni" SKIP
        "                 Ganancia  Ocasional  por  el  año gravable" STRING(WAno,"9999") + STRING(",","X(1)") "el 41.71% del valor de los" skip
        "                 Rendimientos  Financieros  percibidos   por  personas  naturales  y sucesiones" SKIP
        "                 ilíquidas no obligadas a llevar libros de contabilidad."*/
        "" SKIP
        "" skip
        "" SKIP
        ""
        SKIP(3)
        "                 Este Certificado no requiere firma autógrafa según Artículo 10 del Decreto 836" SKIP
        "                 de 1991."
    WITH FRAME FPieCer /*PAGE-BOTTOM*/ USE-TEXT WIDTH 132 NO-BOX STREAM-IO NO-LABELS.*/

DISPLAY SKIP(2)
        SKIP(3)
        "                 Componente inflacionario - Proyecto de Decreto: El 62.97% de los rendimientos" SKIP
        "                 financieros  obtenidos  por  personas  naturales  y  sucesiones ilíquidas, no" SKIP
        "                 obligadas a llevar libros de contabilidad obtenidos en el año gravable" STRING(WAno,"9999") SKIP
        "                 no constituyen renta ni ganancia ocasional." SKIP
        "" SKIP
        "" SKIP
        "" SKIP
        "" SKIP
        "" SKIP(1)
        "                 Este  documento no requiere para su validez firma autógrafa de acuerdo con el" SKIP
        "                 artículo  10  del  Decreto 836 de 1991, recopilado en el artículo 1.6.1.12.12" SKIP
        "                 del  DUT  1625 de octubre 11 de 2016, que regula el contenido del certificado" SKIP
        "                 de retenciones a título de renta."
    WITH FRAME FPieCer USE-TEXT WIDTH 132 NO-BOX STREAM-IO NO-LABELS.


PAGE.

/* Certificado de saldos */

FIND FIRST saldos WHERE saldos.cedula = clientes.nit NO-LOCK NO-ERROR.
IF AVAILABLE saldos THEN DO:
    WAAno = WAno.

    DISPLAY SKIP(37)
            "                   FONDO EMPLEADOS DOCENTES UNIVERSIDAD NACIONAL DE COLOMBIA - FODUN" SKIP(5)
            "                                              CERTIFICA" SKIP(5)
            "                 Que el(la) señor(a)" saldos.nombre FORM "X(50)" SKIP
            "                 identificado(a) con Cédula No." saldos.cedula " es asociado a FODUN" SKIP
            "                 y,  al 31 de Diciembre de" WAAno ", presenta los siguientes saldos:" SKIP(2)
            "                                       DETALLE DE LOS SALDOS" SKIP
            "                 ------------------------------------------------------------------------" SKIP(1)
            /*"                 Saldos que presenta al 31 de Diciembre de : " WAAno SKIP(1)*/
            "                 Aportes Sociales.....................................$" saldos.totalAportes SKIP
            "                 Ahorros..............................................$" saldos.totalAhorros SKIP
            "                 CDAT.................................................$" saldos.totalCDAT SKIP
            "                 Créditos.............................................$" saldos.totalCreditos SKIP
            "                 Intereses pagados durante año" STRING(WAno) "..............$" saldos.totalIntereses SKIP(2)
            "                 Revalorización de Aportes Sociales (   " STRING(WAno) ")...$" saldos.totalRevalorizacionAportes SKIP(4)
            "                 Este Certificado no requiere firma autógrafa según Artículo 10 del Decreto" SKIP
            "                 836 de 1991."
            WITH DOWN WIDTH 120 FRAME F1 NO-BOX STREAM-IO NO-LABELS USE-TEXT.

    PAGE.
END.

/*OUTPUT CLOSE.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE certificadoDeSaldos wWin 
PROCEDURE certificadoDeSaldos :
DEFINE VAR cont AS INTEGER.
DEFINE VAR choice AS LOGICAL.

EMPTY TEMP-TABLE saldos.

FOR EACH rep_ahorros WHERE rep_ahorros.nit = clientes.nit AND rep_ahorros.fecCorte = DATE(12, 31, wAno) NO-LOCK BREAK BY rep_ahorros.nit
                                                                                                                      BY rep_ahorros.tip_ahorro:
    IF FIRST-OF(rep_ahorros.nit) THEN DO:
        FIND FIRST saldos WHERE saldos.cedula = clientes.nit NO-ERROR.
        IF NOT AVAILABLE saldos THEN DO:
            CREATE saldos.
            saldos.cedula = clientes.nit.
            saldos.nombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
        END.
    END.

    CASE rep_ahorros.tip_ahorro:
        WHEN 4 THEN DO:
            saldos.totalAportes = saldos.totalAportes + rep_ahorros.Sdo_disponible.

            IF FIRST-OF(rep_ahorros.tip_ahorro) THEN DO:
                FOR EACH mov_contable WHERE YEAR(mov_contable.fec_contable) = wAno
                                        AND mov_contable.nit = rep_ahorros.nit
                                        AND mov_contable.comentario = "Revalorización de Aportes" NO-LOCK:
                    saldos.totalRevalorizacionAportes = saldos.totalRevalorizacionAportes + mov_contable.cr.
                END.
            END.
        END.
        
        WHEN 1 OR
        WHEN 2 THEN saldos.totalAhorros = saldos.totalAhorros + rep_ahorros.Sdo_disponible.

        WHEN 3 THEN saldos.totalCDAT = saldos.totalCDAT + rep_ahorros.Sdo_disponible.
    END CASE.
END.

FOR EACH rep_creditos WHERE rep_creditos.nit = saldos.cedula AND rep_creditos.fecCorte = date(12, 31, wAno) NO-LOCK BREAK BY rep_credito.nit:
    IF FIRST-OF(rep_creditos.nit) THEN DO:
        FIND FIRST saldos WHERE saldos.cedula = clientes.nit NO-ERROR.
        IF NOT AVAILABLE saldos THEN DO:
            CREATE saldos.
            saldos.cedula = clientes.nit.
            saldos.nombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
        END.
    END.

    saldos.totalCreditos = saldos.totalCreditos + rep_creditos.sdo_capital + rep_creditos.INT_corriente + rep_creditos.INT_MorCobrar.
END.

FOR EACH anexos WHERE anexos.nit = saldos.cedula
                  AND (SUBSTRING(anexos.cuenta,1,4) = "4150")
                  AND anexos.ano = wano NO-LOCK BREAK BY anexos.nit:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST saldos WHERE saldos.cedula = clientes.nit NO-ERROR.
        IF NOT AVAILABLE saldos THEN DO:
            CREATE saldos.
            saldos.cedula = clientes.nit.
            saldos.nombre = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
        END.
    END.

    DO cont = 1 TO 12:
        saldos.totalIntereses = saldos.totalIntereses + anexos.cr[cont] - anexos.db[cont].
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CertificadoReteFuente wWin 
PROCEDURE CertificadoReteFuente :
DEFINE VAR codAhorro AS INTEGER.
DEFINE VAR pPorcentaje AS DECIMAL.
DEFINE VAR cont AS INTEGER.
DEFINE VAR baseCalculada AS DECIMAL.
DEFINE VAR ICA_Tarifa AS DECIMAL.
DEFINE BUFFER bfr_mov_contable FOR mov_contable.

SESSION:SET-WAIT-STATE("General").

/*EMPTY TEMP-TABLE TCer.*/

j = 0.

EMPTY TEMP-TABLE ttmov.

/* Salarios */                                      RUN Salarios.
/* Honorarios */                                    RUN Honorarios.
/* Servicios */                                     RUN Servicios.
/* Arrendamientos */                                RUN Arrendamientos.
/* Rendimientos Financieros */                      RUN RendimientosFinancieros.
/* Compra Activos */                                RUN CompraDeActivosFijos.
/* Demás Costos y Deducciones */                    RUN DemasCostos.
/* Gravamen a los movientos financieros - GMF */    RUN GMF.
/* ICA */                                           RUN ICA.
/* Retención en la Fuente */                        RUN Retefuente.

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Comisiones wWin 
PROCEDURE Comisiones :
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE anexos.nit = clientes.nit
                  AND SUBSTRING(anexos.cuenta,1,8) = "61509512"
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE TCer.nit = clientes.nit
                      AND TCer.CBa = "COMISIOENS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = clientes.nit
               TCer.CBa = "COMISIONES".
    END.

    TCer.Bas = TCer.Bas + anexos.sdo_inicial.

    DO cont = 1 TO 12:
         TCer.Bas = TCer.Bas + anexos.db[cont] - anexos.cr[cont].
    END.

    IF LAST-OF(anexos.cuenta) THEN DO:
        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = wAno NO-LOCK BREAK:
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
            END.
        END.

        FOR EACH docs NO-LOCK:
            FOR EACH mov_contable WHERE mov_contable.agencia = docs.agencia
                                    AND mov_contable.comprobante = docs.comprobante
                                    AND mov_contable.num_documento = docs.num_documento
                                    AND mov_contable.nit = docs.nit
                                    AND YEAR(mov_contable.fec_contable) = wAno
                                    AND (SUBSTRING(mov_contable.cuenta,1,6) = "243515" OR
                                         SUBSTRING(mov_contable.cuenta,1,6) = "243525") NO-LOCK:
                FIND FIRST ttmov WHERE ttmov.Id = ROWID(mov_contable) NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttmov THEN DO:
                    TCer.Ret = TCer.Ret + mov_contable.cr - mov_contable.db.

                    CREATE ttmov.
                    ttmov.id = ROWID(mov_contable).
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CompraDeActivosFijos wWin 
PROCEDURE CompraDeActivosFijos :
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE ttmov.

FOR EACH rep_activosFijos WHERE rep_activosFijos.fecCorte = DATE(12,31,wAno)
                            AND YEAR(rep_activosFijos.fechaCompra) = wAno
                            AND rep_activosFijos.nitProveedor = clientes.nit
                            AND rep_activosFijos.contabilizado = YES NO-LOCK BREAK BY rep_activosFijos.nitProveedor:
    FIND FIRST TCer WHERE TCer.nit = clientes.nit
                      AND TCer.CBa = "COMPRA DE ACTIVOS FIJOS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = clientes.Nit
               TCer.CBa = "COMPRA DE ACTIVOS FIJOS".
    END.

    TCer.Bas = TCer.Bas + rep_activosFijos.valorCompra.

    IF LAST-OF(rep_activosFijos.nitProveedor) THEN DO:
        FOR EACH mov_contable WHERE mov_contable.nit = rep_activosFijos.nitProveedor
                                AND YEAR(mov_contable.fec_contable) = wAno
                                AND SUBSTRING(mov_contable.cuenta,1,6) = "243540" NO-LOCK:
            FIND FIRST ttmov WHERE ttmov.Id = ROWID(mov_contable) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttmov THEN DO:
                TCer.Ret = TCer.Ret + mov_contable.cr - mov_contable.db.

                CREATE ttmov.
                ttmov.id = ROWID(mov_contable).
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DemasCostos wWin 
PROCEDURE DemasCostos :
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR cont AS INTEGER.
DEFINE VAR ctaRete1 AS CHARACTER.
DEFINE VAR ctaRete2 AS CHARACTER.
DEFINE VAR ctaRete3 AS CHARACTER.

EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE anexos.nit = clientes.nit
                  AND (SUBSTRING(anexos.cuenta,1,4) = "2695" OR SUBSTRING(anexos.cuenta,1,6) = "262505" OR SUBSTRING(anexos.cuenta,1,6) = "510521" OR SUBSTRING(anexos.cuenta,1,6) = "510595" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511002" OR SUBSTRING(anexos.cuenta,1,6) = "511005" OR SUBSTRING(anexos.cuenta,1,6) = "511006" OR SUBSTRING(anexos.cuenta,1,6) = "511007" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511009" OR SUBSTRING(anexos.cuenta,1,6) = "511010" OR SUBSTRING(anexos.cuenta,1,6) = "511011" OR SUBSTRING(anexos.cuenta,1,6) = "511012" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511013" OR SUBSTRING(anexos.cuenta,1,6) = "511014" OR SUBSTRING(anexos.cuenta,1,6) = "511015" OR SUBSTRING(anexos.cuenta,1,6) = "511016" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511018" OR SUBSTRING(anexos.cuenta,1,6) = "511019" OR SUBSTRING(anexos.cuenta,1,6) = "511020" OR SUBSTRING(anexos.cuenta,1,6) = "511021" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511022" OR SUBSTRING(anexos.cuenta,1,6) = "511023" OR SUBSTRING(anexos.cuenta,1,6) = "511024" OR SUBSTRING(anexos.cuenta,1,6) = "511025" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511026" OR SUBSTRING(anexos.cuenta,1,8) = "51102701" OR SUBSTRING(anexos.cuenta,1,8) = "51102702" OR SUBSTRING(anexos.cuenta,1,8) = "51102703" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51102704" OR SUBSTRING(anexos.cuenta,1,8) = "51102705" OR SUBSTRING(anexos.cuenta,1,6) = "511031" OR SUBSTRING(anexos.cuenta,1,6) = "511032" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511095" OR SUBSTRING(anexos.cuenta,1,4) = "5115" OR SUBSTRING(anexos.cuenta,1,4) = "5125" OR SUBSTRING(anexos.cuenta,1,6) = "521005" OR
                       SUBSTRING(anexos.cuenta,1,6) = "521020" OR SUBSTRING(anexos.cuenta,1,6) = "523020" OR SUBSTRING(anexos.cuenta,1,6) = "523035" OR /*SUBSTRING(anexos.cuenta,1,6) = "523050" OR*/ /* Solicita Iván 04/04/2019 */
                       SUBSTRING(anexos.cuenta,1,10) = "6140101102" OR SUBSTRING(anexos.cuenta,1,10) = "6140101108" OR SUBSTRING(anexos.cuenta,1,10) = "6140101110" OR SUBSTRING(anexos.cuenta,1,10) = "6140101114" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101116" OR SUBSTRING(anexos.cuenta,1,10) = "6140101118" OR SUBSTRING(anexos.cuenta,1,10) = "6140101122" OR SUBSTRING(anexos.cuenta,1,10) = "6140101136" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101140" OR SUBSTRING(anexos.cuenta,1,10) = "6140101195" OR SUBSTRING(anexos.cuenta,1,10) = "6140101251" OR SUBSTRING(anexos.cuenta,1,8) = "61509501" OR
                       SUBSTRING(anexos.cuenta,1,8) = "61509502" OR SUBSTRING(anexos.cuenta,1,8) = "61509512")
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.

    FIND FIRST TCer WHERE TCer.nit = clientes.nit
                      AND TCer.CBa = "DEMAS COSTOS Y DEDUCCIONES" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = clientes.Nit
               TCer.CBa = "DEMAS COSTOS Y DEDUCCIONES".
    END.

    IF anexos.nit <> "800112808" AND (SUBSTRING(anexos.cuenta,1,4) = "5115" OR SUBSTRING(anexos.cuenta,1,4) = "5125") THEN
        NEXT.

    IF SUBSTRING(anexos.cuenta,1,2) <> "26" THEN
        TCer.Bas = TCer.Bas + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        IF SUBSTRING(anexos.cuenta,1,2) <> "26" THEN
            TCer.Bas = TCer.Bas + anexos.db[cont] - anexos.cr[cont].
        ELSE DO:
            TCer.Bas = TCer.Bas + anexos.db[cont].

            FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                    AND mov_contable.nit = anexos.nit
                                    AND YEAR(mov_contable.fec_contable) = wAno
                                    AND MONTH(mov_contable.fec_contable) = cont
                                    AND (mov_contable.comprobante = 9 OR INDEX(mov_contable.comentario,"rev") > 0 OR INDEX(mov_contable.comentario,"reclasif") > 0)
                                    AND mov_contable.cen_costos = anexos.cen_costos
                                    AND mov_contable.cr > 0 NO-LOCK:
                TCer.Bas = TCer.Bas - mov_contable.cr.
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
                                AND YEAR(mov_contable.fec_contable) = wAno
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
            END.
        END.

        FOR EACH docs NO-LOCK:
            FOR EACH mov_contable WHERE mov_contable.agencia = docs.agencia
                                    AND mov_contable.comprobante = docs.comprobante
                                    AND mov_contable.num_documento = docs.num_documento
                                    AND mov_contable.nit = docs.nit
                                    AND YEAR(mov_contable.fec_contable) = wAno
                                    AND (SUBSTRING(mov_contable.cuenta,1,6) = ctaRete1 OR
                                         SUBSTRING(mov_contable.cuenta,1,6) = ctaRete2 OR
                                         SUBSTRING(mov_contable.cuenta,1,6) = ctaRete3) NO-LOCK:
                FIND FIRST ttmov WHERE ttmov.Id = ROWID(mov_contable) NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttmov THEN DO:
                    IF mov_contable.nit <> "800197268" THEN
                        TCer.Ret = TCer.Ret + mov_contable.cr - mov_contable.db.
                    ELSE
                        TCer.Ret = TCer.Ret + mov_contable.cr.

                    CREATE ttmov.
                    ttmov.id = ROWID(mov_contable).
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY WAno WNit WNomNit 
      WITH FRAME FCer IN WINDOW wWin.
  ENABLE RECT-1 WAno WNit btnMasivoAsociados BtnDone 
      WITH FRAME FCer IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FCer}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gastosDeRepresentacion wWin 
PROCEDURE gastosDeRepresentacion :
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE anexos.nit = clientes.nit
                  AND SUBSTRING(anexos.cuenta,1,8) = "51102601"
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST TCer WHERE TCer.nit = clientes.nit
                      AND TCer.CBa = "GASTOS DE REPRESENTACIÓN" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = clientes.nit
               TCer.CBa = "GASTOS DE REPRESENTACIÓN".
    END.

    TCer.Bas = TCer.Bas + anexos.sdo_inicial.

    DO cont = 1 TO 12:
         TCer.Bas = TCer.Bas + anexos.db[cont] - anexos.cr[cont].
    END.

    IF LAST-OF(anexos.cuenta) THEN DO:
        EMPTY TEMP-TABLE docs.
    
        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = wAno NO-LOCK BREAK:
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
                                    AND YEAR(mov_contable.fec_contable) = wAno
                                    AND SUBSTRING(mov_contable.cuenta,1,4) = "2435"
                                    AND SUBSTRING(mov_contable.cuenta,1,6) <> "243535"NO-LOCK:
                FIND FIRST ttmov WHERE ttmov.Id = ROWID(mov_contable) NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttmov THEN DO:
                    TCer.Ret = TCer.Ret + mov_contable.cr - mov_contable.db.

                    CREATE ttmov.
                    ttmov.id = ROWID(mov_contable).
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GMF wWin 
PROCEDURE GMF :
DEFINE VAR cont AS INTEGER.

FOR EACH anexos WHERE anexos.nit = clientes.nit
                    AND SUBSTRING(anexos.cuenta,1,4) = "2430"
                    AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST TCer WHERE TCer.nit = clientes.nit
                          AND TCer.CBa = "GRAVAMEN A LOS MOVIMIENTOS FINANCIEROS" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = clientes.Nit
                   TCer.CBa = "GRAVAMEN A LOS MOVIMIENTOS FINANCIEROS".
        END.
    END.
        
    TCer.Ret = TCer.ret + anexos.sdo_inicial.
        
    DO cont = 1 TO 12:
         TCer.ret = TCer.ret + anexos.cr[cont] - anexos.db[cont].
    END.

    IF LAST-OF(anexos.nit) THEN
        tCer.Bas = (tCer.Ret * 1000) / 4.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Honorarios wWin 
PROCEDURE Honorarios :
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE anexos.nit = clientes.nit
                  AND (SUBSTRING(anexos.cuenta,1,6) = "511001" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51102711" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51102712" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51102713" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51102714" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51102715" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511030")
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE TCer.nit = clientes.nit
                      AND TCer.CBa = "HONORARIOS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = clientes.nit
               TCer.CBa = "HONORARIOS".
    END.

    TCer.Bas = TCer.Bas + anexos.sdo_inicial.

    DO cont = 1 TO 12:
         TCer.Bas = TCer.Bas + anexos.db[cont] - anexos.cr[cont].
    END.

    IF LAST-OF(anexos.cuenta) THEN DO:
        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = wAno NO-LOCK:
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
            END.
        END.

        FOR EACH docs NO-LOCK:
            FOR EACH mov_contable WHERE mov_contable.agencia = docs.agencia
                                    AND mov_contable.comprobante = docs.comprobante
                                    AND mov_contable.num_documento = docs.num_documento
                                    AND mov_contable.nit = docs.nit
                                    AND YEAR(mov_contable.fec_contable) = wAno
                                    AND (SUBSTRING(mov_contable.cuenta,1,6) = "243515" OR
                                         SUBSTRING(mov_contable.cuenta,1,6) = "243525") NO-LOCK:
                FIND FIRST ttmov WHERE ttmov.Id = ROWID(mov_contable) NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttmov THEN DO:
                    TCer.Ret = TCer.Ret + mov_contable.cr - mov_contable.db.

                    CREATE ttmov.
                    ttmov.id = ROWID(mov_contable).
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ICA wWin 
PROCEDURE ICA :
DEFINE VAR cont AS INTEGER.
DEFINE BUFFER bfrMovContable FOR mov_contable.
    
EMPTY TEMP-TABLE docs.

FOR EACH anexos WHERE anexos.nit = clientes.nit
                    AND (SUBSTRING(anexos.cuenta,1,6) = "511028" OR
                         SUBSTRING(anexos.cuenta,1,8) = "6140101154" OR
                         SUBSTRING(anexos.cuenta,1,8) = "6140101155" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511001" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51102711" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51102712" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51102713" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51102714" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511030" OR
                         SUBSTRING(anexos.cuenta,1,8) = "61509512")
                    AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                          BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE TCer.nit = clientes.nit
                      AND TCer.CBa = "ICA" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = clientes.Nit
               TCer.CBa = "ICA".
    END.

    TCer.Bas = TCer.Bas + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        TCer.Bas = TCer.Bas + anexos.db[cont] - anexos.cr[cont].
    END.
END.
        
FOR EACH anexos WHERE anexos.nit = clientes.nit
                    AND SUBSTRING(anexos.cuenta,1,6) = "243575"
                    AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                          BY anexos.cuenta:
    IF FIRST-OF(anexos.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.

    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST TCer WHERE TCer.nit = clientes.nit
                          AND TCer.CBa = "ICA" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = clientes.Nit
                   TCer.CBa = "ICA".
        END.
    END.

    TCer.Ret = TCer.Ret + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        IF cuentas.naturaleza = "DB" THEN
            TCer.Ret = TCer.Ret + anexos.db[cont] - anexos.cr[cont].
        ELSE
            TCer.Ret = TCer.Ret + anexos.cr[cont] - anexos.db[cont].
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Sdo wWin 
PROCEDURE Imp_Sdo :
DEFI VAR WAAno AS INTEG FORM "9999".

WAAno = WAno.

FOR EACH saldos NO-LOCK:
    DISPLAY SKIP(15)
            "                             FONDO EMPLEADOS DOCENTES UNIVERSIDAD NACIONAL DE COLOMBIA - FODUN" SKIP(5)
            "                                                        CERTIFICA" SKIP(5)
            "                           Que el(la) señor(a)" saldos.nombre FORM "X(50)" SKIP
            "                           identificado(a) con Cédula No." saldos.cedula " es asociado a FODUN" SKIP
            "                           y,  al 31 de Diciembre de" WAAno ", presenta los siguientes saldos:" SKIP(2)
            "                                                 DETALLE DE LOS SALDOS" SKIP
            "                           -----------------------------------------------------------------------------" SKIP(1)
            /*"                           Saldos que presenta al 31 de Diciembre de : " WAAno SKIP(1)*/
            "                           Aportes Sociales.....................................$" saldos.totalAportes SKIP
            "                           Ahorros..............................................$" saldos.totalAhorros SKIP
            "                           CDAT.................................................$" saldos.totalCDAT SKIP
            "                           Créditos.............................................$" saldos.totalCreditos SKIP
            "                           Intereses pagados durante año" STRING(WAno) "..............$" saldos.totalIntereses SKIP(2)
            "                           Revalorización de Aportes Sociales (   " STRING(WAno) ")...$" saldos.totalRevalorizacionAportes SKIP(3)
            "                           Este Certificado no requiere firma autógrafa según Artículo 10 del Decreto" SKIP
            "                           836 de 1991."
        WITH DOWN WIDTH 120 FRAME F1 NO-BOX STREAM-IO NO-LABELS USE-TEXT.

    PAGE.
END.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
WAno = YEAR(W_Fecha) - 1.

EMPTY TEMP-TABLE F1001.

INPUT FROM F1001.csv.
REPEAT:
    CREATE F1001.
    IMPORT DELIMITER ";" F1001.
END.
INPUT CLOSE.

RUN SUPER.
HIDE FRAME FBuscar.

FIND FIRST usuarios WHERE usuarios.usuario = w_usuario NO-LOCK NO-ERROR.

IF usuarios.prioridad = 6 THEN
    btnMasivoAsociados:VISIBLE IN FRAME FCer = TRUE.
ELSE
    btnMasivoAsociados:VISIBLE = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PorcentajesReteFuente wWin 
PROCEDURE PorcentajesReteFuente :
DEFINE INPUT PARAMETER pCuenta AS CHARACTER.
DEFINE OUTPUT PARAMETER pPorcentaje AS DECIMAL.

CASE pCuenta:
    WHEN "24350501" THEN pPorcentaje = 0.
    WHEN "24351501" THEN pPorcentaje = 10 / 100.
    WHEN "24351502" THEN pPorcentaje = 11 / 100.
    WHEN "24351503" THEN pPorcentaje = 0.
    WHEN "24352501" THEN pPorcentaje = 1 / 100.
    WHEN "24352502" THEN pPorcentaje = 3 / 100.
    WHEN "24352503" THEN pPorcentaje = 3.5 / 100.
    WHEN "24352504" THEN pPorcentaje = 6 / 100.
    WHEN "24352505" THEN pPorcentaje = 2 / 100.
    WHEN "24352506" THEN pPorcentaje = 4 / 100.
    WHEN "24352507" THEN pPorcentaje = 0.
    WHEN "24353001" THEN pPorcentaje = 3.5 / 100.
    WHEN "24353002" THEN pPorcentaje = 4 / 100.
    WHEN "24353501" THEN pPorcentaje = 7 / 100.
    WHEN "24353502" THEN pPorcentaje = 7 / 100.
    WHEN "24353503" THEN pPorcentaje = 7 / 100.
    WHEN "24353504" THEN pPorcentaje = 7 / 100.
    WHEN "24353507" THEN pPorcentaje = 7 / 100.
    WHEN "24353508" THEN pPorcentaje = 7 / 100.
    WHEN "24353509" THEN pPorcentaje = 7 / 100.
    WHEN "24353510" THEN pPorcentaje = 7 / 100.
    WHEN "24354001" THEN pPorcentaje = 3.5 / 100.
    WHEN "24358501" THEN pPorcentaje = 3.5 / 100.
    WHEN "24356001" THEN pPorcentaje = 8 / 100.
    WHEN "24356002" THEN pPorcentaje = 5 / 100.

    WHEN "24357501" THEN pPorcentaje = 13.8 / 100.
    WHEN "24357503" THEN pPorcentaje = 0.966 / 100.
    WHEN "24357504" THEN pPorcentaje = 11.4 / 100.
    WHEN "24357505" THEN pPorcentaje = 6.9 / 100.
    WHEN "24357506" THEN pPorcentaje = 4.14 / 100.
END CASE.

IF SUBSTRING(pCuenta,1,4) = "2430" THEN
    pPorcentaje = 4 / 1000.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
IF WSdo THEN DO:
    WSdo = FALSE.

    RUN Imp_Sdo.

    RETURN.
END.

RUN Imp_CU.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RendimientosFinancieros wWin 
PROCEDURE RendimientosFinancieros :
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR cont AS INTEGER.
DEFIN VAR ctaRete AS CHARACTER.
    
EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.
    
FOR EACH anexos WHERE anexos.nit = clientes.nit
                  AND (SUBSTRING(anexos.cuenta,1,8) = "52100505" OR
                       SUBSTRING(anexos.cuenta,1,6) = "615005" OR
                       SUBSTRING(anexos.cuenta,1,6) = "615010" OR
                       SUBSTRING(anexos.cuenta,1,6) = "615020" OR
                       SUBSTRING(anexos.cuenta,1,6) = "615035")
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE TCer.nit = clientes.nit
                      AND TCer.CBa = "RENDIMIENTOS FINANCIEROS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = clientes.nit
               TCer.CBa = "RENDIMIENTOS FINANCIEROS".
    END.

    TCer.Bas = TCer.Bas + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        TCer.Bas = TCer.Bas + anexos.db[cont] - anexos.cr[cont].
    END.

    IF LAST-OF(anexos.cuenta) THEN DO:
        IF SUBSTRING(anexos.cuenta,1,8) = "52100505" THEN
            ctaRete = "243535".
        ELSE
            ctaRete = "243525".

        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = wAno NO-LOCK BREAK:
            FIND FIRST docs WHERE docs.nit = mov_contable.nit
                              AND docs.agencia = mov_contable.agencia
                              AND docs.comprobante = mov_contable.comprobante
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
                    TCer.Ret = TCer.Ret + bfrMovContable.cr - bfrMovContable.db.
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Retefuente wWin 
PROCEDURE Retefuente :
DEFINE VAR cont AS INTEGER.

FOR EACH anexos WHERE anexos.nit = clientes.nit
                    AND SUBSTRING(anexos.cuenta,1,4) = "2435"
                    AND SUBSTRING(anexos.cuenta,1,6) <> "243575"
                    AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST TCer WHERE TCer.nit = clientes.nit
                          AND TCer.CBa = "RETENCIÓN EN LA FUENTE" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = clientes.Nit
                   TCer.CBa = "RETENCIÓN EN LA FUENTE".
        END.
    END.
        
    TCer.Ret = TCer.Ret + anexos.sdo_inicial.
        
    DO cont = 1 TO 12:
         TCer.Ret = TCer.Ret + anexos.cr[cont] - anexos.db[cont].
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Salarios wWin 
PROCEDURE Salarios :
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE anexos.nit = clientes.nit
                  AND (SUBSTRING(anexos.cuenta,1,6) = "510503" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510505" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510509" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510510" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510511" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510512" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510516" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510519" OR
                       SUBSTRING(anexos.cuenta,1,6) = "510520" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101206" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101215" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101227" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101230" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101233" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101236" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101239" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101248" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101260")
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE TCer.nit = clientes.nit
                      AND TCer.CBa = "SALARIOS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = clientes.nit
               TCer.CBa = "SALARIOS".
    END.

    TCer.Bas = TCer.Bas + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        TCer.Bas = TCer.Bas + anexos.db[cont] - anexos.cr[cont].
    END.

    IF LAST-OF(anexos.cuenta) AND (SUBSTRING(anexos.cuenta,1,6) = "510503" OR
                                   SUBSTRING(anexos.cuenta,1,10) = "6140101206") THEN DO:
        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = wAno NO-LOCK:
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
            END.
        END.

        FOR EACH docs NO-LOCK:
            FOR EACH mov_contable WHERE mov_contable.agencia = docs.agencia
                                    AND mov_contable.comprobante = docs.comprobante
                                    AND mov_contable.num_documento = docs.num_documento
                                    AND mov_contable.nit = docs.nit
                                    AND YEAR(mov_contable.fec_contable) = wAno
                                    AND SUBSTRING(mov_contable.cuenta,1,6) = "243505" NO-LOCK:
                FIND FIRST ttmov WHERE ttmov.Id = ROWID(mov_contable) NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttmov THEN DO:
                    TCer.Ret = TCer.Ret + mov_contable.cr - mov_contable.db.

                    CREATE ttmov.
                    ttmov.id = ROWID(mov_contable).
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Servicios wWin 
PROCEDURE Servicios :
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE docs.
EMPTY TEMP-TABLE ttmov.

FOR EACH anexos WHERE anexos.nit = clientes.nit
                  AND (SUBSTRING(anexos.cuenta,1,6) = "511028" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101154" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101155")
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE TCer.nit = clientes.nit
                      AND TCer.CBa = "SERVICIOS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = clientes.nit
               TCer.CBa = "SERVICIOS".
    END.

    TCer.Bas = TCer.Bas + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        TCer.Bas = TCer.Bas + anexos.db[cont] - anexos.cr[cont].
    END.
        
    IF LAST-OF(anexos.cuenta) THEN DO:
        FOR EACH mov_contable WHERE mov_contable.cuenta = anexos.cuenta
                                AND mov_contable.nit = anexos.nit
                                AND YEAR(mov_contable.fec_contable) = wAno NO-LOCK BREAK:
            FIND FIRST docs WHERE docs.nit = mov_contable.nit
                              AND docs.agencia = mov_contable.agencia
                              AND docs.comprobante = mov_contable.comprobante
                              AND docs.num_documento = mov_contable.num_documento
                              AND docs.fec_contable = mov_contable.fec_contable NO-LOCK NO-ERROR.
            IF NOT AVAILABLE docs THEN DO:
                CREATE docs.
                docs.nit = mov_contable.nit.
                docs.agencia = mov_contable.agencia.
                docs.comprobante = mov_contable.comprobante.
                docs.num_documento = mov_contable.num_documento.
                docs.fec_contable = mov_contable.fec_contable.
            END.
        END.

        FOR EACH docs NO-LOCK:
            FOR EACH mov_contable WHERE mov_contable.agencia = docs.agencia
                                    AND mov_contable.comprobante = docs.comprobante
                                    AND mov_contable.num_documento = docs.num_documento
                                    AND mov_contable.nit = docs.nit
                                    AND YEAR(mov_contable.fec_contable) = wAno
                                    AND (SUBSTRING(mov_contable.cuenta,1,6) = "243515" OR
                                         SUBSTRING(mov_contable.cuenta,1,6) = "243525") NO-LOCK:
                FIND FIRST ttmov WHERE ttmov.Id = ROWID(mov_contable) NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ttmov THEN DO:
                    TCer.Ret = TCer.Ret + mov_contable.cr - mov_contable.db.

                    CREATE ttmov.
                    ttmov.id = ROWID(mov_contable).
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

