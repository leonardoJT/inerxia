&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

DEFINE VAR W_Ok AS LOGICAL.
DEFI VAR WSdo AS LOG INIT FALSE.

DEFINE TEMP-TABLE TNit
    FIELD Nit LIKE Clientes.Nit
    FIELD Nom AS CHARACTER FORMAT "X(50)"
    FIELD Tel LIKE Clientes.Tel_Residencia
    FIELD DIR LIKE Clientes.DIR_Residencia
    FIELD totalSinRet AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    INDEX idx_Nit nit.

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
    FIELD totalIntereses AS DECIMAL FORMAT ">>>,>>>,>>9.99".

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
&Scoped-define BROWSE-NAME BNit

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TNit TCer

/* Definitions for BROWSE BNit                                          */
&Scoped-define FIELDS-IN-QUERY-BNit TNit.Nit TNit.Nom TNit.Tel TNit.DIR   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BNit   
&Scoped-define SELF-NAME BNit
&Scoped-define QUERY-STRING-BNit FOR EACH TNit NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BNit OPEN QUERY {&SELF-NAME} FOR EACH TNit NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BNit TNit
&Scoped-define FIRST-TABLE-IN-QUERY-BNit TNit


/* Definitions for BROWSE BRet                                          */
&Scoped-define FIELDS-IN-QUERY-BRet TCer.Cba TCer.Bas TCer.Ret   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRet   
&Scoped-define SELF-NAME BRet
&Scoped-define QUERY-STRING-BRet FOR EACH TCer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRet OPEN QUERY {&SELF-NAME} FOR EACH TCer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRet TCer
&Scoped-define FIRST-TABLE-IN-QUERY-BRet TCer


/* Definitions for FRAME FCer                                           */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FCer ~
    ~{&OPEN-QUERY-BNit}~
    ~{&OPEN-QUERY-BRet}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-318 RECT-319 WAno Btn_Imprimir ~
WNit Btn_Sdos BNit btnPdf BRet BtnDone 
&Scoped-Define DISPLAYED-OBJECTS WAno WNit WNomNit RNit 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BNit 
       MENU-ITEM m_Buscar       LABEL "Buscar"        .


/* Definitions of the field level widgets                               */
DEFINE VARIABLE F_Nit AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nit a buscar" 
     VIEW-AS FILL-IN 
     SIZE 23 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 15 BY 1.54
     BGCOLOR 8 .

DEFINE BUTTON btnPdf 
     LABEL "Cert.RteFte PDF" 
     SIZE 16 BY 1.35.

DEFINE BUTTON Btn_Imprimir 
     LABEL "Cert. ReteFuente" 
     SIZE 16.72 BY 1.12.

DEFINE BUTTON Btn_Sdos 
     LABEL "Cert. Saldos" 
     SIZE 16.72 BY 1.12.

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

DEFINE VARIABLE RNit AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Individual", 1,
"Consolidado", 2
     SIZE 15 BY 1.62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 2.69.

DEFINE RECTANGLE RECT-318
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 3.96.

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 2.85.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BNit FOR 
      TNit SCROLLING.

DEFINE QUERY BRet FOR 
      TCer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BNit wWin _FREEFORM
  QUERY BNit NO-LOCK DISPLAY
      TNit.Nit FORMAT "X(14)":U
  TNit.Nom FORMAT "X(35)":U
  TNit.Tel FORMAT "X(15)"
  TNit.DIR FORMAT "X(35)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92 BY 5.65
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE BRet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRet wWin _FREEFORM
  QUERY BRet NO-LOCK DISPLAY
      TCer.Cba FORMAT "X(50)":U        COLUMN-LABEL "Cta.Retencion"
   TCer.Bas FORMAT ">>,>>>,>>>,>>9" COLUMN-LABEL "Valor-Base"
   TCer.Ret FORMAT  "->>,>>>,>>9"   COLUMN-LABEL "Valor-Retención"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92 BY 10.77
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FCer
     WAno AT ROW 1.81 COL 79 COLON-ALIGNED
     Btn_Imprimir AT ROW 1.88 COL 96.43
     WNit AT ROW 2.88 COL 8 COLON-ALIGNED
     WNomNit AT ROW 2.88 COL 26 COLON-ALIGNED NO-LABEL
     Btn_Sdos AT ROW 3.15 COL 96.43
     BNit AT ROW 4.5 COL 3
     btnPdf AT ROW 5.85 COL 97 WIDGET-ID 16
     RNit AT ROW 7.42 COL 97.29 NO-LABEL
     BRet AT ROW 10.69 COL 3
     BtnDone AT ROW 19.81 COL 97
     " Pantalla" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.19 COL 96 WIDGET-ID 10
     " Imprimir" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 5.12 COL 96 WIDGET-ID 4
     RECT-1 AT ROW 1.54 COL 3
     RECT-318 AT ROW 5.58 COL 96 WIDGET-ID 2
     RECT-319 AT ROW 1.65 COL 95.86 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.29 BY 21.12
         FONT 5
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME FBuscar
     F_Nit AT ROW 1.27 COL 10 COLON-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 39 ROW 2.08
         SIZE 36 BY 2.15
         FONT 4
         TITLE "Busqueda de nit en consulta".


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
         HEIGHT             = 21.12
         WIDTH              = 113.29
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
/* REPARENT FRAME */
ASSIGN FRAME FBuscar:FRAME = FRAME FCer:HANDLE.

/* SETTINGS FOR FRAME FBuscar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FBuscar:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FCer
   FRAME-NAME                                                           */
/* BROWSE-TAB BNit Btn_Sdos FCer */
/* BROWSE-TAB BRet RNit FCer */
ASSIGN 
       BNit:POPUP-MENU IN FRAME FCer             = MENU POPUP-MENU-BNit:HANDLE.

/* SETTINGS FOR RADIO-SET RNit IN FRAME FCer
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WNomNit IN FRAME FCer
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BNit
/* Query rebuild information for BROWSE BNit
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TNit NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BNit */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRet
/* Query rebuild information for BROWSE BRet
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCer NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BRet */
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


&Scoped-define BROWSE-NAME BNit
&Scoped-define SELF-NAME BNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BNit wWin
ON ROW-ENTRY OF BNit IN FRAME FCer
DO:
  FIND FIRST TCer WHERE
             TCer.Nit EQ TNit.Nit AND
             TCer.Bas GT 0 AND
             TCer.Ret GT 0 NO-ERROR.
  IF AVAILABLE TCer THEN
     OPEN QUERY BRet FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit AND 
                TCer.Bas GT 0 AND TCer.Ret GT 0 NO-LOCK INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRet
&Scoped-define SELF-NAME BRet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRet wWin
ON ROW-DISPLAY OF BRet IN FRAME FCer
DO:
  NomCtaBas = "No Existe".
  FIND Cuentas WHERE Cuentas.Cuenta EQ TCer.CBa NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN NomCtaBas = Cuentas.Nombre.
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


&Scoped-define SELF-NAME btnPdf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPdf wWin
ON CHOOSE OF btnPdf IN FRAME FCer /* Cert.RteFte PDF */
DO:
    IF RNit:SCREEN-VALUE = "2" THEN DO:
        MESSAGE "Se van a generar los certificados en PDF" SKIP
                "para toda la agencia. Este proceso tardará" SKIP
                "un tiempo considerable, por lo cual se recomienda" SKIP
                "que abra una nueva sesión para continuar con" SKIP
                "su trabajo mientras se realiza."
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice AS LOGICAL.

        IF choice = NO THEN
            RETURN NO-APPLY.
        ELSE DO:
            EMPTY TEMP-TABLE TCer.

            RUN CertificadoReteFuente.

            FOR EACH TNit:
                FOR EACH TCer WHERE TCer.nit = TNit.nit
                                AND TCer.Bas = 0
                                AND TCer.CBa <> "RETENCIÓN EN LA FUENTE":
                    DELETE TCer.
                END.

                FIND FIRST TCer WHERE TCer.Nit EQ TNit.Nit NO-ERROR.
                IF NOT AVAILABLE TCer THEN DO:
                    DELETE TNit.
                    NEXT.
                END.
            END.

            RUN certificadoDeSaldos.
        END.
    END.
    ELSE
        RUN certificadoDeSaldos.
    
    RUN a_Archivo.

    IF RNit:SCREEN-VALUE = "1" THEN DO:
        RUN Rp_CertificadoReteFuente.r(INPUT WNit,
                                       INPUT WAno,
                                       INPUT 1) NO-ERROR.

        l-file = "Reportes\CertificadosRetencion\" + WNit + "_" + STRING(WAno) + ".pdf".

        ASSIGN FRAME FCer:SENSITIVE = FALSE.

        RUN visorPDF(INPUT l-file) NO-ERROR.

        ASSIGN FRAME FCer:SENSITIVE = TRUE.
    END.
    ELSE DO:
        FOR EACH TNit NO-LOCK:
            RUN Rp_CertificadoReteFuente.r(INPUT TNit.nit,
                                           INPUT WAno,
                                           INPUT 2) NO-ERROR.
        END.
        
        MESSAGE "EL proceso de generación de Certificados en PDF" SKIP
                "finalizó con éxito."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir wWin
ON CHOOSE OF Btn_Imprimir IN FRAME FCer /* Cert. ReteFuente */
DO:
    APPLY "leave" TO wNit.
    
    EMPTY TEMP-TABLE TCer.
    
    RUN CertificadoReteFuente.

    FOR EACH TNit:
        FOR EACH TCer WHERE TCer.nit = TNit.nit
                        AND TCer.Bas = 0
                        AND TCer.CBa <> "RETENCIÓN EN LA FUENTE":
            DELETE TCer.
        END.

        FIND FIRST TCer WHERE TCer.Nit EQ TNit.Nit NO-ERROR.
        IF NOT AVAILABLE TCer THEN
            DELETE TNit.
    END.

    DEFINE VAR Listado AS CHARACTER INITIAL "".

    listado = W_PathSpl + "L_BaseRet.Lst".

    DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
    DEFINE VAR W_sw AS LOGICAL.

    RUN P-DisPos IN W_Manija (INPUT-OUTPUT listado,
                              INPUT-OUTPUT W_Dispositivo).

    IF W_Dispositivo = "" THEN
        RETURN.

    RUN _SetCurs.r ("WAIT").

    OUTPUT TO VALUE(listado) NO-ECHO PAGED PAGE-SIZE 90.
        RUN ProcesoImprimir.
    OUTPUT CLOSE.

    RUN _SetCurs.r ("ARROW").
    IF W_Dispositivo = "P" THEN
        RUN Pantalla IN W_Manija (INPUT listado).
    ELSE
        IF W_Dispositivo = "I" THEN
            RUN adecomm/_osprint.r (INPUT ?,
                                    INPUT Listado,
                                    INPUT 8,
                                    INPUT 1,
                                    INPUT 1,
                                    INPUT 99999,
                                    OUTPUT W_sw).

    IF W_Dispositivo <> "A" THEN
        OS-DELETE VALUE(listado).

    IF W_Dispositivo = "E" THEN
        RUN Imprimir_Excel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Sdos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Sdos wWin
ON CHOOSE OF Btn_Sdos IN FRAME FCer /* Cert. Saldos */
DO:
    DEFINE VAR cont AS INTEGER.
    DEFINE VAR choice AS LOGICAL.

    EMPTY TEMP-TABLE saldos.

    IF WNit:SCREEN-VALUE <> "" THEN DO:
        CREATE saldos.
        saldos.cedula = WNit:SCREEN-VALUE.
        saldos.nombre = WNomNit:SCREEN-VALU.
    END.
    ELSE DO:
        MESSAGE "Se van a imprimir todos los certificados de la agencia." SKIP
                "Está seguro? (Este proceso puede tardar varios minutos)"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice.

        IF choice = NO THEN
            RETURN NO-APPLY.

        FOR EACH ahorros WHERE ahorros.agencia = w_agencia NO-LOCK BREAK BY ahorros.nit:
            IF FIRST-OF(ahorros.nit) THEN DO:
                CREATE saldos.
                saldos.cedula = ahorros.nit.

                FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-ERROR.
                IF AVAILABLE clientes THEN DO:
                    IF clientes.nombre = "" THEN DO:
                        FIND FIRST anexos_clientes WHERE anexos_clientes.nit = clientes.nit NO-LOCK NO-ERROR.
                        IF AVAILABLE anexos_clientes THEN
                            clientes.nombre = anexos_clientes.nombre1 + " " + anexos_clientes.nombre2.
                    END.

                    saldos.nombre = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
                END.
            END.
        END.
    END.

    FOR EACH saldos:
        FOR EACH rep_ahorros WHERE rep_ahorros.nit = saldos.cedula AND rep_ahorros.fecCorte = DATE(12, 31, wAno) NO-LOCK BREAK BY rep_ahorros.tip_ahorro:
            CASE rep_ahorros.tip_ahorro:
                WHEN 4 THEN DO:
                    saldos.totalAportes = saldos.totalAportes + rep_ahorros.Sdo_disponible.

                    IF FIRST-OF(rep_ahorros.tip_ahorro) THEN DO:
                        FOR EACH mov_contable WHERE YEAR(mov_contable.fec_contable) = 2016
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

        FOR EACH rep_creditos WHERE rep_creditos.nit = saldos.cedula AND rep_creditos.fecCorte = date(12, 31, wAno) NO-LOCK:
            saldos.totalCreditos = saldos.totalCreditos + rep_creditos.sdo_capital + rep_creditos.INT_corriente + rep_creditos.INT_MorCobrar.
        END.

        FOR EACH anexos WHERE anexos.nit = saldos.cedula
                          AND SUBSTRING(anexos.cuenta,1,4) = "4185"
                          AND anexos.ano = wano NO-LOCK:
            DO cont = 1 TO 12:
                saldos.totalIntereses = saldos.totalIntereses + anexos.cr[cont] - anexos.db[cont].
            END.
        END.
    END.

    DEFINE VAR Listado AS CHARACTER INITIAL "".
    
    ASSIGN listado = W_PathSpl + "L_CertSdo" + STRING(RANDOM(2000,10000)) + ".lst".
           WSdo = TRUE.

    {Incluido\Imprimir.I "listado"} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FBuscar
&Scoped-define SELF-NAME F_Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Nit wWin
ON LEAVE OF F_Nit IN FRAME FBuscar /* Nit a buscar */
DO:
  FIND TNit WHERE TNit.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE TNit THEN DO: 
     Puntero = ROWID(TNit).

  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Buscar wWin
ON CHOOSE OF MENU-ITEM m_Buscar /* Buscar */
DO:
  VIEW FRAME FBuscar.
  APPLY "entry" TO F_Nit IN FRAME FBuscar.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCer
&Scoped-define SELF-NAME RNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RNit wWin
ON VALUE-CHANGED OF RNit IN FRAME FCer
DO:
    IF SELF:SCREEN-VALUE EQ "2" THEN DO:
        DISABLE WNit WITH FRAME FCer.

        FOR EACH Clientes WHERE clientes.tipo_vinculo <= 5
                            AND clientes.nit <> ?
                            AND LENGTH(clientes.nit) >= 5 NO-LOCK:
            FIND FIRST rep_ahorros WHERE rep_ahorros.nit = clientes.nit
                                     AND rep_ahorros.fecCorte = DATE(12,31,WAno) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE rep_ahorros THEN DO:
                FIND FIRST rep_creditos WHERE rep_creditos.nit = clientes.nit
                                          AND rep_creditos.fecCorte = DATE(12,31,WAno) NO-LOCK NO-ERROR.
                IF NOT AVAILABLE rep_creditos THEN
                    NEXT.
            END.

            CREATE TNit.
            ASSIGN TNit.Nit = Clientes.Nit
                   TNit.Nom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                   TNit.Tel = Clientes.Tel_Residencia
                   TNit.DIR = Clientes.DIR_Residencia.
        END.

        EMPTY TEMP-TABLE TCer.
        
        OPEN QUERY BRet FOR EACH TCer NO-LOCK INDEXED-REPOSITION.
        OPEN QUERY BNit FOR EACH TNit NO-LOCK INDEXED-REPOSITION.
    END.
    ELSE DO:
        ENABLE WNit WITH FRAME FCer.
        WNomNit:SCREEN-VALUE = "".
        APPLY "entry" TO WNit.
        RETURN NO-APPLY.
    END.
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

    EMPTY TEMP-TABLE TNit.
    EMPTY TEMP-TABLE TCer.

    IF SELF:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-ERROR.
        IF AVAILABLE Clientes THEN DO:
            IF clientes.nombre = "" THEN DO:
                FIND FIRST anexos_clientes WHERE anexos_clientes.nit = clientes.nit NO-LOCK NO-ERROR.
                IF AVAILABLE anexos_clientes THEN
                    clientes.nombre = anexos_clientes.nombre1 + " " + anexos_clientes.nombre2.
            END.

            WNomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        END.

        FOR EACH clientes WHERE INDEX(clientes.nit,WNit) = 1 AND LENGTH(clientes.nit) >= 5:
            FIND FIRST TNit WHERE TNit.nit = clientes.nit NO-LOCK NO-ERROR.
            IF NOT AVAILABLE TNit THEN DO:
                IF clientes.nombre = "" THEN DO:
                    FIND FIRST anexos_clientes WHERE anexos_clientes.nit = clientes.nit NO-LOCK NO-ERROR.
                    IF AVAILABLE anexos_clientes THEN
                        clientes.nombre = anexos_clientes.nombre1 + " " + anexos_clientes.nombre2.
                END.

                CREATE TNit.
                ASSIGN TNit.Nit = Clientes.Nit
                       TNit.Nom = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2
                       TNit.Tel = Clientes.Tel_Residencia
                       TNit.DIR = Clientes.DIR_Residencia.
            END.
        END.

        OPEN QUERY BNit FOR EACH TNit NO-LOCK INDEXED-REPOSITION.
        OPEN QUERY BRet FOR EACH TCer WHERE TCer.Nit EQ WNit
                                        AND (TCer.Bas > 0 OR TCer.Ret > 0) NO-LOCK INDEXED-REPOSITION.
    END.

    EMPTY TEMP-TABLE TCer.

    RUN CertificadoReteFuente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BNit
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

FIND FIRST usuarios WHERE usuarios.usuario = w_usuario NO-LOCK NO-ERROR.
IF usuarios.prioridad = 6 THEN
    RNit:SENSITIVE = TRUE.

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

FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND SUBSTRING(anexos.cuenta,1,6) = "511004"
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                      AND TCer.CBa = "ARRENDAMIENTOS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = TNit.Nit
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
                
                FOR EACH bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                          AND bfrMovContable.comprobante = mov_contable.comprobante
                                          AND bfrMovContable.num_documento = mov_contable.num_documento
                                          AND bfrMovContable.nit = mov_contable.nit
                                          AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                          AND SUBSTRING(bfrMovContable.cuenta,1,6) = "244515"
                                          AND bfrMovContable.comentario <> "Cierre Anexos Anual" NO-LOCK:
                    TCer.Ret = TCer.Ret + bfrMovContable.cr - bfrMovContable.db.
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

/*OUTPUT TO VALUE("Reportes\CertificadosRetencion\" + WNit + "_" + STRING(WAno) + ".txt").*/
FOR EACH TNit NO-LOCK:
    OUTPUT TO VALUE("Reportes\CertificadosRetencion\" + TNit.nit + "_" + STRING(WAno) + ".txt").
    DISPLAY SKIP(15)
            "                                  CERTIFICADO DE RETENCION EN LA FUENTE" SKIP
            "                                            AÑO GRAVABLE" STRING(WAno,"9999") SKIP(3)                
            "                En  cumplimiento  de  las  disposiciones  fiscales  vigentes  (artículo 381 del" SKIP
            "                estatuto tributario), el Fondo de Empleados Docentes de la Universidad Nacional" SKIP
            "                de  Colombia  -  FODUN,  con  NIT 800112808-7,  practicó Retención en la Fuente" SKIP
            "                durante el año " TRIM(STRING(WAno,"9999")) FORMAT "X(4)" "a:" CAPS(TNit.Nom) FORMAT "X(40)" ", identificado" SKIP
            "                con documento #" TRIM(TNit.Nit) FORMAT "X(12)" ", valores que fueron consignados en BOGOTÁ:" SKIP(2)
            "       -------------------------------------------------------------------------------" AT 10
            /*         1         2         3         4         5         6         7         8*/
            /*12346578901234657890123465789012346578901234657890123465789012346578901234657890*/
            "       CONCEPTO                                             BASE        VALOR RETENIDO" AT 10
            "       -------------------------------------------------------------------------------" AT 10
        WITH WIDTH 132 FRAME FEnca /*PAGE-TOP*/ USE-TEXT NO-BOX STREAM-IO NO-LABELS.

    
    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "GRAVAMEN A LOS MOVIMIENTOS FINANCIEROS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
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

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
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


    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "RENDIMIENTOS FINANCIEROS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       RENDIMIENTOS FINANCIEROS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret3 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "HONORARIOS Y/O SERVICIOS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       HONORARIOS Y/O SERVICIOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret4 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "SERVICIOS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       SERVICIOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret5 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "COMPRAS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       COMPRA ACTIVOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) /*AND totalBase > 0*/ THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret6 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "DEMAS COSTOS Y DEDUCCIONES"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       OTROS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret7 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "ARRENDAMIENTOS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       ARRENDAMIENTOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret8 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "RETENCIÓN EN LA FUENTE"  NO-LOCK BREAK BY TCer.nit:
        WKCta = "       TOTAL RETENCIÓN EN LA FUENTE".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        IF LAST-OF(TCer.Nit) THEN
            DISPLAY "       -------------------------------------------------------------------------------" AT 10
                    WKCta                                    AT 10  FORM "X(65)"
                    /*totalBase FORM "->>,>>>,>>>,>>>"*/
                    " " totalRet
                WITH WIDTH 132 FRAME F-Base_Ret9 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.


    DISPLAY SKIP(2)
            /*"                                                              Total Retenido: " TotRet*/
            /*"                                                              Sin Retención : " TNit.totalSinRet*/
            SKIP(3)
            "                 El  76.46% de los Rendimientos Financieros percibidos durante el año gravable" SKIP
            "                " STRING(WAno,"9999") ", no constituye Renta ni Ganancia Ocasional." SKIP
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
        WITH FRAME FPieCer /*PAGE-BOTTOM*/ USE-TEXT WIDTH 132 NO-BOX STREAM-IO NO-LABELS.

    PAGE.

    /* Certificado de saldos */

    FIND FIRST saldos WHERE saldos.cedula = TNit.nit NO-LOCK NO-ERROR.
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

    PAGE.

    OUTPUT CLOSE.
END.

/*PAGE.
OUTPUT CLOSE.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE certificadoDeSaldos wWin 
PROCEDURE certificadoDeSaldos :
DEFINE VAR cont AS INTEGER.
DEFINE VAR choice AS LOGICAL.

EMPTY TEMP-TABLE saldos.

FOR EACH TNit NO-LOCK:
    FIND FIRST rep_ahorros WHERE rep_ahorros.fecCorte = DATE(12,31,WAno)
                             AND rep_ahorros.nit = TNit.nit
                             AND rep_ahorros.tip_ahorro = 4 NO-LOCK NO-ERROR.
    IF AVAILABLE rep_ahorros THEN DO:
        CREATE saldos.
        saldos.cedula = TNit.nit.
        saldos.nombre = TNit.nom.

        FOR EACH rep_ahorros WHERE rep_ahorros.nit = saldos.cedula AND rep_ahorros.fecCorte = DATE(12, 31, wAno) NO-LOCK BREAK BY rep_ahorros.tip_ahorro:
            CASE rep_ahorros.tip_ahorro:
                WHEN 4 THEN DO:
                    saldos.totalAportes = saldos.totalAportes + rep_ahorros.Sdo_disponible.

                    IF FIRST-OF(rep_ahorros.tip_ahorro) THEN DO:
                        FOR EACH mov_contable WHERE YEAR(mov_contable.fec_contable) = 2016
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

        FOR EACH rep_creditos WHERE rep_creditos.nit = saldos.cedula AND rep_creditos.fecCorte = date(12, 31, wAno) NO-LOCK:
            saldos.totalCreditos = saldos.totalCreditos + rep_creditos.sdo_capital + rep_creditos.INT_corriente + rep_creditos.INT_MorCobrar.
        END.

        FOR EACH anexos WHERE anexos.nit = saldos.cedula
                          AND (SUBSTRING(anexos.cuenta,1,4) = "4185")
                          AND anexos.ano = wano NO-LOCK:
            DO cont = 1 TO 12:
                saldos.totalIntereses = saldos.totalIntereses + anexos.cr[cont] - anexos.db[cont].
            END.
        END.
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

FOR EACH TNit WHERE LENGTH(Tnit.nit) >= 5 NO-LOCK:
    EMPTY TEMP-TABLE ttmov.

    /* Honorarios */                                    RUN Honorarios.
    /*/* Servicios */                                     RUN Servicios.*/
    /* Arrendamientos */                                RUN Arrendamientos.
    /* Rendimientos Financieros */                      RUN RendimientosFinancieros.
    /* Compra Activos */                                RUN CompraActivos.
    /* Demás Costos y Deducciones */                    RUN DemasCostos.
    /* Gravamen a los movientos financieros - GMF */    RUN GMF.
    /* ICA */                                           RUN ICA.
    /* Retención en la Fuente */                        RUN Retefuente.
END.

OPEN QUERY BNit FOR EACH TNit NO-LOCK INDEXED-REPOSITION.
OPEN QUERY BRet FOR EACH TCer WHERE TCer.Nit EQ WNit
                                AND (TCer.Bas > 0 OR TCer.Ret > 0) NO-LOCK INDEXED-REPOSITION.

/*FOR EACH TNit:
    FOR EACH TCer WHERE TCer.nit = TNit.nit
                    AND TCer.Bas = 0
                    AND TCer.CBa <> "RETENCIÓN EN LA FUENTE":
        DELETE TCer.
    END.

    FIND FIRST TCer WHERE TCer.Nit EQ TNit.Nit NO-ERROR.
    IF NOT AVAILABLE TCer THEN
        DELETE TNit.
END.*/

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CertificadoRetefuenteF1001 wWin 
PROCEDURE CertificadoRetefuenteF1001 :
DEFINE VAR codAhorro AS INTEGER.
DEFINE VAR pPorcentaje AS DECIMAL.
DEFINE VAR cont AS INTEGER.
DEFINE VAR baseCalculada AS DECIMAL.
DEFINE VAR ICA_Tarifa AS DECIMAL.
DEFINE BUFFER bfr_mov_contable FOR mov_contable.

SESSION:SET-WAIT-STATE("General").

EMPTY TEMP-TABLE TCer.

j = 0.

FOR EACH TNit WHERE LENGTH(Tnit.nit) >= 5 NO-LOCK:
    EMPTY TEMP-TABLE ttmov.

    FIND FIRST F1001 WHERE F1001.nit = Tnit.nit
                       AND F1001.concepto = 5002 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE F1001 THEN
        RUN Honorarios.
    ELSE DO:
        FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                          AND TCer.CBa = "HONORARIOS" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = TNit.Nit
                   TCer.CBa = "HONORARIOS"
                   TCer.bas = F1001.base
                   TCer.ret = F1001.retefuente.
        END.

    END.

    FIND FIRST F1001 WHERE F1001.nit = Tnit.nit
                       AND F1001.concepto = 5004 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE F1001 THEN
        RUN Servicios.
    ELSE DO:
        FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                          AND TCer.CBa = "SERVICIOS" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = TNit.Nit
                   TCer.CBa = "SERVICIOS"
                   TCer.bas = F1001.base
                   TCer.ret = F1001.retefuente.
        END.
    END.

    FIND FIRST F1001 WHERE F1001.nit = Tnit.nit
                       AND F1001.concepto = 5005 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE F1001 THEN
        RUN Arrendamientos.
    ELSE DO:
        FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                          AND TCer.CBa = "ARRENDAMIENTOS" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = TNit.Nit
                   TCer.CBa = "ARRENDAMIENTOS"
                   TCer.bas = F1001.base
                   TCer.ret = F1001.retefuente.
        END.
    END.

    FIND FIRST F1001 WHERE F1001.nit = Tnit.nit
                       AND F1001.concepto = 5006 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE F1001 THEN
        RUN RendimientosFinancieros.
    ELSE DO:
        FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                          AND TCer.CBa = "RENDIMIENTOS FINANCIEROS" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = TNit.Nit
                   TCer.CBa = "RENDIMIENTOS FINANCIEROS"
                   TCer.bas = F1001.base
                   TCer.ret = F1001.retefuente.
        END.
    END.

    FIND FIRST F1001 WHERE F1001.nit = Tnit.nit
                   AND F1001.concepto = 5008 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE F1001 THEN
        RUN CompraActivos.
    ELSE DO:
        FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                          AND TCer.CBa = "COMPRAS ACTIVAS" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = TNit.Nit
                   TCer.CBa = "COMPRAS ACTIVAS"
                   TCer.bas = F1001.base
                   TCer.ret = F1001.retefuente.
        END.
    END.

    FIND FIRST F1001 WHERE F1001.nit = Tnit.nit
                       AND F1001.concepto = 5016 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE F1001 THEN
        RUN DemasCostos.
    ELSE DO:
        FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                          AND TCer.CBa = "DEMAS COSTOS Y DEDUCCIONES" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = TNit.Nit
                   TCer.CBa = "DEMAS COSTOS Y DEDUCCIONES"
                   TCer.bas = F1001.base
                   TCer.ret = F1001.retefuente.
        END.
    END.

    RUN GMF.
    RUN ICA.
END.

OPEN QUERY BNit FOR EACH TNit NO-LOCK INDEXED-REPOSITION.
OPEN QUERY BRet FOR EACH TCer WHERE TCer.Nit EQ WNit
                                AND (TCer.Bas > 0 OR TCer.Ret > 0) NO-LOCK INDEXED-REPOSITION.

FOR EACH TNit:
    FOR EACH TCer WHERE TCer.nit = TNit.nit
                    AND TCer.Bas = 0:
        DELETE TCer.
    END.

    FIND FIRST TCer WHERE TCer.Nit EQ TNit.Nit NO-ERROR.
    IF NOT AVAILABLE TCer THEN
        DELETE TNit.
END.

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CompraActivos wWin 
PROCEDURE CompraActivos :
DEFINE VAR cont AS INTEGER.

FOR EACH activosFijos WHERE YEAR(activosFijos.fechaCompra) = wAno
                        AND activosFijos.nitProveedor = TNit.nit NO-LOCK:
    FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                      AND TCer.CBa = "COMPRAS ACTIVOS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = TNit.Nit
               TCer.CBa = "COMPRAS ACTIVOS".
    END.

    TCer.Bas = TCer.Bas + activosFijos.valorCompra.
END.

/*FOR EACH mov_contable WHERE mov_contable.nit = TNit.nit
                        AND YEAR(mov_contable.fec_contable) = wAno
                        AND (SUBSTRING(mov_contable.cuenta,1,6) = "244540" OR
                             SUBSTRING(mov_contable.cuenta,1,6) = "244565")
                        AND mov_contable.comentario <> "Cierre Anexos Anual" NO-LOCK:
    FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                      AND TCer.CBa = "COMPRAS ACTIVOS" NO-ERROR.
    IF AVAILABLE tCer THEN
        TCer.Ret = TCer.Ret + mov_contable.cr - mov_contable.db.
END.*/

FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND (SUBSTRING(anexos.cuenta,1,6) = "244540" OR
                       SUBSTRING(anexos.cuenta,1,6) = "244560")
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    IF FIRST-OF(anexos.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.

    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                          AND TCer.CBa = "COMPRAS ACTIVOS" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = TNit.Nit
                   TCer.CBa = "COMPRAS ACTIVOS".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DemasCostos wWin 
PROCEDURE DemasCostos :
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE docs.

FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND (SUBSTRING(anexos.cuenta,1,2) = "26" OR SUBSTRING(anexos.cuenta,1,6) = "510551" OR SUBSTRING(anexos.cuenta,1,8) = "51100203" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51100204" OR SUBSTRING(anexos.cuenta,1,8) = "51100205" OR SUBSTRING(anexos.cuenta,1,6) = "511008" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511010" OR SUBSTRING(anexos.cuenta,1,6) = "511012" OR SUBSTRING(anexos.cuenta,1,6) = "511016" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511018" OR SUBSTRING(anexos.cuenta,1,6) = "511020" OR SUBSTRING(anexos.cuenta,1,6) = "511022" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511024" OR SUBSTRING(anexos.cuenta,1,6) = "511026" OR SUBSTRING(anexos.cuenta,1,6) = "511028" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511030" OR SUBSTRING(anexos.cuenta,1,6) = "511034" OR SUBSTRING(anexos.cuenta,1,6) = "511036" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511038" OR SUBSTRING(anexos.cuenta,1,6) = "511040" OR SUBSTRING(anexos.cuenta,1,6) = "511042" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511044" OR SUBSTRING(anexos.cuenta,1,6) = "511046" OR SUBSTRING(anexos.cuenta,1,6) = "511048" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511050" OR SUBSTRING(anexos.cuenta,1,8) = "51105201" OR SUBSTRING(anexos.cuenta,1,8) = "51105202" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51105203" OR SUBSTRING(anexos.cuenta,1,8) = "51105204" OR SUBSTRING(anexos.cuenta,1,6) = "511060" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511062" OR SUBSTRING(anexos.cuenta,1,6) = "511064" OR SUBSTRING(anexos.cuenta,1,6) = "511095" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511515" OR SUBSTRING(anexos.cuenta,1,6) = "511524" OR SUBSTRING(anexos.cuenta,1,6) = "511530" OR
                       SUBSTRING(anexos.cuenta,1,6) = "514005" OR SUBSTRING(anexos.cuenta,1,6) = "514015" OR SUBSTRING(anexos.cuenta,1,6) = "514095" OR
                       SUBSTRING(anexos.cuenta,1,6) = "531520" OR SUBSTRING(anexos.cuenta,1,6) = "539520" OR SUBSTRING(anexos.cuenta,1,10) = "6140101108" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101110" OR SUBSTRING(anexos.cuenta,1,10) = "6140101114" OR SUBSTRING(anexos.cuenta,1,10) = "6140101116" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101118" OR SUBSTRING(anexos.cuenta,1,10) = "6140101122"   OR SUBSTRING(anexos.cuenta,1,10) = "6140101136"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101140" OR SUBSTRING(anexos.cuenta,1,10) = "6140101195"   OR SUBSTRING(anexos.cuenta,1,10) = "6140101251"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140102108" OR SUBSTRING(anexos.cuenta,1,10) = "6140102110"   OR SUBSTRING(anexos.cuenta,1,10) = "6140102114"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140102116" OR SUBSTRING(anexos.cuenta,1,10) = "6140102118"   OR SUBSTRING(anexos.cuenta,1,10) = "6140102122"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140102136" OR SUBSTRING(anexos.cuenta,1,10) = "6140102154"   OR SUBSTRING(anexos.cuenta,1,10) = "6140102195"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140102251" OR SUBSTRING(anexos.cuenta,1,10) = "6170401108"   OR SUBSTRING(anexos.cuenta,1,10) = "6170401110"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6170401116" OR SUBSTRING(anexos.cuenta,1,10) = "6170401118"   OR SUBSTRING(anexos.cuenta,1,10) = "6170401122"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6170401136" OR SUBSTRING(anexos.cuenta,1,10) = "6170401195"   OR SUBSTRING(anexos.cuenta,1,10) = "6170401251"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6170401260" OR SUBSTRING(anexos.cuenta,1,8) = "61759501"      OR SUBSTRING(anexos.cuenta,1,8) = "61759505"      OR
                       SUBSTRING(anexos.cuenta,1,4) = "5125")
                  AND SUBSTRING(anexos.cuenta,1,8) <> "51105211"
                  AND SUBSTRING(anexos.cuenta,1,8) <> "51105212"
                  AND SUBSTRING(anexos.cuenta,1,8) <> "51105213"
                  AND SUBSTRING(anexos.cuenta,1,8) <> "51105214"
                  AND SUBSTRING(anexos.cuenta,1,8) <> "53152001"
                  AND SUBSTRING(anexos.cuenta,1,8) <> "53152002"
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                      AND TCer.CBa = "DEMAS COSTOS Y DEDUCCIONES" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = TNit.Nit
               TCer.CBa = "DEMAS COSTOS Y DEDUCCIONES".
    END.

    TCer.Bas = TCer.Bas + anexos.sdo_inicial.

    DO cont = 1 TO 12:
         TCer.Bas = TCer.Bas + anexos.db[cont] - anexos.cr[cont].
    END.
END.

FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND SUBSTRING(anexos.cuenta,1,4) = "2445"
                  AND SUBSTRING(anexos.cuenta,1,6) <> "244515"
                  AND SUBSTRING(anexos.cuenta,1,6) <> "244535"
                  /*AND SUBSTRING(anexos.cuenta,1,6) <> "244540"*/
                  AND SUBSTRING(anexos.cuenta,1,6) <> "244565"
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    IF FIRST-OF(anexos.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.

    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                          AND TCer.CBa = "DEMAS COSTOS Y DEDUCCIONES" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = TNit.Nit
                   TCer.CBa = "DEMAS COSTOS Y DEDUCCIONES".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DemasCostos_bck wWin 
PROCEDURE DemasCostos_bck :
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE docs.

FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND (SUBSTRING(anexos.cuenta,1,2) = "26" OR SUBSTRING(anexos.cuenta,1,6) = "510551" OR SUBSTRING(anexos.cuenta,1,8) = "51100203" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51100204" OR SUBSTRING(anexos.cuenta,1,8) = "51100205" OR SUBSTRING(anexos.cuenta,1,6) = "511008" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511010" OR SUBSTRING(anexos.cuenta,1,6) = "511012" OR SUBSTRING(anexos.cuenta,1,6) = "511016" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511018" OR SUBSTRING(anexos.cuenta,1,6) = "511020" OR SUBSTRING(anexos.cuenta,1,6) = "511022" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511024" OR SUBSTRING(anexos.cuenta,1,6) = "511026" OR SUBSTRING(anexos.cuenta,1,6) = "511028" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511030" OR SUBSTRING(anexos.cuenta,1,6) = "511034" OR SUBSTRING(anexos.cuenta,1,6) = "511036" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511038" OR SUBSTRING(anexos.cuenta,1,6) = "511040" OR SUBSTRING(anexos.cuenta,1,6) = "511042" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511044" OR SUBSTRING(anexos.cuenta,1,6) = "511046" OR SUBSTRING(anexos.cuenta,1,6) = "511048" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511050" OR SUBSTRING(anexos.cuenta,1,8) = "51105201" OR SUBSTRING(anexos.cuenta,1,8) = "51105202" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51105203" OR SUBSTRING(anexos.cuenta,1,8) = "51105204" OR SUBSTRING(anexos.cuenta,1,6) = "511060" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511062" OR SUBSTRING(anexos.cuenta,1,6) = "511064" OR SUBSTRING(anexos.cuenta,1,6) = "511095" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511515" OR SUBSTRING(anexos.cuenta,1,6) = "511524" OR SUBSTRING(anexos.cuenta,1,6) = "511530" OR
                       SUBSTRING(anexos.cuenta,1,6) = "514005" OR SUBSTRING(anexos.cuenta,1,6) = "514015" OR SUBSTRING(anexos.cuenta,1,6) = "514095" OR
                       SUBSTRING(anexos.cuenta,1,6) = "531520" OR SUBSTRING(anexos.cuenta,1,6) = "539520" OR SUBSTRING(anexos.cuenta,1,10) = "6140101108" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101110" OR SUBSTRING(anexos.cuenta,1,10) = "6140101114" OR SUBSTRING(anexos.cuenta,1,10) = "6140101116" OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101118" OR SUBSTRING(anexos.cuenta,1,10) = "6140101122"   OR SUBSTRING(anexos.cuenta,1,10) = "6140101136"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140101140" OR SUBSTRING(anexos.cuenta,1,10) = "6140101195"   OR SUBSTRING(anexos.cuenta,1,10) = "6140101251"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140102108" OR SUBSTRING(anexos.cuenta,1,10) = "6140102110"   OR SUBSTRING(anexos.cuenta,1,10) = "6140102114"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140102116" OR SUBSTRING(anexos.cuenta,1,10) = "6140102118"   OR SUBSTRING(anexos.cuenta,1,10) = "6140102122"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140102136" OR SUBSTRING(anexos.cuenta,1,10) = "6140102154"   OR SUBSTRING(anexos.cuenta,1,10) = "6140102195"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6140102251" OR SUBSTRING(anexos.cuenta,1,10) = "6170401108"   OR SUBSTRING(anexos.cuenta,1,10) = "6170401110"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6170401116" OR SUBSTRING(anexos.cuenta,1,10) = "6170401118"   OR SUBSTRING(anexos.cuenta,1,10) = "6170401122"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6170401136" OR SUBSTRING(anexos.cuenta,1,10) = "6170401195"   OR SUBSTRING(anexos.cuenta,1,10) = "6170401251"   OR
                       SUBSTRING(anexos.cuenta,1,10) = "6170401260" OR SUBSTRING(anexos.cuenta,1,8) = "61759501"      OR SUBSTRING(anexos.cuenta,1,8) = "61759505"      OR
                       SUBSTRING(anexos.cuenta,1,4) = "5125")
                  AND SUBSTRING(anexos.cuenta,1,8) <> "51105211"
                  AND SUBSTRING(anexos.cuenta,1,8) <> "51105212"
                  AND SUBSTRING(anexos.cuenta,1,8) <> "51105213"
                  AND SUBSTRING(anexos.cuenta,1,8) <> "51105214"
                  AND SUBSTRING(anexos.cuenta,1,8) <> "53152002"
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                      AND TCer.CBa = "DEMAS COSTOS Y DEDUCCIONES" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = TNit.Nit
               TCer.CBa = "DEMAS COSTOS Y DEDUCCIONES".
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
                
                FOR EACH bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                          AND bfrMovContable.comprobante = mov_contable.comprobante
                                          AND bfrMovContable.num_documento = mov_contable.num_documento
                                          AND bfrMovContable.nit = mov_contable.nit
                                          AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                          AND SUBSTRING(bfrMovContable.cuenta,1,4) = "2445"
                                          AND SUBSTRING(bfrMovContable.cuenta,1,6) <> "244515"
                                          AND bfrMovContable.comentario <> "Cierre Anexos Anual" NO-LOCK:
                    TCer.Ret = TCer.Ret + bfrMovContable.cr - bfrMovContable.db.
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
  DISPLAY WAno WNit WNomNit RNit 
      WITH FRAME FCer IN WINDOW wWin.
  ENABLE RECT-1 RECT-318 RECT-319 WAno Btn_Imprimir WNit Btn_Sdos BNit btnPdf 
         BRet BtnDone 
      WITH FRAME FCer IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FCer}
  DISPLAY F_Nit 
      WITH FRAME FBuscar IN WINDOW wWin.
  ENABLE F_Nit 
      WITH FRAME FBuscar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FBuscar}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GMF wWin 
PROCEDURE GMF :
DEFINE VAR cont AS INTEGER.

FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND SUBSTRING(anexos.cuenta,1,4) = "2442"
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                          AND TCer.CBa = "GRAVAMEN A LOS MOVIMIENTOS FINANCIEROS" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = TNit.Nit
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

FOR EACH anexos WHERE anexos.nit = TNit.nit
                    AND (SUBSTRING(anexos.cuenta,1,6) = "511001" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51105211" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51105212" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51105213" OR
                         SUBSTRING(anexos.cuenta,1,8) = "51105214" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511058" OR
                         SUBSTRING(anexos.cuenta,1,8) = "61759512" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511054" OR
                         SUBSTRING(anexos.cuenta,1,6) = "511056" OR
                         SUBSTRING(anexos.cuenta,1,8) = "6170401154" OR
                         SUBSTRING(anexos.cuenta,1,8) = "6170401155" OR
                         SUBSTRING(anexos.cuenta,1,8) = "6140101154" OR
                         SUBSTRING(anexos.cuenta,1,8) = "6140101155")
                    AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                        BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                      AND TCer.CBa = "HONORARIOS Y/O SERVICIOS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = TNit.Nit
               TCer.CBa = "HONORARIOS Y/O SERVICIOS".
    END.

    TCer.Bas = TCer.Bas + anexos.sdo_inicial.

    DO cont = 1 TO 12:
         TCer.Bas = TCer.Bas + anexos.db[cont] - anexos.cr[cont].
    END.
END.

FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND (SUBSTRING(anexos.cuenta,1,6) = "244515" OR
                       SUBSTRING(anexos.cuenta,1,6) = "244525")
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    IF FIRST-OF(anexos.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.

    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                          AND TCer.CBa = "HONORARIOS Y/O SERVICIOS" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = TNit.Nit
                   TCer.CBa = "HONORARIOS Y/O SERVICIOS".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Honorarios_bck wWin 
PROCEDURE Honorarios_bck :
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR cont AS INTEGER.

EMPTY TEMP-TABLE docs.

FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND (SUBSTRING(anexos.cuenta,1,6) = "511001" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51105211" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51105212" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51105213" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51105214" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511058" OR
                       SUBSTRING(anexos.cuenta,1,8) = "61759512")
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                      AND TCer.CBa = "HONORARIOS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = TNit.Nit
               TCer.CBa = "HONORARIOS".
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
                
                FOR EACH bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                          AND bfrMovContable.comprobante = mov_contable.comprobante
                                          AND bfrMovContable.num_documento = mov_contable.num_documento
                                          AND bfrMovContable.nit = mov_contable.nit
                                          AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                          AND SUBSTRING(bfrMovContable.cuenta,1,6) = "244515"
                                          AND bfrMovContable.comentario <> "Cierre Anexos Anual" NO-LOCK:
                    TCer.Ret = TCer.Ret + bfrMovContable.cr - bfrMovContable.db.
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

FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND (SUBSTRING(anexos.cuenta,1,6) = "511054" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511056" OR
                       SUBSTRING(anexos.cuenta,1,8) = "6170401154" OR
                       SUBSTRING(anexos.cuenta,1,8) = "6170401155" OR
                       SUBSTRING(anexos.cuenta,1,8) = "6140101154" OR
                       SUBSTRING(anexos.cuenta,1,8) = "6140101155" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511001" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51105211" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51105212" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51105213" OR
                       SUBSTRING(anexos.cuenta,1,8) = "51105214" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511058" OR
                       SUBSTRING(anexos.cuenta,1,8) = "61759512")
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                      AND TCer.CBa = "ICA" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = TNit.Nit
               TCer.CBa = "ICA".
    END.

    TCer.Bas = TCer.Bas + anexos.sdo_inicial.

    DO cont = 1 TO 12:
        TCer.Bas = TCer.Bas + anexos.db[cont] - anexos.cr[cont].
    END.
END.
        
FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND SUBSTRING(anexos.cuenta,1,4) = "2448"
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    IF FIRST-OF(anexos.cuenta) THEN
        FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.

    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                          AND TCer.CBa = "ICA" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = TNit.Nit
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_CU wWin 
PROCEDURE Imp_CU :
{INCLUIDO\RepEncabezado.I}

DEFINE VAR TotalRet AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR WKDoc AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKCer AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKHem AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKPie AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKCta AS CHARACTER FORMAT "X(50)".
DEFINE VAR totalBase AS DECIMAL.
DEFINE VAR pLineas AS CHARACTER EXTENT 50.

ASSIGN FRAME FCer Wano.

FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.

FOR EACH TNit NO-LOCK:
    DISPLAY SKIP(15)
            "                                  CERTIFICADO DE RETENCION EN LA FUENTE" SKIP
            "                                            AÑO GRAVABLE" STRING(WAno,"9999") SKIP(3)                
            "                En  cumplimiento  de  las  disposiciones  fiscales  vigentes  (artículo 381 del" SKIP
            "                estatuto tributario), el Fondo de Empleados Docentes de la Universidad Nacional" SKIP
            "                de  Colombia  -  FODUN,  con  NIT 800112808-7,  practicó Retención en la Fuente" SKIP
            "                durante el año " TRIM(STRING(WAno,"9999")) FORMAT "X(4)" "a:" CAPS(TNit.Nom) FORMAT "X(40)" ", identificado" SKIP
            "                con documento #" TRIM(TNit.Nit) FORMAT "X(12)" ", valores que fueron consignados en BOGOTÁ:" SKIP(2)
            "       -------------------------------------------------------------------------------" AT 10
            /*         1         2         3         4         5         6         7         8*/
            /*12346578901234657890123465789012346578901234657890123465789012346578901234657890*/
            "       CONCEPTO                                             BASE        VALOR RETENIDO" AT 10
            "       -------------------------------------------------------------------------------" AT 10
        WITH WIDTH 132 FRAME FEnca /*PAGE-TOP*/ USE-TEXT NO-BOX STREAM-IO NO-LABELS.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
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

    DISPLAY skip(2)
        WITH WIDTH 132 FRAME F-inicia_varios USE-TEXT STREAM-IO NO-LABELS NO-BOX.


    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "RENDIMIENTOS FINANCIEROS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       RENDIMIENTOS FINANCIEROS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret3 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.
    
    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "HONORARIOS Y/O SERVICIOS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       HONORARIOS Y/O SERVICIOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret4 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.
    
    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "SERVICIOS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       SERVICIOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret5 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "COMPRAS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       COMPRA ACTIVOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) /*AND totalBase > 0*/ THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret6 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "DEMAS COSTOS Y DEDUCCIONES"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       OTROS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret7 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "ARRENDAMIENTOS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       ARRENDAMIENTOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        totalRet = 0.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    /*" " totalRet*/
                WITH WIDTH 132 FRAME F-Base_Ret8 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "RETENCIÓN EN LA FUENTE"  NO-LOCK BREAK BY TCer.nit:
        WKCta = "       TOTAL RETENCIÓN EN LA FUENTE - RENTA".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        IF LAST-OF(TCer.Nit) THEN
            DISPLAY "       -------------------------------------------------------------------------------" AT 10
                    WKCta                                    AT 10  FORM "X(65)"
                    /*ttotalBase FORM "->>,>>>,>>>,>>>"*/
                    " " totalRet
                WITH WIDTH 132 FRAME F-Base_Ret9 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND TCer.Cba = "GRAVAMEN A LOS MOVIMIENTOS FINANCIEROS"
                    /*AND TCer.Ret > 0*/ NO-LOCK BREAK BY TCer.nit:
        WKCta = "       GRAVAMEN A LOS MOVIMIENTOS FINANCIEROS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    " " totalRet
                WITH WIDTH 132 FRAME F-Base_Ret1 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.


    DISPLAY SKIP(2)
            /*"                                                              Total Retenido: " TotRet*/
            /*"                                                              Sin Retención : " TNit.totalSinRet*/
            SKIP(3)
            "                 El  76.46% de los Rendimientos Financieros percibidos durante el año gravable" SKIP
            "                " STRING(WAno,"9999") ", no constituye Renta ni Ganancia Ocasional." SKIP
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
        WITH FRAME FPieCer /*PAGE-BOTTOM*/ USE-TEXT WIDTH 132 NO-BOX STREAM-IO NO-LABELS.

    PAGE.
END.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_CU_bck wWin 
PROCEDURE Imp_CU_bck :
{INCLUIDO\RepEncabezado.I}

DEFINE VAR TotalRet AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR WKEnt AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKDoc AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKCer AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKHem AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKPie AS CHARACTER FORMAT "X(90)".
DEFINE VAR WKCta AS CHARACTER FORMAT "X(50)".
DEFINE VAR totalBase AS DECIMAL.

ASSIGN FRAME FCer Wano.

FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.

FOR EACH TNit NO-LOCK:
    DISPLAY SKIP(10)
            "                                  CERTIFICADO DE RETENCION EN LA FUENTE" SKIP
            "                                            AÑO GRAVABLE" STRING(WAno,"9999") SKIP(3)                
            "                En  cumplimiento  de  las  disposiciones  fiscales  vigentes  (artículo 381 del" SKIP
            "                estatuto tributario), el Fondo de Empleados Docentes de la Universidad Nacional" SKIP
            "                de  Colombia  -  FODUN,  con  NIT 800112808-7,  practicó Retención en la Fuente" SKIP
            "                durante el año " TRIM(STRING(WAno,"9999")) FORMAT "X(4)" "a:" CAPS(TNit.Nom) FORMAT "X(40)" ", identificado" SKIP
            "                con documento #" TRIM(TNit.Nit) FORMAT "X(12)" ", valores que fueron consignados en BOGOTÁ:" SKIP(2)
            "       -------------------------------------------------------------------------------" AT 10
            /*         1         2         3         4         5         6         7         8*/
            /*12346578901234657890123465789012346578901234657890123465789012346578901234657890*/
            "       CONCEPTO                                             BASE        VALOR RETENIDO" AT 10
            "       -------------------------------------------------------------------------------" AT 10
        WITH WIDTH 132 FRAME FEnca /*PAGE-TOP*/ USE-TEXT NO-BOX STREAM-IO NO-LABELS.

    
    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND SUBSTRING(TCer.Cta,1,4) = "2442" NO-LOCK BREAK BY TCer.nit:
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

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND SUBSTRING(TCer.Cta,1,4) = "2448" NO-LOCK BREAK BY TCer.nit:
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

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND SUBSTRING(TCer.Cta,1,6) = "244535" NO-LOCK BREAK BY TCer.nit:
        WKCta = "       RENDIMIENTOS FINANCIEROS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    " " totalRet
                WITH WIDTH 132 FRAME F-Base_Ret3 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND SUBSTRING(TCer.Cta,1,6) = "244515" NO-LOCK BREAK BY TCer.nit:
        WKCta = "       HONORARIOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    " " totalRet
                WITH WIDTH 132 FRAME F-Base_Ret4 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND SUBSTRING(TCer.Cta,1,6) = "244525" NO-LOCK BREAK BY TCer.nit:
        WKCta = "       SERVICIOS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    " " totalRet
                WITH WIDTH 132 FRAME F-Base_Ret5 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND SUBSTRING(TCer.Cta,1,6) = "244540" NO-LOCK BREAK BY TCer.nit:
        WKCta = "       COMPRAS".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    " " totalRet
                WITH WIDTH 132 FRAME F-Base_Ret6 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    totalRet = 0.
    totalBase = 0.

    FOR EACH TCer WHERE TCer.Nit EQ TNit.Nit
                    AND SUBSTRING(TCer.Cta,1,6) = "244570" NO-LOCK BREAK BY TCer.nit:
        WKCta = "       CREE".

        TotalRet = TotalRet + TCer.Ret.
        totalBase = totalBase + TCer.Bas.

        IF LAST-OF(TCer.Nit) AND totalBase > 0 THEN
            DISPLAY WKCta                                    AT 10  FORM "X(50)"
                    totalBase FORM "->>,>>>,>>>,>>9"
                    " " totalRet
                WITH WIDTH 132 FRAME F-Base_Ret7 USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.

    DISPLAY SKIP(2)
            /*"                                                              Total Retenido: " TotRet*/
            /*"                                                              Sin Retención : " TNit.totalSinRet*/
            SKIP(3)
            /*"                 El  80.91%  de los Rendimientos Financieros percibidos durante el año gravable" SKIP
            "                " STRING(WAno,"9999") ", no constituye Renta ni Ganancia Ocasional (Decreto 0563 de Marzo 16" SKIP
            "                 de 2011)."*/
            /*"                 De conformidad con el Decreto 0652 de abril 05 de 2013, no constituye Renta ni"*/ SKIP
            /*"                 Ganancia  Ocasional  por  el  año gravable" STRING(WAno,"9999") + STRING(",","X(1)") "el 41.71% del valor de los"*/ skip
            /*"                 Rendimientos  Financieros  percibidos   por  personas  naturales  y sucesiones"*/ SKIP
            /*"                 ilíquidas no obligadas a llevar libros de contabilidad."*/
            SKIP(3)
            "                 Este Certificado no requiere firma autógrafa según Artículo 10 del Decreto 836" SKIP
            "                 de 1991."
        WITH FRAME FPieCer /*PAGE-BOTTOM*/ USE-TEXT WIDTH 132 NO-BOX STREAM-IO NO-LABELS.

    PAGE.
END.

PAGE.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PorcentajesReteFuente wWin 
PROCEDURE PorcentajesReteFuente :
DEFINE INPUT PARAMETER pCuenta AS CHARACTER.
DEFINE OUTPUT PARAMETER pPorcentaje AS DECIMAL.

CASE pCuenta:
    WHEN "24450501" THEN pPorcentaje = 0.
    WHEN "24451501" THEN pPorcentaje = 10 / 100.
    WHEN "24451502" THEN pPorcentaje = 11 / 100.
    WHEN "24451503" THEN pPorcentaje = 0.
    WHEN "24452501" THEN pPorcentaje = 1 / 100.
    WHEN "24452502" THEN pPorcentaje = 3 / 100.
    WHEN "24452503" THEN pPorcentaje = 3.5 / 100.
    WHEN "24452504" THEN pPorcentaje = 6 / 100.
    WHEN "24452505" THEN pPorcentaje = 2 / 100.
    WHEN "24452506" THEN pPorcentaje = 4 / 100.
    WHEN "24452507" THEN pPorcentaje = 0.
    WHEN "24453001" THEN pPorcentaje = 3.5 / 100.
    WHEN "24453002" THEN pPorcentaje = 4 / 100.
    WHEN "24453501" THEN pPorcentaje = 7 / 100.
    WHEN "24453502" THEN pPorcentaje = 7 / 100.
    WHEN "24453503" THEN pPorcentaje = 7 / 100.
    WHEN "24453504" THEN pPorcentaje = 7 / 100.
    WHEN "24453507" THEN pPorcentaje = 7 / 100.
    WHEN "24453508" THEN pPorcentaje = 7 / 100.
    WHEN "24453509" THEN pPorcentaje = 7 / 100.
    WHEN "24453510" THEN pPorcentaje = 7 / 100.
    WHEN "24454001" THEN pPorcentaje = 3.5 / 100.
    WHEN "24456501" THEN pPorcentaje = 0.
    WHEN "24457001" THEN pPorcentaje = 3.5 / 100.
    WHEN "24470501" THEN pPorcentaje = 8 / 100.
    WHEN "24470502" THEN pPorcentaje = 5 / 100.
    WHEN "24480501" THEN pPorcentaje = 13.8 / 100.
    WHEN "24480503" THEN pPorcentaje = 0.966 / 100.
    WHEN "24480504" THEN pPorcentaje = 11.4 / 100.
    WHEN "24480505" THEN pPorcentaje = 6.9 / 100.
    WHEN "24480506" THEN pPorcentaje = 4.14 / 100.
END CASE.

IF SUBSTRING(pCuenta,1,4) = "2442" THEN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso_Anexos wWin 
PROCEDURE Proceso_Anexos :
DEFINE VAR Neto AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
DEFINE VAR TotDeb LIKE Ahorros.Sdo_Disponible.
DEFINE VAR i AS INTEGER.

FOR EACH anexos WHERE anexos.Cuenta EQ Cuentas.Cuenta
                  AND anexos.Ano EQ WAno
                  AND anexos.Nit EQ TNit.Nit NO-LOCK:
    DO i = 1 TO 12 BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN
            TCer.Ret = TCer.Ret + anexos.DB[i] - anexos.CR[i].
        ELSE
            TCer.Ret = TCer.Ret - anexos.DB[i] + anexos.CR[i].
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RendimientosFinancieros wWin 
PROCEDURE RendimientosFinancieros :
DEFINE BUFFER bfrMovContable FOR mov_contable.
DEFINE VAR cont AS INTEGER.
    
EMPTY TEMP-TABLE docs.
    
FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND (SUBSTRING(anexos.cuenta,1,6) = "514020" OR
                       SUBSTRING(anexos.cuenta,1,6) = "617505" OR
                       SUBSTRING(anexos.cuenta,1,6) = "617510" OR
                       SUBSTRING(anexos.cuenta,1,6) = "617515" OR
                       SUBSTRING(anexos.cuenta,1,6) = "617520" OR
                       SUBSTRING(anexos.cuenta,1,6) = "617540" OR
                       SUBSTRING(anexos.cuenta,1,6) = "617550" OR
                       SUBSTRING(anexos.cuenta,1,8) = "61759502" OR
                       SUBSTRING(anexos.cuenta,1,8) = "61759511")
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                      AND TCer.CBa = "RENDIMIENTOS FINANCIEROS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = TNit.Nit
               TCer.CBa = "RENDIMIENTOS FINANCIEROS".
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
                
                FOR EACH bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                          AND bfrMovContable.comprobante = mov_contable.comprobante
                                          AND bfrMovContable.num_documento = mov_contable.num_documento
                                          AND bfrMovContable.nit = mov_contable.nit
                                          AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                          AND SUBSTRING(bfrMovContable.cuenta,1,6) = "244535"
                                          AND bfrMovContable.comentario <> "Cierre Anexos Anual" NO-LOCK:
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

FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND SUBSTRING(anexos.cuenta,1,4) = "2445"
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit:
    IF FIRST-OF(anexos.nit) THEN DO:
        FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                          AND TCer.CBa = "RETENCIÓN EN LA FUENTE" NO-ERROR.
        IF NOT AVAILABLE TCer THEN DO:
            CREATE TCer.
            ASSIGN TCer.Nit = TNit.Nit
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Servicios wWin 
PROCEDURE Servicios :
DEFINE VAR cont AS INTEGER.
DEFINE BUFFER bfrMovContable FOR mov_contable.
    
EMPTY TEMP-TABLE docs.

FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND (SUBSTRING(anexos.cuenta,1,6) = "511054" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511056" OR
                       SUBSTRING(anexos.cuenta,1,8) = "6170401154" OR
                       SUBSTRING(anexos.cuenta,1,8) = "6170401155" OR
                       SUBSTRING(anexos.cuenta,1,8) = "6140101154" OR
                       SUBSTRING(anexos.cuenta,1,8) = "6140101155")
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                      AND TCer.CBa = "SERVICIOS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = TNit.Nit
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
                
                FOR EACH bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                          AND bfrMovContable.comprobante = mov_contable.comprobante
                                          AND bfrMovContable.num_documento = mov_contable.num_documento
                                          AND bfrMovContable.nit = mov_contable.nit
                                          AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                          AND SUBSTRING(bfrMovContable.cuenta,1,6) = "244525"
                                          AND bfrMovContable.comentario <> "Cierre Anexos Anual" NO-LOCK:
                    TCer.Ret = TCer.Ret + bfrMovContable.cr - bfrMovContable.db.
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Servicios_bck wWin 
PROCEDURE Servicios_bck :
DEFINE VAR cont AS INTEGER.
DEFINE BUFFER bfrMovContable FOR mov_contable.
    
EMPTY TEMP-TABLE docs.

FOR EACH anexos WHERE anexos.nit = TNit.nit
                  AND (SUBSTRING(anexos.cuenta,1,6) = "511054" OR
                       SUBSTRING(anexos.cuenta,1,6) = "511056" OR
                       SUBSTRING(anexos.cuenta,1,8) = "6170401154" OR
                       SUBSTRING(anexos.cuenta,1,8) = "6170401155" OR
                       SUBSTRING(anexos.cuenta,1,8) = "6140101154" OR
                       SUBSTRING(anexos.cuenta,1,8) = "6140101155")
                  AND anexos.ano = wAno NO-LOCK BREAK BY anexos.nit
                                                      BY anexos.cuenta:
    FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
    
    FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1
                      AND TCer.CBa = "SERVICIOS" NO-ERROR.
    IF NOT AVAILABLE TCer THEN DO:
        CREATE TCer.
        ASSIGN TCer.Nit = TNit.Nit
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
                
                FOR EACH bfrMovContable WHERE bfrMovContable.agencia = mov_contable.agencia
                                          AND bfrMovContable.comprobante = mov_contable.comprobante
                                          AND bfrMovContable.num_documento = mov_contable.num_documento
                                          AND bfrMovContable.nit = mov_contable.nit
                                          AND bfrMovContable.fec_contable = mov_contable.fec_contable
                                          AND SUBSTRING(bfrMovContable.cuenta,1,6) = "244525"
                                          AND bfrMovContable.comentario <> "Cierre Anexos Anual" NO-LOCK:
                    TCer.Ret = TCer.Ret + bfrMovContable.cr - bfrMovContable.db.
                END.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TriggersEjecutar_bck wWin 
PROCEDURE TriggersEjecutar_bck :
DO:
    DEFINE VAR codAhorro AS INTEGER.
    DEFINE VAR pPorcentaje AS DECIMAL.

    SESSION:SET-WAIT-STATE("General").

    EMPTY TEMP-TABLE TCer.
    
    FOR EACH TNit NO-LOCK:
        FOR EACH Cuentas WHERE /*Cuentas.Id_Base EQ YES
                           AND Cuentas.Cod_Base NE ""
                           AND */ SUBSTRING(Cuentas.Cuenta,1,4) = "2442"
                            OR SUBSTRING(Cuentas.Cuenta,1,4) = "2445"
                            OR SUBSTRING(Cuentas.Cuenta,1,4) = "2448" NO-LOCK:
            j = j + 1.

            FIND FIRST TCer WHERE INDEX(TNit.nit,TCer.nit) = 1 AND TCer.Cta = cuentas.cuenta  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE TCer THEN DO:
                CREATE TCer.
                ASSIGN TCer.Nit = TNit.Nit
                       TCer.Cta = Cuentas.Cuenta
                       TCer.Nom = Cuentas.Nombre
                       TCer.CBa = cuentas.cuenta.
            END.

            RUN Proceso_Anexos.

            /* Bases */
            RUN PorcentajesReteFuente(INPUT TCer.Cta,
                                      OUTPUT pPorcentaje).

            IF pPorcentaje > 0 THEN DO:
                
            END.
            ELSE DO:
                IF SUBSTRING(cuentas.cuenta,1,6) <> "244535" AND SUBSTRING(cuentas.cuenta,1,4) <> "2442" THEN DO:
                    EMPTY TEMP-TABLE docs.

                    FOR EACH mov_contable WHERE mov_contable.cuenta = cuentas.cuenta
                                            AND YEAR(mov_contable.fec_contable) = Wano
                                            AND mov_contable.nit = TNit.nit NO-LOCK BREAK BY mov_contable.comprobante
                                                                                      BY mov_contable.num_documento:
                        IF FIRST-OF(mov_contable.num_documento) THEN DO:
                            FIND FIRST docs WHERE docs.agencia = mov_contable.agencia
                                              AND docs.comprobante = mov_contable.comprobante
                                              AND docs.num_documento = mov_contable.num_documento NO-LOCK NO-ERROR.
                            IF NOT AVAILABLE docs THEN DO:
                                CREATE docs.
                                ASSIGN docs.agencia = mov_contable.agencia
                                       docs.comprobante = mov_contable.comprobante
                                       docs.num_documento = mov_contable.num_documento.
                            END.
                        END.
                    END.

                    FOR EACH docs NO-LOCK:
                        FOR EACH mov_contable WHERE (SUBSTRING(mov_contable.cuenta,1,1) = "5" OR
                                                     SUBSTRING(mov_contable.cuenta,1,1) = "6" OR
                                                     SUBSTRING(mov_contable.cuenta,1,2) = "17" OR
                                                     SUBSTRING(mov_contable.cuenta,1,2) = "13")
                                                AND mov_contable.agencia = docs.agencia
                                                AND YEAR(mov_contable.fec_contable) = wAno
                                                AND mov_contable.nit = Tnit.nit
                                                AND mov_contable.comprobante = docs.comprobante
                                                AND mov_contable.num_documento = docs.num_documento NO-LOCK:
                            TCer.Bas = TCer.Bas + mov_contable.db - mov_contable.cr.
                        END.
                    END.
                END.
                ELSE DO:
                    IF SUBSTRING(cuentas.cuenta,1,6) = "244535" THEN DO:
                        CASE cuentas.cuenta:
                            WHEN "24453501" THEN DO:
                                FOR EACH mov_ahorros WHERE mov_ahorros.nit = TNit.nit
                                                       AND YEAR(mov_ahorros.fecha) = WAno
                                                       AND (mov_ahorros.cod_ahorro = 3 AND mov_ahorros.fecha > 08/31/2011)
                                                       AND mov_ahorros.descrip = "Abono Liq.Interés" NO-LOCK:
                                    TCer.Bas = TCer.Bas + mov_ahorros.val_efectivo.
                                END.
                            END.

                            WHEN "24453509" THEN DO:
                                FOR EACH mov_ahorros WHERE mov_ahorros.nit = TNit.nit
                                                       AND YEAR(mov_ahorros.fecha) = WAno
                                                       AND (mov_ahorros.cod_ahorro = 7 OR
                                                            mov_ahorros.cod_ahorro = 9 OR
                                                            (mov_ahorros.cod_ahorro = 3 AND mov_ahorros.fecha <= 08/31/2011))
                                                       AND mov_ahorros.descrip = "Abono Liq.Interés" NO-LOCK:
                                    TCer.Bas = TCer.Bas + mov_ahorros.val_efectivo.
                                END.
                            END.

                            WHEN "24453503" THEN DO:
                                FOR EACH mov_ahorros WHERE mov_ahorros.nit = TNit.nit
                                                       AND YEAR(mov_ahorros.fecha) = WAno
                                                       AND (mov_ahorros.cod_ahorro = 4 OR
                                                            mov_ahorros.cod_ahorro = 8)
                                                       AND mov_ahorros.descrip = "Abono Liq.Interés" NO-LOCK:
                                    TCer.Bas = TCer.Bas + mov_ahorros.val_efectivo.
                                END.
                            END.

                            WHEN "24453502" THEN DO:
                                FOR EACH mov_ahorros WHERE mov_ahorros.nit = TNit.nit
                                                       AND YEAR(mov_ahorros.fecha) = WAno
                                                       AND mov_ahorros.cod_ahorro = 5
                                                       AND mov_ahorros.descrip = "Abono Liq.Interés" NO-LOCK:
                                    TCer.Bas = TCer.Bas + mov_ahorros.val_efectivo.
                                END.
                            END.

                            WHEN "24453510" THEN DO:
                                FOR EACH mov_ahorros WHERE mov_ahorros.nit = TNit.nit
                                                       AND YEAR(mov_ahorros.fecha) = WAno
                                                       AND mov_ahorros.cod_ahorro = 6
                                                       AND mov_ahorros.descrip = "Abono Liq.Interés" NO-LOCK:
                                    TCer.Bas = TCer.Bas + mov_ahorros.val_efectivo.
                                END.
                            END.
                        END CASE.
                    END.
                    ELSE DO:
                        CASE cuentas.cuenta:
                            WHEN "24422001" THEN DO:
                                FOR EACH mov_ahorros WHERE mov_ahorros.nit = TNit.nit
                                                       AND YEAR(mov_ahorros.fecha) = WAno
                                                       AND (mov_ahorros.cod_ahorro = 2 OR mov_ahorros.cod_ahorro = 3 OR mov_ahorros.cod_ahorro = 8) NO-LOCK:
                                    FIND FIRST operacion WHERE operacion.cod_operacion = mov_ahorros.cod_operacion NO-LOCK NO-ERROR.
                                    IF AVAILABLE operacion THEN DO:
                                        IF operacion.tipo_operacion = 2 THEN
                                            TCer.Bas = TCer.Bas + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                                    END.
                                END.
                            END.

                            WHEN "24420501" THEN DO:
                                FOR EACH mov_ahorros WHERE mov_ahorros.nit = TNit.nit
                                                       AND YEAR(mov_ahorros.fecha) = WAno
                                                       AND mov_ahorros.cod_ahorro = 4 NO-LOCK:
                                    FIND FIRST operacion WHERE operacion.cod_operacion = mov_ahorros.cod_operacion NO-LOCK NO-ERROR.
                                    IF AVAILABLE operacion THEN DO:
                                        IF operacion.tipo_operacion = 2 THEN
                                            TCer.Bas = TCer.Bas + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                                    END.
                                END.
                            END.

                            WHEN "24421001" THEN DO:
                                FOR EACH mov_ahorros WHERE mov_ahorros.nit = TNit.nit
                                                       AND YEAR(mov_ahorros.fecha) = WAno
                                                       AND mov_ahorros.cod_ahorro = 5 NO-LOCK:
                                    FIND FIRST operacion WHERE operacion.cod_operacion = mov_ahorros.cod_operacion NO-LOCK NO-ERROR.
                                    IF AVAILABLE operacion THEN DO:
                                        IF operacion.tipo_operacion = 2 THEN
                                            TCer.Bas = TCer.Bas + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                                    END.
                                END.
                            END.

                            WHEN "24421002" THEN DO:
                                FOR EACH mov_ahorros WHERE mov_ahorros.nit = TNit.nit
                                                       AND YEAR(mov_ahorros.fecha) = WAno
                                                       AND mov_ahorros.cod_ahorro = 6 NO-LOCK:
                                    FIND FIRST operacion WHERE operacion.cod_operacion = mov_ahorros.cod_operacion NO-LOCK NO-ERROR.
                                    IF AVAILABLE operacion THEN DO:
                                        IF operacion.tipo_operacion = 2 THEN
                                            TCer.Bas = TCer.Bas + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                                    END.
                                END.
                            END.
            
                            WHEN "24421501" THEN DO:
                                FOR EACH mov_ahorros WHERE mov_ahorros.nit = TNit.nit
                                                       AND YEAR(mov_ahorros.fecha) = WAno
                                                       AND mov_ahorros.cod_ahorro = 7 NO-LOCK:
                                    FIND FIRST operacion WHERE operacion.cod_operacion = mov_ahorros.cod_operacion NO-LOCK NO-ERROR.
                                    IF AVAILABLE operacion THEN DO:
                                        IF operacion.tipo_operacion = 2 THEN
                                            TCer.Bas = TCer.Bas + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                                    END.
                                END.
                            END.

                            WHEN "24420502" THEN DO:
                                FOR EACH mov_ahorros WHERE mov_ahorros.nit = TNit.nit
                                                       AND YEAR(mov_ahorros.fecha) = WAno
                                                       AND mov_ahorros.cod_ahorro = 9 NO-LOCK:
                                    FIND FIRST operacion WHERE operacion.cod_operacion = mov_ahorros.cod_operacion NO-LOCK NO-ERROR.
                                    IF AVAILABLE operacion THEN DO:
                                        IF operacion.tipo_operacion = 2 THEN
                                            TCer.Bas = TCer.Bas + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
                                    END.
                                END.
                            END.
                        END CASE.
                    END.
                END.
            END.
        END.
    END.

    OPEN QUERY BNit FOR EACH TNit NO-LOCK INDEXED-REPOSITION.
    OPEN QUERY BRet FOR EACH TCer WHERE TCer.Nit EQ WNit
                                    AND (TCer.Bas > 0 OR TCer.Ret > 0) NO-LOCK INDEXED-REPOSITION.

    FOR EACH TNit:
        FIND FIRST TCer WHERE TCer.Nit EQ TNit.Nit NO-ERROR.
        IF NOT AVAILABLE TCer THEN
            DELETE TNit.
    END.

    SESSION:SET-WAIT-STATE("").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

