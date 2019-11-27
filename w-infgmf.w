&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-InfGMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-InfGMF 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
  ON RETURN TAB.

  {Incluido/Variable.I "SHARED"}

  DEFI VAR W_OfiIni     LIKE Agencias.Agencia.
  DEFI VAR W_OfiFin     LIKE Agencias.Agencia.

  DEFI TEMP-TABLE TMov_Contab LIKE Mov_Contable.
  DEFI TEMP-TABLE TMov_GMF    LIKE Mov_Contable.

  DEFINE TEMP-TABLE TCon
      FIELD TCon     LIKE Mov_Gmf.Cod_Pdcto
      FIELD TNom     AS CHARACTER FORMAT "X(20)"
      FIELD TImpEnt  LIKE Mov_Contable.Db
      FIELD TImpCli  LIKE Mov_Contable.Db
      FIELD TBasEnt  LIKE Mov_Contable.Db
      FIELD TBasCli  LIKE Mov_Contable.Db
      FIELD TBasExe  LIKE Mov_Contable.Db
      FIELD TTot     LIKE Mov_Contable.DB.

  DEFINE TEMP-TABLE TT
      FIELD TCodPro   AS CHARACTER FORMAT "X(12)"
      FIELD TNomPro   AS CHARACTER FORMAT "X(40)"
      FIELD TValor    LIKE Ahorros.Sdo_Disponible.

DEF VAR W_NomCli  AS cha NO-UNDO FORMAT "x(40)".
DEF VAR W_Status  AS LOG NO-UNDO.
DEF VAR InputFile AS CHA NO-UNDO.
DEF TEMP-TABLE TablaCIFIN NO-UNDO
    FIELD TipoCIFIN AS CHA     /* Tipo de Identificacion      */
    FIELD NitCIFIN  AS CHA     /* Cedula o Nit                */
    FIELD NomCIFIN  AS CHA     /* Nombre o Razon Social       */
    FIELD OpeCIFIN  AS CHA     /* Operacion: Marca o Desmarca */
    FIELD FecCIFIN  AS DATE.   /* Fecha de la Operacion       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Det

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RADIO-SET-1 W_CmbOfi BUTTON-5 FecIni FecFin ~
Rs_DT Btn_Imp BtnDone Btn_Imp-2 RECT-319 RECT-320 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-1 W_CmbOfi FecIni FecFin Rs_DT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-InfGMF AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.69
     BGCOLOR 8 .

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Informe por vencimientos y Extracto por Inversión.".

DEFINE BUTTON Btn_Imp-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Informe por vencimientos y Extracto por Inversión.".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(50)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 40 BY 1 TOOLTIP "Agencias Disponibles"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Cuentas Marcadas", 1,
"Cuentas Desmarcadas", 2,
"Inf. de Diferencias", 3
     SIZE 22 BY 2.81 NO-UNDO.

DEFINE VARIABLE Rs_DT AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Detallado", 1,
"Total x Agencia", 2,
"Resumen Conceptos", 3,
"Contable", 4
     SIZE 22 BY 2.81 NO-UNDO.

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 3.38.

DEFINE RECTANGLE RECT-320
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 3.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Det
     RADIO-SET-1 AT ROW 5.62 COL 3.57 NO-LABEL WIDGET-ID 4
     W_CmbOfi AT ROW 2.08 COL 15 COLON-ALIGNED
     BUTTON-5 AT ROW 1.5 COL 63.14
     FecIni AT ROW 3.69 COL 15 COLON-ALIGNED
     FecFin AT ROW 3.69 COL 41.72 COLON-ALIGNED
     Rs_DT AT ROW 5.58 COL 40 NO-LABEL
     Btn_Imp AT ROW 6.46 COL 63
     BtnDone AT ROW 3.42 COL 63
     Btn_Imp-2 AT ROW 6.5 COL 26.29 WIDGET-ID 10
     RECT-319 AT ROW 5.31 COL 38.72
     RECT-320 AT ROW 5.27 COL 2.29 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.86 BY 12.12
         BGCOLOR 17 FONT 5.


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
  CREATE WINDOW W-InfGMF ASSIGN
         HIDDEN             = YES
         TITLE              = "Informes GMF, Programa W-InfGMF.W"
         HEIGHT             = 8.19
         WIDTH              = 73.43
         MAX-HEIGHT         = 17.12
         MAX-WIDTH          = 106.86
         VIRTUAL-HEIGHT     = 17.12
         VIRTUAL-WIDTH      = 106.86
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
/* SETTINGS FOR WINDOW W-InfGMF
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Det
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-InfGMF)
THEN W-InfGMF:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Det
/* Query rebuild information for FRAME F_Det
     _Query            is NOT OPENED
*/  /* FRAME F_Det */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-InfGMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-InfGMF W-InfGMF
ON END-ERROR OF W-InfGMF /* Informes GMF, Programa W-InfGMF.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-InfGMF W-InfGMF
ON WINDOW-CLOSE OF W-InfGMF /* Informes GMF, Programa W-InfGMF.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone W-InfGMF
ON CHOOSE OF BtnDone IN FRAME F_Det /* Salir */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp W-InfGMF
ON CHOOSE OF Btn_Imp IN FRAME F_Det /* Imprimir */
DO:
  ASSIGN FRAME F_Det W_CmbOfi.

  IF INTEG(SUBSTRING(W_CmbOfi,1,3)) EQ 0 THEN
     ASSIGN W_OfiIni = 1
            W_OfiFin = 999.
  ELSE ASSIGN W_OfiIni = INTEG(SUBSTRING(W_CmbOfi,1,3))
              W_OfiFin = INTEG(SUBSTRING(W_CmbOfi,1,3)).

  DEFI VAR Listado AS CHAR FORM "X(40)".
  Listado = W_PathSpl + "\" + W_Usuario + "-InfGmf.Lst".
  
  {Incluido\Imprimir.I "listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imp-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp-2 W-InfGMF
ON CHOOSE OF Btn_Imp-2 IN FRAME F_Det /* Imprimir */
DO:
    ASSIGN FRAME F_Det
                 W_CmbOfi
                 RADIO-SET-1.

    IF INTE(SUBSTR(W_CmbOfi,1,3)) = 0 THEN
        ASSIGN W_OfiIni = 1 W_OfiFin = 999.
    ELSE
        ASSIGN W_OfiIni = INT(SUBSTR(W_CmbOfi,1,3))
               W_OfiFin = INT(SUBSTR(W_CmbOfi,1,3)).

    IF RADIO-SET-1 = 1 THEN DO:
        OUTPUT TO VALUE (W_PathSpl + "Marcadas.Txt").

        DISP "TIP" "CEDULA" AT 10
             "NOMBRE" AT 27
             "F-ACT-GMF" AT 70 SKIP(1)
            WITH WIDTH 99 NO-BOX NO-LABEL FRAME FOR1.

        FOR EACH Agencias NO-LOCK WHERE Agencias.Agencia >= W_OfiIni
                                    AND Agencias.Agencia <= W_OfiFin,
            EACH Ahorros NO-LOCK WHERE Ahorros.Agencia = Agencias.Agencia
                                   AND Ahorros.Fec_Activacion[1] >= DATE(FecIni)
                                   AND Ahorros.Fec_Activacion[1] <= DATE(FecFin):
            FIND FIRST Clientes OF Ahorros NO-LOCK NO-ERROR.

            W_NomCli = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre.

            DISP STRING(Clientes.Tipo_Identificacion) FORMAT "X(4)"
                 Clientes.Nit FORMAT "X(12)"
                 W_NomCli
                 Ahorros.Fec_Activacion[1]
                WITH WIDTH 99 NO-BOX NO-LABEL FRAME FOR2.
        END.
        OUTPUT CLOSE.                          

        MESSAGE "Su Informe de Ha generado en: " + W_PathSpl + "Marcadas.Txt"
            VIEW-AS ALERT-BOX.
    END.

    IF RADIO-SET-1 = 2 THEN DO:
        OUTPUT TO VALUE (W_PathSpl + "DesMarcadas.Txt").

        DISP "TIP" "CEDULA" AT 10 "NOMBRE" AT 27 "F-DES-ACT-GMF" AT 70 SKIP(1) WITH WIDTH 99 NO-BOX NO-LABEL FRAME FOR3.
     FOR EACH Agencias NO-LOCK WHERE Agencias.Agencia >= W_OfiIni AND Agencias.Agencia <= W_OfiFin,
         EACH Ahorros  NO-LOCK WHERE Ahorros.Agencia = Agencias.Agencia
                                 AND Ahorros.Fec_DesActivacion[1] >= DATE(FecIni)
                                 AND Ahorros.Fec_DesActivacion[1] <= DATE(FecFin):
          FIND Clientes OF Ahorros NO-LOCK NO-ERROR.
          W_NomCli = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.Nombre.
          DISP STRING(Clientes.Tipo_Identificacion) FORMAT "X(4)" Clientes.Nit FORMAT "X(12)"
               W_NomCli Ahorros.Fec_DesActivacion[1] WITH WIDTH 99 NO-BOX NO-LABEL FRAME FOR4.
     END.
     OUTPUT CLOSE.
     MESSAGE "Su Informe de Ha generado en: " + W_PathSpl + "DesMarcadas.Txt" VIEW-AS ALERT-BOX.
  END.
  IF RADIO-SET-1 = 3 THEN DO:
     SYSTEM-DIALOG GET-FILE InputFile
            TITLE   "Seleccione el Archivo de Entrada Enviado por CIFIN"
            FILTERS "Archivos Datos (*.csv)" "*.csv","Archivos Texto (*.txt)" "*.txt",
                    "Archivos Datos (*.dat)" "*.dat","Archivos Otros (*.*)"   "*.*"
            MUST-EXIST USE-FILENAME UPDATE W_Status.
     IF NOT W_Status THEN RETURN.
     INPUT FROM InputFile.
     REPEAT:
       CREATE TablaCIFIN.
       IMPORT DELIMITER "|" TablaCIFIN NO-ERROR.
     END.
     INPUT CLOSE.
     OUTPUT TO VALUE (W_PathSpl + "Diferencias.Txt").
     /* Falta definicion archivo de entrada suministrado por CIFIN */
     OUTPUT CLOSE.
     MESSAGE "Su Informe de Ha generado en: " + W_PathSpl + "Diferencias.Txt" VIEW-AS ALERT-BOX.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-InfGMF
ON CHOOSE OF BUTTON-5 IN FRAME F_Det /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FecFin W-InfGMF
ON LEAVE OF FecFin IN FRAME F_Det /* Fecha Final */
DO:
  ASSIGN FecFin.
  
  IF FecFin LT FecIni THEN
     ASSIGN FecFin              = FecIni
            FecFin:SCREEN-VALUE = STRING(FecFin).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FecIni W-InfGMF
ON LEAVE OF FecIni IN FRAME F_Det /* Fecha Inicial */
DO:
  ASSIGN FecIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_DT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_DT W-InfGMF
ON VALUE-CHANGED OF Rs_DT IN FRAME F_Det
DO:
  ASSIGN Rs_DT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-InfGMF
ON MOUSE-SELECT-CLICK OF W_CmbOfi IN FRAME F_Det /* Agencia */
DO:
  IF INTEG(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) EQ 0 THEN
     ASSIGN W_OfiIni = 1
            W_OfiFin = 999.
  ELSE ASSIGN W_OfiIni = INTEG(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3))
              W_OfiFin = INTEG(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-InfGMF 


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
  
  W_CmbOfi:ADD-LAST("000-CONSOLIDADO").
  
  ASSIGN W_OfiIni       = 1
         W_OfiFin       = 999
         FecIni         = TODAY
         FecFin         = TODAY
         FecIni:SCREEN-VALUE   = STRING(TODAY)
         FecFin:SCREEN-VALUE   = STRING(TODAY)
         W_CmbOfi:SCREEN-VALUE = "000-CONSOLIDADO".
                  
  FOR EACH Agencias WHERE Agencias.Estado  NE 3 NO-LOCK:
      W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(25)")).
  END. 
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Detallado W-InfGMF 
PROCEDURE Detallado :
/*
 ------------------------------------------------------------------------*/ 
 DEFI VAR TotD    LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TotC    LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TTotD   LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TTotC   LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TotBas  LIKE Mov_Contable.Db EXTENT 9 INIT 0.
 DEFI VAR TotCpto LIKE Mov_Contable.Db EXTENT 6 INIT 0.

 DEFI VAR T  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR Imp_Entidad     LIKE Mov_Contable.Db INIT 0.
 DEFI VAR Imp_Cliente     LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TA_Imp_Entidad  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TA_Imp_Cliente  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TA_Impuestos    LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TT_Impuestos    LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TT_Imp_Entidad  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TT_Imp_Cliente  LIKE Mov_Contable.Db INIT 0.

 /*variables para base*/
 DEFI VAR Bas_Entidad     LIKE Mov_Contable.Db INIT 0.
 DEFI VAR Bas_Cliente     LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TA_Bas_Entidad  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TA_Bas_Cliente  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TT_Bas_Entidad  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TT_Bas_Cliente  LIKE Mov_Contable.Db INIT 0.
 
 FOR EACH Mov_GMF WHERE Mov_GMF.Agencia GE W_OfiIni  
                    AND Mov_GMF.Agencia LE W_OfiFin  
                    AND Mov_GMF.Fecha   GE FecIni    
                    AND Mov_GMF.Fecha   LE FecFin    NO-LOCK
                        BREAK BY Mov_GMF.Agencia BY Mov_GMF.Cod_Pdcto:
    ASSIGN TotD  = TotD  + Mov_GMF.VrImpto_Entidad 
           TotC  = TotC  + Mov_GMF.VrImpto_Cliente 
           TTotD = TTotD + Mov_GMF.VrImpto_Entidad
           TTotC = TTotC + Mov_GMF.VrImpto_Cliente
           TotBas[1] = TotBas[1] + Mov_GMF.VrBase_Cliente
           TotBas[2] = TotBas[2] + Mov_GMF.VrBase_Cliente
           TotBas[7] = TotBas[7] + Mov_GMF.VrBase_Cliente
           TotBas[3] = TotBas[3] + Mov_GMF.VrBase_Entidad
           TotBas[4] = TotBas[4] + Mov_GMF.VrBase_Entidad
           TotBas[8] = TotBas[8] + Mov_GMF.VrBase_Entidad
           TotBas[5] = TotBas[5] + Mov_GMF.VrBase_Exenta
           TotBas[6] = TotBas[6] + Mov_GMF.VrBase_Exenta
           TotBas[9] = TotBas[9] + Mov_GMF.VrBase_Exenta
           TotCpto[1] = TotCpto[1] + Mov_GMF.VrImpto_Cliente
           TotCpto[5] = TotCpto[5] + Mov_GMF.VrImpto_Cliente
           TotCpto[2] = TotCpto[2] + Mov_GMF.VrImpto_Cliente
           TotCpto[3] = TotCpto[3] + Mov_GMF.VrImpto_Entidad
           TotCpto[4] = TotCpto[4] + Mov_GMF.VrImpto_Entidad
           TotCpto[6] = TotCpto[6] + Mov_GMF.VrImpto_Entidad.
           
    DISPLAY Mov_GMF.Agencia           LABEL "Ag."
            Mov_GMF.Agencia_Tx        LABEL "Orig"    
            Mov_GMF.Fecha             LABEL "Fecha"    FORM "99/99/99"   
            Mov_GMF.Cpte              LABEL "CP"       
            Mov_GMF.Documento         LABEL "No.Docto"
            Mov_GMF.Nit               LABEL "Ced./Nit"       
            Mov_GMF.Cod_Pdcto         LABEL "Pto"
            Mov_GMF.Cta_PdctoOContab  LABEL "Cta-Producto" FORM "X(12)"
            Mov_GMF.VrBase_Cliente    FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Base-Cliente"       
            Mov_GMF.VrImpto_Cliente   FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Impto-Cliente"       
            Mov_GMF.VrBase_Entidad    FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Base-Entidad"       
            Mov_GMF.VrImpto_Entidad   FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Impto-Entidad"
            Mov_GMF.VrBase_Exenta     FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Base-Exenta"
            Mov_GMF.Descrip           LABEL "Descripciòn" FORM "X(20)"
         WITH DOWN WIDTH 250 FRAME Ft USE-TEXT STREAM-IO NO-LABELS NO-BOX.

    IF LAST-OF(Mov_GMF.Cod_Pdcto) THEN DO:
       FIND TCon WHERE TCon.TCon EQ Mov_Gmf.Cod_Pdcto NO-ERROR.
       IF NOT AVAILABLE TCon THEN DO:
            CREATE TCon.
            ASSIGN TCon.TCon = Mov_Gmf.Cod_Pdcto.

            CASE Mov_Gmf.Tipo_Pdcto:
                WHEN 1 THEN DO:
                    FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Mov_Gmf.Cod_Pdcto NO-LOCK NO-ERROR.
                    IF AVAILABLE Pro_Ahorros THEN TCon.TNom = Pro_Ahorros.Nom_Producto.
                END.
                WHEN 2 THEN DO:
                    FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Mov_Gmf.Cod_Pdcto NO-LOCK NO-ERROR.
                    IF AVAILABLE Pro_Creditos THEN TCon.TNom = Pro_Creditos.Nom_Producto.
                END.
                WHEN 3 THEN DO:
                    TCon.TNom = "Pagos".
                END.
            END CASE.
       END.

       TCon.TTot = TCon.TTot + Imp_Entidad + Imp_Cliente.

       DISPLAY "Total Pdcto :"
               Mov_Gmf.Cod_Pdcto  FORMAT "999"
               TCon.TNom          FORMAT "X(12)"
               "Tot.Impto:" 
               TotCpto[1] + TotCpto[3] FORMAT "->>>,>>>,>>>,>>9.99"
               TotBas[1]  FORMAT "->>>,>>>,>>>,>>9.99"      
               TotCpto[1] FORMAT "->>>,>>>,>>>,>>9.99"
               TotBas[3]  FORMAT "->>>,>>>,>>>,>>9.99"
               TotCpto[3] FORMAT "->>>,>>>,>>>,>>9.99"
               TotBas[5]  FORMAT "->>>,>>>,>>>,>>9.99"
                 SKIP(1)             
          WITH FRAME F_Mov WIDTH 150 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

       ASSIGN TCon.TImpEnt = TCon.TImpEnt + Imp_Entidad 
              TCon.TImpCli = TCon.TImpCli + Imp_Cliente
              TCon.TBasEnt = TCon.TBasEnt + Bas_Entidad 
              TCon.TBasCli = TCon.TBasCli + Bas_Cliente
              TCon.TBasExe = TCon.TBasExe + VrBase_Exenta. 

       ASSIGN TA_Imp_Entidad = TA_Imp_Entidad + Imp_Entidad
              TA_Imp_Cliente = TA_Imp_Cliente + Imp_Cliente
              TA_Bas_Entidad = TA_Bas_Entidad + Bas_Entidad
              TA_Bas_Cliente = TA_Bas_Cliente + Bas_Cliente
              TA_Impuestos   = TA_Impuestos + Imp_Entidad + Imp_Cliente.

       ASSIGN TT_Imp_Entidad = TT_Imp_Entidad + Imp_Entidad
              TT_Imp_Cliente = TT_Imp_Cliente + Imp_Cliente
              TT_Bas_Entidad = TT_Bas_Entidad + Bas_Entidad
              TT_Bas_Cliente = TT_Bas_Cliente + Bas_Cliente
              TT_Impuestos   = TT_Impuestos + Imp_Entidad + Imp_Cliente.

       ASSIGN Imp_Entidad    = 0
              Imp_Cliente    = 0
              Bas_Entidad    = 0
              Bas_Cliente    = 0
              TotBas[1]  = 0
              TotCpto[1] = 0
              TotBas[3]  = 0
              TotCpto[3] = 0
              TotBas[5]  = 0.
    END.
          
    TA_Impuestos = 0.  

    IF LAST-OF(Mov_GMF.Agencia) THEN DO:  
       DISPLAY SKIP(1)
               "Total Agencia : "
               Mov_Gmf.Agencia  FORMAT "999"
               "     Tot.Impto: " 
               TotCpto[2] + TotCpto[4] FORMAT "->>>,>>>,>>>,>>>.99"
               TotBas[2]  FORMAT "->>>,>>>,>>>,>>9.99"      
               TotCpto[2] FORMAT "->>>,>>>,>>>,>>9.99"
               TotBas[4]  FORMAT "->>>,>>>,>>>,>>9.99"
               TotCpto[4] FORMAT "->>>,>>>,>>>,>>9.99"
               TotBas[6]  FORMAT "->>>,>>>,>>>,>>9.99"
                 SKIP(1)             
          WITH FRAME F_MovAg WIDTH 150 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

       ASSIGN TotBas[2]  = 0  
              TotCpto[2] = 0  
              TotBas[4]  = 0  
              TotCpto[4] = 0  
              TotBas[6]  = 0. 

       /* DISPLAY "                                              Totales Agencia : "
                Mov_GMF.Agencia
                TotBas[1]
                TotC       
                TotBas[3]
                TotD      SKIP(2) 
             WITH DOWN WIDTH 250 FRAME Ftt USE-TEXT STREAM-IO NO-LABELS NO-BOX.*/

        ASSIGN TA_Imp_Entidad  = 0
               TA_Imp_Cliente  = 0
               TA_Bas_Entidad  = 0
               TA_Bas_Cliente  = 0.
        
        ASSIGN TotC  = 0       
               TotD  = 0.
    END. 
 END.

 DISPLAY "Totales Generales        "
         "Tot.Impto: "                                           
         TotCpto[5] + TotCpto[6] FORMAT "->>>,>>>,>>>,>>>.99"
         TotBas[7]   FORMAT "->>>,>>>,>>>,>>9.99"                                                         
         TotCpto[5]  FORMAT "->>>,>>>,>>>,>>9.99"                                            
         TotBas[8]   FORMAT "->>>,>>>,>>>,>>9.99"                                            
         TotCpto[6]  FORMAT "->>>,>>>,>>>,>>9.99"                                            
         TotBas[9]   FORMAT "->>>,>>>,>>>,>>9.99"                                            
           SKIP(1)
     WITH DOWN WIDTH 150 FRAME Fttt USE-TEXT STREAM-IO NO-LABELS NO-BOX.
                
 ASSIGN TTotC  = 0       
        TTotD  = 0
        TotBas[7]  = 0
        TotCpto[5] = 0
        TotBas[8]  = 0
        TotCpto[6] = 0
        TotBas[9]  = 0.

END PROCE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-InfGMF  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-InfGMF)
  THEN DELETE WIDGET W-InfGMF.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-InfGMF  _DEFAULT-ENABLE
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
  DISPLAY RADIO-SET-1 W_CmbOfi FecIni FecFin Rs_DT 
      WITH FRAME F_Det IN WINDOW W-InfGMF.
  ENABLE RADIO-SET-1 W_CmbOfi BUTTON-5 FecIni FecFin Rs_DT Btn_Imp BtnDone 
         Btn_Imp-2 RECT-319 RECT-320 
      WITH FRAME F_Det IN WINDOW W-InfGMF.
  {&OPEN-BROWSERS-IN-QUERY-F_Det}
  VIEW W-InfGMF.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_MovCont W-InfGMF 
PROCEDURE Inf_MovCont :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR TDeb LIKE Mov_Contable.Db EXTENT 3.
  DEFI VAR TCre LIKE Mov_Contable.Db EXTENT 3.

  FOR EACH TMov_Contab: DELETE TMov_Contab. END.
  FOR EACH TMov_Gmf:    DELETE TMov_Gmf.    END.

  FOR EACH Mov_Contable WHERE Mov_Contable.Agenc GE W_OfiIni
                        AND Mov_Contable.Agenc LE W_OfiFin
                        AND (SUBSTRING(Mov_Contable.Cuenta,1,4) EQ "2442" 
                            OR (SUBSTRING(Mov_Contable.Cuenta,1,2) EQ "21" AND Mov_Contable.Db GT 0))
                        AND Mov_Contable.Fec_Contable            GE FecIni
                        AND Mov_Contable.Fec_Contable            LE FecFin  NO-LOCK
           BREAK BY Mov_Contable.Agenc   BY Mov_Contable.Fec_Contab 
                 BY Mov_Contable.Comprob BY Mov_Contable.Num_Docum:
      IF LAST-OF(Mov_Contable.Num_Docum) THEN DO:
         CREATE TMov_Contab.
         BUFFER-COPY Mov_Contable TO TMov_Contab.
      END.       
  END.

  {Incluido\RepEncabezado.I}

  ASSIGN W_Reporte = "Informe G. M. F. (CONTABLE)      Fecha del Informe Desde: " +
                     STRING(FecIni,"99/99/9999") + " Hasta : " + STRING(FecFin,"99/99/9999")
         W_EncColumna = "                   RESUMIDO".

  VIEW FRAME F-Encabezado. 

  FOR EACH TMov_Contab WHERE SUBSTRING(TMov_Contab.Cuenta,1,4) EQ "2442" BREAK BY TMov_Contab.Cuenta:
      ASSIGN Tdeb[1] = Tdeb[1] + TMov_Contab.Db
             Tdeb[2] = Tdeb[2] + TMov_Contab.Db
             TCre[1] = TCre[1] + TMov_Contab.Cr
             TCre[2] = TCre[2] + TMov_Contab.Cr.

      IF LAST-OF(TMov_Contab.Cuenta) THEN DO:
         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ TMov_Contab.Cuenta NO-LOCK NO-ERROR.
         DISPLAY TMov_Contab.Cuenta LABEL "Cta-Contable"  FORM "X(12)"
                 Cuentas.Nombre     LABEL "Descripción"   FORM "X(30)" WHEN AVAIL(Cuentas)
                 Tdeb[1]            LABEL "D E B I T O S"   FORM ">>>>,>>>,>>9.99"
                 TCre[1]            LABEL "C R E D I T O S" FORM ">>>>,>>>,>>9.99"             
           WITH DOWN WIDTH 250 FRAME F-Detal2 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

         ASSIGN Tdeb[1] = 0
                TCre[1] = 0.
      END.
  END.

  DISPLAY "TOTAL GENERAL                               -----------------  ----------------" SKIP
          "                                           "
           Tdeb[2]             FORM ">>>>,>>>,>>9.99"
           " "
           TCre[2]             FORM ">>>>,>>>,>>9.99"             
           WITH DOWN WIDTH 250 FRAME F-tot2 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  ASSIGN Tdeb[2] = 0
         TCre[2] = 0.

  FOR EACH TMov_Contab:
      FOR EACH Mov_Contable WHERE Mov_Contable.Agenc      EQ TMov_Contab.Agenc   
                              AND Mov_Contable.Comprob    EQ TMov_Contab.Comprob
                              AND Mov_Contable.Num_Docum  EQ TMov_Contab.Num_Docum
                              AND Mov_Contable.Fec_Contab EQ TMov_Contab.Fec_Contab NO-LOCK:
          CREATE TMov_Gmf.
          BUFFER-COPY Mov_Contable TO TMov_Gmf.                                         
      END.
  END.

  ASSIGN W_Reporte = "Informe G. M. F. (CONTABLE)      Fecha del Informe Desde: " +
                     STRING(FecIni,"99/99/9999") + " Hasta : " + STRING(FecFin,"99/99/9999")
         W_EncColumna = "                   DETALLADO".

  VIEW FRAME F-Encabezado.

  FOR EACH TMov_Gmf BREAK BY TMov_Gmf.Fec_contab BY TMov_Gmf.Comprob 
                          BY TMov_Gmf.Num_Docum  BY TMov_Gmf.Cuenta:
      DISPLAY TMov_Gmf.Agenc     LABEL "Ag."
              TMov_Gmf.Fec_Cont  LABEL "Fec-Movto" FORM "99/99/99"
              TMov_Gmf.Comp      LABEL "CP"
              TMov_Gmf.Num_Docum LABEL "No-Docto"
              TMov_Gmf.Cuenta    LABEL "Cta-Contable"  FORM "X(12)"
              TMov_Gmf.Nit       LABEL "Ced./Nit"      FORM "X(12)"
              TMov_Gmf.Coment    LABEL "Descripción"   FORM "X(22)"
              TMov_Gmf.Db        LABEL "D E B I T O S"   FORM ">>>>,>>>,>>9.99"
              TMov_Gmf.Cr        LABEL "C R E D I T O S" FORM ">>>>,>>>,>>9.99"
              TMov_Gmf.Doc_Refer LABEL "Doc-Refer"
              TMov_Gmf.Usuar     LABEL "Usuar" SKIP(0)
          WITH DOWN WIDTH 250 FRAME F-Detal NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-InfGMF 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFI VAR TotD  LIKE Mov_Contable.Db  INIT 0.
 DEFI VAR TotC   LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TTotD  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TTotC  LIKE Mov_Contable.Db INIT 0.

 IF Rs_DT EQ 4 THEN DO:
    RUN Inf_MovCont.
    RETURN.
 END.

 {Incluido\RepEncabezado.I}

 ASSIGN W_Reporte = "Informe G. M. F.      Fecha de Corte Desde: " +
                     STRING(FecIni,"99/99/9999") + " Hasta : " + STRING(FecFin,"99/99/9999")
        W_EncColumna = "                   DETALLADO" + " - Generado Hoy: " + STRING(W_Fecha,"99/99/9999").

 IF Rs_DT EQ 2 THEN
    W_EncColumna = "                       TOTAL POR AGENCIA" + " - Generado Hoy :" + STRING(W_Fecha,"99/99/9999").
 IF Rs_DT EQ 3 THEN
    W_EncColumna = "                       TOTAL POR CONCEPTO" + " - Generado Hoy :" + STRING(W_Fecha,"99/99/9999").
    
 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.
 
 IF Rs_DT EQ 1 THEN DO:
    RUN Detallado.
    RETURN.
 END.
 IF Rs_DT EQ 3 THEN DO:
    RUN XConcepto.
    RETURN.
 END.
                   
 IF Rs_DT EQ 2 THEN 
    FOR EACH Mov_GMF WHERE Mov_GMF.Agencia GE W_OfiIni  
                       AND Mov_GMF.Agencia LE W_OfiFin  
                       AND Mov_GMF.Fecha   GE FecIni    
                       AND Mov_GMF.Fecha   LE FecFin    NO-LOCK
                       BREAK BY Mov_GMF.Agencia:
     ASSIGN TotD  = TotD  + Mov_GMF.VrImpto_Entidad 
            TotC  = TotC  + Mov_GMF.VrImpto_Cliente 
            TTotD = TTotD + Mov_GMF.VrImpto_Entidad
            TTotC = TTotC + Mov_GMF.VrImpto_Cliente.
 
    IF LAST-OF(Mov_GMF.Agencia) THEN DO:          
        DISPLAY "Totales Agencia : "
                Mov_GMF.Agencia
                TotC        
                TotD        
             WITH DOWN WIDTH 250 FRAME Ftt USE-TEXT STREAM-IO NO-LABELS NO-BOX.
                
        ASSIGN TotC  = 0       
               TotD  = 0.
    END. 
 END.
 DISPLAY "Totales Generales             "
                TTotC    LABEL "Impto-Cliente"   
                TTotD    LABEL "Impto-Entidad"   
     WITH DOWN WIDTH 250 FRAME Fttt USE-TEXT STREAM-IO NO-LABELS NO-BOX.
                
 ASSIGN TTotC  = 0       
        TTotD  = 0.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE XConcepto W-InfGMF 
PROCEDURE XConcepto :
/*variable para impuesto*/
 DEFI VAR Imp_Entidad     LIKE Mov_Contable.Db INIT 0.
 DEFI VAR Imp_Cliente     LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TA_Imp_Entidad  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TA_Imp_Cliente  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TA_Impuestos    LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TT_Impuestos    LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TT_Imp_Entidad  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TT_Imp_Cliente  LIKE Mov_Contable.Db INIT 0.
 /*variables para base*/
 DEFI VAR Bas_Entidad     LIKE Mov_Contable.Db INIT 0.
 DEFI VAR Bas_Cliente     LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TA_Bas_Entidad  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TA_Bas_Cliente  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TT_Bas_Entidad  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TT_Bas_Cliente  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TotBas  LIKE Mov_Contable.Db EXTENT 4 INIT 0.

 FOR EACH TCon: DELETE TCon. END.

  IF INTEG(SUBSTRING(W_CmbOfi:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,3)) EQ 0 THEN
     ASSIGN W_OfiIni = 1
            W_OfiFin = 999.
  ELSE ASSIGN W_OfiIni = INTEG(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3))
              W_OfiFin = INTEG(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).

 FOR EACH Mov_GMF WHERE
          Mov_GMF.Agencia GE W_OfiIni  
      AND Mov_GMF.Agencia LE W_OfiFin  
      AND Mov_GMF.Fecha   GE FecIni    
      AND Mov_GMF.Fecha   LE FecFin    NO-LOCK
      BREAK BY Mov_GMF.Agencia BY Mov_GMF.Cod_Pdcto:
    IF FIRST-OF(Mov_Gmf.Agencia) THEN DO:
       FIND Agencias WHERE Agencias.Agencia EQ Mov_Gmf.Agencia NO-LOCK NO-ERROR.
       IF AVAILABLE(Agencias)  THEN
          DISPLAY Agencias.Agencia AT 1 FORMAT "999" 
                  Agencias.Nombre  AT 6 FORMAT "X(25)" SKIP
                /* 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
                            1         2         3         4         5         6         7         8         9         1*/
             "Concepto Nombre Concepto      Base Entidad  Impuesto Entidad   Base Cliente  Impuesto Cliente   Total Impto   Base-Exenta"
          WITH FRAME F_EncAge WIDTH 132 STREAM-IO NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE.
                  
    END.
    ASSIGN Imp_Entidad = Imp_Entidad + Mov_Gmf.VrImpto_Entidad
           Imp_Cliente = Imp_Cliente + Mov_Gmf.VrImpto_Cliente
           Bas_Entidad = Bas_Entidad + Mov_Gmf.VrBase_Entidad
           Bas_Cliente = Bas_Cliente + Mov_Gmf.VrBase_Cliente
           TotBas[1]   = TotBas[1]   + Mov_Gmf.VrBase_Exenta
           TotBas[2]   = TotBas[2]   + Mov_Gmf.VrBase_Exenta
           TotBas[3]   = TotBas[3]   + Mov_Gmf.VrBase_Exenta.
          
    IF LAST-OF(Mov_GMF.Cod_Pdcto) THEN DO:
        FIND TCon WHERE TCon.TCon EQ Mov_Gmf.Cod_Pdcto NO-ERROR.
        IF NOT AVAILABLE TCon THEN DO:
            CREATE TCon.
            ASSIGN TCon.TCon = Mov_Gmf.Cod_Pdcto.
            CASE Mov_Gmf.Tipo_Pdcto:
                WHEN 1 THEN DO:
                    FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Mov_Gmf.Cod_Pdcto NO-LOCK NO-ERROR.
                    IF AVAILABLE Pro_Ahorros THEN
                       TCon.TNom = Pro_Ahorros.Nom_Producto.
                    ELSE IF Mov_Gmf.Cod_Pdcto GE 900 THEN
                       TCon.TNom = "Intereses".
                END.
                WHEN 2 THEN DO:
                    FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Mov_Gmf.Cod_Pdcto NO-LOCK NO-ERROR.
                    IF AVAILABLE Pro_Creditos THEN 
                       TCon.TNom = Pro_Creditos.Nom_Producto.
                END.
                WHEN 3 THEN DO:
                    TCon.TNom = "Pagos".
                END.

            END CASE.
        END.

       TCon.TTot = TCon.TTot + Imp_Entidad + Imp_Cliente.
       DISPLAY Mov_Gmf.Cod_Pdcto  AT 3 FORMAT "999"
               TCon.TNom          AT 7 FORMAT "X(20)"
               Bas_Entidad        AT 29 FORMAT ">>,>>>,>>>,>>9"
               Imp_Entidad        AT 44 FORMAT ">>,>>>,>>>,>>9"
               Bas_Cliente        AT 60 FORMAT ">>,>>>,>>>,>>9"
               Imp_Cliente        AT 76 FORMAT ">>,>>>,>>>,>>9"
               Imp_Entidad + Imp_Cliente          AT 92 FORMAT ">>,>>>,>>>,>>9"
               TotBas[1]                          AT 108
          WITH FRAME F_Mov WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

       ASSIGN TCon.TImpEnt = TCon.TImpEnt + Imp_Entidad 
              TCon.TImpCli = TCon.TImpCli + Imp_Cliente
              TCon.TBasEnt = TCon.TBasEnt + Bas_Entidad 
              TCon.TBasCli = TCon.TBasCli + Bas_Cliente
              TCon.TBasExe = TCon.TBasExe + Mov_Gmf.VrBase_Exenta. 

       ASSIGN TA_Imp_Entidad = TA_Imp_Entidad + Imp_Entidad
              TA_Imp_Cliente = TA_Imp_Cliente + Imp_Cliente
              TA_Bas_Entidad = TA_Bas_Entidad + Bas_Entidad
              TA_Bas_Cliente = TA_Bas_Cliente + Bas_Cliente
              TA_Impuestos   = TA_Impuestos + Imp_Entidad + Imp_Cliente.
       ASSIGN TT_Imp_Entidad = TT_Imp_Entidad + Imp_Entidad
              TT_Imp_Cliente = TT_Imp_Cliente + Imp_Cliente
              TT_Bas_Entidad = TT_Bas_Entidad + Bas_Entidad
              TT_Bas_Cliente = TT_Bas_Cliente + Bas_Cliente
              TT_Impuestos   = TT_Impuestos + Imp_Entidad + Imp_Cliente.
       ASSIGN Imp_Entidad    = 0
              Imp_Cliente    = 0
              Bas_Entidad    = 0
              Bas_Cliente    = 0
              TotBas[1]      = 0.
    END.
           
    IF LAST-OF(Mov_GMF.Agencia) THEN DO:
       DISPLAY "TotAgencia:"
                TA_Bas_Entidad        AT 29 FORMAT ">>,>>>,>>>,>>9"
                TA_Imp_Entidad        AT 44 FORMAT ">>,>>>,>>>,>>9"
                TA_Bas_Cliente        AT 60 FORMAT ">>,>>>,>>>,>>9"
                TA_Imp_Cliente        AT 76 FORMAT ">>,>>>,>>>,>>9"
                TA_Impuestos          AT 92  FORMAT ">>,>>>,>>>,>>9" 
                TotBas[2]             AT 108 SKIP(1)
       WITH FRAME F_TAGE WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
       ASSIGN TA_Imp_Entidad  = 0
              TA_Imp_Cliente  = 0
              TA_Bas_Entidad  = 0
              TA_Bas_Cliente  = 0
              TA_Impuestos    = 0
              TotBas[2]       = 0.
    END. 
 END.
 DISPLAY "TotEntidad:"
          TT_Bas_Entidad        AT 29 FORMAT ">>,>>>,>>>,>>9"
          TT_Imp_Entidad        AT 44 FORMAT ">>,>>>,>>>,>>9"
          TT_Bas_Cliente        AT 60 FORMAT ">>,>>>,>>>,>>9"
          TT_Imp_Cliente        AT 76 FORMAT ">>,>>>,>>>,>>9"
          TT_Impuestos          AT 92  FORMAT ">>,>>>,>>>,>>9" 
          TotBas[3]             AT 108 SKIP(2)
 WITH FRAME F_TEnt WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

 TotBas[3] = 0.

 DISPLAY "Informe Resumido Total por Entidad" SKIP
     "Concepto Nombre Concepto      Base Entidad  Impuesto Entidad   Base Cliente  Impuesto Cliente     Total   Base-Exenta"
     WITH FRAME F_TTEnt STREAM-IO WIDTH 150 NO-LABELS USE-TEXT NO-BOX.
        
 FOR EACH TCon BREAK BY TCon.TCon:
     DISPLAY TCon.TCon    AT 3 FORMAT "999"
             TCon.TNom    AT 7 FORMAT "X(20)"
             TCon.TBasEnt AT 29 FORMAT ">>,>>>,>>>,>>9"
             TCon.TImpEnt AT 44 FORMAT ">>,>>>,>>>,>>9"
             TCon.TBasCli AT 60 FORMAT ">>,>>>,>>>,>>9"
             TCon.TImpCli AT 76 FORMAT ">>,>>>,>>>,>>9"
             TCon.TTot    AT 92 FORMAT ">>,>>>,>>>,>>9"
             TCon.TBasExe AT 108
     WITH FRAME F_TTEntMov STREAM-IO WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
 END.

 DEFINE VAR TC_TipPd LIKE Ahorros.Sdo_Disponible.
 DEFINE VAR TC_Conta LIKE Ahorros.Sdo_Disponible.
 DEFINE VAR TC_Agenc LIKE Ahorros.Sdo_Disponible.
 DEFINE VAR TC_CodPd LIKE Ahorros.Sdo_Disponible.
 DEFINE VAR TC_Todo  LIKE Ahorros.Sdo_Disponible.
 DEFINE VAR NomPro   AS CHARACTER FORMAT "X(40)".

 /*impresion de retiros en cheque*/
 DISPLAY SKIP(2) "RETIROS EN CHEQUE ========================================================================"
     WITH FRAME ENCCHE WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.

 FOR EACH Taquilla WHERE Taquilla.Agencia         GE W_OfiIni
                   AND   Taquilla.Agencia         LE W_OfiFin
                   AND   Taquilla.Estado          EQ 1
                   AND   Taquilla.Fec_Transaccion GE FecIni
                   AND   Taquilla.Fec_Transaccion LE FecFin
                   AND   Taquilla.Naturaleza      EQ "DB"
                   AND   Taquilla.Val_Cheque      GT 0 NO-LOCK
                   BREAK BY Taquilla.Agencia BY Taquilla.Tip_Producto BY Taquilla.Cod_Producto
                         BY Taquilla.Nro_Cuenta:
     IF FIRST-OF(Taquilla.Agencia) THEN DO:
        FIND Agencias WHERE Agencias.Agencia EQ Taquilla.Agencia NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
           DISPLAY SKIP(1) Agencias.Agencia Agencias.Nombre SKIP(1)
           WITH FRAME FEncAge WIDTH 132 NO-LABELS USE-TEXT STREAM-IO NO-BOX.
     END.

     IF Taquilla.Cod_Producto EQ 0 AND Taquilla.Tip_Producto EQ 0 THEN DO:
         IF FIRST-OF(Taquilla.Nro_Cuenta) THEN DO:
             FIND Cuentas WHERE Cuentas.Cuenta EQ Taquilla.Nro_Cuenta NO-LOCK NO-ERROR.
             IF AVAILABLE Cuentas THEN NomPro = STRING(Cuentas.Cuenta,"X(12)") + " - " + Cuentas.Nombre.
         END.
         ASSIGN TC_Agenc = TC_Agenc + Taquilla.Val_Cheque
                TC_TipPd = TC_TipPd + Taquilla.Val_Cheque
                TC_Conta = TC_Conta + Taquilla.Val_Cheque
                TC_Todo  = TC_Todo  + Taquilla.Val_Cheque.
     END.
     ELSE DO:
         IF FIRST-OF(Taquilla.Cod_Producto) THEN
         FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Taquilla.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Ahorros) THEN NomPro = STRING(Pro_Ahorros.Cod_Ahorro,">>>>>>>>99") + " - " + Pro_Ahorros.Nom_Producto.
         ASSIGN TC_Agenc = TC_Agenc + Taquilla.Val_Cheque
                TC_TipPd = TC_TipPd + Taquilla.Val_Cheque
                TC_CodPd = TC_CodPd + Taquilla.Val_Cheque
                TC_Todo  = TC_Todo  + Taquilla.Val_Cheque.
     END.

     IF Taquilla.Cod_Producto EQ 0 AND Taquilla.Tip_Producto EQ 0 THEN DO:
         IF LAST-OF(Taquilla.Nro_Cuenta) THEN DO:
             DISPLAY NomPro
                     TC_Conta               AT 50 NO-LABEL FORMAT "->>>,>>>,>>>,>>9.99"
             WITH FRAME F-CAJA0 WIDTH 180 USE-TEXT STREAM-IO NO-BOX NO-LABELS.
             FIND TT WHERE TT.TCodPro EQ Taquilla.Nro_Cuenta NO-ERROR.
             IF NOT AVAILABLE TT THEN DO:
                 CREATE TT.
                 ASSIGN TT.TCodPro = Taquilla.Nro_Cuenta
                        TT.TNomPro = NomPro.
             END.
             TT.TValor = TT.TValor + TC_Conta.
             TC_Conta = 0.
         END.
     END.
     ELSE DO:
         IF LAST-OF(Taquilla.Cod_Producto) THEN DO:
            DISPLAY NomPro
                    TC_CodPd               AT 50 NO-LABEL FORMAT "->>>,>>>,>>>,>>9.99"
            WITH FRAME F-CAJA WIDTH 180 USE-TEXT STREAM-IO NO-BOX NO-LABELS.
            FIND TT WHERE TT.TCodPro EQ STRING(Taquilla.Cod_Producto) NO-ERROR.
            IF NOT AVAILABLE TT THEN DO:
                CREATE TT.
                ASSIGN TT.TCodPro = STRING(Taquilla.Cod_Producto)
                       TT.TNomPro = NomPro.
            END.
            TT.TValor = TT.TValor + TC_CodPd.
            TC_CodPd = 0.
         END.
     END.


     IF LAST-OF(Taquilla.Tip_Producto) THEN DO:
         DISPLAY "                    Total Tipo Pdcto: " Taquilla.Tip_Producto
                 TC_TipPd               AT 50 NO-LABEL FORMAT "->>>,>>>,>>>,>>9.99" SKIP(1)
         WITH FRAME F-CAJA2 WIDTH 180 USE-TEXT STREAM-IO NO-BOX NO-LABELS.
         TC_TipPd = 0.
     END.
     IF LAST-OF(Taquilla.Agencia) THEN DO:
        DISPLAY SKIP(1)
                "                     Total Agencia: " Taquilla.Agencia
                TC_Agenc               AT 50 NO-LABEL FORMAT "->>>,>>>,>>>,>>9.99"
        WITH FRAME F-CAJA3 WIDTH 180 USE-TEXT STREAM-IO NO-BOX NO-LABELS.
        TC_Agenc = 0.
     END.
 END.
 DISPLAY SKIP(1)
         "                     Total Cooperativa: " 
         TC_Todo               AT 50 NO-LABEL FORMAT "->>>,>>>,>>>,>>9.99"
 WITH FRAME F-CAJA4 WIDTH 180 USE-TEXT STREAM-IO NO-BOX NO-LABELS.
 TC_Todo = 0.

 DISPLAY SKIP(2)
         "TOTALES GENERALES POR CODIGO DE PRODUCTO" SKIP
         "---------------------------------------------------------------" SKIP
         "Codigo         Nombre del Producto                       Valor" SKIP
         "----------------------------------------------------------------" SKIP
          
     WITH FRAME fenctot WIDTH 132 USE-TEXT STREAM-IO NO-BOX NO-LABELS.
 FOR EACH TT:
     DISPLAY  TT.TNomPro TT.TValor
         WITH FRAME totales WIDTH 132 NO-LABELS STREAM-IO USE-TEXT NO-BOX.
 END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

