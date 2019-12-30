&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-arcext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-arcext 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
 {Incluido\VARIABLE.I "SHARED"}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
 DEFINE VARIABLE W_Posicion   AS INTEGER.
 DEFINE VAR VlrTmp AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
 DEFINE VAR Z AS INTEGER.
 DEFINE VAR C AS CHARACTER FORMAT "X(45)"  INITIAL "                               ".
 DEFINE VAR D AS CHARACTER FORMAT "X(45)"  INITIAL "0000000000000000000000000000000".
 DEFINE VAR J AS INTEGER.
 DEFINE VAR K AS INTEGER.
 DEFINE VAR Acum AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
 DEFINE VAR XAcum AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
 DEFINE VAR TelAux AS DECIMAL FORMAT "9999999999".
 DEFINE VAR FecAux AS DATE.
 DEFINE VAR CAMPO  AS CHARACTER FORMAT "X(15)".
 DEFINE VAR CAMPO0 AS CHARACTER FORMAT "X(15)".

 define variable fecha1 as date .
 define variable fecha2 as date.
 define variable i as integer.

 define variable Xsumavalor as decimal initial 0 FORMAT "999,999,999,999".
 define variable xsumatasa  as decimal initial 0.
 define variable xtitulos   as integer initial 0.

 DEFINE TEMP-TABLE TInfo
 FIELD TInf  AS CHARACTER FORMAT "X(10)"
 FIELD TText AS CHARACTER FORMAT "X(80)".


/*DATACREDITO*/


/*1-REGISTRO CONTROL*/
DEFINE VAR CRegCon AS CHARACTER FORMAT "X(33)".
/*2-REGISTRO DE INFORMACION*/
DEFINE VAR IRegInf AS CHARACTER FORMAT "X".

DEFINE TEMP-TABLE TmpDat
 FIELD INumCta AS CHARACTER FORMAT "X(18)"
 FIELD INumIde AS CHARACTER FORMAT "X(11)"
 FIELD INomCli AS CHARACTER FORMAT "X(45)"
 FIELD IFecNac AS DECIMAL   FORMAT "999999"
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
 FIELD ITelLab AS DECIMAL   FORMAT "9999999999"
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
 FIELD IPerPag AS INTEGER   FORMAT "9"
 FIELD IEdaMor AS INTEGER   FORMAT "999"
 FIELD IFecAct AS DECIMAL   FORMAT "99999999"
 FIELD IReclam AS DECIMAL   FORMAT "99"
 FIELD IEstrat AS INTEGER   FORMAT "9"
 FIELD IFil    AS CHARACTER FORMAT "X(14)" INITIAL "              ".


/*REGISTRO DE FIN*/
DEFINE VAR CRegFIN AS CHARACTER FORMAT "X(33)".

/* ahorros fogacoop */
DEFINE TEMP-TABLE TmpAFg
 FIELD FA_TipIde AS CHARACTER FORMAT "X(1)"
 FIELD FA_CedNit AS DECIMAL   FORMAT "999999999999"
 FIELD FA_Apell1 AS CHARACTER FORMAT "X(20)"
 FIELD FA_Apell2 AS CHARACTER FORMAT "X(20)"
 FIELD FA_NomNit AS CHARACTER FORMAT "X(40)"
 FIELD FA_TipAho AS CHARACTER FORMAT "X(5)"
 FIELD FA_ValIni AS DECIMAL   FORMAT "99999999999999999"
 FIELD FA_FecApe AS CHARACTER FORMAT "X(8)"
 FIELD FA_Plazo  AS DECIMAL   FORMAT "9999"
 FIELD FA_FecVto AS CHARACTER FORMAT "X(8)"
 FIELD FA_Tasa   AS DECIMAL   FORMAT "999999.99"
 FIELD FA_Amorti AS DECIMAL   FORMAT "9"
 FIELD FA_Modali AS DECIMAL   FORMAT "9"
 FIELD FA_Saldo  AS DECIMAL   FORMAT "99999999999999999".

/* Creditos fogacoop */
DEFINE TEMP-TABLE TmpCFg
 FIELD FC_TipIde AS CHARACTER FORMAT "X(1)"
 FIELD FC_CedNit AS DECIMAL   FORMAT "999999999999"
 FIELD FC_Apell1 AS CHARACTER FORMAT "X(20)"
 FIELD FC_Apell2 AS CHARACTER FORMAT "X(20)"
 FIELD FC_NomNit AS CHARACTER FORMAT "X(40)"
 FIELD FC_TipObl AS INTEGER   FORMAT "9"
 FIELD FC_Califi AS CHARACTER FORMAT "X"
 FIELD FC_FecApe AS CHARACTER FORMAT "X(8)"
 FIELD FC_FecVen AS CHARACTER FORMAT "X(8)"
 FIELD FC_EdaMor AS INTEGER   FORMAT "9999"
 FIELD FC_Tasa   AS DECIMAL   FORMAT "999999.99"
 FIELD FC_PerPag AS DECIMAL   FORMAT "9"
 FIELD FC_Modali AS DECIMAL   FORMAT "9"
 FIELD FC_SalDeu AS DECIMAL   FORMAT "99999999999999999"
 FIELD FC_SalIni AS DECIMAL   FORMAT "99999999999999999"
 FIELD FC_SalOtr AS DECIMAL   FORMAT "99999999999999999"
 FIELD FC_TipGar AS DECIMAL   FORMAT "9"
 FIELD FC_Provis AS DECIMAL   FORMAT "99999999999999999"
 FIELD FC_CuoExt AS DECIMAL   FORMAT "9999999999"
 FIELD FC_MesExt AS DECIMAL   FORMAT "9999999999".

/* Ahorros SUPERSOLIDARIA */
 DEFINE TEMP-TABLE SSAho
   FIELD SA_TipIde AS CHARACTER FORMAT "X" INITIAL "C"
   FIELD SA_CedNit AS DECIMAL   FORMAT "999999999999"
   FIELD SA_NomNit AS CHARACTER FORMAT "X(40)"
   FIELD SA_CodCon AS DECIMAL   FORMAT "999999999999"
   FIELD SA_NomDep AS CHARACTER FORMAT "X(30)"
   FIELD SA_TipAho AS CHARACTER FORMAT "X(2)"
   FIELD SA_Amorti AS INTEGER   FORMAT "9"
   FIELD SA_FecApe AS CHARACTER FORMAT "99999999"
   FIELD SA_Plazo  AS INTEGER   FORMAT "9999"
   FIELD SA_FecVen AS CHARACTER FORMAT "99999999"
   FIELD SA_Modali AS INTEGER   FORMAT "9"
   FIELD SA_TasNom AS DECIMAL   FORMAT "999.999999"
   FIELD SA_TasEfe AS DECIMAL   FORMAT "999.999999"
   FIELD SA_IntPag AS DECIMAL   FORMAT "999999999"
   FIELD SA_Saldo  AS DECIMAL   FORMAT "99999999999999999"
   FIELD SA_ValIni AS DECIMAL   FORMAT "99999999999999999"
   FIELD SA_NumCta AS DECIMAL   FORMAT "99999999999999".
   
/* Aportes SUPERSOLIDARIA */
DEFINE TEMP-TABLE SSApo
  FIELD SS_TipIde AS CHARACTER FORMAT "X"
  FIELD SS_NumNit AS DECIMAL   FORMAT "999999999999"
  FIELD SS_Saldo  AS DECIMAL   FORMAT "99999999999999".

/* Aportes SUPERSOLIDARIA */
DEFINE TEMP-TABLE SScli
  FIELD Sc_TipIde AS CHARACTER FORMAT "X"
  FIELD Sc_NumNit AS DECIMAL   FORMAT "999999999999"
  FIELD Sc_apell1 AS CHARACTER FORMAT "X(20)"
  FIELD Sc_Apell2 AS CHARACTER FORMAT "X(20)"
  FIELD Sc_NomNit AS CHARACTER FORMAT "X(30)"
  FIELD Sc_FecApe AS CHARACTER FORMAT "99999999"
  FIELD Sc_tel    AS CHARACTER FORMAT "X(15)"
  FIELD Sc_dir    AS character FORMAT "X(25)"
  FIELD Sc_soc    AS character FORMAT "9"
  FIELD Sc_ciiu   AS character FORMAT "9999"
  FIELD Sc_mun    AS character FORMAT "999".

/* Creditos SUPERSOLIDARIA */
 DEFINE TEMP-TABLE SSCre
   FIELD SC_TipIde AS CHARACTER FORMAT "X" INITIAL "C"
   FIELD SC_CodCon AS DECIMAL   FORMAT "999999999999"
   FIELD SC_CedNit AS DECIMAL   FORMAT "999999999999"
   FIELD SC_NomNit AS CHARACTER FORMAT "X(40)"
   FIELD SC_Clasif AS DECIMAL   FORMAT "9"
   FIELD SC_Catego AS CHARACTER FORMAT "X"
   FIELD SC_FecApe AS CHARACTER FORMAT "X(8)"
   FIELD SC_EdaMor AS DECIMAL   FORMAT "9999"
   FIELD SC_TipCuo AS DECIMAL   FORMAT "9"
   FIELD SC_ValCuo AS DECIMAL   FORMAT "9999999"
   FIELD SC_CuoPag AS DECIMAL   FORMAT "999"
   FIELD SC_Pagare AS DECIMAL   FORMAT "9999999999"
   FIELD SC_FecVen AS CHARACTER FORMAT "X(8)"
   FIELD SC_TasNom AS DECIMAL   FORMAT "99.99999"
   FIELD SC_TasEfe AS DECIMAL   FORMAT "99.99999"
   FIELD SC_Amorti AS DECIMAL   FORMAT "9"
   FIELD SC_Modali AS DECIMAL   FORMAT "9"
   FIELD SC_SdoCap AS DECIMAL   FORMAT "9999999999"
   FIELD SC_PLAZO  AS DECIMAL   FORMAT "9999"
   FIELD SC_Monto  AS DECIMAL   FORMAT "9999999999"
   FIELD SC_SdoInt AS DECIMAL   FORMAT "9999999999"
   FIELD SC_SdoOtr AS DECIMAL   FORMAT "9999999999"
   FIELD SC_ClaGar AS CHARACTER FORMAT "X(12)"
   FIELD SC_VlrPro AS DECIMAL   FORMAT "9999999999"
   FIELD SC_ConInt AS DECIMAL   FORMAT "9999999999"
   FIELD SC_VlrExt AS DECIMAL   FORMAT "9999999999"
   FIELD SC_MesExt AS DECIMAL   FORMAT "999".

DEFINE TEMP-TABLE brecha
   FIELD producto as integer
   FIELD banda    as integer
   FIELD valor   as decimal FORMAT "999,999,999,999" 
   FIELD tasa    as decimal
   FIELD Nro     as integer
   INDEX id_producto producto ASCENDING.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FrM

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FecCorte Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS FecCorte 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-arcext AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "Procesar" 
     SIZE 9.72 BY 1.12
     BGCOLOR 15 .

DEFINE VARIABLE FecCorte AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de Corte" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 3" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-4 
     LABEL "Salir" 
     SIZE 13 BY 1.65.

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 5" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 6" 
     SIZE 13 BY 1.62.

DEFINE VARIABLE CED AS CHARACTER FORMAT "X(15)":U 
     LABEL "Documento Procesado" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE m1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1.08
     BGCOLOR 18 FGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE m2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Archivo en Proceso" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Prc AS DECIMAL FORMAT "999999":U INITIAL 0 
     LABEL "Registro Procesado" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 1.62.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 5.38.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 1.62.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 5.38.

DEFINE VARIABLE t-2 AS LOGICAL INITIAL no 
     LABEL "CIFIN" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .54 NO-UNDO.

DEFINE VARIABLE t-3 AS LOGICAL INITIAL no 
     LABEL "Ahorro" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE t-4 AS LOGICAL INITIAL no 
     LABEL "Crédito" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE t-5 AS LOGICAL INITIAL no 
     LABEL "Individual Captaciones" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE T-6 AS LOGICAL INITIAL no 
     LABEL "Individual Aportes" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .77 NO-UNDO.

DEFINE VARIABLE T-7 AS LOGICAL INITIAL no 
     LABEL "Individual Cartera" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .77 NO-UNDO.

DEFINE VARIABLE t-8 AS LOGICAL INITIAL no 
     LABEL "Individual de Clientes" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .77 NO-UNDO.

DEFINE VARIABLE t-9 AS LOGICAL INITIAL no 
     LABEL "Brecha" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE t1 AS LOGICAL INITIAL no 
     LABEL "DATACREDITO" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1.08 NO-UNDO.

DEFINE VARIABLE t2 AS LOGICAL INITIAL no 
     LABEL "Ahorro" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .54 NO-UNDO.

DEFINE VARIABLE t3 AS LOGICAL INITIAL no 
     LABEL "Crédito" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .54 NO-UNDO.

DEFINE VARIABLE t4 AS LOGICAL INITIAL no 
     LABEL "Individual Captaciones" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE T5 AS LOGICAL INITIAL no 
     LABEL "Individual Aportes" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE T6 AS LOGICAL INITIAL no 
     LABEL "Individual Cartera" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE t7 AS LOGICAL INITIAL no 
     LABEL "Individual de Clientes" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE t8 AS LOGICAL INITIAL no 
     LABEL "Brecha" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .54 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frm0
     m1 AT ROW 1.27 COL 29 COLON-ALIGNED NO-LABEL
     BUTTON-3 AT ROW 1.54 COL 98
     m2 AT ROW 2.62 COL 29 COLON-ALIGNED
     BUTTON-2 AT ROW 3.15 COL 98
     BUTTON-6 AT ROW 4.77 COL 98
     t1 AT ROW 5.31 COL 10
     t-2 AT ROW 6.65 COL 10
     t2 AT ROW 8.81 COL 9
     t3 AT ROW 8.81 COL 21
     t8 AT ROW 8.81 COL 34
     t-3 AT ROW 8.81 COL 53
     t-4 AT ROW 8.81 COL 65
     t-9 AT ROW 8.81 COL 77
     t4 AT ROW 11.5 COL 9
     t-5 AT ROW 11.5 COL 53
     T5 AT ROW 12.58 COL 9
     T-6 AT ROW 12.58 COL 53
     T6 AT ROW 13.65 COL 9
     T-7 AT ROW 13.65 COL 53
     t7 AT ROW 14.73 COL 9
     t-8 AT ROW 14.73 COL 53
     BUTTON-4 AT ROW 17.42 COL 98
     CED AT ROW 18.23 COL 27 COLON-ALIGNED
     BUTTON-5 AT ROW 19.31 COL 103
     Prc AT ROW 19.58 COL 27 COLON-ALIGNED
     "  SUPERBANCARIA" VIEW-AS TEXT
          SIZE 20 BY 1.08 AT ROW 10.15 COL 54
          FGCOLOR 7 
     "  REVISORIA FISCAL" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 7.88 COL 54
          FGCOLOR 7 
     "  SUPERSOLIDARIA" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 10.42 COL 9
          FGCOLOR 7 
     "  FOGACOOP" VIEW-AS TEXT
          SIZE 13 BY 1.08 AT ROW 7.73 COL 9
          FGCOLOR 7 
     RECT-25 AT ROW 8.27 COL 7
     RECT-26 AT ROW 10.69 COL 7
     RECT-27 AT ROW 8.27 COL 51
     RECT-28 AT ROW 10.69 COL 51
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.72 BY 21.38
         BGCOLOR 17 FONT 5.

DEFINE FRAME FrM
     FecCorte AT ROW 1.27 COL 6
     Btn_Done AT ROW 1.27 COL 34
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 45 ROW 17.69
         SIZE 44 BY 2.69
         BGCOLOR 17 FONT 5
         TITLE "Entre Fecha de Corte"
         DEFAULT-BUTTON Btn_Done.


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
  CREATE WINDOW w-arcext ASSIGN
         HIDDEN             = YES
         TITLE              = "Generacion de Archivos Entidades Externas"
         HEIGHT             = 21.38
         WIDTH              = 113.72
         MAX-HEIGHT         = 22.15
         MAX-WIDTH          = 113.72
         VIRTUAL-HEIGHT     = 22.15
         VIRTUAL-WIDTH      = 113.72
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR WINDOW w-arcext
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FrM:FRAME = FRAME frm0:HANDLE.

/* SETTINGS FOR FRAME FrM
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FecCorte IN FRAME FrM
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME frm0
                                                                        */
/* SETTINGS FOR BUTTON BUTTON-6 IN FRAME frm0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CED IN FRAME frm0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN m1 IN FRAME frm0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN m2 IN FRAME frm0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Prc IN FRAME frm0
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-arcext)
THEN w-arcext:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-arcext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-arcext w-arcext
ON END-ERROR OF w-arcext /* Generacion de Archivos Entidades Externas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-arcext w-arcext
ON WINDOW-CLOSE OF w-arcext /* Generacion de Archivos Entidades Externas */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done w-arcext
ON CHOOSE OF Btn_Done IN FRAME FrM /* Procesar */
DO:
  HIDE FRAME Frm.
  RUN Procesar.
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done w-arcext
ON ENTRY OF Btn_Done IN FRAME FrM /* Procesar */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FecCorte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FecCorte w-arcext
ON ENTRY OF FecCorte IN FRAME FrM /* Fecha de Corte */
DO:
  on return tab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FecCorte w-arcext
ON LEAVE OF FecCorte IN FRAME FrM /* Fecha de Corte */
DO:
  ASSIGN FecCorte.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-arcext 


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
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AhoFgc w-arcext 
PROCEDURE AhoFgc :
/*------------------------------------------------------------------------------
  Purpose: Genera Archivo Plano Ahorros FOGACOOP     
------------------------------------------------------------------------------*/
 Assign Prc   = 0
        acum  = 0
        xacum = 0.

 FOR EACH Ahorros WHERE Ahorros.Estado = 1 AND
                        Ahorros.Cod_Ahorro NE 1 
                        NO-LOCK:
 IF (Ahorros.Sdo_disponible + sdo_canje) > 0 THEN DO:
     ASSIGN Prc = Prc + 1
         Ced = Ahorros.Nit.
     DISPLAY Prc Ced WITH NO-LABELS FRAME FRM0.
     FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
     CREATE TmpAFG.
     ASSIGN TmpAFG.FA_TipIde = "C"
            TmpAFG.FA_CedNit = DECIMAL(Ahorros.Nit).
     IF Ahorros.Fec_Apertura EQ ? THEN
       TmpAFG.FA_FecApe = "31/12/1999".
     ElSE
         Assign TmpAFG.FA_FecApe = STRING(DAY(Ahorros.Fec_Apertura),"99") + "/" +
                                   STRING(MONTH(Ahorros.Fec_Apertura),"99")  + "/" +
                                   STRING(YEAR(Ahorros.Fec_Apertura),"9999").

     Assign TmpAFG.FA_Plazo  = Ahorros.Plazo
         TmpAFG.FA_Modali = 1.

      ASSIGN TmpAFG.FA_ValIni = Ahorros.Sdo_disponible + Ahorros.sdo_canje
             TmpAFG.FA_Saldo  = Ahorros.Sdo_Disponible + ahorros.sdo_canje.

     IF Ahorros.Fec_Vencimiento EQ ? THEN
        TmpAFG.FA_FecVto = "31/12/2003".
     ELSE
        TmpAFG.FA_FecVto = STRING(DAY(Ahorros.Fec_Vencimiento),"99") + "/" + 
                        STRING(MONTH(Ahorros.Fec_Vencimiento),"99")  + "/" +
                        STRING(YEAR(Ahorros.Fec_Vencimiento),"9999").
     FIND Terceros WHERE Terceros.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
     IF AVAILABLE(Terceros) THEN
       ASSIGN W_Posicion       = INDEX(Terceros.Apellido, "_")
              TmpAFG.FA_Apell1 = SUBSTRING(Terceros.Apellido, 1, W_Posicion - 1).
       IF TmpAFG.FA_Apell1 = " " THEN TmpAFG.FA_Apell1 = "Apellido".

       IF Terceros.Nombre ne " " then
           assign TmpAFG.FA_NomNit = Terceros.Nombre.
       Else  Assign TmpAFG.FA_NomNit = TmpAFG.FA_Apell1.
      
       ASSIGN TmpAFG.FA_Apell2 = IF W_Posicion NE 0 THEN 
                                 SUBSTRING(Terceros.Apellido, W_Posicion + 1, 30)
                              ELSE " ".
       IF TmpAFG.FA_Apell2 = " " THEN TmpAFG.FA_Apell2 = "Apellido".
     CASE Pro_Ahorros.Tip_Ahorro:
       WHEN 1 THEN
         ASSIGN TmpAFG.FA_TipAho = "CAHO"
          TmpAFG.FA_Amorti = 1
          TmpAFG.FA_Plazo  = 1
          TmpAFG.FA_Tasa = 4.28.
       WHEN 3 THEN DO:
           ASSIGN TmpAFG.FA_TipAho = "CDAT"
           TmpAFG.FA_Amorti = 30.
           CASE Ahorros.Per_Liquidacion:
              WHEN 0 THEN
                 ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa * 360
                        TmpAFG.FA_Plazo  = Ahorros.Plazo.
              WHEN 4 THEN
                 ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa * 12
                        TmpAFG.FA_Plazo  = Ahorros.Plazo * 30.
              WHEN 6 THEN
                 ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa * 4
                        TmpAFG.FA_Plazo  = Ahorros.Plazo * 90.
              WHEN 8 THEN
                 ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa * 2
                        TmpAFG.FA_Plazo  = Ahorros.Plazo * 180.
              WHEN 9 THEN
                 ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa
                        TmpAFG.FA_Plazo  = Ahorros.Plazo * 360.
              WHEN 10 THEN
                 ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa * 360
                        TmpAFG.FA_Plazo  = Ahorros.Plazo.
              OTHERWISE
                 ASSIGN TmpAFG.FA_Tasa = Ahorros.Tasa
                        TmpAFG.FA_Plazo  = Ahorros.Plazo.
           END CASE.
       END.
       OTHERWISE 
         ASSIGN TmpAFG.FA_TipAho = "OTRO"
             TmpAFG.FA_Amorti = 30
             TmpAFG.FA_Plazo  = Ahorros.Plazo * 30
             TmpAFG.FA_Tasa = 10.03.
    END CASE.
  END.
 END.

OUTPUT TO VALUE("C:\INFRED\" + STRING(W_agencia,"99") + "FGCAHO.TXT") NO-CONVERT.
 FOR EACH TmpAFG BREAK BY FA_TipAho:
  Acum = Acum + FA_Saldo.
  Xacum = xacum + FA_ValIni.
  K = K + 1.
  DO J = 1 TO 14 BY 1:
    CASE J:
     WHEN 1 THEN Campo = STRING(FA_TipIde).
     WHEN 2 THEN Campo = STRING(FA_CedNit).
     WHEN 3 THEN Campo = STRING(FA_Apell1).
     WHEN 4 THEN Campo = STRING(FA_Apell2).
     WHEN 5 THEN Campo = STRING(FA_NomNit).
     WHEN 6 THEN Campo = STRING(FA_TipAho).
     WHEN 7 THEN Campo = STRING(FA_ValIni).
     WHEN 8 THEN Campo = STRING(FA_FecApe).
     WHEN 9 THEN Campo = STRING(FA_Plazo).
     WHEN 10 THEN Campo = FA_FecVto.
     WHEN 11 THEN Campo = STRING(FA_Tasa).
     WHEN 12 THEN Campo = STRING(FA_Amorti).
     WHEN 13 THEN Campo = STRING(FA_Modali).
     WHEN 14 THEN Campo = STRING(FA_Saldo).
    END CASE.
    CAMPO0 = "4," + STRING(J) + ",1," + STRING(K) + "," + Campo.
    PUT UNFORMATTED
    CAMPO0 AT 1.
  END.
END.
K = 0.
OUTPUT CLOSE.
OUTPUT TO VALUE("C:\INFRED\" + "TOTFGCAHO.TXT") NO-CONVERT.
display "Total Captacion " Acum Xacum.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calificacion w-arcext 
PROCEDURE Calificacion :
FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
  CASE Pro_Creditos.Tip_Credito:
   WHEN 1 THEN
    TmpDat.ITipCre = 2.
   WHEN 2 THEN
    TmpDat.ITipCre = 1.
   WHEN 3 THEN
    TmpDat.ITipCre = 3.
  END CASE.
  
  CASE Creditos.Categoria:
   WHEN "A" THEN 
    ASSIGN TmpDat.INoveda = 01
           ICalifi = "A"
           IEdaMor = 1.
   WHEN "A" THEN 
    ASSIGN TmpDat.INoveda = 01
           ICalifi = "A"
           IEdaMor = 1.
   WHEN "B" THEN
      ASSIGN INoveda = 06
             ICalifi = "B"
             IEdaMor = 31.
   WHEN "C" THEN 
    DO:
     ICalifi = "C".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN
      ASSIGN INoveda = 07
             IEdaMor = 61.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN
      ASSIGN INoveda = 09
             IEdaMor = 121.
    END.
   WHEN "D" THEN 
    DO:
     ICalifi = "D".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN
      ASSIGN INoveda = 08
             IEdaMor = 91.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN
      ASSIGN INoveda = 09
             IEdaMor = 181.
    END.
   WHEN "E" THEN
    DO:
     ICalifi = "E".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN
      ASSIGN INoveda = 09
             IEdaMor = 181.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN
      ASSIGN INoveda = 09
             IEdaMor = 361.
    END.
  END CASE. 
  IF MONTH(Creditos.Fec_CanceTotal) EQ MONTH(FecCorte) THEN
     ASSIGN INoveda = 05.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CiuRes w-arcext 
PROCEDURE CiuRes :
DEFINE INPUT   PARAMETER Ciudad LIKE Clientes.Lugar_Residencia.
 DEFINE OUTPUT  PARAMETER CiuBak AS CHARACTER FORMAT "X(15)".
 
 FIND Ubicacion WHERE Ubicacion.Ubicacion = Ciudad NO-LOCK NO-ERROR.
 IF AVAILABLE(Ubicacion) THEN CiuBak = Ubicacion.Nombre.
 ELSE CiuBak = "".
 IF LENGTH(CiuBak) LT 15 THEN DO:
   Z = 15 - LENGTH(CiuBak).
   Ciubak = CiuBak + SUBSTRING(C,1,Z).
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CodeudoresDtCr w-arcext 
PROCEDURE CodeudoresDtCr :
DEFINE VARIABLE i AS INTEGER.

/*revisar bien este proceso  */

 /*FOR EACH Garantias WHERE Garantias.Pagare         EQ Creditos.Pagare AND
                          Garantias.Tipo_Garantia   EQ 2               AND
                          Garantias.Estado         EQ 1               NO-LOCK
                    BREAK BY Garantias.Pagare:
   ASSIGN i = i + 1
          Prc = Prc + 1.
   DISPLAY Prc WITH NO-LABELS FRAME FRM0.

   CREATE TmpDat.
/**/
  FIND Terceros WHERE Terceros.Nit = Garantias.Nit_Codeudor NO-LOCK NO-ERROR.
  IF AVAILABLE(Terceros) THEN
   DO:
    ASSIGN TmpDat.INomCli = STRING(TRIM(REPLACE(Terceros.Apellido,"_"," ")) + " " + TRIM(Terceros.Nombre),"X(45)")
           TmpDat.ITelLab = DECIMAL(Terceros.Tel_Comercial) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN TmpDat.ITelLab = 0.
    RUN CiuRes(INPUT Terceros.Ciu_Comercial, OUTPUT TmpDat.ICiuLab).
    RUN TipIdentificacion.
   END.
  FIND Clientes WHERE Clientes.Nit = Garantias.Nit_Codeudor NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN
   DO:
    RUN CiuRes(INPUT Clientes.Lugar_Residencia, OUTPUT TmpDat.ICiuRes).
    ASSIGN TmpDat.IFecNac = DECIMAL(STRING(YEAR(Clientes.Fec_Nacimiento),"9999") + STRING(MONTH(Clientes.Fec_Nacimiento),"99"))
           TmpDat.IDirRes = SUBSTRING(Clientes.Dir_Residencia,1,30)
           TmpDat.IDirCor = SUBSTRING(Clientes.Dir_Residencia,1,30)
           TmpDat.ICiuCor = TmpDat.ICiuRes
           TmpDat.ICiuRad = TmpDat.ICiuRes
           TmpDat.ITelRes = STRING(Clientes.Tel_Residencia,"9999999999").
    TmpDat.ICiiu   = DECIMAL(Clientes.Codigo_CIIU).
   END.
  ELSE
   DO:
    ASSIGN TmpDat.ICiuRes = "               "
           TmpDat.IFecNac = 0
           TmpDat.IDirRes = "               "
           TmpDat.IDirCor = "               "
           TmpDat.ICiuCor = "               "
           TmpDat.ICiuRad = "               "
           TmpDat.ITelRes = "0000000000"
           TmpDat.ICiiu   = 0.
    IF LENGTH(TmpDat.IDirRes) LT 30 THEN
     ASSIGN Z = 30 - LENGTH(TmpDat.IDirRes)
            TmpDat.IDirRes = TmpDat.IDirRes + SUBSTRING(C,1,Z).
    IF LENGTH(TmpDat.IDirCor) LT 30 THEN
     ASSIGN Z = 30 - LENGTH(TmpDat.IDirCor)
            TmpDat.IDirCor = TmpDat.IDirCor + SUBSTRING(C,1,Z).
   END.

  IF TmpDat.IFecNac = 0 THEN
     TmpDat.IFecNac = DECIMAL(STRING(YEAR(FecCorte),"9999") + STRING(MONTH(FecCorte),"99")).
  FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.cod_Empresa NO-LOCK NO-ERROR.
     IF AVAILABLE(Empresas) THEN
       RUN PeriodoEmpresa.
  /**/
   ASSIGN
   TmpDat.INumCta = STRING(W_agencia,"99") + STRING(Creditos.Pagare,"X(14)") + "C" + STRING(i)
   TmpDat.INumIde = Garantias.Nit_Codeudor
   TmpDat.IFecApe = DECIMAL(STRING(YEAR(Creditos.Fec_Desembolso),"9999") + STRING(MONTH(Creditos.Fec_Desembolso),"99"))
   TmpDat.ICuoMes = Creditos.Cuota
   TmpDat.IAdjeti = 0
   TmpDat.IValIni = Creditos.Monto
   TmpDat.ISdoCre = Creditos.Sdo_Capital
   TmpDat.IValDis = 0
   TmpDat.ITipMon = 1
   TmpDat.ITipCre = 2
   TmpDat.ITotCuo = Creditos.Plazo
   TmpDat.ICuoCan = Creditos.Cuo_Pagadas
   TmpDat.ISdoMor = 0
   TmpDat.IReclam = 0.
    IF TmpDat.IFecApe = 0 THEN
      TmpDat.IFecApe = DECIMAL(STRING(YEAR(FecCorte)) + STRING(MONTH(FecCorte),"99")).
/**/
  RUN Nomagencia (INPUT Creditos.agencia, OUTPUT TmpDat.IOfiRad).
  IF Creditos.Estado = 1 AND  Creditos.Sdo_Capital GT 0 THEN
    TmpDat.IForPag = 0.
  ELSE
    TmpDat.IForPag = 1.
  
  IF DAY(Creditos.Fec_Desembolso) GT DAY(FecCorte) THEN
     TmpDat.IFecAct = DECIMAL(STRING(YEAR(FecCorte),"9999") + STRING(MONTH(FecCorte),"99") + STRING(DAY(FecCorte),"99")).
  ELSE
     TmpDat.IFecAct = DECIMAL(STRING(YEAR(FecCorte),"9999") + STRING(MONTH(FecCorte),"99") + STRING(DAY(Creditos.Fec_Desembolso),"99")).

  FOR EACH Atrasos WHERE Atrasos.Pagare = Creditos.Pagare AND Atrasos.Nit = Creditos.Nit:
   VlrTmp = VlrTmp + Atrasos.Vlr_CapMor.
  END.
  ASSIGN TmpDat.ISdoMor = VlrTmp
         VlrTmp         = 0.
/**/
   IF LAST-OF(Garantias.Pagare) THEN
     i = 0.
 END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreFgc w-arcext 
PROCEDURE CreFgc :
/* Genera archivo plano con informacion de creditos para FOGACOOP*/

 Assign Prc   = 0
        Acum  = 0
        Xacum = 0.
        
 FOR EACH Creditos WHERE Creditos.Estado         EQ 1 AND
                         Creditos.Fec_Desembolso LE FecCorte and
                         creditos.sdo_capital > 0 NO-LOCK:
  ASSIGN Prc = Prc + 1
         Ced = Creditos.Nit.
  DISPLAY Prc Ced WITH NO-LABELS FRAME FRM0.
  
  FIND Pro_Creditos WHERE Pro_Creditos.Cod_credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
  CREATE TmpCFG.
  ASSIGN TmpCFG.FC_TipIde = "C"
         TmpCFG.FC_CedNit = DECIMAL(Creditos.Nit)
         TmpCFG.FC_SalIni = Creditos.cuota
         TmpCFG.FC_FecApe = STRING(DAY(Creditos.Fec_Desembolso),"99") + "/" +
                            STRING(MONTH(Creditos.Fec_Desembolso),"99")  + "/" +
                            STRING(YEAR(Creditos.Fec_Desembolso),"9999")
         TmpCFG.FC_Modali = 1
         TmpCFG.FC_SalDeu = Creditos.Sdo_Capital
         TmpCFG.FC_PerPag = 1
         TmpCFG.FC_SalOtr = 0
         TmpCFG.FC_Provis = 0
         TmpCFG.FC_CuoExt = 1
         TmpCFG.FC_MesExt = 1
         TmpCFG.FC_Tasa   = Creditos.Tasa * 12.
  FIND Terceros WHERE Terceros.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Terceros) THEN
    ASSIGN W_Posicion       = INDEX(Terceros.Apellido, "_")
           TmpCFG.FC_Apell2 = IF W_Posicion NE 0 THEN 
                                 SUBSTRING(Terceros.Apellido, W_Posicion + 1, 30)
                              ELSE ""
           TmpCFG.FC_Apell1 = SUBSTRING(Terceros.Apellido, 1, W_Posicion - 1)
           TmpCFG.FC_NomNit = Terceros.Nombre.
  if TmpCFG.FC_Apell1 = " " then TmpCFG.FC_Apell1 = "Apellido".
  if TmpCFG.FC_Apell2 = " " then TmpCFG.FC_Apell2 = "Apellido".
  if TmpCFG.FC_NomNit = " " then TmpCFG.FC_NomNit = TmpCFG.FC_Apell1.
  FIND FIRST Garantias WHERE Garantias.Num_credito EQ Creditos.Num_credito

                              NO-LOCK NO-ERROR.
  IF AVAILABLE(Garantias) THEN TmpCFG.FC_TipGar = 1.
  ELSE TmpCFG.FC_TipGar = 2.
  CASE Pro_Creditos.Tip_Credito:
    WHEN 1 THEN
      TmpCFG.FC_TipObl = 1. /*Consumo*/
    WHEN 2 THEN
      TmpCFG.FC_TipObl = 2. /*Comercial*/
    WHEN 2 THEN
      TmpCFG.FC_TipObl = 3. /*Hipotecario*/
    otherwise
      TmpCFG.FC_TipObl = 1. 
  END CASE.
  CASE Creditos.Categoria:
   WHEN "A" THEN 
    ASSIGN TmpCFG.FC_EdaMor = 1
           TmpCFG.FC_Califi = "A".
   WHEN "A" THEN 
    ASSIGN TmpCFG.FC_EdaMor = 1
           TmpCFG.FC_Califi = "A".
   WHEN "B" THEN
    ASSIGN TmpCFG.FC_EdaMor = 31
           TmpCFG.FC_Califi = "B".
   WHEN "C" THEN 
    DO:
     TmpCFG.FC_Califi = "C".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN TmpCFG.FC_EdaMor = 61.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN TmpCFG.FC_EdaMor = 121.
    END.
   WHEN "D" THEN 
    DO:
     TmpCFG.FC_Califi = "D".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN TmpCFG.FC_EdaMor = 91.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN TmpCFG.FC_EdaMor = 181.
    END.
   WHEN "E" THEN
    DO:
     TmpCFG.FC_Califi = "E".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN TmpCFG.FC_EdaMor = 181.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN TmpCFG.FC_EdaMor = 361.
    END.
   WHEN "E" THEN
    DO:
     TmpCFG.FC_Califi = "E".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN TmpCFG.FC_EdaMor = 181.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN TmpCFG.FC_EdaMor = 361.
    END.
   WHEN "E" THEN
    DO:
     TmpCFG.FC_Califi = "E".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN TmpCFG.FC_EdaMor = 181.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN TmpCFG.FC_EdaMor = 361.
    END.
  END CASE. 
  CASE Creditos.Per_Pago:
   WHEN 1 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 7)).
   WHEN 2 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 10)).
   WHEN 3 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 15)).
   WHEN 4 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 30)).
   WHEN 5 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 60)).
   WHEN 6 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 90)).
   WHEN 7 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 120)).
   WHEN 8 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 180)).
   WHEN 9 THEN
     FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 360)).
  END CASE.
  FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 30)).
  TmpCFG.FC_FecVen = STRING(DAY(FecAux),"99") + "/" +
                     STRING(MONTH(FecAux),"99")  + "/" +
                     STRING(YEAR(FecAux),"9999").
 END.
OUTPUT TO VALUE("C:\INFRED\" + STRING(W_agencia,"99") + "FGCCRE.TXT") NO-CONVERT.
 FOR EACH TmpCFG:
   K = K + 1.
   acum = acum + FC_SalDeu.
   DO J = 1 TO 20 BY 1:
    CASE J:
     WHEN 1 THEN Campo = STRING(FC_TipIde).
     WHEN 2 THEN Campo = STRING(FC_CedNit).
     WHEN 3 THEN Campo = STRING(FC_Apell1).
     WHEN 4 THEN Campo = STRING(FC_Apell2).
     WHEN 5 THEN Campo = STRING(FC_NomNit).
     WHEN 6 THEN Campo = STRING(FC_TipObl).
     WHEN 7 THEN Campo = STRING(FC_Califi).
     WHEN 8 THEN Campo = STRING(FC_FecApe).
     WHEN 9 THEN Campo = STRING(FC_FecVen).
     WHEN 10 THEN Campo = STRING(FC_EdaMor).
     WHEN 11 THEN Campo = STRING(FC_Tasa).
     WHEN 12 THEN Campo = STRING(FC_PerPag).
     WHEN 13 THEN Campo = STRING(FC_Modali).
     WHEN 14 THEN Campo = STRING(FC_SalDeu).
     WHEN 15 THEN Campo = STRING(FC_SalIni).
     WHEN 16 THEN Campo = STRING(FC_SalOtr).
     WHEN 17 THEN Campo = STRING(FC_TipGar).
     WHEN 18 THEN Campo = STRING(FC_Provis).
     WHEN 19 THEN Campo = STRING(FC_CuoExt).
     WHEN 20 THEN Campo = STRING(FC_MesExt).
    END CASE.
    CAMPO0 = "5," + STRING(J) + ",1," + STRING(K) + "," + Campo.
    PUT UNFORMATTED
    CAMPO0 AT 1.
   END.
END.
OUTPUT CLOSE.

OUTPUT TO VALUE("C:\INFRED\" + "TOTFGCre.TXT") NO-CONVERT.
display "totales Cartera " Acum.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DataCredito w-arcext 
PROCEDURE DataCredito :
DEFINE VAR XMul    AS INTEGER   FORMAT "99".
DEFINE VAR XDiaAtr AS INTEGER   FORMAT "9999".
Prc = 0.
FOR EACH Creditos WHERE /*Creditos.nit GE "22005017" and Creditos.nit LT "22005205" and*/
                        Creditos.Pagare NE "" AND 
                        Creditos.Sdo_Capital GT 0 AND
                        (Creditos.Estado EQ 1 OR 
                         MONTH(Creditos.Fec_CanceTotal) EQ MONTH(FecCorte)) AND
                         Creditos.Fec_Desembolso LE FecCorte
                        NO-LOCK BREAK BY Creditos.Nit:
  ASSIGN Prc = Prc + 1
         Ced = Creditos.Nit.
  DISPLAY Prc Ced WITH NO-LABELS FRAME FRM0.
  CREATE TmpDat.
  FIND Terceros WHERE Terceros.Nit = Creditos.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Terceros) THEN
   DO:
    ASSIGN TmpDat.INomCli = STRING(TRIM(REPLACE(Terceros.Apellido,"_"," ")) + " " + TRIM(Terceros.Nombre),"X(45)")
           TmpDat.ITelLab = DECIMAL(Terceros.Tel_Comercial) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN TmpDat.ITelLab = 0.
    RUN CiuRes(INPUT Terceros.Ciu_Comercial, OUTPUT TmpDat.ICiuLab).
    RUN TipIdentificacion.
   END.
  FIND Clientes WHERE Clientes.Nit = Creditos.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN
   DO:
    RUN CiuRes(INPUT Clientes.Lugar_Residencia, OUTPUT TmpDat.ICiuRes).
    ASSIGN TmpDat.IFecNac = DECIMAL(STRING(YEAR(Clientes.Fec_Nacimiento),"9999") + STRING(MONTH(Clientes.Fec_Nacimiento),"99")).
           TmpDat.IDirRes = SUBSTRING(Clientes.Dir_Residencia,1,30).
           TmpDat.IDirCor = SUBSTRING(Clientes.Dir_Residencia,1,30).
           TmpDat.ICiuCor = TmpDat.ICiuRes.
           TmpDat.ICiuRad = TmpDat.ICiuRes.
           TmpDat.ITelRes = STRING(Clientes.Tel_Residencia,"9999999999").
           TmpDat.ICiiu   = DECIMAL(Clientes.Codigo_CIIU).
    IF LENGTH(TmpDat.IDirRes) LT 30 THEN
     ASSIGN Z = 30 - LENGTH(TmpDat.IDirRes)
            TmpDat.IDirRes = TmpDat.IDirRes + SUBSTRING(C,1,Z).
    IF LENGTH(TmpDat.IDirCor) LT 30 THEN
     ASSIGN Z = 30 - LENGTH(TmpDat.IDirCor)
            TmpDat.IDirCor = TmpDat.IDirCor + SUBSTRING(C,1,Z).
    RUN Calificacion.
   END.
  ELSE
   DO:
    ASSIGN TmpDat.ICiuRes = "               "
           TmpDat.IFecNac = 0
           TmpDat.IDirRes = "               "
           TmpDat.IDirCor = "               "
           TmpDat.ICiuCor = "               "
           TmpDat.ICiuRad = "               "
           TmpDat.ITelRes = "0000000000"
           TmpDat.ICiiu   = 0.
    IF LENGTH(TmpDat.IDirRes) LT 30 THEN
     ASSIGN Z = 30 - LENGTH(TmpDat.IDirRes)
            TmpDat.IDirRes = TmpDat.IDirRes + SUBSTRING(C,1,Z).
    IF LENGTH(TmpDat.IDirCor) LT 30 THEN
     ASSIGN Z = 30 - LENGTH(TmpDat.IDirCor)
            TmpDat.IDirCor = TmpDat.IDirCor + SUBSTRING(C,1,Z).
   END.

  IF TmpDat.IFecNac = ? THEN
     TmpDat.IFecNac = DECIMAL(STRING(YEAR(FecCorte),"9999") + STRING(MONTH(FecCorte),"99")).
  FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
  IF AVAILABLE(Empresas) THEN
       RUN PeriodoEmpresa.
  ASSIGN
   TmpDat.INumCta = STRING(W_agencia,"99") + STRING(Creditos.Pagare,"X(18)")
   TmpDat.INumIde = Creditos.Nit
   TmpDat.IFecApe = DECIMAL(STRING(YEAR(Creditos.Fec_Desembolso),"9999") + STRING(MONTH(Creditos.Fec_Desembolso),"99"))
   TmpDat.ICuoMes = Creditos.Cuota
   TmpDat.IAdjeti = 0
   TmpDat.IValIni = Creditos.Monto
   TmpDat.ISdoCre = Creditos.Sdo_Capital
   TmpDat.IValDis = 0
   TmpDat.ITipMon = 1
   TmpDat.ITotCuo = Creditos.Plazo
   TmpDat.ICuoCan = Creditos.Cuo_Pagadas
   TmpDat.ISdoMor = 0
   TmpDat.IReclam = 0.
  IF TmpDat.IFecApe = 0 THEN
     TmpDat.IFecApe = DECIMAL(STRING(YEAR(FecCorte)) + STRING(MONTH(FecCorte),"99")).
  RUN Nomagencia (INPUT Creditos.agencia, OUTPUT TmpDat.IOfiRad).
  IF Creditos.Estado = 1 AND  Creditos.Sdo_Capital GT 0 THEN
    TmpDat.IForPag = 0.
  ELSE
    TmpDat.IForPag = 1.
  
  IF DAY(Creditos.Fec_Desembolso) GT DAY(FecCorte) THEN
     TmpDat.IFecAct = DECIMAL(STRING(YEAR(FecCorte),"9999") + STRING(MONTH(FecCorte),"99") + STRING(DAY(FecCorte),"99")).
  ELSE
     TmpDat.IFecAct = DECIMAL(STRING(YEAR(FecCorte),"9999") + STRING(MONTH(FecCorte),"99") + STRING(DAY(Creditos.Fec_Desembolso),"99")).


  FOR EACH Atrasos WHERE Atrasos.Pagare = Creditos.Pagare AND Atrasos.Nit = Creditos.Nit:
   VlrTmp = VlrTmp + Atrasos.Vlr_CapMor.
  END.
  ASSIGN TmpDat.ISdoMor = VlrTmp
         VlrTmp         = 0.
/*  RUN CodeudoresDtCr.*/
END.

CRegCon = "00000000000000000012345606" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + "M".
CRegFin = "zzzzzzzzzzzzzzzzzz" + STRING(YEAR(FecCorte)) + STRING(MONTH(FecCorte),"99") + 
          STRING(DAY(FecCorte),"99") + STRING(Prc,"99999999") + STRING(0,"99999999").
OUTPUT TO VALUE("C:\INFRED\" + STRING(W_agencia,"99") + "DATACR.TXT") NO-CONVERT.
PUT UNFORMATTED CRegCon.
FOR EACH TmpDat:
  PUT UNFORMATTED
   TmpDat.INumCta    AT 1   FORMAT "X(18)"
   TmpDat.INumIde    AT 19  FORMAT "X(11)"
   TmpDat.INomCli    AT 30  FORMAT "X(45)"
   TmpDat.IFecNac    AT 75  FORMAT "999999"
   TmpDat.IFecApe    AT 81  FORMAT "999999"
   TmpDat.IFecVen    AT 87  FORMAT "999999"
   TmpDat.ICuoMes    AT 93  FORMAT "9999999999"
   TmpDat.INoveda    AT 103 FORMAT "99"
   TmpDat.IAdjeti    AT 105 FORMAT "99"
   TmpDat.ITipIde    AT 107 FORMAT "9"
   TmpDat.IValIni    AT 108 FORMAT "9999999999"
   TmpDat.ISdoCre    AT 118 FORMAT "9999999999"
   TmpDat.ISdoMor    AT 128 FORMAT "9999999999"
   TmpDat.IValDis    AT 138 FORMAT "9999999999"
   TmpDat.ITipMon    AT 148 FORMAT "9"
   TmpDat.ITipCre    AT 149 FORMAT "9"
   TmpDat.ITipGar    AT 150 FORMAT "9"
   TmpDat.ICalifi    AT 151 FORMAT "X"
   TmpDat.ICiuRes    AT 152 FORMAT "X(15)"
   TmpDat.IDirRes    AT 167 FORMAT "X(30)"
   TmpDat.ITelRes    AT 197 FORMAT "X(10)"
   TmpDat.ICiuLab    AT 207 FORMAT "X(15)"
   TmpDat.ITelLab    AT 222 FORMAT "9999999999"
   TmpDat.ICiuCor    AT 232 FORMAT "X(15)"
   TmpDat.IDirCor    AT 247 FORMAT "X(30)"
   TmpDat.ICiiu      AT 277 FORMAT "999999"
   TmpDat.ITotCuo    AT 283 FORMAT "999"
   TmpDat.ICuoCan    AT 286 FORMAT "999"
   TmpDat.ICuoMor    AT 289 FORMAT "999"
   TmpDat.IFecPag    AT 292 FORMAT "99999999"
   TmpDat.IOfiRad    AT 300 FORMAT "X(15)"
   TmpDat.ICiuRad    AT 315 FORMAT "X(15)"
   TmpDat.IForPag    AT 330 FORMAT "9"
   TmpDat.IPerPag    AT 331 FORMAT "9"
   TmpDat.IEdaMor    AT 332 FORMAT "999"
   TmpDat.IFecAct    AT 335 FORMAT "99999999"
   TmpDat.IReclam    AT 343 FORMAT "99"
   TmpDat.IEstrat    AT 345 FORMAT "9"
   TmpDat.IFil       AT 346 FORMAT "X(14)".
END.
DISPLAY CRegFin NO-LABELS.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-arcext  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-arcext)
  THEN DELETE WIDGET w-arcext.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-arcext  _DEFAULT-ENABLE
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
  DISPLAY m1 m2 t1 t-2 t2 t3 t8 t-3 t-4 t-9 t4 t-5 T5 T-6 T6 T-7 t7 t-8 CED Prc 
      WITH FRAME frm0 IN WINDOW w-arcext.
  ENABLE RECT-25 RECT-26 RECT-27 RECT-28 BUTTON-3 BUTTON-2 t1 t-2 t2 t3 t8 t-3 
         t-4 t-9 t4 t-5 T5 T-6 T6 T-7 t7 t-8 BUTTON-4 BUTTON-5 
      WITH FRAME frm0 IN WINDOW w-arcext.
  {&OPEN-BROWSERS-IN-QUERY-frm0}
  DISPLAY FecCorte 
      WITH FRAME FrM IN WINDOW w-arcext.
  ENABLE FecCorte Btn_Done 
      WITH FRAME FrM IN WINDOW w-arcext.
  {&OPEN-BROWSERS-IN-QUERY-FrM}
  VIEW w-arcext.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Fgbrecha w-arcext 
PROCEDURE Fgbrecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR NomPdt AS CHARACTER FORMAT "X(15)".
DEFINE VAR Prom   AS DECIMAL FORMAT ">>.99".
DEFINE VAR Tot    AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9".
ASSIGN FECHA1 = FecCorte.
DO I = 1 TO 8 BY 1:
  CASE i:
        WHEN 1 THEN
                run hallarango ((FECHA1 - 999),(FECHA1 + 30)).
        WHEN 2 THEN  
                run hallarango ((FECHA1 + 31),(FECHA1 + 60)).
        WHEN 3 THEN  
                run hallarango ((FECHA1 + 61),(FECHA1 + 90)).
        WHEN 4 THEN
                run hallarango ((FECHA1 + 91),(FECHA1 + 120)).
        WHEN 5 THEN  
                run hallarango ((FECHA1 + 121),(FECHA1 + 180)).
        WHEN 6 THEN  
                run hallarango ((FECHA1 + 181),(FECHA1 + 270)).
        WHEN 7 THEN  
                run hallarango ((FECHA1 + 271),(FECHA1 + 360)).
        WHEN 8 THEN  
                run hallarango ((FECHA1 + 361),(FECHA1 + 9999)).
   END CASE.
END.

/******/
/* AHORROS A LA VISTA*/
/*******/
 ASSIGN Xsumavalor = 0
        xsumatasa  = 0
        xtitulos   = 0.

for each ahorros WHERE AHORROS.ESTADO = 1 AND SDO_DISPONIBLE > 0 AND AHORROS.COD_Ahorro = 2:
    ASSIGN XSUMAVALOR = XSUMAVALOR + SDO_DISPONIBLE + SDO_CANJE + int_pagar
           XSUMATASA  = XSUMATASa + TASA
           XTITULOS   = XTITULOS + 1.
end.
create brecha.
assign brecha.producto = 3
       brecha.Banda    = i
       brecha.valor    = xsumavalor
       brecha.tasa     = 0.20 * XTITULOS
       brecha.Nro      = xtitulos.

OUTPUT TO C:\INFRED\BRECHA.txt.

DISPLAY "BRECHA DE LIQUIDEZ" AT 25 skip
        "MES : "             AT 5
        MONTH(FecCorte)      At 16 skip(1)
        "PRODUCTO             BANDA           VALOR          TASA PROMEDIO " AT 1
        "__________________________________________________________________" AT 1.
        
FOR EACH brecha BREAK BY Brecha.Producto BY Banda:
  IF FIRST-OF(Brecha.Producto) THEN
  DO:
    CASE Brecha.Producto:
      WHEN 1 THEN
        NomPdt = "Ahorros".
      WHEN 2 THEN
        NomPdt = "Creditos".
      WHEN 3 THEN
        NomPdt = "A la Vista".
    END CASE.
  END.
  ELSE
   NomPdt = "".
  Prom = (Brecha.Tasa / Brecha.Nro) * 12.
  DISPLAY NomPdt      AT 1
          banda       AT 17
          valor       AT 30 FORMAT ">,>>>,>>>,>>>,>>9"
          Prom        AT 60 FORMAT ">>9.999" WITH WIDTH 132 NO-BOX USE-TEXT NO-LABELS.    
  Tot = Tot + Valor.
  IF LAST-OF(Brecha.Producto) THEN
  DO:
     DISPLAY SKIP(1) 
             "Total: " AT 1
             Tot AT 29 SKIP(2) 
             WITH WIDTH 132 FRAME Ftot USE-TEXT NO-BOX NO-LABELS.
     Tot = 0.
  END.     
END.
OUTPUT TO CLOSE.

MESSAGE "fIN DE PROCESO  " SKIP(1)
         VIEW-AS ALERT-BOX 
         TITLE "" UPDATE choice3 AS LOGICAL.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hallarango w-arcext 
PROCEDURE hallarango :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter xfec1 as date.
define input parameter xfec2 as date.
ASSIGN Xsumavalor = 0
       xsumatasa  = 0
       xtitulos   = 0.

/*****************/
/*  HALLA AHORROS*/
/*****************/
for each ahorros WHERE FEC_VENCIMIENTO GE XFEC1 AND 
                       FEC_VENCIMIENTO LE XFEC2 AND
                       AHORROS.ESTADO = 1 AND (
                       AHORROS.COD_Ahorro = 5 OR AHORROS.COD_Ahorro = 3):

    ASSIGN XSUMAVALOR = XSUMAVALOR + SDO_DISPONIBLE + SDO_CANJE + int_pagar
           XTITULOS   = XTITULOS + 1.
    CASE Ahorros.Per_Liquidacion:
         WHEN 0 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 360) / 12).
         WHEN 1 THEN
           XSUMATASA = XSUMATASA + (( Ahorros.Tasa * 52) / 12).
         WHEN 2 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 36) / 12).
         WHEN 3 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 26) / 12).
         WHEN 4 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 12) / 12).
         WHEN 5 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 6) / 12).
         WHEN 6 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 4) / 12).
         WHEN 7 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 3) / 12).
         WHEN 8 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 2) / 12).
         WHEN 9 THEN
           XSUMATASA = XSUMATASA + (Ahorros.Tasa) / 12.
         WHEN 10 THEN
           XSUMATASA = XSUMATASA + ((Ahorros.Tasa * 360) / 12).
     END CASE.

end.
create brecha.
assign brecha.producto = 1
       brecha.Banda    = I
       brecha.valor    = xsumavalor
       brecha.tasa     = xsumatasa
       brecha.Nro      = xtitulos.

/******************/
/*  hALLA CREDITOS*/
/******************/

 ASSIGN Xsumavalor = 0
        xsumatasa  = 0
        xtitulos   = 0.

for each CREDITOS WHERE (FEC_DESEMBOLSO + (CREDITOS.PLAZO * 30)) GE XFEC1 AND 
                       CREDITOS.ESTADO = 1 :

    IF I = 8 THEN DO:
       IF CREDITOS.SDO_CAPITAL > (CREDITOS.CUOTA * 10) THEN 
          ASSIGN XSUMAVALOR = XSUMAVALOR + (CREDITOS.SDO_CAPITAL - (CREDITOS.CUOTA * 10))
                 XSUMATASA  = XSUMATASa + CREDITOS.TASA
                 XTITULOS   = XTITULOS + 1.

    END.
    ELSE DO:
       ASSIGN XSUMAVALOR = XSUMAVALOR + CREDITOS.CUOTA
              XSUMATASA  = XSUMATASa + CREDITOS.TASA
              XTITULOS   = XTITULOS + 1.
    END.
    
end.
create brecha.
assign brecha.producto = 2
       brecha.Banda    = i
       brecha.valor    = xsumavalor
       brecha.tasa     = xsumatasa
       brecha.Nro      = xtitulos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Nomagencia w-arcext 
PROCEDURE Nomagencia :
DEFINE INPUT  PARAMETER agencia LIKE agencias.agencia.
 DEFINE OUTPUT PARAMETER OfiBak  AS CHARACTER FORMAT "X(15)".
 FIND agencias WHERE agencias.agencia EQ agencia NO-LOCK NO-ERROR.
 IF AVAILABLE(agencias) THEN
  OfiBak = agencias.Nombre.
 ELSE
  OfiBak = "MEDELLIN".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PeriodoEmpresa w-arcext 
PROCEDURE PeriodoEmpresa :
DEFINE VAR NumDias AS DECIMAL FORMAT "99999".
 DEFINE VAR FecAuxi AS DATE.
 DEFINE VAR NCuoAtr AS DECIMAL FORMAT "99999".
 DEFINE VAR NCuoEmp AS DECIMAL FORMAT "99999". 
 DEFINE VAR Multdor AS DECIMAL FORMAT "99".
 DEFINE VAR DifAno  AS DECIMAL FORMAT "99".
 DEFINE VAR DifCuo  AS DECIMAL FORMAT "99".

 CASE Empresas.For_Pago:
  WHEN 1 THEN ASSIGN NumDias = Creditos.Plazo * 7
                     NCuoEmp = 52
                     Multdor = 7
                     TmpDat.IPerPag = 9.
  WHEN 2 THEN ASSIGN NumDias = Creditos.Plazo * 10
                     NCuoEmp = 36
                     MultDor = 10
                     IPerPag = 9.
  WHEN 3 THEN ASSIGN NumDias = Creditos.Plazo * 15
                     NCuoEmp = 26
                     MultDor = 15
                     IPerPAg = 9.
  WHEN 4 THEN ASSIGN NumDias = Creditos.Plazo * 30
                     NCuoEmp = 12
                     Multdor = 30
                     IPerPag = 1.
 END CASE.
 ASSIGN FecAuxi = Creditos.Fec_Desembolso + NumDias
        IFecVen = DECIMAL(STRING(YEAR(FecAuxi)) + STRING(MONTH(FecAuxi),"99")). 

 NCuoAtr = 10.
 ASSIGN ICuoMor = NCuoAtr
        IFecPag = 0.
  ASSIGN ICuoMor = 0
         IFecPag = 0
         INoveda = 0
         ICalifi = "A"
         IEdaMor = 0.
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesar w-arcext 
PROCEDURE Procesar :
ASSIGN FRAME frm0 t1 t2 t3 t4 t5 t6 t7 t8.
IF T1 THEN DO:
   m2 = "C:\infred\DATACR.TXT".
   DISPLAY "Generando Archivo Creditos DATACREDITO..." @ m1 m2 WITH FRAME Frm0.
   RUN DataCredito.
   t1 = TRUE.
   DISPLAY T1 WITH FRAME Frm0.
END.
IF T2 THEN DO:
   m2 = "C:\infred\FGCAHO.TXT".
   DISPLAY "Generando Archivo Ahorros FOGACOOP..." @ m1 m2 WITH FRAME Frm0.
   RUN AhoFgc.
   t2 = TRUE.
   DISPLAY T2 WITH FRAME Frm0.
END.
IF T3 THEN DO:
   m2 = "C:\infred\FGCcre.TXT".
   DISPLAY "Generando Archivo Creditos FOGACOOP..." @ m1 m2 WITH FRAME Frm0.
   RUN CreFgc.
   t3 = TRUE.
   DISPLAY T3 WITH FRAME Frm0.
END.
IF T4 THEN DO:
   m2 = "C:\infred\SSAho.csv".
   DISPLAY "Generando Archivo Ahorros SUPERSOLIDARIA..." @ m1 m2 WITH FRAME Frm0.
   RUN SSAhorro.
   t4 = TRUE.
   DISPLAY T4 WITH FRAME Frm0.
END.
IF T5 THEN DO:
   m2 = "C:\infred\SSApo.csv".
   DISPLAY "Generando Archivo Aportes SUPERSOLIDARIA..." @ m1 m2 WITH FRAME Frm0.
   RUN SSAportes.
   t5 = TRUE.
   DISPLAY T5 WITH FRAME Frm0.
END.
IF T6 THEN DO:
   m2 = "C:\infred\SSCre.csv".
   DISPLAY "Generando Archivo Creditos SUPERSOLIDARIA..." @ m1 m2 WITH FRAME Frm0.
   RUN SSCreditos.
   t6 = TRUE.
   DISPLAY T6 WITH FRAME Frm0.
END.
IF T7 THEN DO:
   m2 = "C:\infred\SScli.csv".
   DISPLAY "Generando Archivo Clientes SUPERSOLIDARIA..." @ m1 m2 WITH FRAME Frm0.
   RUN SScliente.
   t6 = TRUE.
   DISPLAY T6 WITH FRAME Frm0.
END.
IF T8 THEN DO:
   m2 = "C:\infred\FGCbre.TXT".
   DISPLAY "Generando Archivo BRECHA DE LIQUIDEZ FOGACOOP..." @ m1 m2 WITH FRAME Frm0.
   RUN FGBRECHA.
   t6 = TRUE.
   DISPLAY T6 WITH FRAME Frm0.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SSAhorro w-arcext 
PROCEDURE SSAhorro :
/* Purpose:  Genera archivo plano de ahorro para SUPERSOLIDARIA */

 Assign Prc = 0
        Acum = 0
        Xacum = 0.

 FOR EACH Ahorros WHERE (Ahorros.Sdo_Disponible + Sdo_Canje) GT 0 AND
                        Ahorros.Estado         EQ 1 NO-LOCK:
   FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
   IF Pro_Ahorros.Tip_Ahorro EQ 4 THEN NEXT.
   ASSIGN Prc = Prc + 1
          Ced = Ahorros.Nit.
   DISPLAY Prc Ced WITH NO-LABELS FRAME FRM0.
   CREATE SSAho.
   CASE Pro_Ahorros.Tip_Ahorro:
    WHEN 1 THEN
      ASSIGN SSAho.SA_NomDep = "A la Vista"
             SSAho.SA_TipAho = "1"
             SSAho.SA_CodCon = 2105
             SSAho.SA_Plazo = 999
             SSAho.SA_Amorti = 1
             SSAho.SA_TasNom = 2.21.
    WHEN 2 THEN
      ASSIGN SSAho.SA_NomDep = "Contractual"
             SSAho.SA_TipAho = "3"
             SSAho.SA_CodCon = 2125
             SSAho.SA_Plazo = (Ahorros.Plazo * 30)
             SSAho.SA_Amorti = 30
             SSAho.SA_TasNom = 7.44.
    WHEN 3 THEN DO:
      ASSIGN SSAho.SA_NomDep = "CDAT"
             SSAho.SA_TipAho = "2"
             SSAho.SA_CodCon = 2110.
      CASE Ahorros.Per_Liquidacion:
           WHEN 0  THEN DO:
                    ASSIGN SSAho.SA_Plazo  = 1
                           SSAho.SA_Amorti = 1
                           SSAho.SA_TasNom = Ahorros.Tasa * 360.
                    END.
           WHEN 1  THEN DO:
                   ASSIGN SSAho.SA_Plazo = Ahorros.Plazo * 7
                          SSAho.SA_Amorti = 7
                          SSAho.SA_TasNom = Ahorros.Tasa * 52.
                   END.
           WHEN 2  THEN DO:
                   ASSIGN SSAho.SA_Plazo = Ahorros.Plazo * 10
                          SSAho.SA_Amorti = 10
                          SSAho.SA_TasNom = Ahorros.Tasa * 36.
                   END.
           WHEN 3  THEN DO:
                   ASSIGN SSAho.SA_Plazo = Ahorros.Plazo * 15
                          SSAho.SA_Amorti = 15
                          SSAho.SA_TasNom = Ahorros.Tasa * 26.
                    END.
           WHEN 4  THEN DO:
                   ASSIGN SSAho.SA_Plazo = Ahorros.Plazo * 30
                           SSAho.SA_Amorti = 30
                           SSAho.SA_TasNom = Ahorros.Tasa * 12.
                    END.
           WHEN 5  THEN DO:
                   ASSIGN SSAho.SA_Plazo = Ahorros.Plazo * 60
                          SSAho.SA_Amorti = 60
                          SSAho.SA_TasNom = Ahorros.Tasa * 6.
                   END.
           WHEN 6  THEN DO:
                   ASSIGN SSAho.SA_Plazo = Ahorros.Plazo * 90
                          SSAho.SA_Amorti = 90
                          SSAho.SA_TasNom = Ahorros.Tasa * 4.
                    END.
           WHEN 7  THEN DO:
                   ASSIGN SSAho.SA_Plazo = Ahorros.Plazo * 120
                          SSAho.SA_Amorti = 120
                          SSAho.SA_TasNom = Ahorros.Tasa * 3.
                   END.
           WHEN 8  THEN DO:
                   ASSIGN SSAho.SA_Plazo = Ahorros.Plazo * 180
                          SSAho.SA_Amorti = 180
                          SSAho.SA_TasNom = Ahorros.Tasa * 2.
                   END.
           WHEN 9  THEN DO:
                   ASSIGN SSAho.SA_Plazo = Ahorros.Plazo * 360
                          SSAho.SA_Amorti = 360
                          SSAho.SA_TasNom = Ahorros.Tasa * 1.
                   END.
           WHEN 10 THEN DO:
                   ASSIGN SSAho.SA_Plazo = Ahorros.Plazo
                          SSAho.SA_Amorti = Ahorros.plazo
                          SSAho.SA_TasNom = Ahorros.Tasa.
                   END.
      END CASE.
      RUN NVEF IN W_ManFin (INPUT Ahorros.Tasa / 100, Ahorros.Plazo, OUTPUT SSAho.SA_TasEfe) NO-ERROR.
    END.
   END CASE.
   Assign SSAho.SA_TasEfe = SSAho.SA_TasNom.
   ASSIGN SSAho.SA_TipIde = "C"
          SSAho.SA_CedNit = DECIMAL(Ahorros.Nit).
   IF Ahorros.Fec_Apertura > Ahorros.Fec_vencimiento then
      Assign Ahorros.Fec_apertura = Ahorros.fec_vencimiento - 1.
   IF Ahorros.Fec_Apertura EQ ? THEN
      Assign SSAho.SA_FecApe = "31/12/1999".
   ELSE
      Assign SSAho.SA_FecApe = STRING(DAY(Ahorros.Fec_Apertura),"99")    + "/" +
                               STRING(MONTH(Ahorros.Fec_Apertura),"99")  + "/" +
                               STRING(YEAR(Ahorros.Fec_Apertura),"9999"). 
   
   IF Ahorros.Fec_Vencimiento EQ ? THEN
      Assign SSAho.SA_FecVen = "31/12/2003".
   ELSE                        
      Assign SSAho.SA_FecVen = STRING(DAY(Ahorros.Fec_Vencimiento),"99")    + "/" +
                               STRING(MONTH(Ahorros.Fec_Vencimiento),"99") + "/" +
                               STRING(YEAR(Ahorros.Fec_Vencimiento),"9999").

    IF Ahorros.Fec_Vencimiento < FecCorte THEN
      Assign SSAho.SA_FecVen = "31/12/2003".

    SSAho.SA_Modali = 2.
    SSAho.SA_Saldo  = Ahorros.Sdo_Disponible + sdo_canje.
    SSAho.SA_ValIni = Ahorros.Sdo_Disponible + sdo_canje.
    SSAho.SA_NumCta = DECIMAL(Ahorros.Cue_Ahorros).
    SSAho.SA_intpag = Ahorros.Int_causado.
   FIND Terceros WHERE Terceros.Nit EQ Ahorros.Nit NO-LOCK NO-ERROR.
   IF AVAILABLE(Terceros) THEN
      ASSIGN SSaho.SA_NomNit = Terceros.Nombre.
   ELSE ASSIGN SSaho.SA_NomNit = "NOMBRE NO REGISTRADO".

 END.
OUTPUT TO VALUE("C:\INFRED\" + STRING(W_agencia,"99") + "SSAho.csv") NO-CONVERT.
FOR EACH SSAho:
     Assign acum = acum + SSAho.SA_saldo.
     CAMPO0 = SA_TipIde + ";" +
              STRING(SA_CedNit) + ";" +
              STRING(SA_CodCon) + ";" +
              SA_NomDep         + ";" +
              SA_TipAho         + ";" +
              STRING(SA_Amorti) + ";" + 
              SA_FecApe         + ";" +
              STRING(SA_Plazo)  + ";" +
              SA_FecVen         + ";" +
              STRING(SA_Modali) + ";" +
              STRING(SA_TasNom) + ";" +
              STRING(SA_TasEfe) + ";" +
              STRING(SA_INTPAG) + ";" +
              STRING(SA_Saldo)  + ";" +
              STRING(SA_ValIni) + ";" +
              STRING(SA_NumCta) .
     PUT UNFORMATTED
         CAMPO0 AT 1.
END.
OUTPUT CLOSE.
OUTPUT TO VALUE("C:\INFRED\" + "TOTSSAHO.txt") NO-CONVERT.
display "Total Captacion " Acum.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SSAportes w-arcext 
PROCEDURE SSAportes :
/*- Purpose:     Imprime Archivo plano de Aportes para SUPERSOLIDARIA -*/

 assign Prc = 0
        acum = 0.
 FOR EACH Ahorros WHERE Ahorros.Cod_Ahorro EQ 1 AND
                        (Ahorros.Sdo_Disponible + sdo_canje) GT 0 AND
                        Ahorros.Estado         EQ 1 NO-LOCK:
   ASSIGN Prc = Prc + 1
          Ced = Ahorros.Nit.
   DISPLAY Prc Ced WITH NO-LABELS FRAME FRM0.
   CREATE SSApo.
   ASSIGN SSApo.SS_TipIde = "C"
          SSApo.SS_NumNit = DECIMAL(Ahorros.Nit)
          SSApo.SS_Saldo  = Ahorros.Sdo_Disponible + Ahorros.Sdo_canje.
 END.
OUTPUT TO VALUE("C:\INFRED\" + STRING(W_agencia,"99") + "SSApo.csv") NO-CONVERT.
FOR EACH SSApo:
   Assign acum = acum + SSApo.SS_saldo.
   CAMPO0 = SS_TipIde + ";" +
              STRING(SS_NumNit) + ";" +
              STRING(SS_Saldo) + ";" + "0".
   PUT UNFORMATTED
       CAMPO0 AT 1.
END.
OUTPUT CLOSE.
OUTPUT TO VALUE("C:\INFRED\" + "TOTSSAPO.txt") NO-CONVERT.
display "Total Aporte " Acum.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sscliente w-arcext 
PROCEDURE sscliente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH Terceros NO-lock:
 find first clientes where clientes.nit = terceros.nit no-error.
 IF AVAILABLE(CLIENTES) THEN DO:
   ASSIGN Prc = Prc + 1
          Ced = Terceros.Nit.
   DISPLAY Prc Ced WITH NO-LABELS FRAME FRM0.
   CREATE SSCli.
   ASSIGN SScli.Sc_TipIde = "C"
          SScli.Sc_Numnit = DECIMAL(Terceros.Nit)
          SScli.Sc_tel    = "2345453"
          SScli.Sc_dir    = TERCEROS.DIR_comercial
          SScli.Sc_soc    = "1"
          SScli.Sc_ciiu   = "0000"
          SScli.Sc_mun    = "05001".

   ASSIGN W_Posicion      = INDEX(Terceros.Apellido, "_")
          SScli.Sc_Apell1 = SUBSTRING(Terceros.Apellido, 1, W_Posicion - 1).
   IF SScli.Sc_Apell1     = " " THEN SScli.Sc_Apell1 = "Apellido".

   IF Terceros.Nombre ne " " then
      Assign SScli.Sc_Nomnit   = Terceros.Nombre.
   Else Assign SScli.Sc_Nomnit = SScli.Sc_Apell1.
      
   ASSIGN SScli.Sc_Apell2 = IF W_Posicion NE 0 THEN 
                                 SUBSTRING(Terceros.Apellido, W_Posicion + 1, 30)
                              ELSE " ".
  IF SScli.Sc_Apell2 = " " THEN SScli.Sc_Apell2 = "Apellido".

  IF (Terceros.Fec_Creacion EQ ?)  or (Terceros.Fec_creacion > FecCorte) THEN
      Assign SScli.Sc_FecApe = "31/12/1994".
   ELSE
      Assign SScli.Sc_FecApe = STRING(DAY(Terceros.Fec_Creacion),"99")    + "/" +
                               STRING(MONTH(Terceros.Fec_Creacion),"99")  + "/" +
                                STRING(YEAR(Terceros.Fec_Creacion),"9999"). 
 END.
END.
OUTPUT TO VALUE("C:\INFRED\" + STRING(W_agencia,"99") + "SScli.CSV") NO-CONVERT.
FOR EACH SScli:
    CAMPO0 = SScli.Sc_TipIde + ";" +
             string(SScli.Sc_Numnit) + ";" +
             SScli.Sc_Apell1 + ";" +
             SScli.Sc_Apell2 + ";" +
             SScli.Sc_Nomnit + ";" +
             SScli.Sc_FecApe + ";" + 
             SScli.Sc_tel    + ";" +
             SScli.Sc_dir    + ";" +
             SScli.Sc_soc    + ";" +
             "1"             + ";" +   
             SScli.Sc_ciiu   + ";" +
             SScli.Sc_mun.
     PUT UNFORMATTED
         CAMPO0 AT 1.
END.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SSCreditos w-arcext 
PROCEDURE SSCreditos :
/*- Purpose: Genera archivo plano de creditos para SUPERSOLIDARIA */
 Assign  Prc = 0
         acum = 0.

 FOR EACH Creditos WHERE Creditos.Sdo_Capital    GT 0 AND
                         Creditos.Fec_Desembolso LE FecCorte AND
                         Creditos.Estado         EQ 1 NO-LOCK:
   FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
   IF Pro_Creditos.Tip_Credito EQ 4 THEN NEXT.
   ASSIGN Prc = Prc + 1
          Ced = Creditos.Nit.
   DISPLAY Prc Ced WITH NO-LABELS FRAME FRM0.
   CREATE SSCre.
   ASSIGN SSCre.SC_TipIde = "C"
          SSCre.SC_CodCon = 144105
          SSCre.SC_CedNit = DECIMAL(Creditos.Nit)
          SSCre.SC_TipCuo = 1
          SSCre.SC_FecApe = STRING(DAY(Creditos.Fec_Desembolso),"99") + "/" +
                            STRING(MONTH(Creditos.Fec_Desembolso),"99") + "/" +
                            STRING(YEAR(Creditos.Fec_Desembolso),"9999") 
          SSCre.SC_ValCuo = Creditos.Cuota
          SSCre.SC_CuoPag = Creditos.Cuo_Pagadas
          SSCre.SC_Pagare = DECIMAL(Creditos.Pagare)
          SSCre.SC_SdoCap = Creditos.Sdo_Capital
          SSCre.SC_Monto  = Creditos.Monto
          SSCre.SC_SdoOtr = 0
          SSCre.SC_SdoInt = Creditos.Int_Corrientes
          SSCre.SC_VlrPro = 0
          SSCre.SC_ConInt = 0
          SSCre.SC_VlrExt = 0
          SSCre.SC_MesExt = 0
          SSCre.SC_Modali = 2
          SSCre.SC_TasNom = Creditos.Tasa * 12.
          
   if creditos.plazo > 12 then SSCre.SC_PLAZO  = 2.
   else SSCre.SC_PLAZO  = 1.


   FIND Terceros WHERE Terceros.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
   IF AVAILABLE(Terceros) THEN
      ASSIGN SSCre.SC_NomNit = Terceros.Nombre.
   ELSE ASSIGN SSCre.SC_NomNit = "NOMBRE NO REGISTRADO".

  FIND FIRST Garantias WHERE Garantias.Num_credito = Creditos.Num_Credito NO-LOCK NO-ERROR.
  IF AVAILABLE(Garantias) THEN DO:
    IF Garantias.Tipo_Garantia = 1 THEN DO:
      CASE Garantias.Tipo_Garantia:
       WHEN 1 THEN SSCre.SC_ClaGar = "1".
       WHEN 2 THEN SSCre.SC_ClaGar = "1".
       WHEN 3 THEN SSCre.SC_ClaGar = "1".
       WHEN 4 THEN SSCre.SC_ClaGar = "1".
       OTHERWISE SSCre.SC_ClaGar = "1".
      END CASE.
    END.
    IF Garantias.Tipo_Garantia = 2 THEN SSCre.SC_ClaGar = "1".
  END.
  Assign SSCre.SC_ClaGar = "1".
  RUN NVEF IN W_ManFin (INPUT Creditos.Tasa / 100, Creditos.Plazo, OUTPUT SSCre.SC_TasEfe) NO-ERROR.          
  SSCre.SC_TasEfe = Creditos.Tasa * 12.
  CASE Pro_Creditos.Tip_Credito:
    WHEN 1 THEN SSCre.SC_Clasif = 2. /*Consumo*/
    WHEN 2 THEN SSCre.SC_Clasif = 1. /*Comercial*/
    WHEN 2 THEN SSCre.SC_Clasif = 3. /*Hipotecario*/
    WHEN 4 THEN SSCre.SC_Clasif = 4. /*Sin Identificar*/
  END CASE.
  Assign SSCre.SC_Clasif = 2.
  CASE Creditos.categoria:
   WHEN "A" THEN ASSIGN SSCre.SC_EdaMor  = 0
                      SSCre.SC_Catego = "A".
   WHEN "A" THEN ASSIGN SSCre.SC_EdaMor = 0
                      SSCre.SC_Catego = "A".
   WHEN "B" THEN ASSIGN SSCre.SC_EdaMor = 31
                      SSCre.SC_Catego = "B".
   WHEN "C" THEN 
    DO:
     SSCre.SC_Catego = "C".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN SSCre.SC_EdaMor = 61.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN SSCre.SC_EdaMor = 121.
    END.
   WHEN "D" THEN 
    DO:
     SSCre.SC_Catego = "D".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN SSCre.SC_EdaMor = 91.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN SSCre.SC_EdaMor = 181.
    END.
   WHEN "E" THEN
    DO:
     SSCre.SC_Catego = "E".
     IF Pro_Creditos.Tip_Credito EQ 1 THEN SSCre.SC_EdaMor = 181.
     IF Pro_Creditos.Tip_Credito EQ 3 THEN SSCre.SC_EdaMor = 361.
    END.
    OTHERWISE ASSIGN SSCre.SC_EdaMor  = 0
                      SSCre.SC_Catego = "A".
  END CASE. 
  CASE Creditos.Per_Pago:
   WHEN 1 THEN ASSIGN SSCre.SC_Amorti = 2
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 7)).
   WHEN 2 THEN ASSIGN SSCre.SC_Amorti = 2
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 10)).
   WHEN 3 THEN ASSIGN SSCre.SC_Amorti = 3
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 15)).
   WHEN 4 THEN ASSIGN SSCre.SC_Amorti = 4
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 30)).
   WHEN 5 THEN ASSIGN SSCre.SC_Amorti = 5
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 60)).
   WHEN 6 THEN ASSIGN SSCre.SC_Amorti = 6
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 90)).
   WHEN 7 THEN ASSIGN SSCre.SC_Amorti = 6
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 120)).
   WHEN 8 THEN ASSIGN SSCre.SC_Amorti = 7
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 180)).
   WHEN 9 THEN ASSIGN SSCre.SC_Amorti = 1
                      FecAux = (Creditos.Fec_Desembolso + (Creditos.Plazo * 360)).
  END CASE.
  SSCre.SC_Amorti = 30.
  SSCre.SC_FecVen = STRING(DAY(FecAux),"99") + "/" +
                    STRING(MONTH(FecAux),"99")  + "/" +
                    STRING(YEAR(FecAux),"9999").
END.
OUTPUT TO VALUE("C:\INFRED\" + STRING(W_agencia,"99") + "SSCre.csv") NO-CONVERT.
 FOR EACH SSCre:
     assign acum = acum + SSCre.SC_sdocap.
     Assign CAMPO0 = SC_TipIde           + ";" +
                     STRING(SC_CedNit)   + ";" +
                     STRING(SC_CodCon)   + ";" +
                     "0"                 + ";" +
                     STRING(SC_Pagare)   + ";" +
                     SC_FecApe           + ";" +
                     SC_FecVen           + ";" +
                     STRING(SC_EdaMor)   + ";" +
                     STRING(SC_TipCuo)   + ";" +
                     STRING(SC_CuoPag)   + ";" +
                     STRING(SC_Amorti)   + ";" +
                     STRING(SC_Modali)   + ";" +
                     STRING(SC_TasNom)   + ";" +
                     STRING(SC_TasEfe)   + ";" +
                     STRING(SC_Monto)    + ";" +
                     STRING(SC_ValCuo)   + ";" +
                     STRING(SC_SdoCap)   + ";" +
                     STRING(SC_SdoInt)   + ";" +                                              
                     STRING(SC_SdoOtr)   + ";" +
                     "0"                 + ";" +
                     SC_FecApe           + ";" +
                     STRING(SC_VlrPro)   + ";" +
                     "0"                 + ";" +
                     STRING(SC_ConInt)   + ";" +
                     STRING(SC_VlrExt)   + ";" +
                     STRING(SC_MesExt).
   PUT UNFORMATTED
       CAMPO0 AT 1.
END.
OUTPUT CLOSE.
OUTPUT TO VALUE("C:\INFRED\" + "TOTSSCre.txt") NO-CONVERT.
  display "Total Creditos " Acum .
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TipIdentificacion w-arcext 
PROCEDURE TipIdentificacion :
CASE Terceros.Tipo_Identificacion:
  WHEN 1 THEN TmpDat.ITipIde = 1.
  WHEN 2 THEN TmpDat.ITipIde = 4.
  WHEN 3 THEN TmpDat.ITipIde = 2.
  OTHERWISE   TmpDat.ITipIde = 1.
 END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

