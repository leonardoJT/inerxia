&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME W-Cierres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Cierres 
/*------------------------------------------------------------------------
  File:        W-Prc_Auto.W 
  Description: Procesos del Cierre diario
  Author:      GAER
  Created:     Feb.17/2004
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

   ON RETURN TAB.

   {Incluido/Variable.I "SHARED"}

   {Incluido/VARCON.I   "SHARED"}
        
   DEFI VAR W_ProgEjec LIKE Programas.Ejecutable.
   DEFI VAR AgIni      LIKE Agencias.Agencia.
   DEFI VAR AgFin      LIKE Agencias.Agencia.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Cons
&Scoped-define BROWSE-NAME Br_Proc

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ProcDia Varios

/* Definitions for BROWSE Br_Proc                                       */
&Scoped-define FIELDS-IN-QUERY-Br_Proc ProcDia.Agencia ProcDia.Cod_Proceso Varios.Descrip ProcDia.Fecha_Proc ProcDia.Estado   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Proc   
&Scoped-define SELF-NAME Br_Proc
&Scoped-define QUERY-STRING-Br_Proc FOR EACH ProcDia WHERE ProcDia.Agenc EQ 0 NO-LOCK, ~
                                   EACH Varios  WHERE Varios.Tipo   EQ 8                                           AND Varios.Codigo  EQ ProcDia.Cod_Proceso                                           AND Varios.Estado  EQ 1                          NO-LOCK   INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Proc OPEN QUERY {&SELF-NAME} FOR EACH ProcDia WHERE ProcDia.Agenc EQ 0 NO-LOCK, ~
                                   EACH Varios  WHERE Varios.Tipo   EQ 8                                           AND Varios.Codigo  EQ ProcDia.Cod_Proceso                                           AND Varios.Estado  EQ 1                          NO-LOCK   INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Proc ProcDia Varios
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Proc ProcDia
&Scoped-define SECOND-TABLE-IN-QUERY-Br_Proc Varios


/* Definitions for FRAME F_Cons                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Br_Proc Btn_FinImp W_FecCons W_AgCons 
&Scoped-Define DISPLAYED-OBJECTS W_FecCons W_AgCons 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 W_MesC W_AnoC 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Cierres AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_FinImp 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "&Regresar" 
     SIZE 8.72 BY 1.42 TOOLTIP "Regresa a la ventana principal de Procesos".

DEFINE VARIABLE W_AgCons AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Agencia a Consultar" 
     VIEW-AS FILL-IN 
     SIZE 4.43 BY .81 TOOLTIP "Agencia a Consultar, con cero todas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_FecCons AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Consulta" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE BUTTON Btn_Consul 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "&Consultar" 
     SIZE 10 BY 1.62 TOOLTIP "Consulta Procesos".

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.42
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE Cmb_proc AS CHARACTER FORMAT "X(50)":U 
     LABEL "Procesos del Dia" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 33.72 BY 1 TOOLTIP "Selecciones el Proceso a Ejecutar"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 33.86 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_AnoC AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DiaC AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Dia" 
     VIEW-AS FILL-IN 
     SIZE 3.29 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_MesC AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Mes" 
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Nmes AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-120
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.72 BY 6.73.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 6.73.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Proc FOR 
      ProcDia, 
      Varios SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Proc W-Cierres _FREEFORM
  QUERY Br_Proc NO-LOCK DISPLAY
      ProcDia.Agencia FORMAT "999":U          COLUMN-LABEL "Ag."
      ProcDia.Cod_Proceso FORMAT "99999":U
      Varios.Descrip FORMAT "X(25)":U         COLUMN-LABEL "Descripciòn del Proceso" 
      ProcDia.Fecha_Proc FORMAT "99/99/9999":U
      ProcDia.Estado FORMAT "9":U             COLUMN-LABEL "Estado-Ejec."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 52.72 BY 4.5
         BGCOLOR 18 FGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Cons
     Br_Proc AT ROW 1.08 COL 2.14
     Btn_FinImp AT ROW 5.77 COL 46.29
     W_FecCons AT ROW 6.04 COL 11.72 COLON-ALIGNED
     W_AgCons AT ROW 6.04 COL 37.72 COLON-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.57 ROW 5.54
         SIZE 55.86 BY 7.04
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Procesos Configurados".

DEFINE FRAME F_Proc
     BUTTON-5 AT ROW 1.54 COL 54
     W_CmbOfi AT ROW 1.69 COL 15 COLON-ALIGNED
     Btn_Consul AT ROW 3.15 COL 54
     Cmb_proc AT ROW 3.42 COL 49.72 RIGHT-ALIGNED
     Btn_Done AT ROW 6.38 COL 54 HELP
          "Sale del proceso de Depreciación y Ajustes"
     W_DiaC AT ROW 6.65 COL 14 COLON-ALIGNED
     W_MesC AT ROW 6.65 COL 21.72 COLON-ALIGNED
     W_Nmes AT ROW 6.65 COL 25.43 COLON-ALIGNED NO-LABEL
     W_AnoC AT ROW 6.65 COL 43.14 COLON-ALIGNED
     "Agen. Còdig. Prior. Descripciòn  (Por Ejecutar:)" VIEW-AS TEXT
          SIZE 31.14 BY .5 AT ROW 2.88 COL 17.14
          BGCOLOR 18 FGCOLOR 15 
     RECT-120 AT ROW 1.27 COL 2
     RECT-314 AT ROW 1.27 COL 53
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.72 BY 11.69
         BGCOLOR 17 FGCOLOR 0 FONT 4.


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
  CREATE WINDOW W-Cierres ASSIGN
         HIDDEN             = YES
         TITLE              = "Procesos del Dia, Programa W-Prc_Auto.W"
         HEIGHT             = 7.31
         WIDTH              = 65.14
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
/* SETTINGS FOR WINDOW W-Cierres
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Cons
   FRAME-NAME                                                           */
/* BROWSE-TAB Br_Proc 1 F_Cons */
ASSIGN 
       FRAME F_Cons:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Proc
                                                                        */
/* SETTINGS FOR COMBO-BOX Cmb_proc IN FRAME F_Proc
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN W_AnoC IN FRAME F_Proc
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_DiaC IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_MesC IN FRAME F_Proc
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Nmes IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Cierres)
THEN W-Cierres:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Proc
/* Query rebuild information for BROWSE Br_Proc
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ProcDia WHERE ProcDia.Agenc EQ 0 NO-LOCK,
                            EACH Varios  WHERE Varios.Tipo   EQ 8
                                          AND Varios.Codigo  EQ ProcDia.Cod_Proceso
                                          AND Varios.Estado  EQ 1
                         NO-LOCK   INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Proc */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cons
/* Query rebuild information for FRAME F_Cons
     _Query            is NOT OPENED
*/  /* FRAME F_Cons */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Cierres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Cierres W-Cierres
ON END-ERROR OF W-Cierres /* Procesos del Dia, Programa W-Prc_Auto.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Cierres W-Cierres
ON WINDOW-CLOSE OF W-Cierres /* Procesos del Dia, Programa W-Prc_Auto.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  
  /*APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Btn_Consul
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consul W-Cierres
ON CHOOSE OF Btn_Consul IN FRAME F_Proc /* Consultar */
DO:
  ASSIGN FRAME F_Proc:SENSITIVE = FALSE
         FRAME F_Cons:VISIBLE   = TRUE
         W_FecCons:SCREEN-VALUE = STRING(W_Fecha)
         W_AgCons:SCREEN-VALUE  = "0"
         W_AgCons
         AgIni                  = 1
         AgFin                  = 999
         W_FecCons.
  
  CLOSE QUERY Br_Proc. 

  APPLY "LEAVE" TO W_FecCons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Cierres
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Salir */
DO:
  IF Cmb_Proc:NUM-ITEMS GT 0 THEN DO:
     MESSAGE "Existen Procesos pendientes de Ejecuciòn...Revise por favor"
         VIEW-AS ALERT-BOX TITLE "INFORMATIVO".
  END.
  
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cons
&Scoped-define SELF-NAME Btn_FinImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_FinImp W-Cierres
ON CHOOSE OF Btn_FinImp IN FRAME F_Cons /* Regresar */
DO:
   CLOSE QUERY Br_Proc.
    
   ASSIGN FRAME F_Proc:SENSITIVE = TRUE
          FRAME F_Cons:VISIBLE   = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Cierres
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_proc W-Cierres
ON VALUE-CHANGED OF Cmb_proc IN FRAME F_Proc /* Procesos del Dia */
DO:
    ERROR-STATUS:ERROR = NO.

    FIND FIRST ProcDia WHERE ProcDia.Agencia EQ INTEG(SUBSTRING(Cmb_Proc:SCREEN-VALUE,1,3))
                         AND ProcDia.Cod_Proceso EQ INTEG(SUBSTRING(Cmb_Proc:SCREEN-VALUE,7,5))
                         AND ProcDia.Fecha_Proc EQ W_Fecha
                         AND ProcDia.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAIL(ProcDia) THEN DO:
        FIND FIRST Varios WHERE Varios.Tipo EQ 8
                            AND Varios.Codigo EQ ProcDia.Cod_Proceso
                            AND Varios.Programa GT 0 NO-LOCK NO-ERROR.
        IF AVAIL(Varios) THEN DO:
            FIND FIRST Programas WHERE Programas.Programa EQ Varios.Programa
                                   AND Programas.Estado EQ 1
                                   AND Programas.Ejecutable GT " " NO-LOCK NO-ERROR.
            IF AVAIL(Programas) THEN DO:
                RUN revisarAgencias.
                IF ERROR-STATUS:ERROR THEN
                    RETURN NO-APPLY.

                ASSIGN W_ProgEjec = Programas.Ejecutable
                       FRAME F_Proc:SENSITIVE = FALSE.
                
                RUN VALUE(W_ProgEjec).

                FRAME F_Proc:SENSITIVE = TRUE.

                RUN ComboProc.
            END.
            ELSE
                MESSAGE "No Existe Programas.Ejecutable para ejecutar el proceso...Revise por Favor"
                    VIEW-AS ALERT-BOX.
        END.
        ELSE
            MESSAGE "No Existe Tipo en varios con Programa para ejecutar el proceso...Revise por Favor"
                VIEW-AS ALERT-BOX.
    END.
    ELSE
        MESSAGE "No Existe el Proceso SIN ejecutar para este Dìa...Revise por Favor"
            VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cons
&Scoped-define SELF-NAME W_AgCons
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_AgCons W-Cierres
ON LEAVE OF W_AgCons IN FRAME F_Cons /* Agencia a Consultar */
DO:
  ASSIGN W_AgCons.      
        
  CLOSE QUERY Br_Proc. 
    
  ASSIGN AgIni = W_AgCons
         AgFin = W_AgCons.  
    
  IF W_AgCons EQ 0 THEN
     ASSIGN AgIni = 1
            AgFin = 999.
    
  OPEN QUERY Br_Proc FOR EACH ProcDia WHERE ProcDia.Agencia    GE AgIni
                                        AND ProcDia.Agencia    LE AgFin
                                        AND ProcDia.Fecha_Proc EQ W_FecCons NO-LOCK,
                         EACH Varios  WHERE Varios.Tipo   EQ 8
                                        AND Varios.Codigo EQ ProcDia.Cod_Proceso
                                        AND Varios.Estado EQ 1 NO-LOCK.               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-Cierres
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_Proc /* Agencia */
DO:
    IF INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) EQ 0 THEN DO:
        W_OfiCierre = 0.
        W_OfiIni = 0.

        FIND LAST Agencias WHERE Agencias.Agencia GT 0 NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
            W_OfiFin = Agencias.Agencia.
    END.
    ELSE
        ASSIGN W_OfiIni = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3))
               W_OfiFin = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3))
               W_OfiCierre = W_OfiIni.

    RUN ComboProc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cons
&Scoped-define SELF-NAME W_FecCons
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecCons W-Cierres
ON LEAVE OF W_FecCons IN FRAME F_Cons /* Fecha Consulta */
DO:
  ASSIGN W_FecCons.      
        
  CLOSE QUERY Br_Proc.  
    
  OPEN QUERY Br_Proc FOR EACH ProcDia WHERE ProcDia.Agencia    GE AgIni
                                        AND ProcDia.Agencia    LE AgFin
                                        AND ProcDia.Fecha_Proc EQ W_FecCons NO-LOCK,
                         EACH Varios  WHERE Varios.Tipo   EQ 8
                                        AND Varios.Codigo EQ ProcDia.Cod_Proceso
                                        AND Varios.Estado EQ 1 NO-LOCK.        
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Proc
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Cierres 


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
    FRAME F_Cons:VISIBLE = FALSE.
    CLOSE QUERY Br_Proc.

    W_CmbOfi:ADD-LAST("000 - Todas las agencias").

    ASSIGN W_OfiIni = W_Agencia
           W_OfiFin = W_Agencia
           W_MesC:SCREEN-VALUE IN FRAME F_Proc = STRING(MONTH(W_Fecha))
           W_DiaC:SCREEN-VALUE = STRING(DAY(W_Fecha))
           W_AnoC:SCREEN-VALUE = STRING(YEAR(W_Fecha)).

    FOR EACH Agencias WHERE Agencias.Agencia GT 0 NO-LOCK:
        W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + STRING(Agencias.Nombre,"X(25)")).
    END.

    W_CmbOfi:SCREEN-VALUE = "000 - Todas las agencias".

    APPLY "VALUE-CHANGED" TO W_CmbOfi.

    CASE MONTH(W_Fecha):
        WHEN 1 THEN W_NMes = "Enero".
        WHEN 2 THEN W_NMes = "Febrero".
        WHEN 3 THEN W_NMes = "Marzo".
        WHEN 4 THEN W_NMes = "Abril".
        WHEN 5 THEN W_NMes = "Mayo".
        WHEN 6 THEN W_NMes = "Junio".
        WHEN 7 THEN W_NMes = "Julio".
        WHEN 8 THEN W_NMes = "Agosto".
        WHEN 9 THEN W_NMes = "Septiembre".
        WHEN 10 THEN W_NMes = "Octubre".
        WHEN 11 THEN W_NMes = "Noviembre".
        WHEN 12 THEN W_NMes = "Diciembre".
    END CASE.

    W_NMes:SCREEN-VALUE = W_NMes.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ComboProc W-Cierres 
PROCEDURE ComboProc :
DO WITH FRAME F_Proc:
END.

Cmb_Proc:LIST-ITEMS = "".

FOR EACH Agencias WHERE Agencias.Agencia GE W_OfiIni
                    AND Agencias.Agencia LE W_OfiFin NO-LOCK:
    FOR EACH Varios WHERE Varios.Tipo EQ 8
                      AND Varios.Estado EQ 1 NO-LOCK BY Varios.Nro_Prioridad:
        FIND FIRST ProcDia WHERE ProcDia.Agencia EQ Agencias.Agencia
                             AND ProcDia.Fecha_Proc EQ W_Fecha
                             AND ProcDia.Cod_Proceso EQ Varios.Codigo
                             AND ProcDia.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAIL(ProcDia) THEN
            Cmb_Proc:ADD-LAST(STRING(ProcDia.Agencia,"999") + " - " + STRING(ProcDia.Cod_Proceso,"99999") + " - " + STRING(Varios.Nro_Prioridad,"99")   + " - " + STRING(Varios.Descripcion,"X(32)")).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Cierres  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Cierres)
  THEN DELETE WIDGET W-Cierres.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Cierres  _DEFAULT-ENABLE
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
  DISPLAY W_CmbOfi Cmb_proc W_DiaC W_MesC W_Nmes W_AnoC 
      WITH FRAME F_Proc IN WINDOW W-Cierres.
  ENABLE RECT-120 RECT-314 BUTTON-5 W_CmbOfi Btn_Consul Cmb_proc Btn_Done 
      WITH FRAME F_Proc IN WINDOW W-Cierres.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  DISPLAY W_FecCons W_AgCons 
      WITH FRAME F_Cons IN WINDOW W-Cierres.
  ENABLE Br_Proc Btn_FinImp W_FecCons W_AgCons 
      WITH FRAME F_Cons IN WINDOW W-Cierres.
  {&OPEN-BROWSERS-IN-QUERY-F_Cons}
  VIEW W-Cierres.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RevisarAgencias W-Cierres 
PROCEDURE RevisarAgencias :
IF W_ofiCierre = 0 THEN DO:
    FIND FIRST agencias WHERE agencia.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE agencias THEN DO:
        MESSAGE "Todas las Oficinas deben estar en estado 'Cerradas'." SKIP
                "Desea ponerlas en este estado? (Es necesario para la" SKIP
                "realización del proceso)"
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO TITLE "Cierre de Agencias" UPDATE flagCierreOfiCons AS LOGICAL.

        IF flagCierreOfiCons = NO THEN
            RETURN ERROR.
        ELSE DO:
            FOR EACH agencias WHERE agencias.estado = 1:
                agencias.estado = 2.
            END.
        END.
    END.
END.
ELSE DO:
    FIND FIRST agencias WHERE agencias.agencia = W_OfiCierre NO-ERROR.
    IF AVAILABLE agencias THEN DO:
        IF agencias.estado = 1 THEN DO:
            MESSAGE "Todas las Oficinas deben estar en estado 'Cerradas'." SKIP
                    "Desea ponerlas en este estado? (Es necesario para la" SKIP
                    "realización del proceso)"
                VIEW-AS ALERT-BOX INFO BUTTONS YES-NO TITLE "Cierre de Agencias" UPDATE flagCierreOfiInd AS LOGICAL.

            IF flagCierreOfiInd = NO THEN
                RETURN ERROR.
            ELSE
                agencias.estado = 2.
        END.
    END.
END.

ERROR-STATUS:ERROR = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

