&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

{src/adm2/widgetprto.i}
/*{incluido\VARIABLE.i "SHARED"}*/

DEFINE VAR Hora1 AS INTEGER.
DEFINE VAR Hora2 AS INTEGER.
ASSIGN Hora1 = TIME.
DEFINE VARIABLE i AS INTEGER NO-UNDO. /*Agencias*/

DEFINE TEMP-TABLE TTCtasInc
    FIELD Agencia   AS INTEGER
    FIELD Nit       AS CHARACTER
    FIELD Cuenta    AS CHARACTER
    FIELD Fecha     AS DATE
    FIELD Documento AS CHARACTER
    FIELD DB        AS DECIMAL
    FIELD CR        AS DECIMAL
    FIELD Usuario   AS CHARACTER
    FIELD Estado    AS CHARACTER.

DEFINE VARIABLE viAno AS INTEGER NO-UNDO.
DEFINE VARIABLE viMes AS INTEGER NO-UNDO.
DEFINE VARIABLE viAgeIni AS INTEGER NO-UNDO.
DEFINE VARIABLE viAgeFin AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-Agencias F-Ano F-Mes BUTTON-1 ~
BContabilizar Btn_Cancelar BtnDone-2 BUTTON-95 RECT-1 RECT-274 
&Scoped-Define DISPLAYED-OBJECTS F-Estado F-Hora1 F-Hora2 CB-Agencias F-Ano ~
F-Mes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 BUTTON-1 Btn_Consulta Btn_Cancelar BUTTON-95 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BContabilizar 
     IMAGE-UP FILE "imagenes/proceso_g.bmp":U
     LABEL "Contabilizar" 
     SIZE 7 BY 1.65 TOOLTIP "Contabilizar".

DEFINE BUTTON BTN-Titulo 
     LABEL "" 
     SIZE 58.72 BY 1.12.

DEFINE BUTTON BtnDone-2 DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 7 BY 1.65 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/borrar.bmp":U
     LABEL "&Cancelar" 
     SIZE 7 BY 1.65 TOOLTIP "Cancelar"
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 7 BY 1.65 TOOLTIP "Buscar".

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 7 BY 1.65 TOOLTIP "Información".

DEFINE BUTTON BUTTON-95 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 95" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE CB-Agencias AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todas las Agencias" 
     LABEL "Agencia a Procesar" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Ano AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año a Procesar" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Estado AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estado" 
     VIEW-AS FILL-IN 
     SIZE 58 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Hora1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Inicio proceso:" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Hora2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fin Proceso" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-Mes AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Mes a Procesar" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 8.62.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY 8.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     F-Estado AT ROW 8.54 COL 17 COLON-ALIGNED WIDGET-ID 92
     F-Hora1 AT ROW 9.38 COL 17 COLON-ALIGNED WIDGET-ID 94
     F-Hora2 AT ROW 9.38 COL 39 COLON-ALIGNED WIDGET-ID 96
     CB-Agencias AT ROW 3.42 COL 23 COLON-ALIGNED WIDGET-ID 84
     F-Ano AT ROW 4.5 COL 23 COLON-ALIGNED WIDGET-ID 90
     F-Mes AT ROW 4.5 COL 51 COLON-ALIGNED WIDGET-ID 88
     BTN-Titulo AT ROW 1.31 COL 15.72 WIDGET-ID 68
     BUTTON-1 AT ROW 1.54 COL 91 WIDGET-ID 58
     Btn_Consulta AT ROW 3.15 COL 91 WIDGET-ID 54
     BContabilizar AT ROW 4.77 COL 91 WIDGET-ID 56
     Btn_Cancelar AT ROW 6.38 COL 91 WIDGET-ID 52
     BtnDone-2 AT ROW 8 COL 91 WIDGET-ID 50
     BUTTON-95 AT ROW 10.15 COL 93 WIDGET-ID 60
     "PROCESO MAYORIZACION" VIEW-AS TEXT
          SIZE 25 BY .81 AT ROW 1.46 COL 32.57 WIDGET-ID 74
          BGCOLOR 11 FONT 1
     RECT-1 AT ROW 2.62 COL 1 WIDGET-ID 2
     RECT-274 AT ROW 1.27 COL 90 WIDGET-ID 66
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 1
         SIZE 98.14 BY 10.54
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Mayorizacion - wMayorizacion"
         HEIGHT             = 10.46
         WIDTH              = 98.14
         MAX-HEIGHT         = 38.23
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 38.23
         VIRTUAL-WIDTH      = 182.86
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* SETTINGS FOR FRAME fMain
   FRAME-NAME UNDERLINE Custom                                          */
/* SETTINGS FOR BUTTON BTN-Titulo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME fMain
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-95 IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR FILL-IN F-Estado IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Hora1 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Hora2 IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Mayorizacion - wMayorizacion */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Mayorizacion - wMayorizacion */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BContabilizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BContabilizar wWin
ON CHOOSE OF BContabilizar IN FRAME fMain /* Contabilizar */
DO:
    F-Hora1:SCREEN-VALUE = "".
    F-Hora2:SCREEN-VALUE = "".
    ASSIGN Hora1 = TIME.
    F-Hora1:SCREEN-VALUE = STRING(Hora1,"HH:MM:SS").

/*********************************************************************************************/
/**                                Borra Saldo de Sal_Cuentas                               **/
/*********************************************************************************************/
    DO i = viAgeIni TO viAgeFin:
        F-Estado:SCREEN-VALUE = "Borrando Sal_Cuentas para el mes " + STRING(ViMes) +
                                " en la agencia " + STRING(i) + " ...".
        FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Agencia EQ i AND
                                  Sal_Cuenta.Ano EQ viAno
                                  EXCLUSIVE-LOCK BY Sal_Cuenta.Agencia
                                  BY Sal_Cuenta.Ano:
            UPDATE Sal_Cuenta.DB[viMes] = 0
                   Sal_Cuenta.CR[viMes] = 0.
        END.
    END.

/*********************************************************************************************/
/**                                   Borra Saldo de Anexos                                 **/
/*********************************************************************************************/
    DO i = viAgeIni TO viAgeFin:
        F-Estado:SCREEN-VALUE = "Borrando Anexos para el mes " + STRING(ViMes) +
                                " en la agencia " + STRING(i) + " ...".
        FOR EACH Anexos WHERE Anexos.Agencia EQ i AND
                              Anexos.Ano EQ viAno
                              EXCLUSIVE-LOCK BY Anexos.Agencia
                              BY Anexos.Ano:
            UPDATE Anexos.DB[viMes] = 0
                   Anexos.CR[viMes] = 0.
        END.
    END.


/*********************************************************************************************/
/**                                     Actualiza Saldos                                    **/
/*********************************************************************************************/
    DO i = viAgeIni TO viAgeFin: /*1*/
        F-Estado:SCREEN-VALUE = "Mayorizando el mes " + STRING(ViMes) +
                                " en la agencia " + STRING(i) + " ...".
        FOR EACH Mov_Contable WHERE Mov_Contable.Agencia EQ i AND
                                    (YEAR(Mov_Contable.Fec_Contable) EQ viAno AND
                                     MONTH(Mov_Contable.Fec_Contable) EQ viMes)
                                    NO-LOCK BY Mov_Contable.Agencia
                                    BY Mov_Contable.Cuenta BY Mov_Contable.Fec_Contable: /*2*/
            FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta
                                     USE-INDEX ICuenta NO-LOCK NO-ERROR.
            IF AVAILABLE Cuentas THEN DO: /*3*/
                FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Agencia EQ Mov_Contable.Agencia AND
                                            Sal_Cuenta.Cen_Costos EQ 999 AND
                                            Sal_Cuenta.Cuenta EQ Mov_Contable.Cuenta AND
                                            Sal_Cuenta.Ano EQ viAno
                                            USE-INDEX Idppal EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Sal_Cuenta THEN DO: /*5*/
                    IF Mov_Contable.Db GT 0 THEN 
                        UPDATE Sal_Cuenta.DB[viMes] = Sal_Cuenta.DB[viMes] + Mov_Contable.Db.
                    IF Mov_Contable.Cr GT 0 THEN
                        UPDATE Sal_Cuenta.CR[viMes] = Sal_Cuenta.CR[viMes] + Mov_Contable.Cr.
                END. /*-5*/
                IF NOT AVAILABLE Sal_Cuenta THEN DO: /*6*/
                    CREATE Sal_Cuenta.
                    UPDATE Sal_Cuenta.Agencia    = Mov_Contable.Agencia
                           Sal_Cuenta.Cuenta     = Mov_Contable.Cuenta
                           Sal_Cuenta.Cen_Costos = 999
                           Sal_Cuenta.Ano        = viAno
                           Sal_Cuenta.DB[viMes]  = Mov_Contable.Db
                           Sal_Cuenta.CR[viMes]  = Mov_Contable.Cr.
                    CREATE TTCtasInc.
                    UPDATE TTCtasInc.Agencia   = Mov_Contable.Agencia
                           TTCtasInc.Nit       = Mov_Contable.Nit
                           TTCtasInc.Cuenta    = Mov_Contable.Cuenta
                           TTCtasInc.Fecha     = Mov_Contable.Fec_Contable
                           TTCtasInc.Documento = Mov_Contable.Doc_Referencia
                           TTCtasInc.DB        = Mov_Contable.DB
                           TTCtasInc.CR        = Mov_Contable.CR
                           TTCtasInc.Usuario   = Mov_Contable.Usuario
                           TTCtasInc.Estado    = "Crea Sal_Cuenta".
                END. /*-6*/
                IF Mov_Contable.Nit NE "" THEN DO: /*7*/
                    FIND FIRST Anexos WHERE Anexos.Agencia EQ Mov_Contable.Agencia AND
                                            Anexos.Cuenta EQ Mov_Contable.Cuenta AND
                                            Anexos.Ano EQ viAno AND
                                            Anexos.Mes GE 0 AND
                                            Anexos.Nit EQ Mov_Contable.Nit
                                            USE-INDEX IdOfCueAnmNit EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE Anexos THEN DO: /*8*/
                        UPDATE Anexos.Base[viMes] = Anexos.Base[viMes] + Mov_Contable.Base.
                        IF Mov_Contable.Db GT 0 THEN
                            UPDATE Anexos.DB[viMes] = Anexos.DB[viMes] + Mov_Contable.Db.
                        IF Mov_Contable.Cr GT 0 THEN
                            UPDATE Anexos.CR[viMes] = Anexos.CR[viMes] + Mov_Contable.Cr.
                    END. /*-8*/
                    IF NOT AVAILABLE Anexos THEN DO: /*9*/
                        CREATE Anexos.
                        UPDATE Anexos.Agencia     = Mov_Contable.Agencia
                               Anexos.Nit         = Mov_Contable.Nit
                               Anexos.Cuenta      = Mov_Contable.Cuenta
                               Anexos.Ano         = viAno
                               Anexos.Cen_Costos  = 999
                               Anexos.Base[viMes] = Mov_Contable.Base
                               Anexos.DB[viMes]   = Mov_Contable.Db
                               Anexos.Cr[viMes]   = Mov_Contable.Cr.
                        CREATE TTCtasInc.
                        UPDATE TTCtasInc.Agencia   = Mov_Contable.Agencia
                               TTCtasInc.Nit       = Mov_Contable.Nit
                               TTCtasInc.Cuenta    = Mov_Contable.Cuenta
                               TTCtasInc.Fecha     = Mov_Contable.Fec_Contable
                               TTCtasInc.Documento = Mov_Contable.Doc_Referencia
                               TTCtasInc.DB        = Mov_Contable.DB
                               TTCtasInc.CR        = Mov_Contable.CR
                               TTCtasInc.Usuario   = Mov_Contable.Usuario
                               TTCtasInc.Estado    = "Crea Anexos".
                    END. /*-9*/
                END. /*-7*/
            END. /*-3*/
            IF NOT AVAILABLE Cuentas THEN DO: /*4*/
                CREATE TTCtasInc.
                UPDATE TTCtasInc.Agencia   = Mov_Contable.Agencia
                       TTCtasInc.Nit       = Mov_Contable.Nit
                       TTCtasInc.Cuenta    = Mov_Contable.Cuenta
                       TTCtasInc.Fecha     = Mov_Contable.Fec_Contable
                       TTCtasInc.Documento = Mov_Contable.Doc_Referencia
                       TTCtasInc.DB        = Mov_Contable.DB
                       TTCtasInc.CR        = Mov_Contable.CR
                       TTCtasInc.Usuario   = Mov_Contable.Usuario
                       TTCtasInc.Estado    = "Cuenta I".
            END. /*-4*/
        END. /*-2*/
    END. /*-1*/
    

/*********************************************************************************************/
/**                    Generacion Archivo plano de Inconsistencias                          **/
/*********************************************************************************************/
    F-Estado:SCREEN-VALUE = "Generando inconsistencias en C:\info_fodun\IncMayorizacion.csv".
    OUTPUT TO C:\info_fodun\IncMayorizacion.csv.
    PUT "Agencia;Nit;Cuenta;Fecha;Documento;DB;CR;Usuario;Estado" SKIP.
    FOR EACH TTCtasInc NO-LOCK:
        EXPORT DELIMITER ";" TTCtasInc.
    END.
    OUTPUT CLOSE.
    
    ASSIGN Hora2 = TIME.
    F-Estado:SCREEN-VALUE = "Proceso terminado".
    F-Hora2:SCREEN-VALUE = STRING(Hora2,"HH:MM:SS").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone-2 wWin
ON CHOOSE OF BtnDone-2 IN FRAME fMain
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


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Agencias wWin
ON VALUE-CHANGED OF CB-Agencias IN FRAME fMain /* Agencia a Procesar */
DO:
    IF SUBSTRING(CB-Agencias:SCREEN-VALUE,1,3) EQ "000" THEN DO:
        FIND FIRST Agencias NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN ASSIGN viAgeIni = Agencias.Agencia.
        FIND LAST Agencias NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN ASSIGN viAgeFin = Agencias.Agencia.
    END.
    ELSE 
        ASSIGN viAgeIni = INTEGER(SUBSTRING(CB-Agencias:SCREEN-VALUE,1,3))
               viAgeFin = viAgeIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Ano
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Ano wWin
ON LEAVE OF F-Ano IN FRAME fMain /* Año a Procesar */
DO:
    IF INTEGER(F-Ano:SCREEN-VALUE) LT 2007 THEN DO:
        MESSAGE "Año no existe en el Sistema."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK
            TITLE "Error de Año".
        F-Ano:SCREEN-VALUE = STRING(YEAR(TODAY)).
        RETURN NO-APPLY.
    END.
    IF INTEGER(F-Ano:SCREEN-VALUE) GT (YEAR(TODAY)) THEN DO:
        MESSAGE "No puede procesar un año" SKIP
                "Superior a la actual."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK
            TITLE "Error de Año".
        F-Ano:SCREEN-VALUE = STRING(YEAR(TODAY)).
        RETURN NO-APPLY.
    END.
    ELSE ASSIGN viAno = INTEGER(F-Ano:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Mes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Mes wWin
ON LEAVE OF F-Mes IN FRAME fMain /* Mes a Procesar */
DO:
    IF INTEGER(F-Mes:SCREEN-VALUE) LT 1 OR 
       INTEGER(F-Mes:SCREEN-VALUE) GT 12 THEN DO:
        MESSAGE "Mes no existe."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK
            TITLE "Error de Mes".
        F-Mes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(MONTH(TODAY)).
        RETURN NO-APPLY.
    END.
    IF INTEGER(F-Mes:SCREEN-VALUE) GT MONTH(TODAY) AND 
       viAno EQ YEAR(TODAY) THEN DO:
        MESSAGE "No Puede Procesar una fecha" SKIP
                "Superior a la actual."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK
            TITLE "Error de Mes".
        F-Mes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(MONTH(TODAY)).
        RETURN NO-APPLY.
    END.
    ELSE ASSIGN viMes = INTEGER(F-Mes:SCREEN-VALUE).
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
  DISPLAY F-Estado F-Hora1 F-Hora2 CB-Agencias F-Ano F-Mes 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE CB-Agencias F-Ano F-Mes BUTTON-1 BContabilizar Btn_Cancelar BtnDone-2 
         BUTTON-95 RECT-1 RECT-274 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.


  DO WITH FRAME fMain:
     FOR EACH Agencias NO-LOCK:
         CB-Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencia.Nombre).
     END.
     FIND FIRST Agencias NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN ASSIGN viAgeIni = Agencias.Agencia.
     FIND LAST Agencias NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN ASSIGN viAgeFin = Agencias.Agencia.
     F-Ano:SCREEN-VALUE = STRING(YEAR(TODAY)).
     F-Mes:SCREEN-VALUE = STRING(MONTH(TODAY)).
     ASSIGN viAno = YEAR(TODAY)
            viMes = MONTH(TODAY).
  END.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

