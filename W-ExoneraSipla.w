&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wfexo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wfexo 
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

{Incluido\VARIABLE.I "SHARED"}
DEFINE VAR P_Nit       LIKE Clientes.Nit.
DEFINE VAR P_Nombre    AS CHARACTER FORMAT "X(30)".
DEFINE VAR P_Apellido  AS CHARACTER FORMAT "X(40)".
DEFINE VAR P_CliAge    LIKE Clientes.Agencia.
DEFINE VAR Listado AS CHARACTER INITIAL "".
DEFINE VAR WListado AS CHARACTER INITIAL "".
DEFINE VAR j AS DECIMAL.
DEFINE VAR k AS INTEGER.
DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
DEFINE VAR W_sw          AS LOGICAL. 
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR i AS INTEGER.
DEFINE VARIABLE AgeIni   AS INTEGER FORMAT "999" INITIAL 0.
DEFINE VARIABLE AgeFin   AS INTEGER FORMAT "999" INITIAL 999.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FExo

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Clientes

/* Definitions for FRAME FExo                                           */
&Scoped-define FIELDS-IN-QUERY-FExo Clientes.Id_ExoneradoSipla ~
Clientes.FecIni_NoSipla Clientes.FecFin_NoSipla 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FExo Clientes.Id_ExoneradoSipla ~
Clientes.FecIni_NoSipla Clientes.FecFin_NoSipla 
&Scoped-define ENABLED-TABLES-IN-QUERY-FExo Clientes
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-FExo Clientes
&Scoped-define QUERY-STRING-FExo FOR EACH Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-FExo OPEN QUERY FExo FOR EACH Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FExo Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-FExo Clientes


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Clientes.Id_ExoneradoSipla ~
Clientes.FecIni_NoSipla Clientes.FecFin_NoSipla 
&Scoped-define ENABLED-TABLES Clientes
&Scoped-define FIRST-ENABLED-TABLE Clientes
&Scoped-Define ENABLED-OBJECTS Btn_buscar BUTTON-1 BtnEjeInfo Btn_Info e1 R ~
WNit Informe Btn_Salvar BtnDone RECT-1 
&Scoped-Define DISPLAYED-FIELDS Clientes.Id_ExoneradoSipla ~
Clientes.FecIni_NoSipla Clientes.FecFin_NoSipla 
&Scoped-define DISPLAYED-TABLES Clientes
&Scoped-define FIRST-DISPLAYED-TABLE Clientes
&Scoped-Define DISPLAYED-OBJECTS e1 R WAge WNit WNom FecInforme Informe 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wfexo AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 15 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON BtnEjeInfo 
     LABEL "Ejecutar" 
     SIZE 21 BY 1.12.

DEFINE BUTTON Btn_buscar 
     LABEL "Buscar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_Info 
     LABEL "Informe Exonerados" 
     SIZE 17 BY 1.12.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 17 BY 1.12.

DEFINE BUTTON BUTTON-1 
     LABEL "Imprimir" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE e1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 80 BY 9.15
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE Informe AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 80 BY 2.96
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE FecInforme AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Exoneracion" 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WAge AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNit AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNom AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todas las Agencias", 1,
"Una Agencia", 2
     SIZE 30 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38 BY 1.35.

DEFINE BUTTON Btn_anterior 
     LABEL "Anterior" 
     SIZE 9 BY .81.

DEFINE BUTTON Btn_siguiente 
     LABEL "Siguiente" 
     SIZE 9 BY .81.

DEFINE BUTTON BUTTON-154 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 154" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(20)":U 
     LABEL "Buscar" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE RECTANGLE R1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R3
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R4
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R5
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R6
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R7
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R8
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R9
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY .54
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY FExo FOR 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FExo
     Btn_buscar AT ROW 12.15 COL 70
     BUTTON-1 AT ROW 10.96 COL 70
     BtnEjeInfo AT ROW 10.96 COL 48
     Btn_Info AT ROW 8.54 COL 5
     e1 AT ROW 13.38 COL 4.86 NO-LABEL
     R AT ROW 11.23 COL 8 NO-LABEL
     WAge AT ROW 11.23 COL 36 COLON-ALIGNED NO-LABEL
     WNit AT ROW 1.27 COL 8 COLON-ALIGNED
     WNom AT ROW 1.27 COL 20 COLON-ALIGNED NO-LABEL
     FecInforme AT ROW 1.27 COL 70 COLON-ALIGNED
     Clientes.Id_ExoneradoSipla AT ROW 2.35 COL 38
          LABEL "Exonerado del Control?"
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .77
     Clientes.FecIni_NoSipla AT ROW 2.35 COL 70 COLON-ALIGNED
          LABEL "Desde"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Clientes.FecFin_NoSipla AT ROW 3.35 COL 70 COLON-ALIGNED
          LABEL "Hasta"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Informe AT ROW 5.31 COL 5 NO-LABEL
     Btn_Salvar AT ROW 8.54 COL 23
     BtnDone AT ROW 8.54 COL 70
     RECT-1 AT ROW 10.96 COL 5
     "  Justificacion de la Exencion" VIEW-AS TEXT
          SIZE 80 BY .81 AT ROW 4.38 COL 5
          BGCOLOR 18 FGCOLOR 15 FONT 1
     "  Informe de Exonerados del Control del Lavado de Activos" VIEW-AS TEXT
          SIZE 80 BY .77 AT ROW 10.08 COL 5
          BGCOLOR 18 FGCOLOR 15 FONT 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88.14 BY 22.08
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME F_Progreso
     R1 AT ROW 1.23 COL 16.29
     R2 AT ROW 1.23 COL 18.29
     R3 AT ROW 1.23 COL 14.29
     R4 AT ROW 1.23 COL 12.29
     R5 AT ROW 1.23 COL 10.29
     R6 AT ROW 1.23 COL 8.29
     R7 AT ROW 1.23 COL 6.29
     R8 AT ROW 1.23 COL 4.29
     R9 AT ROW 1.23 COL 2.29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 48 ROW 12.04
         SIZE 21 BY 1.08
         BGCOLOR 17 .

DEFINE FRAME F_Buscar
     BUTTON-154 AT ROW 1.27 COL 46
     Buscar AT ROW 1.62 COL 7 COLON-ALIGNED
     Btn_anterior AT ROW 1.62 COL 27
     Btn_siguiente AT ROW 1.62 COL 36
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 10.42
         SIZE 53 BY 2.96
         BGCOLOR 17 FONT 4
         TITLE "Buscar".


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
  CREATE WINDOW wfexo ASSIGN
         HIDDEN             = YES
         TITLE              = "Exoneracion Control del Lavado de Activos"
         HEIGHT             = 22.08
         WIDTH              = 87.72
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wfexo 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wfexo
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Buscar:FRAME = FRAME FExo:HANDLE
       FRAME F_Progreso:FRAME = FRAME FExo:HANDLE.

/* SETTINGS FOR FRAME FExo
   Custom                                                               */
/* SETTINGS FOR FILL-IN Clientes.FecFin_NoSipla IN FRAME FExo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN FecInforme IN FRAME FExo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.FecIni_NoSipla IN FRAME FExo
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Clientes.Id_ExoneradoSipla IN FRAME FExo
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN WAge IN FRAME FExo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WNom IN FRAME FExo
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Buscar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Buscar:HIDDEN           = TRUE
       FRAME F_Buscar:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Progreso
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wfexo)
THEN wfexo:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FExo
/* Query rebuild information for FRAME FExo
     _TblList          = "bdcentral.Clientes"
     _Query            is OPENED
*/  /* FRAME FExo */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wfexo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wfexo wfexo
ON END-ERROR OF wfexo /* Exoneracion Control del Lavado de Activos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wfexo wfexo
ON WINDOW-CLOSE OF wfexo /* Exoneracion Control del Lavado de Activos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wfexo
ON CHOOSE OF BtnDone IN FRAME FExo /* Salir */
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


&Scoped-define SELF-NAME BtnEjeInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnEjeInfo wfexo
ON CHOOSE OF BtnEjeInfo IN FRAME FExo /* Ejecutar */
DO:
    ASSIGN FRAME fexo r WAge.
    IF r EQ 1 THEN ASSIGN AgeIni = 0 AgeFin = 999.
    ELSE ASSIGN AgeIni = WAge AgeFin = WAge.
    DEFINE VAR WNomCli AS CHARACTER FORMAT "X(40)".
    Listado = W_PathSpl + "\ExoneradosSipla2.txt".  
    OS-DELETE VALUE(Listado).
    DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" INITIAL "".
    DEFINE VAR TotReg AS DECIMAL.

    OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
    {Incluido\RepEncabezado.i}
      W_Reporte   = "REPORTE   : Clientes Exonerados del Control del Lavado de Activos: "  
                  + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
            W_EncColumna = "NIT          NOMBRE                               TEL.RESIDENCIA        TEL.COMERCIAL".
      VIEW FRAME F-Encabezado.
      ASSIGN j = 0 k = 0.
      FOR EACH Clientes WHERE 
               Clientes.Agencia GE AgeIni AND
               Clientes.Agencia LE AgeFin AND
               Clientes.Id_ExoneradoSipla AND
               Clientes.FecIni_NoSipla NE ? AND
               Clientes.FecFin_NoSipla NE ? NO-LOCK:
          WNomCli = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
          ASSIGN j = j + 1
                 TotReg = Totreg + 1.
          RUN Progreso.
          DISPLAY Clientes.Nit WNomCli Clientes.Tel_Residencia Clientes.Tel_Comercial
              WITH FRAME Fmov WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
      END.
    DISPLAY SKIP "Total de Registros Reportados: " TotReg WITH FRAME FT WIDTH 132 NO-LABELS.
    VIEW FRAME F-Ftr.
    PAGE.
OUTPUT CLOSE.
W_ok = e1:READ-FILE(Listado) IN FRAME fexo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME Btn_anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_anterior wfexo
ON CHOOSE OF Btn_anterior IN FRAME F_Buscar /* Anterior */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,34) IN FRAME Fexo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FExo
&Scoped-define SELF-NAME Btn_buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_buscar wfexo
ON CHOOSE OF Btn_buscar IN FRAME FExo /* Buscar */
DO:
    IF E1:SCREEN-VALUE EQ "" THEN
     MESSAGE "No se ha ejecutado ningún Informe" SKIP
             "Escoja el informe y presione clic" SKIP
             "en el boton ejecutar!." VIEW-AS ALERT-BOX INFORMATION.
  ELSE DO:
    Buscar:SCREEN-VALUE IN FRAME F_Buscar = E1:SELECTION-TEXT.
    ASSIGN FRAME F_Buscar Buscar.
    W_Ok = E1:SEARCH(Buscar,32).
    VIEW FRAME F_Buscar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Info wfexo
ON CHOOSE OF Btn_Info IN FRAME FExo /* Informe Exonerados */
DO:
  IF SELF:LABEL EQ "Informe Exonerados" THEN DO:
     SELF:LABEL = "Ocultar Informe".
     wfexo:HEIGHT = 22.08.
  END.
  ELSE DO:
     SELF:LABEL = "Informe Exonerados".
     wfexo:HEIGHT = 8.88.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wfexo
ON CHOOSE OF Btn_Salvar IN FRAME FExo /* Salvar */
DO:
  DO WITH FRAME FExo:
     IF Clientes.Id_ExoneradoSipla:SCREEN-VALUE EQ "yes" AND 
       (DATE(Clientes.FecIni_NoSipla:SCREEN-VALUE) EQ ? OR 
        DATE(Clientes.FecFin_NoSipla:SCREEN-VALUE) EQ ?) THEN DO:
        MESSAGE "Deben digitarse ambas fechas, para establecer" SKIP
                "el limite del control!" VIEW-AS ALERT-BOX WARNING.
        APPLY "entry" TO Clientes.FecIni_NoSipla.
        RETURN NO-APPLY.
     END.
     ELSE DO:
        FIND CURRENT Clientes.
        ASSIGN Clientes.Id_ExoneradoSipla Clientes.FecIni_NoSipla Clientes.FecFin_NoSipla.
        FIND LAST Hoja_Vida WHERE
             Hoja_Vida.Tipo     EQ 18   AND
             Hoja_Vida.Codigo   EQ 7    AND
             Hoja_Vida.Nit      EQ WNit NO-ERROR.
        IF NOT AVAILABLE Hoja_Vida THEN DO:
           CREATE Hoja_Vida.
           ASSIGN Hoja_Vida.Codigo        = 7
                  Hoja_Vida.Fec_Grabacion = W_Fecha
                  Hoja_Vida.Hora_Grabacion = TIME 
                  Hoja_Vida.Nit            = Clientes.Nit
                  Hoja_Vida.Tipo           = 18.
        END.
        ASSIGN Hoja_Vida.Observacion    = Informe:SCREEN-VALUE
               Hoja_Vida.Usuario        = W_Usuario.
        IF NOT Clientes.Id_ExoneradoSipla THEN
           Hoja_Vida.Observacion = Hoja_Vida.Observacion + ", inactivado el control:" + STRING(W_Fecha).
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME Btn_siguiente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_siguiente wfexo
ON CHOOSE OF Btn_siguiente IN FRAME F_Buscar /* Siguiente */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,33) IN FRAME Fexo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FExo
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wfexo
ON CHOOSE OF BUTTON-1 IN FRAME FExo /* Imprimir */
DO:
    IF Listado EQ "" THEN 
     MESSAGE "No se ha ejecutado ningún informe" VIEW-AS ALERT-BOX.
  ELSE
  DO:
    ASSIGN WListado = Listado.
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado,INPUT-OUTPUT W_Dispositivo).
  OS-RENAME VALUE(WListado) VALUE(Listado).
  
    IF W_Dispositivo = "P" THEN  
      RUN Pantalla IN W_Manija (INPUT Listado).
    ELSE                                                  
      IF W_Dispositivo = "I" THEN
         RUN adecomm/_osprint.r ( INPUT  ?, INPUT Listado,INPUT  ?,INPUT  1,INPUT  1,
                                          INPUT  99999,OUTPUT W_sw).
    IF W_Dispositivo <> "A" THEN
        OS-DELETE VALUE(Listado).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME BUTTON-154
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-154 wfexo
ON CHOOSE OF BUTTON-154 IN FRAME F_Buscar /* Button 154 */
DO:
  HIDE FRAME F_Buscar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FExo
&Scoped-define SELF-NAME Clientes.Id_ExoneradoSipla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Id_ExoneradoSipla wfexo
ON VALUE-CHANGED OF Clientes.Id_ExoneradoSipla IN FRAME FExo /* Exonerado del Control? */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
     ENABLE Clientes.FecIni_NoSipla clientes.FecFin_NoSipla WITH FRAME fexo.
  ELSE DO:
     DISABLE Clientes.FecIni_NoSipla clientes.FecFin_NoSipla WITH FRAME fexo.
      ASSIGN Clientes.FecIni_NoSipla:SCREEN-VALUE = ""
             Clientes.FecFin_NoSipla:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME WNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WNit wfexo
ON LEAVE OF WNit IN FRAME FExo /* Cliente */
DO:
  ASSIGN FRAME FExo WNit.
  FIND Clientes WHERE Clientes.Nit EQ WNit NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Clientes THEN DO:
     RUN C-Clientes.R(INPUT 1, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_CliAge).
     FIND Clientes WHERE Clientes.Nit EQ WNit NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Clientes THEN DO:
        MESSAGE "Debe digitarse un nit de cliente valido" SKIP
                "Reintente de nuevo!" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
  END.
  ASSIGN WNom:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
         Id_ExoneradoSipla:SCREEN-VALUE = STRING(Clientes.Id_ExoneradoSipla)
         FecIni_NoSipla:SCREEN-VALUE   = STRING(Clientes.FecIni_NoSipla,"99/99/99")
         FecFin_NoSipla:SCREEN-VALUE   = STRING(Clientes.FecFin_NoSipla,"99/99/99").
  IF NOT Clientes.Id_ExoneradoSipla THEN
     DISABLE Clientes.FecIni_NoSipla Clientes.FecFin_NoSipla WITH FRAME FExo.
  FIND LAST Hoja_Vida WHERE
       Hoja_Vida.Tipo     EQ 18   AND
       Hoja_Vida.Codigo   EQ 7    AND
       Hoja_Vida.Nit      EQ WNit NO-LOCK NO-ERROR.
  IF AVAILABLE Hoja_Vida THEN
     ASSIGN Informe:SCREEN-VALUE    = Hoja_Vida.Observacion
            FecInforme:SCREEN-VALUE = STRING(Hoja_Vida.Fec_Grabacion).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wfexo 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wfexo  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wfexo  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wfexo)
  THEN DELETE WIDGET wfexo.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wfexo  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-FExo}
  GET FIRST FExo.
  DISPLAY e1 R WAge WNit WNom FecInforme Informe 
      WITH FRAME FExo IN WINDOW wfexo.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Id_ExoneradoSipla Clientes.FecIni_NoSipla 
          Clientes.FecFin_NoSipla 
      WITH FRAME FExo IN WINDOW wfexo.
  ENABLE Btn_buscar BUTTON-1 BtnEjeInfo Btn_Info e1 R WNit 
         Clientes.Id_ExoneradoSipla Clientes.FecIni_NoSipla 
         Clientes.FecFin_NoSipla Informe Btn_Salvar BtnDone RECT-1 
      WITH FRAME FExo IN WINDOW wfexo.
  {&OPEN-BROWSERS-IN-QUERY-FExo}
  DISPLAY Buscar 
      WITH FRAME F_Buscar IN WINDOW wfexo.
  ENABLE BUTTON-154 Buscar Btn_anterior Btn_siguiente 
      WITH FRAME F_Buscar IN WINDOW wfexo.
  {&OPEN-BROWSERS-IN-QUERY-F_Buscar}
  ENABLE R1 R2 R3 R4 R5 R6 R7 R8 R9 
      WITH FRAME F_Progreso IN WINDOW wfexo.
  {&OPEN-BROWSERS-IN-QUERY-F_Progreso}
  VIEW wfexo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wfexo 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wfexo 
PROCEDURE initializeObject :
RUN SUPER.
  FecInforme:SCREEN-VALUE IN FRAME Fexo = STRING(TODAY,"99/99/9999").
  Wfexo:HEIGHT = 8.88.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Progreso wfexo 
PROCEDURE Progreso :
DO WITH FRAME F_Progreso:
  IF j EQ 250 THEN DO:
        j = 0.
        k = k + 1.
        ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
               R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15.
               R9:BGCOLOR = 15.
        CASE k:
          WHEN 1 THEN R1:BGCOL = 18.
          WHEN 2 THEN R2:BGCOL = 18.
          WHEN 3 THEN R3:BGCOL = 18.
          WHEN 4 THEN R4:BGCOL = 18.
          WHEN 5 THEN R5:BGCOL = 18.
          WHEN 6 THEN R6:BGCOL = 18.
          WHEN 7 THEN R7:BGCOL = 18.
          WHEN 8 THEN R8:BGCOL = 18.
          WHEN 9 THEN R9:BGCOL = 18.
        END CASE.
        IF k = 9 THEN k = 0.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

