&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
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
{Incluido\VARIABLE.I "SHARED"}
/*definicion de variable del calendario*/

DEF VAR WNomProceso AS CHARACTER FORMAT "X(20)".
DEF VAR WEstado     AS CHARACTER FORMAT "X(10)".
DEF VAR location AS INTEGER FORMAT "99" NO-UNDO.
DEF VAR lweekday AS INTEGER NO-UNDO.
DEF VAR lstday AS INTEGER NO-UNDO.
DEF VAR d AS INTEGER NO-UNDO.
DEF VAR tnum AS INTEGER format "z9" NO-UNDO.
DEF VAR tchar AS CHAR FORMAT "X(9)" NO-UNDO.
DEF VAR lastday AS INTEGER NO-UNDO.
DEF VAR lastdate AS DATE NO-UNDO.
DEF VAR lentdate AS DATE LABEL " Date" INIT TODAY NO-UNDO.
DEF VAR lmonth AS INTEGER NO-UNDO.
DEF VAR lm LIKE lmonth no-undo.
DEF VAR lyear AS INTEGER FORMAT "9999" NO-UNDO.
def var lyr like lyear no-undo.
DEF var cur as WIDGET-HANDLE.
DEF VAR mnthname AS CHAR FORMAT "stdate(10)" EXTENT 12 INIT
["January",
 "February",
 "March",
 "April",
 "May",
 "June",
 "July",
 "August",
 "September",
 "October",
 "November",
 "December"] NO-UNDO.
def var lbut as logical NO-UNDO.
DEF VAR newdate AS DATE NO-UNDO.
def var totbut as int no-undo.
def var bhandle as widget-handle extent 42 no-undo.
def var lname as char no-undo.
def var i as integer no-undo.
def var lday as integer no-undo.
def var ok as logical no-undo.
DEF VAR barray AS HANDLE EXTENT 42 NO-UNDO.
DEF VAR pmod AS LOGICAL NO-UNDO.
/*fin definicion variables calendario*/

/*variables locales del programa*/
  DEFINE TEMP-TABLE TEMPORAL
       FIELD Dia1      AS INTEGER FORMAT ">>" LABEL "Dom."
       FIELD Dia2      AS INTEGER FORMAT ">>" LABEL "Lun."
       FIELD Dia3      AS INTEGER FORMAT ">>" LABEL "Mar."
       FIELD Dia4      AS INTEGER FORMAT ">>" LABEL "Mie."
       FIELD Dia5      AS INTEGER FORMAT ">>" LABEL "Jue."
       FIELD Dia6      AS INTEGER FORMAT ">>" LABEL "Vie."
       FIELD Dia7      AS INTEGER FORMAT ">>" LABEL "Sab."
       FIELD Tipo1     LIKE Calendario.Tipo
       FIELD Tipo2     LIKE Calendario.Tipo
       FIELD Tipo3     LIKE Calendario.Tipo
       FIELD Tipo4     LIKE Calendario.Tipo
       FIELD Tipo5     LIKE Calendario.Tipo
       FIELD Tipo6     LIKE Calendario.Tipo
       FIELD Tipo7     LIKE Calendario.Tipo
       FIELD Cierre    AS LOGICAL.

  DEF VAR Lo           AS LOGICAL.
  DEF VAR W_Sw         AS LOGICAL  INITIAL FALSE.
  DEF VAR Sw_Generar   AS LOGICAL.
  /*DEF VAR I                AS INTEGER.*/
  DEF VAR J                AS INTEGER. 
  DEF VAR W_Rta            AS LOGICAL.
  DEF VAR W_EraDiaCierre   AS LOGICAL.
  DEF VAR W_EraDiaCieAnual AS LOGICAL.
  DEF VAR W_Nomdia     AS CHAR.
  DEF VAR W_DiaSel     AS INTEGER FORMAT "99".
  DEF VAR W_AuxMes     AS INTEGER FORMAT "99".
  DEF VAr W_FecStr     AS DATE    FORMAT "99/99/9999".
  DEF VAR In-Date      AS DATE    FORMAT "99/99/9999".
  DEF VAR W_FecNva     AS DATE    FORMAT "99/99/9999".
  DEF VAR Numdia       AS INTEGER FORMAT "99".
  DEF VAR P_Agencia    AS INTEGER FORMAT "999".
  DEF VAR NumMes       AS INTEGER FORMAT "9".
  DEF VAR Sem          AS INTEGER FORMAT "9".
  DEF VAR W_ValCal     AS INTEGER FORMAT "99".
  DEF VAR W_DiaSelAnt  AS INTEGER FORMAT "99"    INITIAL 1.
  DEF VAR W_ProcCie    AS INTEGER FORMAT "99999" INITIAL 1.
  DEF VAR W_Crear      AS LOGICAL                INITIAL TRUE.
  DEF VAR W_MesAnt     AS CHAR    FORMAT "X(9)"  INITIAL "".
  DEF VAR W_Rowid      AS ROWID.
  DEF VAR W_AuxFec     AS DATE.
  DEF VAR W_Vb         AS HANDLE.
  DEF VAR Day-name     AS CHAR    NO-UNDO
                          INIT "Domingo,Lunes,Martes,Miercoles,Jueves,Viernes,Sabado".
  DEF VAR W_CodPro     LIKE Varios.Codigo.
  DEF VAR W_RowTempo     AS ROWID.


  /*tabla temporal para la consulta de configuraicon de procesos*/
  DEFINE TEMP-TABLE TProcDia LIKE ProcDia.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Brw-Ofi

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Agencias ProcDia Calendario

/* Definitions for BROWSE Brw-Ofi                                       */
&Scoped-define FIELDS-IN-QUERY-Brw-Ofi Agencias.Agencia Agencias.Nombre 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw-Ofi 
&Scoped-define QUERY-STRING-Brw-Ofi FOR EACH Agencias ~
      WHERE Agencias.Estado <> 3 NO-LOCK
&Scoped-define OPEN-QUERY-Brw-Ofi OPEN QUERY Brw-Ofi FOR EACH Agencias ~
      WHERE Agencias.Estado <> 3 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-Brw-Ofi Agencias
&Scoped-define FIRST-TABLE-IN-QUERY-Brw-Ofi Agencias


/* Definitions for BROWSE B_ProcDia                                     */
&Scoped-define FIELDS-IN-QUERY-B_ProcDia ProcDia.Agencia ProcDia.Cod_Proceso WNomProceso WEstado ProcDia.Fecha_Proc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_ProcDia   
&Scoped-define SELF-NAME B_ProcDia
&Scoped-define QUERY-STRING-B_ProcDia FOR EACH ProcDia NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_ProcDia OPEN QUERY {&SELF-NAME} FOR EACH ProcDia NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_ProcDia ProcDia
&Scoped-define FIRST-TABLE-IN-QUERY-B_ProcDia ProcDia


/* Definitions for FRAME F-Main                                         */
&Scoped-define FIELDS-IN-QUERY-F-Main Calendario.Tipo Calendario.Habil ~
Calendario.Estado Calendario.Cierre Calendario.Cieanual ~
Calendario.Trimestre_Cierre 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F-Main Calendario.Tipo ~
Calendario.Habil Calendario.Estado Calendario.Cierre Calendario.Cieanual ~
Calendario.Trimestre_Cierre 
&Scoped-define ENABLED-TABLES-IN-QUERY-F-Main Calendario
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F-Main Calendario
&Scoped-define QUERY-STRING-F-Main FOR EACH Calendario SHARE-LOCK
&Scoped-define OPEN-QUERY-F-Main OPEN QUERY F-Main FOR EACH Calendario SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F-Main Calendario
&Scoped-define FIRST-TABLE-IN-QUERY-F-Main Calendario


/* Definitions for FRAME FCalendario                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FCalendario ~
    ~{&OPEN-QUERY-Brw-Ofi}

/* Definitions for FRAME F_ConProceso                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_ConProceso ~
    ~{&OPEN-QUERY-B_ProcDia}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Calendario.Tipo Calendario.Habil ~
Calendario.Estado Calendario.Cierre Calendario.Cieanual ~
Calendario.Trimestre_Cierre 
&Scoped-define ENABLED-TABLES Calendario
&Scoped-define FIRST-ENABLED-TABLE Calendario
&Scoped-Define ENABLED-OBJECTS monthimage RECT-145 RECT-146 RECT-147 ~
RECT-15 RECT-16 RECT-17 RECT-219 RECT-281 Com-Agencia BInfo b-prevbmonth ~
b-prevyear b-nextmonth b-nextyear BSalvar BProce BCreaCal BMensaje ~
Cont_Entrada BtnDone BAyuda 
&Scoped-Define DISPLAYED-FIELDS Calendario.Tipo Calendario.Habil ~
Calendario.Estado Calendario.Cierre Calendario.Cieanual ~
Calendario.Trimestre_Cierre 
&Scoped-define DISPLAYED-TABLES Calendario
&Scoped-define FIRST-DISPLAYED-TABLE Calendario
&Scoped-Define DISPLAYED-OBJECTS Com-Agencia ldday styear 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 FConfiguracion FHorario FCalendario FMensaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetDataValue wWin 
FUNCTION GetDataValue RETURNS DATE
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetDataValue wWin 
FUNCTION SetDataValue RETURNS LOGICAL
  ( pcdate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-nextmonth 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Btn 3":L 
     SIZE 2.57 BY .62.

DEFINE BUTTON b-nextyear 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Btn 3":L 
     SIZE 2.57 BY .62.

DEFINE BUTTON b-prevbmonth 
     IMAGE-UP FILE "imagenes/arwup.gif":U
     LABEL "Btn 3":L 
     SIZE 2.57 BY .62.

DEFINE BUTTON b-prevyear 
     IMAGE-UP FILE "imagenes/arwup.gif":U
     LABEL "Btn 3":L 
     SIZE 2.57 BY .62.

DEFINE BUTTON B1  NO-FOCUS NO-CONVERT-3D-COLORS
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b10 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b11 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b12 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b13 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b14 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b15 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b16 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b17  NO-FOCUS
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b18 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b19 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON B2  NO-FOCUS
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b20 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b21 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b22 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b23 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b24 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b25 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b26 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b27 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b28 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b29 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON B3  NO-FOCUS
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b30 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b31 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b32 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b33 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b34 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b35 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b36 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b37 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b38 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b39 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON B4  NO-FOCUS
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b40 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b41 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b42 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b5 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b6 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b7 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b8 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b9 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON BAyuda 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 48" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BCreaCal 
     LABEL "Generación" 
     SIZE 12 BY 1.38.

DEFINE BUTTON BInfo 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 49" 
     SIZE 9 BY 1.62.

DEFINE BUTTON BMensaje 
     LABEL "Mensaje" 
     SIZE 12 BY 1.38.

DEFINE BUTTON BProce 
     LABEL "Procesos" 
     SIZE 12 BY 1.38.

DEFINE BUTTON BSalvar 
     LABEL "Salvar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "Salir" 
     SIZE 12 BY 1.35
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-119 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 119" 
     SIZE 9 BY 1.62.

DEFINE BUTTON BUTTON-120 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 120" 
     SIZE 9 BY 1.62.

DEFINE BUTTON Cont_Entrada 
     LABEL "Control Entrada" 
     SIZE 12 BY 1.38.

DEFINE VARIABLE Com-Agencia AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ldday AS CHARACTER FORMAT "XXX":U 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1.08
     BGCOLOR 17 FGCOLOR 7 FONT 6 NO-UNDO.

DEFINE VARIABLE styear AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY 1.08
     BGCOLOR 17 FGCOLOR 7 FONT 6 NO-UNDO.

DEFINE IMAGE monthimage
     FILENAME "imagenes/month1.gif":U
     SIZE 11 BY .92.

DEFINE RECTANGLE RECT-145
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 1.04.

DEFINE RECTANGLE RECT-146
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 1.04.

DEFINE RECTANGLE RECT-147
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 1.04.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY 1.62.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 28 BY 5.65
     BGCOLOR 17 .

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 8.86 BY 1.62.

DEFINE RECTANGLE RECT-219
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11 BY 5.38.

DEFINE RECTANGLE RECT-281
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14 BY 9.15.

DEFINE BUTTON BSalCal 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 47" 
     SIZE 8 BY 1.65.

DEFINE BUTTON Btn_Gen 
     LABEL "Generar" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE W_Ano1 AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Con AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Registros Creados..." 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rad-Ofi AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Actual", 1,
"Selección", 2,
"Todas", 3
     SIZE 33 BY .54 NO-UNDO.

DEFINE VARIABLE Rad-Sel AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Sin Configuración", 1,
"Copia Agencia Actual", 2
     SIZE 19 BY 1.35 NO-UNDO.

DEFINE RECTANGLE RECT-141
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 1.08.

DEFINE BUTTON BSalCon 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 46" 
     SIZE 8 BY 1.65.

DEFINE BUTTON BUTTON-167 
     LABEL "Asignar el Proceso" 
     SIZE 38 BY 1.35.

DEFINE BUTTON BUTTON-168 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 168" 
     SIZE 8 BY 1.65.

DEFINE BUTTON BUTTON-170 
     LABEL "Borrar Procesos" 
     SIZE 38 BY 1.35.

DEFINE VARIABLE Cmb_Procesos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Procesos Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 38 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Creacion AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todos los Dias del Año", 1,
"Todos los días de Cierre Mensual", 2,
"El día de Hoy", 3
     SIZE 34 BY 2.69 NO-UNDO.

DEFINE RECTANGLE RECT-291
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 3.5.

DEFINE BUTTON BSalHor 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 43" 
     SIZE 8 BY 1.65.

DEFINE VARIABLE Com-Hora1 AS INTEGER FORMAT "99":U INITIAL 8 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Com-Hora2 AS INTEGER FORMAT "99":U INITIAL 5 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Com-Min1 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0","15","30","45" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Com-Min2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0","15","30","45" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Com-Tiempo1 AS CHARACTER FORMAT "X(3)":U INITIAL "A.M" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "A.M","P.M" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Com-Tiempo2 AS CHARACTER FORMAT "X(3)":U INITIAL "P.M" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "A.M","P.M" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON BSalMen 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 45" 
     SIZE 8 BY 1.65.

DEFINE BUTTON BUTTON-169 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 169" 
     SIZE 9 BY 1.62.

DEFINE VARIABLE W_Busca AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 20.14 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE R_Busca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cod.Proceso", 1,
"Fecha", 2
     SIZE 26 BY .81
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-292
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 1.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Brw-Ofi FOR 
      Agencias SCROLLING.

DEFINE QUERY B_ProcDia FOR 
      ProcDia SCROLLING.

DEFINE QUERY F-Main FOR 
      Calendario SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw-Ofi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw-Ofi wWin _STRUCTURED
  QUERY Brw-Ofi NO-LOCK DISPLAY
      Agencias.Agencia COLUMN-LABEL "Age" FORMAT "999":U
      Agencias.Nombre FORMAT "X(35)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS MULTIPLE SIZE 36 BY 4.85
         BGCOLOR 15 FGCOLOR 7 FONT 5.

DEFINE BROWSE B_ProcDia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_ProcDia wWin _FREEFORM
  QUERY B_ProcDia NO-LOCK DISPLAY
      ProcDia.Agencia     FORMAT "999"   COLUMN-LABEL "Age"
  ProcDia.Cod_Proceso FORMAT "99999" COLUMN-LABEL "Proceso"
  WNomProceso          FORMAT "X(40)" COLUMN-LABEL "Nombre"
  WEstado              FORMAT "X(10)" COLUMN-LABEL "Estado"
  ProcDia.Fecha_Proc  FORMAT "99/99/9999"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 58 BY 5.92
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     B1 AT ROW 5.04 COL 43
     Com-Agencia AT ROW 1.27 COL 37 COLON-ALIGNED NO-LABEL
     BInfo AT ROW 1.81 COL 101
     b-prevbmonth AT ROW 2.62 COL 51
     ldday AT ROW 2.62 COL 52 COLON-ALIGNED NO-LABEL
     styear AT ROW 2.62 COL 56.57 COLON-ALIGNED NO-LABEL
     b-prevyear AT ROW 2.62 COL 63.72
     b-nextmonth AT ROW 3.15 COL 51
     b-nextyear AT ROW 3.15 COL 63.72
     BUTTON-119 AT ROW 3.42 COL 101
     b5 AT ROW 5.04 COL 55
     b6 AT ROW 5.04 COL 58
     b7 AT ROW 5.04 COL 61
     BUTTON-120 AT ROW 5.04 COL 101
     b8 AT ROW 5.85 COL 43
     b9 AT ROW 5.85 COL 46
     b10 AT ROW 5.85 COL 49
     b11 AT ROW 5.85 COL 52
     b12 AT ROW 5.85 COL 55
     b13 AT ROW 5.85 COL 58
     b14 AT ROW 5.85 COL 61
     b15 AT ROW 6.65 COL 43
     b16 AT ROW 6.65 COL 46
     b18 AT ROW 6.65 COL 52
     b19 AT ROW 6.65 COL 55
     b20 AT ROW 6.65 COL 58
     b21 AT ROW 6.65 COL 61
     b22 AT ROW 7.46 COL 43
     b23 AT ROW 7.46 COL 46
     b24 AT ROW 7.46 COL 49
     b25 AT ROW 7.46 COL 52
     b26 AT ROW 7.46 COL 55
     b27 AT ROW 7.46 COL 58
     b28 AT ROW 7.46 COL 61
     b29 AT ROW 8.27 COL 43
     b30 AT ROW 8.27 COL 46
     b31 AT ROW 8.27 COL 49
     b32 AT ROW 8.27 COL 52
     b33 AT ROW 8.27 COL 55
     b34 AT ROW 8.27 COL 58
     b35 AT ROW 8.27 COL 61
     b36 AT ROW 9.08 COL 43
     b17 AT ROW 6.65 COL 49
     b37 AT ROW 9.08 COL 46
     b38 AT ROW 9.08 COL 49
     b39 AT ROW 9.08 COL 52
     b40 AT ROW 9.08 COL 55
     b41 AT ROW 9.08 COL 58
     b42 AT ROW 9.08 COL 61
     Calendario.Tipo AT ROW 10.96 COL 35 HELP
          "Indique si el Día es Normal o Festivo" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Normal", yes,
"Festivo", no
          SIZE 33 BY .54
          BGCOLOR 17 FGCOLOR 7 FONT 5
     BSalvar AT ROW 10.96 COL 100
     Calendario.Habil AT ROW 12.04 COL 35 HELP
          "Inidique si el Dia es Hábil o no Hábil" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Hábil", yes,
"No Hábil", no
          SIZE 36 BY .54
          BGCOLOR 17 FGCOLOR 7 FONT 5
     B2 AT ROW 5.04 COL 46
     BProce AT ROW 12.58 COL 100
     Calendario.Estado AT ROW 13.12 COL 35 HELP
          "Indique si el Día esta Abierto o Cerrado para trabajar" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Día Abierto", 1,
"Día Cerrado", 2
          SIZE 34 BY .54
          BGCOLOR 17 FGCOLOR 7 FONT 5
     B3 AT ROW 5.04 COL 49
     BCreaCal AT ROW 13.92 COL 100
     Calendario.Cierre AT ROW 14.19 COL 35
          LABEL "Cierre de Mes"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .54
          FGCOLOR 7 FONT 5
     Calendario.Cieanual AT ROW 15 COL 35
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY .54
          FGCOLOR 7 FONT 5
     B4 AT ROW 5.04 COL 52
     BMensaje AT ROW 15.27 COL 100
     Calendario.Trimestre_Cierre AT ROW 15.81 COL 35
          LABEL "Cierre Trimestre"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .77
          FGCOLOR 7 FONT 5
     Cont_Entrada AT ROW 16.62 COL 100
     BtnDone AT ROW 17.96 COL 100
     BAyuda AT ROW 20.92 COL 106
     " Do Ln Ma Mi Ju Vi Sa" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 4.23 COL 42
          FGCOLOR 7 FONT 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 22.35
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     monthimage AT ROW 2.81 COL 40
     RECT-145 AT ROW 10.69 COL 34
     RECT-146 AT ROW 11.77 COL 34
     RECT-147 AT ROW 12.85 COL 34
     RECT-15 AT ROW 2.35 COL 39
     RECT-16 AT ROW 4.5 COL 39
     RECT-17 AT ROW 2.35 COL 58
     RECT-219 AT ROW 1.54 COL 100
     RECT-281 AT ROW 10.69 COL 99
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 22.35
         BGCOLOR 17 FONT 4.

DEFINE FRAME FMensaje
     Calendario.Acontecimiento AT ROW 1.27 COL 3 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 34 BY 8.88
          BGCOLOR 15 
     BSalMen AT ROW 10.69 COL 30
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 36 ROW 10.69
         SIZE 39 BY 12.38
         BGCOLOR 17 FONT 4
         TITLE "Mensaje del Día".

DEFINE FRAME FConfiguracion
     Cmb_Procesos AT ROW 1.27 COL 21 COLON-ALIGNED
     R_Creacion AT ROW 3.42 COL 24 NO-LABEL
     BUTTON-167 AT ROW 6.65 COL 23
     BUTTON-170 AT ROW 8.27 COL 23
     BUTTON-168 AT ROW 10.69 COL 45
     BSalCon AT ROW 10.69 COL 53
     "Especifique como será la configuración" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 2.62 COL 24
          FGCOLOR 7 
     RECT-291 AT ROW 2.88 COL 23
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 22 ROW 10.69
         SIZE 63 BY 12.38
         BGCOLOR 17 FONT 5
         TITLE "Configuración de Procesos".

DEFINE FRAME FCalendario
     Brw-Ofi AT ROW 1.27 COL 2
     W_Ano1 AT ROW 6.38 COL 5 COLON-ALIGNED
     Rad-Sel AT ROW 6.38 COL 19 NO-LABEL
     Rad-Ofi AT ROW 8.27 COL 4 NO-LABEL
     W_Con AT ROW 9.35 COL 27 COLON-ALIGNED
     Btn_Gen AT ROW 10.69 COL 20
     BSalCal AT ROW 10.69 COL 30
     " Agencia" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 7.46 COL 5
          FGCOLOR 7 FONT 5
     RECT-141 AT ROW 8 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 36 ROW 10.69
         SIZE 39 BY 12.38
         BGCOLOR 17 FONT 4
         TITLE "Generación de nuevos Calendarios".

DEFINE FRAME FHorario
     Calendario.Control_Horas AT ROW 3.15 COL 6
          LABEL "Control Horas Laborales"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .5
          FONT 5
     Com-Hora1 AT ROW 6.12 COL 9 COLON-ALIGNED NO-LABEL
     Com-Min1 AT ROW 6.12 COL 16 COLON-ALIGNED NO-LABEL
     Com-Tiempo1 AT ROW 6.12 COL 23 COLON-ALIGNED NO-LABEL
     Com-Hora2 AT ROW 8 COL 9 COLON-ALIGNED NO-LABEL
     Com-Min2 AT ROW 8 COL 16 COLON-ALIGNED NO-LABEL
     Com-Tiempo2 AT ROW 8 COL 23 COLON-ALIGNED NO-LABEL
     BSalHor AT ROW 10.69 COL 30
     "Hora Inicial (HH:MM)" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 4.77 COL 6
          FGCOLOR 7 FONT 5
     "Hora Final (HH:MM)" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 7.19 COL 6
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 36 ROW 10.69
         SIZE 39 BY 12.38
         BGCOLOR 17 FONT 4
         TITLE "Control de Entrada".

DEFINE FRAME F_ConProceso
     B_ProcDia AT ROW 1.27 COL 3
     BUTTON-169 AT ROW 8.54 COL 52
     W_Busca AT ROW 9 COL 28 COLON-ALIGNED NO-LABEL
     R_Busca AT ROW 9.08 COL 4 NO-LABEL
     RECT-292 AT ROW 8.54 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 22 ROW 12.85
         SIZE 63 BY 10.23
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Procesos".


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
         TITLE              = "SFG - Control del Sistema"
         HEIGHT             = 22.35
         WIDTH              = 113.86
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
ASSIGN FRAME FCalendario:FRAME = FRAME F-Main:HANDLE
       FRAME FConfiguracion:FRAME = FRAME F-Main:HANDLE
       FRAME FHorario:FRAME = FRAME F-Main:HANDLE
       FRAME FMensaje:FRAME = FRAME F-Main:HANDLE
       FRAME F_ConProceso:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON b-nextmonth IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON b-nextyear IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON b-prevbmonth IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON b-prevyear IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B1 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       B1:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b10 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b10:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b11 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b11:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b12 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b12:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b13 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b13:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b14 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b14:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b15 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b15:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b16 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b16:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b17 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b17:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b18 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b18:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b19 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b19:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON B2 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       B2:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b20 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b20:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b21 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b21:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b22 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b22:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b23 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b23:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b24 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b24:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b25 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b25:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b26 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b26:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b27 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b27:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b28 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b28:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b29 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b29:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON B3 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       B3:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b30 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b30:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b31 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b31:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b32 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b32:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b33 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b33:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b34 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b34:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b35 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b35:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b36 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b36:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b37 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b37:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b38 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b38:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b39 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b39:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON B4 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       B4:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b40 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b40:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b41 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b41:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b42 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b42:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b5 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b5:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b6 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b6:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b7 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b7:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b8 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b8:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b9 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b9:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON BUTTON-119 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-120 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Calendario.Cierre IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET Calendario.Estado IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR RADIO-SET Calendario.Habil IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN ldday IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN styear IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET Calendario.Tipo IN FRAME F-Main
   EXP-HELP                                                             */
/* SETTINGS FOR TOGGLE-BOX Calendario.Trimestre_Cierre IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME FCalendario
   NOT-VISIBLE 1                                                        */
/* BROWSE-TAB Brw-Ofi RECT-141 FCalendario */
ASSIGN 
       FRAME FCalendario:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE Brw-Ofi IN FRAME FCalendario
   NO-ENABLE                                                            */
ASSIGN 
       Brw-Ofi:HIDDEN  IN FRAME FCalendario                = TRUE.

/* SETTINGS FOR BUTTON Btn_Gen IN FRAME FCalendario
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Con IN FRAME FCalendario
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FConfiguracion
   NOT-VISIBLE 1                                                        */
ASSIGN 
       FRAME FConfiguracion:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FHorario
   NOT-VISIBLE 1                                                        */
ASSIGN 
       FRAME FHorario:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX Com-Hora1 IN FRAME FHorario
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Com-Hora2 IN FRAME FHorario
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Com-Min1 IN FRAME FHorario
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Com-Min2 IN FRAME FHorario
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Com-Tiempo1 IN FRAME FHorario
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Com-Tiempo2 IN FRAME FHorario
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX Calendario.Control_Horas IN FRAME FHorario
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME FMensaje
   NOT-VISIBLE 1                                                        */
ASSIGN 
       FRAME FMensaje:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_ConProceso
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_ProcDia RECT-292 F_ConProceso */
ASSIGN 
       FRAME F_ConProceso:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw-Ofi
/* Query rebuild information for BROWSE Brw-Ofi
     _TblList          = "bdcentral.Agencias"
     _Options          = "NO-LOCK"
     _Where[1]         = "Agencias.Estado <> 3"
     _FldNameList[1]   > bdcentral.Agencias.Agencia
"Agencias.Agencia" "Age" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > bdcentral.Agencias.Nombre
"Agencias.Nombre" ? "X(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE Brw-Ofi */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_ProcDia
/* Query rebuild information for BROWSE B_ProcDia
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ProcDia NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_ProcDia */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _TblList          = "bdCentral.Calendario"
     _Query            is OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Control del Sistema */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Control del Sistema */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-nextmonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-nextmonth wWin
ON CHOOSE OF b-nextmonth IN FRAME F-Main /* Btn 3 */
DO:
      IF lmonth < 12 THEN
      lmonth = lmonth + 1.
      
      ELSE DO: 
           lmonth = 1.
           lyear = lyear + 1.
      END.
     
      RUN setday. 
      RUN setimage. 
      pmod = TRUE.
      /*DYNAMIC-FUNCTION('setDataModified':U,
      INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-nextyear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-nextyear wWin
ON CHOOSE OF b-nextyear IN FRAME F-Main /* Btn 3 */
DO:

     lyear = lyear + 1.
     FIND FIRST Calendario WHERE Calendario.Ano EQ LYear NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Calendario) THEN
     DO:
         MESSAGE "El calendario para el año " STRING(Lyear) " no ha sido creado" SKIP
                 "utilice el boton de 'Generación de Calendario' para crear un nuevo" SKIP
                 "Calendario!" VIEW-AS ALERT-BOX INFORMATION.
         Lyear = Lyear - 1.
         RETURN NO-APPLY.
     END.
     RUN setday. 
     RUN  setimage. 
     pmod = TRUE.
    /* DYNAMIC-FUNCTION('setDataModified':U,INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-prevbmonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-prevbmonth wWin
ON CHOOSE OF b-prevbmonth IN FRAME F-Main /* Btn 3 */
DO:
   
    IF  lmonth > 1 THEN
    lmonth = lmonth - 1.
    
    ELSE DO:
            lmonth = 12.
            lyear = lyear - 1.
    END.
    
    RUN setday. 
    RUN setimage. 
    pmod = TRUE.
    /*DYNAMIC-FUNCTION('setDataModified':U,
     INPUT pMod).   */
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-prevyear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-prevyear wWin
ON CHOOSE OF b-prevyear IN FRAME F-Main /* Btn 3 */
DO:

     lyear = lyear - 1.
     FIND FIRST Calendario WHERE Calendario.Ano EQ LYear NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Calendario) THEN
     DO:
         MESSAGE "El calendario para el año " STRING(Lyear) " no ha sido creado" SKIP
                 "utilice el boton de 'Generación de Calendario' para crear un nuevo" SKIP
                 "Calendario!" VIEW-AS ALERT-BOX INFORMATION.
         Lyear = Lyear + 1.
         RETURN NO-APPLY.
     END.
     RUN setday. 
     RUN setimage. 
     pmod = TRUE.
     /*DYNAMIC-FUNCTION('setDataModified':U,
     INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B1 wWin
ON CHOOSE OF B1 IN FRAME F-Main
,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25,b26,b27,b28,
 b29,b30,b31,b32,b33,b34,b35,b36,b37,b38,b39,b40,b41,b42 DO:
     ASSIGN
       lday = integer(self:label).
     RUN dispdate.
   
  pmod = TRUE.
  /*RUN BuscarDia.*/
/*DYNAMIC-FUNCTION('setDataModified':U,
     INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BCreaCal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BCreaCal wWin
ON CHOOSE OF BCreaCal IN FRAME F-Main /* Generación */
DO:
    HIDE FRAME FConfiguracion FRAME FHorario FRAME FMensaje FRAME FParametros.
    VIEW FRAME FCalendario.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BInfo wWin
ON CHOOSE OF BInfo IN FRAME F-Main /* Button 49 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BMensaje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BMensaje wWin
ON CHOOSE OF BMensaje IN FRAME F-Main /* Mensaje */
DO:
    HIDE FRAME FCalendario FRAME FConfiguracion FRAME FHorario FRAME FParametros.
    VIEW FRAME FMensaje.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BProce
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BProce wWin
ON CHOOSE OF BProce IN FRAME F-Main /* Procesos */
DO:
    HIDE FRAME FCalendario FRAME FHorario FRAME FMensaje FRAME FParametros.
    VIEW FRAME FConfiguracion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCalendario
&Scoped-define SELF-NAME BSalCal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BSalCal wWin
ON CHOOSE OF BSalCal IN FRAME FCalendario /* Button 47 */
DO:
  ENABLE ALL EXCEPT ldday styear WITH FRAME F-Main.
  HIDE FRAME FCalendario FRAME FHorario FRAME FMensaje FRAME FConfiguracion.
  RUN InitializeObject.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConfiguracion
&Scoped-define SELF-NAME BSalCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BSalCon wWin
ON CHOOSE OF BSalCon IN FRAME FConfiguracion /* Button 46 */
DO:
  HIDE FRAME FConfiguracion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FHorario
&Scoped-define SELF-NAME BSalHor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BSalHor wWin
ON CHOOSE OF BSalHor IN FRAME FHorario /* Button 43 */
DO:
  HIDE FRAME FHorario.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FMensaje
&Scoped-define SELF-NAME BSalMen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BSalMen wWin
ON CHOOSE OF BSalMen IN FRAME FMensaje /* Button 45 */
DO:
  HIDE FRAME FMensaje.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BSalvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BSalvar wWin
ON CHOOSE OF BSalvar IN FRAME F-Main /* Salvar */
DO:
    DEFINE VAR W_Hora1 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0.
  DEFINE VAR W_Hora2 AS INTEGER FORMAT ">>>,>>>,>>>" INITIAL 0.
  ASSIGN FRAME F-Main LDDay.
   IF Calendario.CieAnual:SCREEN-VALUE IN FRAME F-Main = STRING(TRUE) THEN    
     DO:
       FIND Calendario WHERE Calendario.Ano      = INTEGER(StYear:SCREEN-VALUE IN FRAME F-Main)
                       AND   Calendario.Agencia  = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) 
                       AND   Calendario.CieAnual = TRUE NO-LOCK NO-ERROR. 
       IF AVAILABLE(Calendario) THEN
         DO:
           RUN MostrarMensaje IN W_Manija (INPUT 332, OUTPUT W_Rta).
           RETURN NO-APPLY.
         END.
     END.
   ELSE
    IF Calendario.Cierre:SCREEN-VALUE IN FRAME F-Main = STRING(TRUE) THEN    
     DO:
       FIND Calendario WHERE Calendario.Agencia  = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                       AND   Calendario.Ano      = INTEGER(StYear:SCREEN-VALUE IN FRAME F-Main)
                       AND   Calendario.Mes      = LMonth 
                       AND   Calendario.Dia     <> INTEGER(LDDay)  
                       AND   Calendario.Cierre   = TRUE NO-LOCK NO-ERROR. 
       IF AVAILABLE(Calendario) THEN
         DO:
           RUN MostrarMensaje IN W_Manija (INPUT 277, OUTPUT W_Rta).
           RETURN NO-APPLY.
         END.
     END.
   ELSE
     IF W_EraDiaCierre THEN
       DO:
         RUN MostrarMensaje IN W_Manija (INPUT 278, OUTPUT W_Rta).
         IF W_Rta THEN
           DO:
             W_FecNva = DATE(Calendario.Mes,Calendario.Dia,INTEGER(StYear:SCREEN-VALUE IN FRAME F-Main)).
             FIND ProcDia WHERE ProcDia.Fecha_Proc  = W_FecNva   AND 
                                ProcDia.Cod_Proceso = W_ProcCie  AND 
                                ProcDia.Agencia     = W_Agencia  NO-ERROR.
             IF AVAILABLE(ProcDia) THEN
               DELETE ProcDia.
             RUN MostrarMensaje IN W_Manija (INPUT 279, OUTPUT W_Rta).
           END.
         ELSE
           DO:
             ASSIGN Calendario.Cierre:SCREEN-VALUE IN FRAME F-Main = STRING(TRUE).
             RETURN NO-APPLY.          
           END.
       END.

   IF Calendario.Control_Horas:SCREEN-VALUE IN FRAME FHorario = "YES" THEN
    DO:
     DO WITH FRAME FHorario:
      IF Com-Tiempo1:SCREEN-VALUE = "A.M" THEN 
         ASSIGN W_Hora1 = (INTEGER(Com-Hora1:SCREEN-VALUE) * 3600) + (INTEGER(Com-Min1:SCREEN-VALUE) * 60).
      ELSE
         ASSIGN W_Hora1 = (INTEGER(Com-Hora1:SCREEN-VALUE) * 3600) + (INTEGER(Com-Min1:SCREEN-VALUE) * 60)
                W_Hora1 = W_Hora1 + 43200.

      IF Com-Tiempo2:SCREEN-VALUE = "A.M" THEN
         ASSIGN W_Hora2 = (INTEGER(Com-Hora2:SCREEN-VALUE) * 3600) + (INTEGER(Com-Min2:SCREEN-VALUE) * 60).
      ELSE
        ASSIGN W_Hora2 = (INTEGER(Com-Hora2:SCREEN-VALUE) * 3600) + (INTEGER(Com-Min2:SCREEN-VALUE) * 60)
               W_Hora2 = W_Hora2 + 43200.
     END.
    END.
   ASSIGN W_AuxFec  = DATE(LMonth,INTEGER(Ldday),INTEGER(StYear:SCREEN-VALUE IN FRAME F-MAIN)).
   /* Inicia comentario */
   FIND Calendario  WHERE Calendario.Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                    AND   Calendario.Ano     = INTEGER(StYear:SCREEN-VALUE IN FRAME F-Main)
                    AND   Calendario.Mes     = LMonth 
                    AND   Calendario.Dia     = INTEGER(LDDay)     
                    SHARE-LOCK NO-WAIT NO-ERROR. 
   IF AVAILABLE(Calendario) THEN DO:
        ASSIGN FRAME FMensaje Calendario.Acontecimiento
               FRAME F-Main Calendario.Tipo
                            Calendario.Habil
                            Calendario.Estado        
                            Calendario.CieAnual
                            Calendario.Cierre
               FRAME FHorario Calendario.Control_Horas.
        ASSIGN Calendario.Trimestre_Cierre = LOGICAL(Calendario.Trimestre_Cierre:SCREEN-VALUE IN FRAME F-Main)
               Calendario.Hora_Inicial = W_Hora1
               Calendario.Hora_Final   = W_Hora2.
   END.
   /* Fin del comentario */
  FIND CURRENT Calendario NO-LOCK NO-ERROR.
  
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME F-Main /* Salir */
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


&Scoped-define FRAME-NAME FCalendario
&Scoped-define SELF-NAME Btn_Gen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Gen wWin
ON CHOOSE OF Btn_Gen IN FRAME FCalendario /* Generar */
DO:
  DEFINE VAR I      AS INTEGER.
  DEFINE VAR W_Rpta AS LOGICAL.
  ASSIGN Rad-Ofi
         Rad-Sel
         W_Ano1.
  CASE Rad-Ofi:
   WHEN 1 THEN
    DO:
     ASSIGN P_Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)).
     RUN ValidarCalendario.
     IF Sw_Generar THEN
        RUN Generar.
    END.
   WHEN 2 THEN
    DO:
     IF Rad-Sel = 1 THEN
      DO:
       IF Brw-Ofi:MULTIPLE THEN
        DO:
         DO I = 1 TO Brw-Ofi:NUM-SELECTED-ROWS :
           W_RPTA = Brw-Ofi:FETCH-SELECTED-ROW(I).
           GET CURRENT Brw-Ofi SHARE-LOCK NO-WAIT. 
           ASSIGN P_Agencia = Agencias.Agencia.
           RUN ValidarCalendario.
           IF Sw_Generar THEN
              RUN Generar.
           IF ERROR-STATUS:ERROR THEN
             W_Rpta = Brw-Ofi:DESELECT-SELECTED-ROW(I).
         END.
        END.
       IF Brw-Ofi:NUM-SELECTED-ROWS > 0 THEN
         W_Rpta = Brw-Ofi:DELETE-SELECTED-ROWS().
       OPEN QUERY Brw-Ofi FOR EACH Agencias WHERE Agencias.Estado = 1 NO-LOCK.
      END.
     ELSE
      DO:
       IF W_Ano1 = INTEGER(StYear:SCREEN-VALUE IN FRAME F-MAIN) THEN
        DO:
         FIND FIRST Calendario WHERE Calendario.Ano     = W_Ano1
                               AND   Calendario.Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                               NO-LOCK NO-ERROR.
         IF AVAILABLE(Calendario) THEN
          DO:
           IF Brw-Ofi:MULTIPLE THEN
            DO:
             DO I = 1 TO Brw-Ofi:NUM-SELECTED-ROWS :
               W_RPTA = Brw-Ofi:FETCH-SELECTED-ROW(I).
               GET CURRENT Brw-Ofi SHARE-LOCK NO-WAIT. 
               ASSIGN P_Agencia = Agencias.Agencia.
               RUN ValidarCalendario.
               IF Sw_Generar THEN
                  RUN Generar-Copia.
               IF ERROR-STATUS:ERROR THEN
                 W_Rpta = Brw-Ofi:DESELECT-SELECTED-ROW(I).
             END.
            END.
           IF Brw-Ofi:NUM-SELECTED-ROWS > 0 THEN
             W_Rpta = Brw-Ofi:DELETE-SELECTED-ROWS().
           OPEN QUERY Brw-Ofi FOR EACH Agencias WHERE Agencias.estado = 1 NO-LOCK.
          END.
         ELSE
          MESSAGE "La Agencia de Origen no Tiene" SKIP
                  "Nimgún Registro Para Copiar."       
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        END.
       ELSE
        MESSAGE "El Año de la Agencia Origen Debe Ser Igual" SKIP
                "al Año de Generación del Calendario."       
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      END.
    END.
   WHEN 3 THEN
    DO:
     IF Rad-Sel = 1 THEN
      DO:
       FOR EACH Agencias WHERE Agencias.Estado NE 3:
           ASSIGN P_Agencia = Agencias.Agencia.
           RUN ValidarCalendario.
           IF Sw_Generar THEN
              RUN Generar.
       END.
      END.
     ELSE
      DO:
       IF W_Ano1 = INTEGER(StYear:SCREEN-VALUE IN FRAME F-MAIN) THEN
        DO:
         FIND FIRST Calendario WHERE Calendario.Ano     = W_Ano1
                               AND   Calendario.Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                               NO-LOCK NO-ERROR.
         IF AVAILABLE(Calendario) THEN
          DO:
           FOR EACH Agencias WHERE Agencias.Agencia <> INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)):
               ASSIGN P_Agencia = Agencias.Agencia.
               RUN ValidarCalendario.
               IF Sw_Generar THEN
                  RUN Generar-Copia.
           END.
          END.
         ELSE
          MESSAGE "La Agencia de Origen no Tiene" SKIP
                  "Nimgún Registro Para Copiar."       
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        END.
       ELSE
        MESSAGE "El Año de la Agencia Origen Debe Ser Igual" SKIP
                "al Año de Generación del Calendario."       
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      END.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConfiguracion
&Scoped-define SELF-NAME BUTTON-167
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-167 wWin
ON CHOOSE OF BUTTON-167 IN FRAME FConfiguracion /* Asignar el Proceso */
DO:
  IF Cmb_Procesos:SCREEN-VALUE IN FRAME FConfiguracion = ? THEN DO:
     MESSAGE "Debe escogerse un Proceso a configurar" VIEW-AS ALERT-BOX.
     APPLY "entry" TO Cmb_Procesos IN FRAME FConfiguracion.
     RETURN NO-APPLY.
  END.
  RUN Asignar_Procesos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-168
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-168 wWin
ON CHOOSE OF BUTTON-168 IN FRAME FConfiguracion /* Button 168 */
DO:
  OPEN QUERY B_ProcDia
     FOR EACH ProcDia WHERE
              ProcDia.Agencia     EQ INTEGER(SUBSTRING(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) AND
              ProcDia.Cod_Proceso EQ INTEGER(SUBSTRING(Cmb_Procesos:SCREEN-VALUE IN FRAME FConfiguracion,1,5)) NO-LOCK INDEXED-REPOSITION.
  VIEW FRAME F_ConProceso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConProceso
&Scoped-define SELF-NAME BUTTON-169
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-169 wWin
ON CHOOSE OF BUTTON-169 IN FRAME F_ConProceso /* Button 169 */
DO:
  HIDE FRAME F_ConProceso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FConfiguracion
&Scoped-define SELF-NAME BUTTON-170
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-170 wWin
ON CHOOSE OF BUTTON-170 IN FRAME FConfiguracion /* Borrar Procesos */
DO:
  IF Cmb_Procesos:SCREEN-VALUE IN FRAME FConfiguracion = ? THEN DO:
     MESSAGE "Debe escogerse un Proceso a condfigurar" VIEW-AS ALERT-BOX.
     APPLY "entry" TO Cmb_Procesos IN FRAME FConfiguracion.
     RETURN NO-APPLY.
  END.
  RUN Borrar_Procesos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_ProcDia
&Scoped-define FRAME-NAME F_ConProceso
&Scoped-define SELF-NAME B_ProcDia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_ProcDia wWin
ON ROW-DISPLAY OF B_ProcDia IN FRAME F_ConProceso
DO:
  FIND Varios WHERE Varios.Tipo EQ 8 AND
                    Varios.Codigo EQ ProcDia.Cod_Proceso NO-LOCK NO-ERROR.
  IF AVAILABLE Varios THEN WNomProceso = Varios.Descripcion.
  
  IF ProcDia.Estado EQ 1 THEN WEstado = "No Proc".
  ELSE WEstado = "Procesado".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Calendario.Cieanual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Calendario.Cieanual wWin
ON ENTRY OF Calendario.Cieanual IN FRAME F-Main /* Cierre Anual */
DO:
  W_EraDiaCieAnual = FALSE.
  IF Calendario.CieAnual:SCREEN-VALUE IN FRAME F-Main = STRING(TRUE) THEN
      W_EraDiaCieAnual  = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Calendario.Cierre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Calendario.Cierre wWin
ON ENTRY OF Calendario.Cierre IN FRAME F-Main /* Cierre de Mes */
DO:
  W_EraDiaCierre = FALSE.
  IF Calendario.Cierre:SCREEN-VALUE IN FRAME F-Main = STRING(TRUE) THEN
    W_EraDiaCierre  = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Calendario.Cierre wWin
ON MOUSE-SELECT-CLICK OF Calendario.Cierre IN FRAME F-Main /* Cierre de Mes */
DO:
  APPLY "VALUE-CHANGED" TO Calendario.Cierre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Com-Agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Com-Agencia wWin
ON VALUE-CHANGED OF Com-Agencia IN FRAME F-Main
DO:
  RUN BuscarDia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FHorario
&Scoped-define SELF-NAME Calendario.Control_Horas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Calendario.Control_Horas wWin
ON MOUSE-SELECT-CLICK OF Calendario.Control_Horas IN FRAME FHorario /* Control Horas Laborales */
DO:
  APPLY "VALUE-CHANGED" TO Calendario.Control_horas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Calendario.Control_Horas wWin
ON VALUE-CHANGED OF Calendario.Control_Horas IN FRAME FHorario /* Control Horas Laborales */
DO:
  IF Calendario.Control_horas:SCREEN-VALUE IN FRAME FHorario EQ "YES" THEN
   DO:
    ENABLE Com-Hora1 Com-Hora2 Com-Min1 Com-Min2 Com-Tiempo1 Com-Tiempo2 WITH FRAME FHorario.
   END.
  ELSE
   DO:
    DISABLE Com-Hora1 Com-Hora2 Com-Min1 Com-Min2 Com-Tiempo1 Com-Tiempo2 WITH FRAME FHorario.
   END.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Cont_Entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cont_Entrada wWin
ON CHOOSE OF Cont_Entrada IN FRAME F-Main /* Control Entrada */
DO:
  HIDE FRAME FCalendario FRAME FConfiguracion FRAME Mensaje FRAME FParametros.
  VIEW FRAME FHorario.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Calendario.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Calendario.Estado wWin
ON MOUSE-SELECT-CLICK OF Calendario.Estado IN FRAME F-Main /* Estado */
DO:
  APPLY "VALUE-CHANGED" TO Calendario.Estado.
  IF SELF:SCREEN-VALUE EQ "1" THEN DO:
     FIND FIRST ProcDia WHERE 
                ProcDia.Agencia EQ INTEGER(SUBSTRING(Com-Agencia:SCREEN-VALUE,1,3)) AND
                ProcDia.Cod_Proceso EQ 7 AND
          MONTH(ProcDia.Fecha_Proc) EQ LMonth AND
           YEAR(ProcDia.Fecha_Proc) EQ LYear  AND
                ProcDia.Estado      EQ 2 NO-LOCK NO-ERROR.
     IF AVAILABLE ProcDia THEN DO:
        MESSAGE "No se puede abrir el día" SKIP
                "ya ha sido efectuado el cierre del mes actual" VIEW-AS ALERT-BOX WARNING.
        SELF:SCREEN-VALUE = "2".
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Calendario.Habil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Calendario.Habil wWin
ON MOUSE-SELECT-CLICK OF Calendario.Habil IN FRAME F-Main /* Habil */
DO:
  APPLY "VALUE-CHANGED" TO Calendario.Habil.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCalendario
&Scoped-define SELF-NAME Rad-Ofi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rad-Ofi wWin
ON VALUE-CHANGED OF Rad-Ofi IN FRAME FCalendario
DO:
  ASSIGN Rad-Ofi.
  CASE Rad-Ofi:
   WHEN 1 THEN
    DO:
     BROWSE Brw-Ofi:HIDDEN  = TRUE.
     DISABLE Rad-Sel WITH FRAME FCalendario.
     Rad-Sel:SCREEN-VALUE IN FRAME FCalendario = "1".
    END.
   WHEN 2 THEN
    DO:
     BROWSE Brw-Ofi:HIDDEN  = FALSE.
     ENABLE Rad-Sel Brw-Ofi WITH FRAME FCalendario.
     OPEN QUERY Brw-Ofi FOR EACH Agencias WHERE Agencias.Estado = 1 NO-LOCK.
    END.
   WHEN 3 THEN
    DO:
     BROWSE Brw-Ofi:HIDDEN  = TRUE.
     ENABLE Rad-Sel  WITH FRAME FCalendario.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Calendario.Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Calendario.Tipo wWin
ON MOUSE-SELECT-CLICK OF Calendario.Tipo IN FRAME F-Main /* Tipo */
DO:
  APPLY "VALUE-CHANGED" TO Calendario.Tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FCalendario
&Scoped-define SELF-NAME W_Ano1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Ano1 wWin
ON LEAVE OF W_Ano1 IN FRAME FCalendario /* Año */
DO:
  ASSIGN W_Ano1.
  IF W_Ano1 <> 0 AND W_Ano1 >= INTEGER(YEAR(TODAY) - 2) THEN
     ENABLE  Btn_Gen WITH FRAME FCalendario.
  ELSE
     MESSAGE "El Año Debe Ser Mayor o igual a " INTEGER(YEAR(TODAY) - 2)        
     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_ConProceso
&Scoped-define SELF-NAME W_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Busca wWin
ON LEAVE OF W_Busca IN FRAME F_ConProceso
DO:
  IF R_Busca:SCREEN-VALUE IN FRAME F_ConProceso EQ "1" THEN
     OPEN QUERY B_ProcDia FOR EACH ProcDia WHERE ProcDia.Cod_Proceso EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK INDEXED-REPOSITION.
  IF R_Busca:SCREEN-VALUE IN FRAME F_ConProceso EQ "2" THEN
     OPEN QUERY B_ProcDia FOR EACH ProcDia WHERE ProcDia.Fecha_Proc EQ DATE(SELF:SCREEN-VALUE) NO-LOCK INDEXED-REPOSITION.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Brw-Ofi
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_Procesos wWin 
PROCEDURE Asignar_Procesos :
DEFINE VAR WFF AS DATE.
ASSIGN FRAME FConfiguracion Cmb_Procesos R_Creacion.
CASE R_Creacion:
  WHEN 1 THEN DO:
     FOR EACH Calendario WHERE 
              Calendario.Agencia EQ INTEGER(SUBSTRING(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) AND
              Calendario.Ano     EQ INTEGER(styear):
        WFF = DATE(STRING(Calendario.Dia,"99") + "/" + STRING(Calendario.Mes,"99") + "/" + STRING(Calendario.Ano,"9999")).
        FIND ProcDia WHERE
             ProcDia.Agencia EQ Calendario.Agencia AND
             ProcDia.Cod_Proceso EQ INTEGER(SUBSTRING(Cmb_Procesos,1,5)) AND
             ProcDia.Fecha_Proc  EQ WFF NO-ERROR.
        IF NOT AVAILABLE ProcDia THEN DO:
           CREATE ProcDia.
           ASSIGN ProcDia.Agencia     = Calendario.Agencia
                  ProcDia.Fecha_Proc  = WFF
                  ProcDia.Cod_Proceso = INTEGER(SUBSTRING(Cmb_Procesos,1,5))
                  ProcDia.Estado      = 1.
        END.
     END.
  END.
  WHEN 2 THEN DO:
     FOR EACH Calendario WHERE 
              Calendario.Agencia EQ INTEGER(SUBSTRING(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) AND
              Calendario.Ano     EQ INTEGER(styear) AND
              Calendario.Cierre  EQ YES:
        WFF = DATE(STRING(Calendario.Dia,"99") + "/" + STRING(Calendario.Mes,"99") + "/" + STRING(Calendario.Ano,"9999")).
        FIND ProcDia WHERE
             ProcDia.Agencia EQ Calendario.Agencia AND
             ProcDia.Cod_Proceso EQ INTEGER(SUBSTRING(Cmb_Procesos,1,5)) AND
             ProcDia.Fecha_Proc  EQ WFF NO-ERROR.
        IF NOT AVAILABLE ProcDia THEN DO:
           CREATE ProcDia.
           ASSIGN ProcDia.Agencia     = Calendario.Agencia
                  ProcDia.Fecha_Proc  = WFF
                  ProcDia.Cod_Proceso = INTEGER(SUBSTRING(Cmb_Procesos,1,5))
                  ProcDia.Estado      = 1.
        END.
     END.
  END.
  WHEN 3 THEN DO:
     FIND Calendario WHERE 
          Calendario.Agencia EQ INTEGER(SUBSTRING(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) AND
          Calendario.Ano     EQ INTEGER(styear) AND
          Calendario.Mes     EQ INTEGER(lmonth) AND
          Calendario.Dia     EQ INTEGER(ldday) NO-ERROR.
        WFF = DATE(STRING(Calendario.Dia,"99") + "/" + STRING(Calendario.Mes,"99") + "/" + STRING(Calendario.Ano,"9999")).
        FIND ProcDia WHERE
             ProcDia.Agencia EQ Calendario.Agencia AND
             ProcDia.Cod_Proceso EQ INTEGER(SUBSTRING(Cmb_Procesos,1,5)) AND
             ProcDia.Fecha_Proc  EQ WFF NO-ERROR.
        IF NOT AVAILABLE ProcDia THEN DO:
           CREATE ProcDia.
           ASSIGN ProcDia.Agencia     = Calendario.Agencia
                  ProcDia.Fecha_Proc  = WFF
                  ProcDia.Cod_Proceso = INTEGER(SUBSTRING(Cmb_Procesos,1,5))
                  ProcDia.Estado      = 1.
        END.
  END.
END CASE.
MESSAGE "Asignacion Terminada Exitosamente" VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borrar_Procesos wWin 
PROCEDURE Borrar_Procesos :
DEFINE VAR WFF AS DATE.
ASSIGN FRAME FConfiguracion Cmb_Procesos R_Creacion.
CASE R_Creacion:
  WHEN 1 THEN DO:
    FOR EACH ProcDia WHERE
             ProcDia.Agencia EQ INTEGER(SUBSTRING(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) AND
             ProcDia.Cod_Proceso EQ INTEGER(SUBSTRING(Cmb_Procesos:SCREEN-VALUE,1,5)) AND
             YEAR(ProcDia.Fecha_Proc) EQ INTEGER(styear) AND 
             ProcDia.Estado EQ 1:
        DELETE ProcDia.
    END.
  END.
  WHEN 2 THEN DO:
     FOR EACH Calendario WHERE 
              Calendario.Agencia EQ INTEGER(SUBSTRING(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) AND
              Calendario.Ano     EQ INTEGER(styear) AND
              Calendario.Cierre  EQ YES:
        FIND ProcDia WHERE
             ProcDia.Agencia EQ Calendario.Agencia AND
             ProcDia.Cod_Proceso EQ INTEGER(SUBSTRING(Cmb_Procesos,1,5)) AND
             ProcDia.Fecha_Proc  EQ WFF NO-ERROR.
        IF AVAILABLE ProcDia AND ProcDia.Estado EQ 1 THEN
           DELETE ProcDia.
     END.
  END.
  WHEN 3 THEN DO:
     FIND Calendario WHERE 
          Calendario.Agencia EQ INTEGER(SUBSTRING(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3)) AND
          Calendario.Ano     EQ INTEGER(styear) AND
          Calendario.Mes     EQ INTEGER(lmonth) AND
          Calendario.Dia     EQ INTEGER(ldday) NO-ERROR.
        WFF = DATE(STRING(Calendario.Dia,"99") + "/" + STRING(Calendario.Mes,"99") + "/" + STRING(Calendario.Ano,"9999")).
        FIND ProcDia WHERE
             ProcDia.Agencia EQ Calendario.Agencia AND
             ProcDia.Cod_Proceso EQ INTEGER(SUBSTRING(Cmb_Procesos,1,5)) AND
             ProcDia.Fecha_Proc  EQ WFF NO-ERROR.
        IF AVAILABLE ProcDia AND ProcDia.Estado EQ 1 THEN
           DELETE ProcDia.
  END.
END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuscarDia wWin 
PROCEDURE BuscarDia :
DEFINE VARIABLE W_Hour   AS INTEGER.
  DEFINE VARIABLE W_Min    AS INTEGER.
  DEFINE VARIABLE W_Seg    AS INTEGER.
  DEFINE VARIABLE W_Tiempo AS INTEGER.
  DEFINE VARIABLE W_Factor AS INTEGER INITIAL 0.
  ASSIGN FRAME F-Main LDDay.

  FIND Calendario WHERE Calendario.Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                  AND   Calendario.Ano     = INTEGER(StYear:SCREEN-VALUE IN FRAME F-MAIN)
                  AND   Calendario.Mes     = LMonth
                  AND   Calendario.Dia     = INTEGER(LDDay)
                  NO-LOCK NO-ERROR. 
  IF AVAILABLE(Calendario) THEN
   DO:
     DO WITH FRAME FHorario:
       IF Calendario.Control_Horas = YES THEN
        DO:
         ENABLE Com-Hora1 Com-Hora2 Com-Min1 Com-Min2 Com-Tiempo1 Com-Tiempo2.
         W_Factor = 0.
         IF Calendario.Hora_Inicial <= 43199 THEN
           ASSIGN Com-Tiempo1:SCREEN-VALUE = "A.M"
                  W_Factor = 0.
         ELSE
            ASSIGN Com-Tiempo1:SCREEN-VALUE = "P.M"
                   W_Factor = 43200.
         ASSIGN W_Tiempo  = Calendario.Hora_Inicial - W_Factor
                W_Seg     = W_Tiempo MOD 60
                W_Tiempo  = (W_Tiempo - W_Seg) / 60
                W_Min     = W_Tiempo MOD 60
                W_Hour    = (W_Tiempo - W_Min) / 60
                Com-Hora1 = W_Hour
                Com-Min1  = W_Min NO-ERROR.
         DISPLAY Com-Hora1 Com-Min1.
         W_Factor = 0.
         IF Calendario.Hora_Final <= 43199 THEN
            ASSIGN Com-Tiempo2:SCREEN-VALUE = "A.M"
                   W_Factor = 0.
         ELSE
            ASSIGN Com-Tiempo2:SCREEN-VALUE = "P.M"
                   W_Factor = 43200.
         ASSIGN W_Tiempo  = Calendario.Hora_Final - W_Factor
                W_Seg     = W_Tiempo MOD 60
                W_Tiempo  = (W_Tiempo - W_Seg) / 60
                W_Min     = W_Tiempo MOD 60
                W_Hour    = (W_Tiempo - W_Min) / 60
                Com-Hora2 = W_Hour
                Com-Min2  = W_Min NO-ERROR.
         DISPLAY Com-Hora2 Com-Min2.
        END.
       ELSE
        DO:
         ASSIGN Com-Hora1   = 8
                Com-Hora2   = 5
                Com-Min1    = 0
                Com-Min2    = 0
                Com-Tiempo1 = "A.M"
                Com-Tiempo2 = "P.M".
         DISPLAY Com-Hora1 Com-Hora2 Com-Min1 Com-Min2 Com-Tiempo1 Com-Tiempo2.
         DISABLE Com-Hora1 Com-Hora2 Com-Min1 Com-Min2 Com-Tiempo1 Com-Tiempo2.
        END.
     END.
     DISPLAY Calendario.Acontecimiento WITH FRAME FMensaje.
     DISPLAY Calendario.Tipo
             Calendario.Habil  
             Calendario.Estado 
             Calendario.Cierre
             Calendario.CieAnual 
             Calendario.Trimestre_Cierre WITH FRAME F-Main. 
     DISPLAY Calendario.Control_Horas WITH FRAME FHorario.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DispDate wWin 
PROCEDURE DispDate :
DO WITH FRAME {&FRAME-NAME}:
   OK = monthimage:LOAD-IMAGE("imagenes\month" + STRING(lmonth) + ".gif") IN FRAME F-Main.
   newdate = DATE(lmonth,lday,lyear).
   ASSIGN ldday = string(lday,"z9") 
          styear =  string(lyear,"9999").
   DISPLAY  ldday styear WITH FRAME {&FRAME-NAME}. 
 END.
 RUN BuscarDia.
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

  {&OPEN-QUERY-F-Main}
  GET FIRST F-Main.
  DISPLAY Com-Agencia ldday styear 
      WITH FRAME F-Main IN WINDOW wWin.
  IF AVAILABLE Calendario THEN 
    DISPLAY Calendario.Tipo Calendario.Habil Calendario.Estado Calendario.Cierre 
          Calendario.Cieanual Calendario.Trimestre_Cierre 
      WITH FRAME F-Main IN WINDOW wWin.
  ENABLE monthimage RECT-145 RECT-146 RECT-147 RECT-15 RECT-16 RECT-17 RECT-219 
         RECT-281 Com-Agencia BInfo b-prevbmonth b-prevyear b-nextmonth 
         b-nextyear Calendario.Tipo BSalvar Calendario.Habil BProce 
         Calendario.Estado BCreaCal Calendario.Cierre Calendario.Cieanual 
         BMensaje Calendario.Trimestre_Cierre Cont_Entrada BtnDone BAyuda 
      WITH FRAME F-Main IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY Cmb_Procesos R_Creacion 
      WITH FRAME FConfiguracion IN WINDOW wWin.
  ENABLE RECT-291 Cmb_Procesos R_Creacion BUTTON-167 BUTTON-170 BUTTON-168 
         BSalCon 
      WITH FRAME FConfiguracion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FConfiguracion}
  DISPLAY W_Ano1 Rad-Sel Rad-Ofi W_Con 
      WITH FRAME FCalendario IN WINDOW wWin.
  ENABLE RECT-141 W_Ano1 Rad-Sel Rad-Ofi BSalCal 
      WITH FRAME FCalendario IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FCalendario}
  DISPLAY Com-Hora1 Com-Min1 Com-Tiempo1 Com-Hora2 Com-Min2 Com-Tiempo2 
      WITH FRAME FHorario IN WINDOW wWin.
  IF AVAILABLE Calendario THEN 
    DISPLAY Calendario.Control_Horas 
      WITH FRAME FHorario IN WINDOW wWin.
  ENABLE Calendario.Control_Horas BSalHor 
      WITH FRAME FHorario IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FHorario}
  IF AVAILABLE Calendario THEN 
    DISPLAY Calendario.Acontecimiento 
      WITH FRAME FMensaje IN WINDOW wWin.
  ENABLE Calendario.Acontecimiento BSalMen 
      WITH FRAME FMensaje IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FMensaje}
  DISPLAY W_Busca R_Busca 
      WITH FRAME F_ConProceso IN WINDOW wWin.
  ENABLE RECT-292 B_ProcDia BUTTON-169 W_Busca R_Busca 
      WITH FRAME F_ConProceso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConProceso}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar wWin 
PROCEDURE Generar :
DISABLE ALL WITH FRAME F-Main.
      W_Con = 0.
      DO I = 1 TO 12 BY 1:
        NumMes = I.
        IF NumMes = 1 OR NumMes = 3 OR NumMes = 5 OR NumMes = 7 OR NumMes = 8 OR 
          NumMes = 10 OR NumMes = 12 THEN
          DO:
            DO J = 1 TO 31:
              CREATE Calendario.
              ASSIGN Calendario.Agencia = P_Agencia
                     Calendario.Estado  = 2
                     Calendario.Ano     = W_Ano1
                     Calendario.Mes     = I
                     Calendario.Dia     = J.
              W_Con = W_Con + 1.
              DISPLAY W_Con WITH FRAME FCalendario.
            END.
          END. 
        ELSE
          IF NumMes = 4 OR NumMes = 6 OR NumMes = 9 OR NumMes = 11 THEN
            DO:
              DO J = 1 TO 30:
                CREATE Calendario.
                ASSIGN Calendario.Agencia = P_Agencia
                       Calendario.Estado  = 2
                       Calendario.Ano     = W_Ano1
                       Calendario.Mes     = I
                       Calendario.Dia     = J.
                W_Con = W_Con + 1.
                DISPLAY W_Con WITH FRAME FCalendario.
              END.
            END. 
          ELSE
            DO:
              IF W_Ano1 MODULO 4 = 0 THEN
                DO:
                  DO J = 1 TO 29:
                    CREATE Calendario.
                    ASSIGN Calendario.Agencia = P_Agencia
                           Calendario.Estado  = 2
                           Calendario.Ano     = W_Ano1
                           Calendario.Mes     = I
                           Calendario.Dia     = J.
                    W_Con = W_Con + 1.
                    DISPLAY W_Con WITH FRAME FCalendario.
                  END. 
                END.
              ELSE
               DO:
                DO J = 1 TO 28:
                 CREATE Calendario.
                 ASSIGN Calendario.Agencia = P_Agencia
                        Calendario.Estado  = 2
                        Calendario.Ano     = W_Ano1
                        Calendario.Mes     = I
                        Calendario.Dia     = J.
                 W_Con = W_Con + 1.
                 DISPLAY W_Con WITH FRAME FCalendario.
                END.           
               END.
            END.
      END.
    RELEASE Calendario.
    FIND Calendario WHERE Calendario.Agencia = P_Agencia   AND 
                          Calendario.Ano     = W_Ano1      AND
                          Calendario.Mes     = MONTH(TODAY)  AND
                          Calendario.Dia     = DAY(TODAY) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE(Calendario) THEN
      ASSIGN Calendario.Estado = 1
             Calendario.Habil  = yes.    
    FIND CURRENT Calendario NO-LOCK NO-ERROR. 
    DISABLE Btn_Gen WITH FRAME FCalendario.
    APPLY "ENTRY" TO W_Ano1.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerarAuto wWin 
PROCEDURE GenerarAuto :
DEFINE BUFFER Tmp-Calend  FOR Calendario.
      DEFINE BUFFER Tmp-Procdia FOR Procdia.
      DEFINE VAR W_Btn     AS LOGICAL INITIAL FALSE.
      
      DISABLE ALL WITH FRAME F-Main.
      W_Con = 0.
      DO I = 1 TO 12 BY 1:
        NumMes = I.
        IF NumMes = 1 OR NumMes = 3 OR NumMes = 5 OR NumMes = 7 OR NumMes = 8 OR 
          NumMes = 10 OR NumMes = 12 THEN
          DO:
            DO J = 1 TO 31:
              FIND Tmp-Calend WHERE Tmp-Calend.Ano     = W_Ano1
                              AND   Tmp-Calend.Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                              AND   Tmp-Calend.Mes     = I
                              AND   Tmp-Calend.Dia     = J
                              NO-LOCK NO-ERROR.
              IF AVAILABLE(Tmp-Calend) THEN
               DO:
                CREATE Calendario.
                ASSIGN Calendario.Agencia       = P_Agencia
                       Calendario.Estado        = Tmp-Calend.Estado
                       Calendario.Ano           = W_Ano1
                       Calendario.Mes           = I
                       Calendario.Dia           = J
                       Calendario.Cieanual      = Tmp-Calend.Cieanual
                       Calendario.Cierre        = Tmp-Calend.Cierre
                       Calendario.Control_Horas = Tmp-Calend.Control_Horas
                       Calendario.Habil         = Tmp-Calend.Habil
                       Calendario.Hora_Inicial  = Tmp-Calend.Hora_Inicial
                       Calendario.Hora_Final    = Tmp-Calend.Hora_Final
                       Calendario.Tipo          = Tmp-Calend.Tipo.
                W_Con = W_Con + 1.
                DISPLAY W_Con WITH FRAME FCalendario.
               END.
            END.
          END. 
        ELSE
          IF NumMes = 4 OR NumMes = 6 OR NumMes = 9 OR NumMes = 11 THEN
            DO:
              DO J = 1 TO 30:
                FIND Tmp-Calend WHERE Tmp-Calend.Ano     = W_Ano1
                                AND   Tmp-Calend.Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                                AND   Tmp-Calend.Mes     = I
                                AND   Tmp-Calend.Dia     = J
                                NO-LOCK NO-ERROR.
                IF AVAILABLE(Tmp-Calend) THEN
                 DO:
                  CREATE Calendario.
                  ASSIGN Calendario.Agencia       = P_Agencia
                         Calendario.Estado        = Tmp-Calend.Estado
                         Calendario.Ano           = W_Ano1
                         Calendario.Mes           = I
                         Calendario.Dia           = J
                         Calendario.Cieanual      = Tmp-Calend.Cieanual
                         Calendario.Cierre        = Tmp-Calend.Cierre
                         Calendario.Control_Horas = Tmp-Calend.Control_Horas
                         Calendario.Habil         = Tmp-Calend.Habil
                         Calendario.Hora_Inicial  = Tmp-Calend.Hora_Inicial
                         Calendario.Hora_Final    = Tmp-Calend.Hora_Final
                         Calendario.Tipo          = Tmp-Calend.Tipo.
                  W_Con = W_Con + 1.
                  DISPLAY W_Con WITH FRAME FCalendario.
                 END.
              END.
            END. 
          ELSE
            DO:
              IF W_Ano1 MODULO 4 = 0 THEN
                DO:
                  DO J = 1 TO 29:
                    FIND Tmp-Calend WHERE Tmp-Calend.Ano     = W_Ano1
                                    AND   Tmp-Calend.Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                                    AND   Tmp-Calend.Mes     = I
                                    AND   Tmp-Calend.Dia     = J
                                    NO-LOCK NO-ERROR.
                    IF AVAILABLE(Tmp-Calend) THEN
                     DO:
                      CREATE Calendario.
                      ASSIGN Calendario.Agencia       = P_Agencia
                             Calendario.Estado        = Tmp-Calend.Estado
                             Calendario.Ano           = W_Ano1
                             Calendario.Mes           = I
                             Calendario.Dia           = J
                             Calendario.Cieanual      = Tmp-Calend.Cieanual
                             Calendario.Cierre        = Tmp-Calend.Cierre
                             Calendario.Control_Horas = Tmp-Calend.Control_Horas
                             Calendario.Habil         = Tmp-Calend.Habil
                             Calendario.Hora_Inicial  = Tmp-Calend.Hora_Inicial
                             Calendario.Hora_Final    = Tmp-Calend.Hora_Final
                             Calendario.Tipo          = Tmp-Calend.Tipo.
                      W_Con = W_Con + 1.
                      DISPLAY W_Con WITH FRAME FCalendario.
                     END.
                  END. 
                END.
              ELSE
               DO:
                DO J = 1 TO 28:
                 FIND Tmp-Calend WHERE Tmp-Calend.Ano     = W_Ano1
                                 AND   Tmp-Calend.Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                                 AND   Tmp-Calend.Mes     = I
                                 AND   Tmp-Calend.Dia     = J
                                 NO-LOCK NO-ERROR.
                 IF AVAILABLE(Tmp-Calend) THEN
                  DO:
                   CREATE Calendario.
                   ASSIGN Calendario.Agencia       = P_Agencia
                          Calendario.Estado        = Tmp-Calend.Estado
                          Calendario.Ano           = W_Ano1
                          Calendario.Mes           = I
                          Calendario.Dia           = J
                          Calendario.Cieanual      = Tmp-Calend.Cieanual
                          Calendario.Cierre        = Tmp-Calend.Cierre
                          Calendario.Control_Horas = Tmp-Calend.Control_Horas
                          Calendario.Habil         = Tmp-Calend.Habil
                          Calendario.Hora_Inicial  = Tmp-Calend.Hora_Inicial
                          Calendario.Hora_Final    = Tmp-Calend.Hora_Final
                          Calendario.Tipo          = Tmp-Calend.Tipo.
                   W_Con = W_Con + 1.
                   DISPLAY W_Con WITH FRAME FCalendario.
                  END. 
                END.           
               END.
            END.
      END.
    RELEASE Calendario.
    DISABLE Btn_Gen WITH FRAME FCalendario.
    
    MESSAGE "Desea Copiar Los Procesos Configurados en "   SKIP 
            "la Agencia Origen Hacia la Agencia Destino ?" UPDATE W_Btn
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK-CANCEL.
    IF W_Btn THEN
      DO:
       W_Con = 0.
       DISPLAY W_Con WITH FRAME FCalendario.
       FOR EACH Tmp-ProcDia WHERE Tmp-ProcDia.Agencia          = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                            AND   YEAR(Tmp-ProcDia.Fecha_Proc) = W_Ano1
                            NO-LOCK :
           CREATE ProcDia.
           ASSIGN ProcDia.Cod_Proceso = Tmp-ProcDia.Cod_Proceso
                  ProcDia.Fecha_Proc  = Tmp-ProcDia.Fecha_Proc
                  ProcDia.Agencia     = P_Agencia.
           W_Con = W_Con + 1.
           DISPLAY W_Con WITH FRAME FCalendario.
       END.
      END.
    APPLY "ENTRY" TO W_Ano1.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerarCopia wWin 
PROCEDURE GenerarCopia :
DEFINE BUFFER Tmp-Calend  FOR Calendario.
      DEFINE BUFFER Tmp-Procdia FOR Procdia.
      DEFINE VAR W_Btn     AS LOGICAL INITIAL FALSE.
      
      DISABLE ALL WITH FRAME F-Main.
      W_Con = 0.
      DO I = 1 TO 12 BY 1:
        NumMes = I.
        IF NumMes = 1 OR NumMes = 3 OR NumMes = 5 OR NumMes = 7 OR NumMes = 8 OR 
          NumMes = 10 OR NumMes = 12 THEN
          DO:
            DO J = 1 TO 31:
              FIND Tmp-Calend WHERE Tmp-Calend.Ano     = W_Ano1
                              AND   Tmp-Calend.Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                              AND   Tmp-Calend.Mes     = I
                              AND   Tmp-Calend.Dia     = J
                              NO-LOCK NO-ERROR.
              IF AVAILABLE(Tmp-Calend) THEN
               DO:
                CREATE Calendario.
                ASSIGN Calendario.Agencia       = P_Agencia
                       Calendario.Estado        = Tmp-Calend.Estado
                       Calendario.Ano           = W_Ano1
                       Calendario.Mes           = I
                       Calendario.Dia           = J
                       Calendario.Cieanual      = Tmp-Calend.Cieanual
                       Calendario.Cierre        = Tmp-Calend.Cierre
                       Calendario.Control_Horas = Tmp-Calend.Control_Horas
                       Calendario.Habil         = Tmp-Calend.Habil
                       Calendario.Hora_Inicial  = Tmp-Calend.Hora_Inicial
                       Calendario.Hora_Final    = Tmp-Calend.Hora_Final
                       Calendario.Tipo          = Tmp-Calend.Tipo.
                W_Con = W_Con + 1.
                DISPLAY W_Con WITH FRAME F-Generar.
               END.
            END.
          END. 
        ELSE
          IF NumMes = 4 OR NumMes = 6 OR NumMes = 9 OR NumMes = 11 THEN
            DO:
              DO J = 1 TO 30:
                FIND Tmp-Calend WHERE Tmp-Calend.Ano     = W_Ano1
                                AND   Tmp-Calend.Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                                AND   Tmp-Calend.Mes     = I
                                AND   Tmp-Calend.Dia     = J
                                NO-LOCK NO-ERROR.
                IF AVAILABLE(Tmp-Calend) THEN
                 DO:
                  CREATE Calendario.
                  ASSIGN Calendario.Agencia       = P_Agencia
                         Calendario.Estado        = Tmp-Calend.Estado
                         Calendario.Ano           = W_Ano1
                         Calendario.Mes           = I
                         Calendario.Dia           = J
                         Calendario.Cieanual      = Tmp-Calend.Cieanual
                         Calendario.Cierre        = Tmp-Calend.Cierre
                         Calendario.Control_Horas = Tmp-Calend.Control_Horas
                         Calendario.Habil         = Tmp-Calend.Habil
                         Calendario.Hora_Inicial  = Tmp-Calend.Hora_Inicial
                         Calendario.Hora_Final    = Tmp-Calend.Hora_Final
                         Calendario.Tipo          = Tmp-Calend.Tipo.
                  W_Con = W_Con + 1.
                  DISPLAY W_Con WITH FRAME F-Generar.
                 END.
              END.
            END. 
          ELSE
            DO:
              IF W_Ano1 MODULO 4 = 0 THEN
                DO:
                  DO J = 1 TO 29:
                    FIND Tmp-Calend WHERE Tmp-Calend.Ano     = W_Ano1
                                    AND   Tmp-Calend.Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                                    AND   Tmp-Calend.Mes     = I
                                    AND   Tmp-Calend.Dia     = J
                                    NO-LOCK NO-ERROR.
                    IF AVAILABLE(Tmp-Calend) THEN
                     DO:
                      CREATE Calendario.
                      ASSIGN Calendario.Agencia       = P_Agencia
                             Calendario.Estado        = Tmp-Calend.Estado
                             Calendario.Ano           = W_Ano1
                             Calendario.Mes           = I
                             Calendario.Dia           = J
                             Calendario.Cieanual      = Tmp-Calend.Cieanual
                             Calendario.Cierre        = Tmp-Calend.Cierre
                             Calendario.Control_Horas = Tmp-Calend.Control_Horas
                             Calendario.Habil         = Tmp-Calend.Habil
                             Calendario.Hora_Inicial  = Tmp-Calend.Hora_Inicial
                             Calendario.Hora_Final    = Tmp-Calend.Hora_Final
                             Calendario.Tipo          = Tmp-Calend.Tipo.
                      W_Con = W_Con + 1.
                      DISPLAY W_Con WITH FRAME F-Generar.
                     END.
                  END. 
                END.
              ELSE
               DO:
                DO J = 1 TO 28:
                 FIND Tmp-Calend WHERE Tmp-Calend.Ano     = W_Ano1
                                 AND   Tmp-Calend.Agencia = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                                 AND   Tmp-Calend.Mes     = I
                                 AND   Tmp-Calend.Dia     = J
                                 NO-LOCK NO-ERROR.
                 IF AVAILABLE(Tmp-Calend) THEN
                  DO:
                   CREATE Calendario.
                   ASSIGN Calendario.Agencia       = P_Agencia
                          Calendario.Estado        = Tmp-Calend.Estado
                          Calendario.Ano           = W_Ano1
                          Calendario.Mes           = I
                          Calendario.Dia           = J
                          Calendario.Cieanual      = Tmp-Calend.Cieanual
                          Calendario.Cierre        = Tmp-Calend.Cierre
                          Calendario.Control_Horas = Tmp-Calend.Control_Horas
                          Calendario.Habil         = Tmp-Calend.Habil
                          Calendario.Hora_Inicial  = Tmp-Calend.Hora_Inicial
                          Calendario.Hora_Final    = Tmp-Calend.Hora_Final
                          Calendario.Tipo          = Tmp-Calend.Tipo.
                   W_Con = W_Con + 1.
                   DISPLAY W_Con WITH FRAME F-Generar.
                  END. 
                END.           
               END.
            END.
      END.
    RELEASE Calendario.
    DISABLE Btn_Gen WITH FRAME F-Generar.
    
    MESSAGE "Desea Copiar Los Procesos Configurados en "   SKIP 
            "la Agencia Origen Hacia la Agencia Destino ?" UPDATE W_Btn
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK-CANCEL.
    IF W_Btn THEN
      DO:
       W_Con = 0.
       DISPLAY W_Con WITH FRAME FCalendario.
       FOR EACH Tmp-ProcDia WHERE Tmp-ProcDia.Agencia          = INTEGER(SUBSTR(Com-Agencia:SCREEN-VALUE IN FRAME F-Main,1,3))
                            AND   YEAR(Tmp-ProcDia.Fecha_Proc) = W_Ano1
                            NO-LOCK :
           CREATE ProcDia.
           ASSIGN ProcDia.Cod_Proceso = Tmp-ProcDia.Cod_Proceso
                  ProcDia.Fecha_Proc  = Tmp-ProcDia.Fecha_Proc
                  ProcDia.Agencia     = P_Agencia.
           W_Con = W_Con + 1.
           DISPLAY W_Con WITH FRAME F-Generar.
       END.
      END.
    APPLY "ENTRY" TO W_Ano1.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
ASSIGN Com-Agencia:LIST-ITEMS IN FRAME F-Main = ""
       Cmb_Procesos:LIST-ITEMS IN FRAME FConfiguracion = "".

FIND FIRST Calendario WHERE Calendario.Ano EQ YEAR(TODAY) NO-LOCK NO-ERROR.
IF NOT AVAILABLE(Calendario) THEN
DO:
    MESSAGE "No existe Calendario para esta oficina en este año!" SKIP
            "Debe crearce un nuevo calendario!" VIEW-AS ALERT-BOX ERROR TITLE "No existe Calendario".
    DISABLE ALL EXCEPT BCreaCal WITH FRAME F-Main.
END.

FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK BREAK BY Agencias.Agencia:
  FIND FIRST Calendario WHERE
             Calendario.Agencia EQ Agencias.Agencia AND
             Calendario.Ano     EQ YEAR(TODAY) NO-LOCK NO-ERROR.
  IF AVAILABLE Calendario THEN
    Com-Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " " + STRING(Agencias.Nombre,"X(20)")) IN FRAME F-Main.
END.
FOR EACH Varios WHERE Varios.Tipo EQ 8 AND
                      Varios.Estado EQ 1 NO-LOCK:
   Cmb_Procesos:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME FConfiguracion.
END.
FIND Agencias WHERE Agencias.Agencia EQ W_Agencia AND
                    Agencias.Estado NE 3 NO-LOCK NO-ERROR.
IF AVAILABLE(Agencias) THEN
  Com-Agencia:SCREEN-VALUE IN FRAME F-Main = STRING(Agencias.Agencia,"999") + " " +  STRING(Agencias.Nombre,"X(20)") NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "La Agencia Actual no tiene configurado el Calendario" SKIP
             "El programa se posicionará en la primera agencia" SKIP
             "Que tenga el calendario configurado" VIEW-AS ALERT-BOX.
     Com-Agencia:SCREEN-VALUE IN FRAME F-Main = Com-Agencia:ENTRY(1) NO-ERROR.
  END.
                            
  RUN SUPER.
  ASSIGN
  lmonth = MONTH(W_Fecha)
  lyear = YEAR(W_Fecha)
  lday = DAY(W_Fecha).

 
/*       ASSIGN                                         */
/*       styear:sensitive in frame {&FRAME-NAME} = TRUE */
/*       ldday:sensitive in frame {&FRAME-NAME} = TRUE  */
/*       stdate:sensitive in frame {&FRAME-NAME} = TRUE. */
   DO WITH FRAME {&FRAME-NAME}:
       barray[1] = b1:HANDLE.
       barray[2] = b2:HANDLE.
       barray[3] = b3:HANDLE.
       barray[4] = b4:HANDLE.
       barray[5] = b5:HANDLE.
       barray[6] = b6:HANDLE.
       barray[7] = b7:HANDLE.
       barray[8] = b8:HANDLE.
       barray[9] = b9:HANDLE.
      barray[10] = b10:HANDLE.
      barray[11] = b11:HANDLE.
      barray[12] = b12:HANDLE.
      barray[13] = b13:HANDLE.
      barray[14] = b14:HANDLE.
      barray[15] = b15:HANDLE.
      barray[16] = b16:HANDLE.
      barray[17] = b17:HANDLE.
      barray[18] = b18:HANDLE.
      barray[19] = b19:HANDLE.
      barray[20] = b20:HANDLE.
      barray[21] = b21:HANDLE.
      barray[22] = b22:HANDLE.
      barray[23] = b23:HANDLE.
      barray[24] = b24:HANDLE.
      barray[25] = b25:HANDLE.
      barray[26] = b26:HANDLE.
      barray[27] = b27:HANDLE.
      barray[28] = b28:HANDLE.
      barray[29] = b29:HANDLE.
      barray[30] = b30:HANDLE.
      barray[31] = b31:HANDLE.
      barray[32] = b32:HANDLE.
      barray[33] = b33:HANDLE.
      barray[34] = b34:HANDLE.
      barray[35] = b35:HANDLE.
      barray[36] = b36:HANDLE.
      barray[37] = b37:HANDLE.
      barray[38] = b38:HANDLE.
      barray[39] = b39:HANDLE.
      barray[40] = b40:HANDLE.
      barray[41] = b41:HANDLE.
      barray[42] = b42:HANDLE.
       
      RUN setday.
      RUN setimage.   
     
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDay wWin 
PROCEDURE SetDay :
lweekday = WEEKDAY (DATE(lmonth,1,lyear)).
            lm = lmonth + 1.
            lyr = lyear.
            
            IF lm > 12 THEN DO:
                lm = 1.
                lyr =  lyr + 1.
            END.

           ASSIGN
           lastdate = (date(lm,1,lyr)) - 1
           lastday = day(lastdate)
           lstday = lweekday.
           
           IF lday >= 28 THEN DO:
                   DO WHILE lastday < lday:
                      lday = lday - 1.
                   END.
           END.  /*lday >= 28*/  
          
           RUN dispdate.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetImage wWin 
PROCEDURE SetImage :
ASSIGN      
     tnum = 0.
   
          DO d = 1 to 42:
                     IF d >= lstday AND d <= (lastday + lstday - 1) THEN DO:     
                          ASSIGN
                          tnum = tnum + 1      
                          barray[d]:LABEL = STRING(tnum, "z9").
                           
                          
                          IF barray[d]:SENSITIVE = FALSE THEN barray[d]:SENSITIVE = TRUE. 
                    END. /*d >= lastday*/
                   
                   ELSE DO:
                         IF barray[d]:SENSITIVE = TRUE THEN barray[d]:SENSITIVE = FALSE.
                         barray[d]:LABEL = "  ".
                   END.
             END. /*do d = 1 to 42*/
             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidarCalendario wWin 
PROCEDURE ValidarCalendario :
DEFINE VAR W_Btn     AS LOGICAL INITIAL FALSE.
  
  ASSIGN FRAME FCalendario W_Ano1.
  ASSIGN Sw_Generar = FALSE.
  IF W_Ano1 >= (YEAR(TODAY) - 2) THEN
   DO:
    FIND FIRST Calendario WHERE Calendario.Agencia = P_Agencia AND
                                Calendario.Ano     = W_Ano1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Calendario) THEN
     DO:
       MESSAGE "El Calendario Que Se Desea Generar" SKIP
               "ya Existe Para la Agencia " P_Agencia       
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     END.
    ELSE
     DO:
      MESSAGE "Desea Generar el Calendario Para el Año" W_Ano1 SKIP 
              "y la Agencia " P_Agencia " ?"     UPDATE W_Btn
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK-CANCEL.
      IF W_Btn THEN
          ASSIGN Sw_Generar = TRUE.
       ELSE
          ASSIGN Sw_Generar = FALSE.          
     END.
   END.
  ELSE
   DO:
       MESSAGE "El Calendario Que Se Desea Generar No "   SKIP
               "Debe Ser Menor a " YEAR(TODAY) - 1 " Modifique"  SKIP 
               "el Año e Intente de Nuevo."
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       APPLY "ENTRY" TO W_Ano1 IN FRAME FCalendario.
       RETURN NO-APPLY.
     END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetDataValue wWin 
FUNCTION GetDataValue RETURNS DATE
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  Returns the current value of the SmartDataField object.
   Params:  none
    Notes:  This function must be defined by the developer of the object
            to return its value.
------------------------------------------------------------------------------*/
 newdate = DATE(lmonth,lday,lyear).
  RETURN newdate .   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetDataValue wWin 
FUNCTION SetDataValue RETURNS LOGICAL
  ( pcdate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  This function receives the value for the SmartDataField and assigns it.
   Params:  The parameter and its datatype must be defined by the developer.
    Notes:  
------------------------------------------------------------------------------*/
IF pcdate <> ? THEN DO: 
  ASSIGN
  lmonth = MONTH(pcdate)
  lyear = YEAR(pcdate)
  lday = DAY(pcdate).
END.
ELSE DO: 
  ASSIGN
  lmonth = MONTH(TODAY)
  lyear = YEAR(TODAY)
  lday = DAY(TODAY).
   

END.

      RUN setday.
      RUN setimage.   

     

  
  RETURN TRUE. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

