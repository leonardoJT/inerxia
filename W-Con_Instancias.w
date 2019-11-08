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



    /*  ABRE SOLICITUDES DIGITALIZADAS - WILLIAM MARTINEZ RUIZ 24-11-2008  */


/*************************************** Raiz Imagenes ******************************************
*
*/
DEFINE VARIABLE cRaiz AS CHARACTER INITIAL "\\192.168.1.111\progress\objetos\Imagenes\SOLICITUDES\" NO-UNDO.
/*
*
*************************************************************************************************/

DEFINE VARIABLE cFullPathName AS CHARACTER FORMAT "x(250)" NO-UNDO.
DEFINE VARIABLE hInstance AS INTEGER   NO-UNDO.

{windows.i}
{winfunc.i}

    PROCEDURE ShellExecute{&A} EXTERNAL "shell32" :
       DEFINE INPUT PARAMETER HWND AS LONG.
       DEFINE INPUT PARAMETER lpOperation AS CHARACTER.
       DEFINE INPUT PARAMETER lpFile AS CHARACTER.
       DEFINE INPUT PARAMETER lpParameters AS CHARACTER.
       DEFINE INPUT PARAMETER lpDirectory AS CHARACTER.
       DEFINE INPUT PARAMETER nShowCmd AS LONG.
       DEFINE RETURN PARAMETER hInstance AS LONG.
    END.

    DEFINE VARIABLE RutaFoto AS CHARACTER FORMAT "x(80)"   NO-UNDO.

/*************************************************************************************************/


/* Local Variable Definitions ---                                       */
DEFINE VAR i AS DECIMAL.
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR nomusu AS CHARACTER FORMAT "X(30)".
DEFINE VAR TotTra AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR TotCon AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR TotRet AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR wx_nombre AS CHARACTER FORMAT "X(30)".

DEFINE VAR TMG AS INTEGER FORMAT "9999".
DEFINE VARIABLE viCnt AS INTEGER     NO-UNDO.
DEFINE BUFFER BC FOR creditos.

DEFINE TEMP-TABLE TTot /*muestra instancias totalizadas*/
  FIELD TOrdIns LIKE Cfg_Instancias.Orden
  FIELD TCodIns LIKE Instancias.Instancia
  FIELD TNomIns AS CHARACTER FORMAT "X(50)"
  FIELD TNroIns AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TValIns AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9"
  FIELD TValSdo AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9"
  FIELD TDesIns AS DECIMAL FORMAT ">>>,>>>,>>>,>>9"
  FIELD TTMIIns LIKE Instancias.TMI
  FIELD ttipo LIKE instancias.tipo_instancia
  INDEX idx1 tcodins.
  
DEFINE TEMP-TABLE TAge
  FIELD TCodAge LIKE Agencias.Agencia
  FIELD TNomAge LIKE Agencias.Nombre
  FIELD TInsAge LIKE Instancias.Instancia
  FIELD TPorAge AS DECIMAL FORMAT ">>9,9"
  FIELD TNroAge AS INTEGER FORMAT "9999"
  FIELD TValAge AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TDesAge AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  INDEX idx2 tcodage.
  
DEFINE TEMP-TABLE TUsu 
  FIELD TAgeUsu LIKE Agencias.Agencia
  FIELD TASlUsu LIKE Agencias.Agencia
  FIELD TPorUsu AS DECIMAL FORMAT ">>9,9"
  FIELD TCodUsu LIKE Usuarios.Usuario
  FIELD TNomUsu LIKE Usuarios.Nombre
  FIELD TNroUsu AS DECIMAL FORMAT "9999"
  FIELD TValUsu AS DECIMAL FORMAT ">>>,>>>,>>>,>>9" /*Totmonto solicitudes*/
  FIELD TDesUsu AS DECIMAL FORMAT "->>>,>>>,>>>,>>9" /*TotValor a desembolsar*/
  INDEX idx3 tageusu tcodusu
  INDEX idx4 tcodusu.
  
DEFINE TEMP-TABLE TSol
  FIELD TAgeSol LIKE Agencias.Agencia
  FIELD TCodSol LIKE Creditos.Cod_Credito
  FIELD TNomSol LIKE Pro_Creditos.Nom_Producto
  FIELD TNitSol LIKE Creditos.Nit
  FIELD TNumSol LIKE Creditos.Num_Credito
  FIELD TSolSol LIKE Solicitud.Num_Solicitud
  FIELD TMonSol LIKE Creditos.Monto
  FIELD TValDes LIKE creditos.Monto /*valor real a desembolsar (monto - deudas_a_Recoger)*/
  FIELD TPlaSol LIKE Creditos.Plazo
  FIELD TPerSol AS   CHARACTER FORMAT "X(10)"
  FIELD TSdoSol LIKE Creditos.Sdo_Capital
  FIELD TFciSol LIKE Mov_Instancias.Fec_Ingreso
  FIELD TFcrSol LIKE Mov_Instancias.Fec_Retiro
  FIELD TVigSol AS INTEGER FORMAT "99999"
  FIELD TUsuSol LIKE Usuarios.Usuario
  FIELD TNomCli LIKE Pro_Creditos.Nom_Producto
  INDEX idx6 TAgeSol TUsuSol.

DEFINE TEMP-TABLE TInfoA
  FIELD TAgencia  LIKE Agencias.Agencia
  FIELD TNomAgen  LIKE Agencias.Nombre
  FIELD TCon_Aprobada AS INTEGER FORMAT "99999"
  FIELD TVal_Aprobada AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TPor_Aprobada AS DECIMAL FORMAT ">9,99"
  FIELD TCon_Cancelada AS INTEGER FORMAT "99999"          
  FIELD TVal_Cancelada AS DECIMAL FORMAT ">>,>>>,>>>,>>9" 
  FIELD TPor_Cancelada AS DECIMAL FORMAT ">9,99"
  FIELD TCon_Estudio AS INTEGER FORMAT "99999"          
  FIELD TVal_Estudio AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TPor_Estudio AS DECIMAL FORMAT ">9,99".

DEFINE TEMP-TABLE TUsuDet
  FIELD TAgeDet LIKE Agencias.Agencia
  FIELD TCodDet LIKE Usuarios.Usuario
  FIELD TNomDet LIKE Usuarios.Nombre
  FIELD TInsDet LIKE Instancias.Instancia EXTENT 7
  FIELD TNInDet LIKE Instancias.Nom_Instancia EXTENT 7
  FIELD TVInDet AS DECIMAL FORMAT ">,>>>,>>>,>>9" EXTENT 7
  FIELD TCInDet AS DECIMAL FORMAT "999" EXTENT 7.
  
DEFINE TEMP-TABLE TOrdIns
  FIELD TOrdIns AS INTEGER FORMAT "99"
  FIELD TCodIns LIKE Instancias.Instancia
  FIELD TNomIns LIKE Instancias.Nom_Instancia.

DEFINE TEMP-TABLE TIU  /*informe detallado por usuarios*/
  FIELD u_age LIKE Agencias.Agencia
  FIELD u_usu LIKE Usuarios.Usuario
  FIELD u_ins LIKE Instancias.Instancia
  FIELD u_con AS INTEGER FORMAT ">>,>>9"
  FIELD u_val LIKE Ahorros.Sdo_Disponible.

DEFINE TEMP-TABLE TMI LIKE Mov_Instancias 
    INDEX Xu Agencia Instancia Usuario
    INDEX Xs Agencia Instancia Usuario Estado Fec_Ingreso.

DEFINE TEMP-TABLE estadistico
  FIELD Tage LIKE solicitud.agencia
  FIELD Tnit LIKE solicitud.nit
  FIELD TNOM LIKE CLIENTES.NOMBRE
  FIELD Tsol LIKE solicitud.num_solicitud
  FIELD Tmon LIKE solicitud.monto
  FIELD ttas LIKE solicitud.tasa
  FIELD tpla LIKE solicitud.plazo
  FIELD Tnumc LIKE creditos.num_credito
  FIELD Tmonc LIKE creditos.monto
  FIELD ttasc LIKE creditos.tasa
  FIELD tplac LIKE creditos.plazo
  FIELD I20U LIKE mov_instancias.usuario
  FIELD I20FI LIKE Mov_instancias.fec_ingreso
  FIELD I20FF LIKE MOV_INSTANCIAS.FEC_RETIRO
  FIELD I20t  AS   integer
  FIELD I30U LIKE mov_instancias.usuario
  FIELD I30FI LIKE Mov_instancias.fec_ingreso
  FIELD I30FF LIKE MOV_INSTANCIAS.FEC_RETIRO
  FIELD I30t  AS   integer
  FIELD I35U LIKE mov_instancias.usuario
  FIELD I35FI LIKE Mov_instancias.fec_ingreso
  FIELD I35FF LIKE MOV_INSTANCIAS.FEC_RETIRO
  FIELD I35t  AS   integer 
  FIELD I40U LIKE mov_instancias.usuario
  FIELD I40FI LIKE Mov_instancias.fec_ingreso
  FIELD I40FF LIKE MOV_INSTANCIAS.FEC_RETIRO
  FIELD I40t  AS   integer
  FIELD I50U LIKE mov_instancias.usuario
  FIELD I50FI LIKE Mov_instancias.fec_ingreso
  FIELD I50FF LIKE MOV_INSTANCIAS.FEC_RETIRO
  FIELD I50t  AS   INTEGER
  FIELD I70U LIKE mov_instancias.usuario
  FIELD I70FI LIKE Mov_instancias.fec_ingreso
  FIELD I70FF LIKE MOV_INSTANCIAS.FEC_RETIRO
  FIELD I70t  AS   integer
  FIELD I75U LIKE mov_instancias.usuario
  FIELD I75FI LIKE Mov_instancias.fec_ingreso
  FIELD I75FF LIKE MOV_INSTANCIAS.FEC_RETIRO
  FIELD I75t  AS   integer
  FIELD I78U LIKE mov_instancias.usuario
  FIELD I78FI LIKE Mov_instancias.fec_ingreso
  FIELD I78FF LIKE MOV_INSTANCIAS.FEC_RETIRO
  FIELD I78t  AS   integer
  FIELD I80U LIKE mov_instancias.usuario
  FIELD I80FI LIKE Mov_instancias.fec_ingreso
  FIELD I80FF LIKE MOV_INSTANCIAS.FEC_RETIRO
  FIELD I80t  AS   integer
  FIELD I950U LIKE mov_instancias.usuario
  FIELD I950FI LIKE Mov_instancias.fec_ingreso
  FIELD I950FF LIKE MOV_INSTANCIAS.FEC_RETIRO
  FIELD I950t  AS   integer
    INDEX idx1 tnit tsol.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define BROWSE-NAME BIns2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cfg_Instancias Mov_Instancias TAge TTot TSol ~
TUsu

/* Definitions for BROWSE BIns2                                         */
&Scoped-define FIELDS-IN-QUERY-BIns2 Cfg_Instancias.Agencia Cfg_Instancias.Usuario NomUsu   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BIns2   
&Scoped-define SELF-NAME BIns2
&Scoped-define QUERY-STRING-BIns2 FOR EACH Cfg_Instancias
&Scoped-define OPEN-QUERY-BIns2 OPEN QUERY {&SELF-NAME} FOR EACH Cfg_Instancias.
&Scoped-define TABLES-IN-QUERY-BIns2 Cfg_Instancias
&Scoped-define FIRST-TABLE-IN-QUERY-BIns2 Cfg_Instancias


/* Definitions for BROWSE Br_ConM                                       */
&Scoped-define FIELDS-IN-QUERY-Br_ConM Mov_Instancias.Agencia Mov_Instancias.Instancia Mov_Instancias.Nit Mov_Instancias.Num_Solicitud Mov_Instancias.Estado Mov_Instancias.Usuario Mov_Instancias.Fec_Ingreso Mov_Instancias.Hora_Ingreso Mov_Instancias.Fec_Retiro Mov_Instancias.Hora_Retiro Mov_Instancias.Cuenta Mov_Instancias.Descripcion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_ConM Mov_Instancias.Agencia Mov_Instancias.Instancia ~
 Mov_Instancias.Estado Mov_Instancias.Usuario   
&Scoped-define ENABLED-TABLES-IN-QUERY-Br_ConM Mov_Instancias
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Br_ConM Mov_Instancias
&Scoped-define SELF-NAME Br_ConM
&Scoped-define OPEN-QUERY-Br_ConM /*OPEN QUERY {&SELF-NAME} FOR EACH Mov_Instancias NO-LOCK INDEXED-REPOSITION.*/.
&Scoped-define TABLES-IN-QUERY-Br_ConM Mov_Instancias
&Scoped-define FIRST-TABLE-IN-QUERY-Br_ConM Mov_Instancias


/* Definitions for BROWSE B_Agencias                                    */
&Scoped-define FIELDS-IN-QUERY-B_Agencias TAge.TCodAge TAge.TNomAge TAge.TPOrAge TAge.TNroAge TAge.TValAge TAge.TDesAge   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Agencias   
&Scoped-define SELF-NAME B_Agencias
&Scoped-define QUERY-STRING-B_Agencias FOR EACH TAge NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Agencias OPEN QUERY {&SELF-NAME} FOR EACH TAge NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Agencias TAge
&Scoped-define FIRST-TABLE-IN-QUERY-B_Agencias TAge


/* Definitions for BROWSE B_Instancias                                  */
&Scoped-define FIELDS-IN-QUERY-B_Instancias TTot.TOrdIns TTot.TCodIns TTot.TNomIns TTot.TNroIns TTot.TValSdo TTot.TValIns TTot.TDesIns   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Instancias   
&Scoped-define SELF-NAME B_Instancias
&Scoped-define QUERY-STRING-B_Instancias FOR EACH TTot NO-LOCK  BY TTot.TOrdIns INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Instancias OPEN QUERY {&SELF-NAME} FOR EACH TTot NO-LOCK  BY TTot.TOrdIns INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Instancias TTot
&Scoped-define FIRST-TABLE-IN-QUERY-B_Instancias TTot


/* Definitions for BROWSE B_Pasa                                        */
&Scoped-define FIELDS-IN-QUERY-B_Pasa Cfg_Instancias.Agencia Cfg_Instancias.Usuario NomUsu   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Pasa   
&Scoped-define SELF-NAME B_Pasa
&Scoped-define OPEN-QUERY-B_Pasa /*OPEN QUERY {&SELF-NAME} FOR EACH Cfg_Instancias NO-LOCK INDEXED-REPOSITION.*/.
&Scoped-define TABLES-IN-QUERY-B_Pasa Cfg_Instancias
&Scoped-define FIRST-TABLE-IN-QUERY-B_Pasa Cfg_Instancias


/* Definitions for BROWSE B_Solicitud                                   */
&Scoped-define FIELDS-IN-QUERY-B_Solicitud TSol.TCodSol TSol.TNomSol TSol.TNomCli TSol.TNitSol TSol.TNumSol TSol.TSolSol TSol.TMonSol TSol.TValDes TSol.TPlaSol TSol.TPerSol TSol.TFciSol TSol.TFcrSol TSol.TVigSol   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Solicitud   
&Scoped-define SELF-NAME B_Solicitud
&Scoped-define QUERY-STRING-B_Solicitud FOR EACH TSol NO-LOCK BY TSol.TVigSol DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Solicitud OPEN QUERY {&SELF-NAME} FOR EACH TSol NO-LOCK BY TSol.TVigSol DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Solicitud TSol
&Scoped-define FIRST-TABLE-IN-QUERY-B_Solicitud TSol


/* Definitions for BROWSE B_Usuarios                                    */
&Scoped-define FIELDS-IN-QUERY-B_Usuarios TUsu.TAgeUsu TUsu.TCodUsu TUsu.TNomUsu TUsu.TPorUsu TUsu.TNroUsu TUsu.TValUsu TUsu.TDesUsu   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Usuarios   
&Scoped-define SELF-NAME B_Usuarios
&Scoped-define QUERY-STRING-B_Usuarios FOR EACH TUsu NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Usuarios OPEN QUERY {&SELF-NAME} FOR EACH TUsu NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Usuarios TUsu
&Scoped-define FIRST-TABLE-IN-QUERY-B_Usuarios TUsu


/* Definitions for FRAME F_AInstancias                                  */

/* Definitions for FRAME F_Consulta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consulta ~
    ~{&OPEN-QUERY-B_Agencias}~
    ~{&OPEN-QUERY-B_Instancias}~
    ~{&OPEN-QUERY-B_Solicitud}~
    ~{&OPEN-QUERY-B_Usuarios}

/* Definitions for FRAME F_Corr                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Corr ~
    ~{&OPEN-QUERY-Br_ConM}

/* Definitions for FRAME F_Instancias2                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Instancias2 ~
    ~{&OPEN-QUERY-BIns2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_EncInstancias T_Refresh BUTTON-162 ~
B_Instancias BUTTON-167 BUTTON-171 B_Agencias B_Usuarios Btn_Eliminar ~
Btn_Corr BUTTON-176 B_Solicitud BUTTON-169 BUTTON-168 
&Scoped-Define DISPLAYED-OBJECTS Cmb_EncInstancias T_Refresh edw 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValTotCred wWin 
FUNCTION getValTotCred RETURNS INTEGER
  ( INPUT ipcNit        AS CHARACTER, 
    INPUT ipiNumCre     AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Organizar 
       MENU-ITEM m_NumSolicitud LABEL "Num.Solicitud" 
       MENU-ITEM m_NumCredito   LABEL "Num.Credito"   
       MENU-ITEM m_Vigencia_Mayor LABEL "Vigencia"      
       MENU-ITEM m_Monto        LABEL "Monto"         
       MENU-ITEM m_Plazo        LABEL "Plazo"         
       MENU-ITEM m_Fecha_de_Ingreso LABEL "Fecha de Ingreso"
       MENU-ITEM m_Fecha_de_Retiro LABEL "Fecha de Retiro"
       MENU-ITEM m_Vigencia     LABEL "Vigencia"      .

DEFINE MENU POPUP-MENU-B_Solicitud 
       SUB-MENU  m_Organizar    LABEL "Organizar"     
       MENU-ITEM m_Mover_hacia_otro_Usuario LABEL "Asignar a otro Usuario"
       MENU-ITEM m_Asignar_a_otra_Instancia LABEL "Asignar a otra Instancia".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-165 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 165" 
     SIZE 7 BY 1.88.

DEFINE BUTTON BUTTON-173 
     LABEL "Asignar al usuario seleccionado" 
     SIZE 34 BY 1.12.

DEFINE BUTTON Btn_Corr 
     LABEL "Modificar" 
     SIZE 9.86 BY 1.69.

DEFINE BUTTON Btn_Eliminar 
     LABEL "Borrar Solicit." 
     SIZE 10.29 BY 1.65
     FONT 4.

DEFINE BUTTON BUTTON-162 
     LABEL "Filtros" 
     SIZE 15 BY .81.

DEFINE BUTTON BUTTON-167 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 167" 
     SIZE 9.57 BY 1.88.

DEFINE BUTTON BUTTON-168 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 168" 
     SIZE 4 BY 1.08.

DEFINE BUTTON BUTTON-169 
     LABEL "Salir" 
     SIZE 10 BY 1.62
     FONT 5.

DEFINE BUTTON BUTTON-171 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 171" 
     SIZE 9.57 BY 1.88.

DEFINE BUTTON BUTTON-176 
     LABEL "Ver Prioridades" 
     SIZE 15 BY .81.

DEFINE VARIABLE Cmb_EncInstancias AS CHARACTER FORMAT "X(40)":U 
     LABEL "Procesos Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE edw AS CHARACTER FORMAT "X(256)":U 
     LABEL "Registros" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 17 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE T_Refresh AS LOGICAL INITIAL no 
     LABEL "Refrescar Automaticamente" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .77 NO-UNDO.

DEFINE BUTTON BUTTON-180 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 180" 
     SIZE 12.14 BY 1.54.

DEFINE VARIABLE W_CedNitCorr AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 18 FGCOLOR 0  NO-UNDO.

DEFINE BUTTON BUTTON-163 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 163" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE Fec_Fin AS DATE FORMAT "99/99/99":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fec_Ini AS DATE FORMAT "99/99/99":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Estado AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Vigentes", no,
"Revisadas", yes
     SIZE 24 BY 1.08 NO-UNDO.

DEFINE VARIABLE R_Visualiza AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Instancia", 1,
"Proceso", 2
     SIZE 20 BY .81 NO-UNDO.

DEFINE BUTTON BUTTON-230 
     LABEL "Cerrar" 
     SIZE 11.29 BY .81.

DEFINE IMAGE P_Foto
     FILENAME "adeicon/blank":U
     SIZE 18 BY 5.12.

DEFINE BUTTON BUTTON-174 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 174" 
     SIZE 10 BY 1.35.

DEFINE BUTTON BUTTON-175 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 175" 
     SIZE 10 BY 1.38.

DEFINE VARIABLE R_Imp AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Informe General de la Entidad", 1,
"Informe Detallado de Asesores", 2,
"informe Base para Estadistico", 3,
"Informe visualizado X solicitud", 4,
"Informe Neto Fabrica del Periodo", 5
     SIZE 31 BY 2.96
     FGCOLOR 7  NO-UNDO.

DEFINE BUTTON Btn_OutIn 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 181" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-182 
     LABEL "Asignar Instancia" 
     SIZE 34 BY 1.35
     FONT 5.

DEFINE VARIABLE Cmb_Agencias2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 44 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_instancias2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Instancias" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 44 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Proceso AS CHARACTER FORMAT "X(256)":U 
     LABEL "Proceso" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 44 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-177 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 177" 
     SIZE 6 BY 1.35.

DEFINE VARIABLE PR_Alta AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 28 BY .62
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE PR_Media AS CHARACTER FORMAT "X40)":U 
      VIEW-AS TEXT 
     SIZE 29.29 BY .62
     BGCOLOR 14 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE PR_Nomral AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     BGCOLOR 10  NO-UNDO.

DEFINE RECTANGLE RECT-290
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 28 BY 1.04
     BGCOLOR 10 .

DEFINE RECTANGLE RECT-291
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 30 BY 1.04
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-292
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 31 BY 1.04
     BGCOLOR 14 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BIns2 FOR 
      Cfg_Instancias SCROLLING.

DEFINE QUERY Br_ConM FOR 
      Mov_Instancias SCROLLING.

DEFINE QUERY B_Agencias FOR 
      TAge SCROLLING.

DEFINE QUERY B_Instancias FOR 
      TTot SCROLLING.

DEFINE QUERY B_Pasa FOR 
      Cfg_Instancias SCROLLING.

DEFINE QUERY B_Solicitud FOR 
      TSol SCROLLING.

DEFINE QUERY B_Usuarios FOR 
      TUsu SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BIns2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BIns2 wWin _FREEFORM
  QUERY BIns2 DISPLAY
      Cfg_Instancias.Agencia
  Cfg_Instancias.Usuario
      NomUsu COLUMN-LABEL "Nombre"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 53 BY 8.08
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_ConM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_ConM wWin _FREEFORM
  QUERY Br_ConM NO-LOCK DISPLAY
      Mov_Instancias.Agencia FORMAT "999":U                  COLUMN-LABEL "Ag."   COLUMN-BGCOLOR 1
      Mov_Instancias.Instancia FORMAT "99999":U              COLUMN-LABEL "Instan." COLUMN-BGCOLOR 1
      Mov_Instancias.Nit FORMAT "X(12)":U                    COLUMN-LABEL "Ced./Nit"
      Mov_Instancias.Num_Solicitud FORMAT "99999999":U       COLUMN-LABEL "No.Solicitud"
      Mov_Instancias.Estado FORMAT "yes/no":U                COLUMN-BGCOLOR 1
      Mov_Instancias.Usuario FORMAT "X(12)":U                COLUMN-LABEL "Usuario" COLUMN-BGCOLOR 1
      Mov_Instancias.Fec_Ingreso FORMAT "99/99/9999":U       COLUMN-LABEL "F-Ingres."
      Mov_Instancias.Hora_Ingreso FORMAT "99999":U           COLUMN-LABEL "Hora-Ing"
      Mov_Instancias.Fec_Retiro FORMAT "99/99/9999":U        COLUMN-LABEL "F-Retiro"
      Mov_Instancias.Hora_Retiro FORMAT "99999":U            COLUMN-LABEL "Hora-Ret"
      Mov_Instancias.Cuenta FORMAT "X(14)":U
      Mov_Instancias.Descripcion FORMAT "X(50)":U
ENABLE Mov_Instancias.Agencia Mov_Instancias.Instancia 
       Mov_Instancias.Estado  Mov_Instancias.Usuario
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108.43 BY 12.12
         FGCOLOR 0 FONT 4 ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.

DEFINE BROWSE B_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Agencias wWin _FREEFORM
  QUERY B_Agencias NO-LOCK DISPLAY
      TAge.TCodAge COLUMN-LABEL "Agencia"
      TAge.TNomAge COLUMN-LABEL "Nombre" FORMAT "X(28)"
      TAge.TPOrAge COLUMN-LABEL "Porc"
      TAge.TNroAge COLUMN-LABEL "#Reg"
      TAge.TValAge COLUMN-LABEL "Monto Total"
      TAge.TDesAge WIDTH 30 COLUMN-LABEL "Desembolso Total"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 49 BY 5.92
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE B_Instancias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Instancias wWin _FREEFORM
  QUERY B_Instancias NO-LOCK DISPLAY
      TTot.TOrdIns COLUMN-LABEL "Ord"
   TTot.TCodIns COLUMN-LABEL "Codigo"
   TTot.TNomIns COLUMN-LABEL "Nombre de la Instancia"
   TTot.TNroIns COLUMN-LABEL "#Reg"
   TTot.TValSdo COLUMN-LABEL "Total-Saldos" FORMAT ">>>,>>>,>>>,>>9"
   TTot.TValIns COLUMN-LABEL "Total-Montos" FORMAT ">>>,>>>,>>>,>>9"
   TTot.TDesIns COLUMN-LABEL "Total-Desembolso" FORMAT ">>>,>>>,>>>,>>9"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 100 BY 5.92
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE B_Pasa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Pasa wWin _FREEFORM
  QUERY B_Pasa NO-LOCK DISPLAY
      Cfg_Instancias.Agencia FORMAT "999":U
      Cfg_Instancias.Usuario FORMAT "X(4)":U
      NomUsu COLUMN-LABEL "Nombre"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 53 BY 9.96
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.

DEFINE BROWSE B_Solicitud
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Solicitud wWin _FREEFORM
  QUERY B_Solicitud NO-LOCK DISPLAY
      TSol.TCodSol COLUMN-LABEL "Cod.Pdt"
  TSol.TNomSol WIDTH 10 COLUMN-LABEL "Nombre Producto"
  TSol.TNomCli WIDTH 20 COLUMN-LABEL "Nombre Cliente"
  TSol.TNitSol COLUMN-LABEL "NitCliente"
  TSol.TNumSol COLUMN-LABEL "Num.Cred"
  TSol.TSolSol COLUMN-LABEL "Num.Soli"
  TSol.TMonSol FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Monto"
  TSol.TValDes FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Val.a Desemb"
  TSol.TPlaSol COLUMN-LABEL "Plazo"
  TSol.TPerSol COLUMN-LABEL "Per.Pago"
  TSol.TFciSol COLUMN-LABEL "Fec.Inicio"
  TSol.TFcrSol COLUMN-LABEL "Fec.Retiro"
  TSol.TVigSol COLUMN-LABEL "Vigencia" FORM "-999999"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 100 BY 6.46
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.

DEFINE BROWSE B_Usuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Usuarios wWin _FREEFORM
  QUERY B_Usuarios NO-LOCK DISPLAY
      TUsu.TAgeUsu COLUMN-LABEL "Age"
  TUsu.TCodUsu COLUMN-LABEL "Codigo"
  TUsu.TNomUsu WIDTH 18 COLUMN-LABEL "Nombre"
  TUsu.TPorUsu COLUMN-LABEL "Porc"
  TUsu.TNroUsu COLUMN-LABEL "#Reg"
  TUsu.TValUsu COLUMN-LABEL "Monto Total"
  TUsu.TDesUsu COLUMN-LABEL "Desemb. Total"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 49 BY 5.92
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Corr
     W_CedNitCorr AT ROW 13.65 COL 48.29 COLON-ALIGNED NO-LABEL
     Br_ConM AT ROW 1.15 COL 1.72
     BUTTON-180 AT ROW 13.42 COL 95.14
     "Cédula/Nit a Consultar" VIEW-AS TEXT
          SIZE 22.14 BY .62 AT ROW 14 COL 28.14
          BGCOLOR 7 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.43 ROW 2.19
         SIZE 110.14 BY 14.85
         BGCOLOR 17 FGCOLOR 15 
         TITLE BGCOLOR 17 FGCOLOR 15 "Modificación de Mov_Instancias".

DEFINE FRAME F_Consulta
     Cmb_EncInstancias AT ROW 1.27 COL 18 COLON-ALIGNED
     T_Refresh AT ROW 1.27 COL 60
     BUTTON-162 AT ROW 1.27 COL 86
     B_Instancias AT ROW 2.35 COL 3
     BUTTON-167 AT ROW 2.35 COL 104
     BUTTON-171 AT ROW 4.23 COL 104
     B_Agencias AT ROW 9.35 COL 3
     B_Usuarios AT ROW 9.35 COL 54
     Btn_Eliminar AT ROW 9.88 COL 104
     Btn_Corr AT ROW 14.04 COL 104.14
     edw AT ROW 15.54 COL 80 COLON-ALIGNED
     BUTTON-176 AT ROW 15.54 COL 88
     B_Solicitud AT ROW 16.35 COL 3
     BUTTON-169 AT ROW 19.58 COL 104
     BUTTON-168 AT ROW 22 COL 104
     "Agencias con usuarios que gestionan la instancia" VIEW-AS TEXT
          SIZE 49 BY .81 AT ROW 8.54 COL 3
          FGCOLOR 7 FONT 5
     "Usuarios que se encuentran gestionando la instancia" VIEW-AS TEXT
          SIZE 46 BY .81 AT ROW 8.54 COL 54
          FGCOLOR 7 FONT 5
     "Solicitudes o Créditos que estan siendo gestionados por el usuario" VIEW-AS TEXT
          SIZE 58 BY .81 AT ROW 15.54 COL 3
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.29 BY 22.08
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Foto
     BUTTON-230 AT ROW 6.12 COL 5 WIDGET-ID 4
     P_Foto AT ROW 1 COL 1 WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 96 ROW 1.27
         SIZE 19 BY 6.73
         TITLE "Foto del Asociado"
         CANCEL-BUTTON BUTTON-230 WIDGET-ID 100.

DEFINE FRAME F_Imprimir
     R_Imp AT ROW 1.27 COL 6 NO-LABEL
     BUTTON-174 AT ROW 4.5 COL 15
     BUTTON-175 AT ROW 4.5 COL 26
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 64 ROW 1.81
         SIZE 39 BY 5.92
         BGCOLOR 17 FONT 5
         TITLE "Parámetros de Impresión".

DEFINE FRAME F_Filtros
     R_Estado AT ROW 1.27 COL 1.72 NO-LABEL
     Fec_Ini AT ROW 2.62 COL 12 COLON-ALIGNED
     Fec_Fin AT ROW 3.69 COL 12 COLON-ALIGNED
     R_Visualiza AT ROW 4.77 COL 3 NO-LABEL
     BUTTON-163 AT ROW 6.12 COL 16
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 78 ROW 2.08
         SIZE 25 BY 7.81
         BGCOLOR 17 FONT 4
         TITLE "Filtros".

DEFINE FRAME F_Prioridades
     BUTTON-177 AT ROW 1.27 COL 94
     PR_Nomral AT ROW 1.58 COL 1 COLON-ALIGNED NO-LABEL
     PR_Alta AT ROW 1.58 COL 60 COLON-ALIGNED NO-LABEL
     PR_Media AT ROW 1.62 COL 28.72 COLON-ALIGNED NO-LABEL
     RECT-290 AT ROW 1.38 COL 2
     RECT-291 AT ROW 1.35 COL 61
     RECT-292 AT ROW 1.38 COL 30
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 16.35
         SIZE 100 BY 2.69
         BGCOLOR 17 FONT 4
         TITLE "Prioridades".

DEFINE FRAME F_AInstancias
     B_Pasa AT ROW 1.27 COL 4
     BUTTON-165 AT ROW 11.5 COL 51
     BUTTON-173 AT ROW 12.04 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 22 ROW 8.27
         SIZE 59 BY 13.46
         BGCOLOR 17 FONT 5
         TITLE "Usuarios disponibles que pueden recibir el registro".

DEFINE FRAME F_Instancias2
     Cmb_Agencias2 AT ROW 1.27 COL 11 COLON-ALIGNED
     Cmb_Proceso AT ROW 2.35 COL 11 COLON-ALIGNED
     Cmb_instancias2 AT ROW 3.42 COL 11 COLON-ALIGNED
     BIns2 AT ROW 4.5 COL 4
     BUTTON-182 AT ROW 13.12 COL 4
     Btn_OutIn AT ROW 13.12 COL 50
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 18 ROW 6.65
         SIZE 59 BY 14.81
         BGCOLOR 17 FONT 4
         TITLE "Cambio a otra Instancia".


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
         TITLE              = "Consulta de Movimientos de Instancias"
         HEIGHT             = 22.04
         WIDTH              = 113.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         FONT               = 4
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
ASSIGN FRAME F_AInstancias:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Filtros:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Foto:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Imprimir:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Instancias2:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Prioridades:FRAME = FRAME F_Consulta:HANDLE.

/* SETTINGS FOR FRAME F_AInstancias
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_Pasa 1 F_AInstancias */
ASSIGN 
       FRAME F_AInstancias:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Consulta
   FRAME-NAME                                                           */
/* BROWSE-TAB B_Instancias F_Filtros F_Consulta */
/* BROWSE-TAB B_Agencias F_AInstancias F_Consulta */
/* BROWSE-TAB B_Usuarios B_Agencias F_Consulta */
/* BROWSE-TAB B_Solicitud BUTTON-176 F_Consulta */
ASSIGN 
       B_Agencias:COLUMN-RESIZABLE IN FRAME F_Consulta       = TRUE.

ASSIGN 
       B_Instancias:COLUMN-RESIZABLE IN FRAME F_Consulta       = TRUE.

ASSIGN 
       B_Solicitud:POPUP-MENU IN FRAME F_Consulta             = MENU POPUP-MENU-B_Solicitud:HANDLE
       B_Solicitud:COLUMN-RESIZABLE IN FRAME F_Consulta       = TRUE.

ASSIGN 
       B_Usuarios:COLUMN-RESIZABLE IN FRAME F_Consulta       = TRUE.

/* SETTINGS FOR FILL-IN edw IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Corr
   NOT-VISIBLE Custom                                                   */
/* BROWSE-TAB Br_ConM W_CedNitCorr F_Corr */
ASSIGN 
       FRAME F_Corr:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Filtros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Filtros:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Foto
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Foto:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Imprimir
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Imprimir:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Instancias2
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BIns2 Cmb_instancias2 F_Instancias2 */
ASSIGN 
       FRAME F_Instancias2:HIDDEN           = TRUE
       FRAME F_Instancias2:MOVABLE          = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Agencias2 IN FRAME F_Instancias2
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Prioridades
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Prioridades:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN PR_Alta IN FRAME F_Prioridades
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN PR_Media IN FRAME F_Prioridades
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN PR_Nomral IN FRAME F_Prioridades
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BIns2
/* Query rebuild information for BROWSE BIns2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cfg_Instancias.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BIns2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_ConM
/* Query rebuild information for BROWSE Br_ConM
     _START_FREEFORM
/*OPEN QUERY {&SELF-NAME} FOR EACH Mov_Instancias NO-LOCK INDEXED-REPOSITION.*/
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_ConM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Agencias
/* Query rebuild information for BROWSE B_Agencias
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TAge NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Agencias */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Instancias
/* Query rebuild information for BROWSE B_Instancias
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TTot NO-LOCK  BY TTot.TOrdIns INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Instancias */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Pasa
/* Query rebuild information for BROWSE B_Pasa
     _START_FREEFORM
/*OPEN QUERY {&SELF-NAME} FOR EACH Cfg_Instancias NO-LOCK INDEXED-REPOSITION.*/
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE B_Pasa */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Solicitud
/* Query rebuild information for BROWSE B_Solicitud
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TSol NO-LOCK BY TSol.TVigSol DESCENDING INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Solicitud */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Usuarios
/* Query rebuild information for BROWSE B_Usuarios
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TUsu NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Usuarios */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Corr
/* Query rebuild information for FRAME F_Corr
     _Query            is NOT OPENED
*/  /* FRAME F_Corr */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Instancias2
/* Query rebuild information for FRAME F_Instancias2
     _Query            is NOT OPENED
*/  /* FRAME F_Instancias2 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME F_Consulta:HANDLE
       ROW             = 1
       COLUMN          = 2
       HEIGHT          = 1.08
       WIDTH           = 4
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta de Movimientos de Instancias */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta de Movimientos de Instancias */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BIns2
&Scoped-define FRAME-NAME F_Instancias2
&Scoped-define SELF-NAME BIns2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BIns2 wWin
ON ROW-DISPLAY OF BIns2 IN FRAME F_Instancias2
DO:
  FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario NO-LOCK NO-ERROR.
  IF AVAILABLE Usuarios THEN
     NomUsu = Usuarios.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_Corr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Corr wWin
ON CHOOSE OF Btn_Corr IN FRAME F_Consulta /* Modificar */
DO:
  FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.

  IF W_Agencia EQ 10 AND Usuarios.Prioridad GE 5 THEN DO:
     ASSIGN FRAME F_Consulta:SENSITIVE = FALSE
            FRAME F_Corr:VISIBLE       = TRUE.
            /*Mov_Instancias.Agencia:BGCOL IN BROWSE Br_ConM   = 10
            Mov_Instancias.Instancia:BGCOL                   = 10
            Mov_Instancias.Estado:BGCOL                      = 10 
            Mov_Instancias.Usuario:BGCOL                     = 10.*/
     APPLY "Entry" TO W_CedNitCorr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Eliminar wWin
ON CHOOSE OF Btn_Eliminar IN FRAME F_Consulta /* Borrar Solicit. */
DO:
  FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  IF Usuarios.Grupo NE 1 THEN DO:
     MESSAGE "Usuario No Autorizado para operar esta Opción."
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  IF Fec_Fin EQ ? OR Fec_Fin GE (W_Fecha - 30) THEN DO:
     MESSAGE "Debe Digitar por Filtros la Fecha Hasta Menor de (Hoy - 30)."
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  MESSAGE "               Segura(o) de Eliminar Solicitudes hasta la Fecha de " Fec_Fin
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar ELIMINAR" 
           UPDATE W_SiB AS LOGICAL.
  IF NOT W_SiB THEN
     RETURN.

  SESSION:SET-WAIT-STATE("General").

/*   DISABLE TRIGGERS FOR LOAD OF Solicitud. */
  FOR EACH Solicitud WHERE Solicitud.Estado    NE 2 AND
                           Solicitud.Fec_Solic LE Fec_Fin:
      FOR EACH Mov_Instancias WHERE 
          Mov_Instancias.Nit           EQ Solicitud.Nit AND
          Mov_Instancias.Num_Solicitud EQ Solicitud.Num_Solicitud:
          DELETE Mov_Instancias.
     END.
     DELETE Solicitud.
  END.

  SESSION:SET-WAIT-STATE("").

  MESSAGE "Las solicitudes fueron Eliminadas...El programa Finaliza y Regresa al Menú."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  APPLY "CHOOSE" TO BUTTON-169.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias2
&Scoped-define SELF-NAME Btn_OutIn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutIn wWin
ON CHOOSE OF Btn_OutIn IN FRAME F_Instancias2 /* Button 181 */
DO:
  HIDE FRAME f_instancias2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME BUTTON-162
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-162 wWin
ON CHOOSE OF BUTTON-162 IN FRAME F_Consulta /* Filtros */
DO:
  VIEW FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME BUTTON-163
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-163 wWin
ON CHOOSE OF BUTTON-163 IN FRAME F_Filtros /* Button 163 */
DO:
  HIDE FRAME F_Filtros.
  APPLY "Value-Changed" TO Cmb_EncInstancias IN FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AInstancias
&Scoped-define SELF-NAME BUTTON-165
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-165 wWin
ON CHOOSE OF BUTTON-165 IN FRAME F_AInstancias /* Button 165 */
DO:
  HIDE FRAME F_AInstancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME BUTTON-167
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-167 wWin
ON CHOOSE OF BUTTON-167 IN FRAME F_Consulta /* Button 167 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-169
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-169 wWin
ON CHOOSE OF BUTTON-169 IN FRAME F_Consulta /* Salir */
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


&Scoped-define SELF-NAME BUTTON-171
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-171 wWin
ON CHOOSE OF BUTTON-171 IN FRAME F_Consulta /* Button 171 */
DO:
  VIEW FRAME F_Imprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_AInstancias
&Scoped-define SELF-NAME BUTTON-173
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-173 wWin
ON CHOOSE OF BUTTON-173 IN FRAME F_AInstancias /* Asignar al usuario seleccionado */
DO:
  /*cerrar la instancia actual*/
  FIND FIRST Mov_Instancias WHERE 
       Mov_Instancias.Instancia     EQ TTot.TCodIns AND
       Mov_Instancias.Num_Solicitud EQ TSol.TSolSol AND
       Mov_Instancias.Nit           EQ TSol.TNitSol AND
       Mov_Instancias.Usuario       EQ TUsu.TCodUsu AND
       NOT Mov_Instancias.Estado    NO-ERROR.
   IF AVAILABLE Mov_Instancias THEN
      ASSIGN Mov_Instancias.Estado      = YES
             Mov_Instancias.Fec_Retiro  = W_Fecha
             Mov_Instancias.Hora_Retiro = TIME
             Mov_Instancias.Descripcion = Mov_Instancias.Descripcion + 
             "Asignación Forzada por usuario: " + W_Usuario.

  FIND FIRST Mov_Instancias WHERE 
       Mov_Instancias.Agencia       EQ Cfg_Instancias.Agencia AND
       Mov_Instancias.Instancia     EQ TTot.TCodIns AND
       Mov_Instancias.Num_Solicitud EQ TSol.TSolSol AND
       Mov_Instancias.Nit           EQ TSol.TNitSol AND
       Mov_Instancias.Usuario       EQ Cfg_Instancias.Usuario AND
       Mov_Instancias.Estado                              NO-ERROR.
  IF AVAILABLE Mov_Instancias THEN
      ASSIGN Mov_Instancias.Estado      = NO
             Mov_Instancias.Fec_Ingreso  = W_Fecha
             Mov_Instancias.Hora_Ingreso = TIME.
  ELSE DO:
    CREATE Mov_Instancias.
    ASSIGN Mov_Instancias.Agencia       = Cfg_Instancias.Agencia
           Mov_Instancias.Estado        = NO
           Mov_Instancias.Fec_Ingreso   = W_Fecha
           Mov_Instancias.Hora_Ingreso  = TIME
           Mov_Instancias.Instancia     = TTot.TCodIns
           Mov_Instancias.Nit           = TSol.TNitSol
           Mov_Instancias.Cuenta        = STRING(TSol.TNumSol)
           Mov_Instancias.Num_Solicitud = TSol.TSolSol
           Mov_Instancias.Usuario       = Cfg_Instancias.Usuario.
  END.
  APPLY "value-changed" TO Cmb_EncInstancias IN FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imprimir
&Scoped-define SELF-NAME BUTTON-174
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-174 wWin
ON CHOOSE OF BUTTON-174 IN FRAME F_Imprimir /* Button 174 */
DO:
  HIDE FRAME F_Imprimir.
  DEFINE VAR Listado     AS CHARACTER INITIAL "".
  Listado = W_PathSpl + "ConInstan.LST".
  {INCLUIDO\Imprimir.I "Listado"} 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-175
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-175 wWin
ON CHOOSE OF BUTTON-175 IN FRAME F_Imprimir /* Button 175 */
DO:
  HIDE FRAME F_Imprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME BUTTON-176
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-176 wWin
ON CHOOSE OF BUTTON-176 IN FRAME F_Consulta /* Ver Prioridades */
DO:
  VIEW FRAME F_Prioridades.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Prioridades
&Scoped-define SELF-NAME BUTTON-177
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-177 wWin
ON CHOOSE OF BUTTON-177 IN FRAME F_Prioridades /* Button 177 */
DO:
  HIDE FRAME F_Prioridades.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Corr
&Scoped-define SELF-NAME BUTTON-180
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-180 wWin
ON CHOOSE OF BUTTON-180 IN FRAME F_Corr /* Button 180 */
DO:
  ASSIGN FRAME F_Corr:VISIBLE       = FALSE
         FRAME F_Consulta:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias2
&Scoped-define SELF-NAME BUTTON-182
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-182 wWin
ON CHOOSE OF BUTTON-182 IN FRAME F_Instancias2 /* Asignar Instancia */
DO:
    ASSIGN FRAME F_Consulta Cmb_EncInstancias.
    ASSIGN FRAME F_Instancias2 Cmb_Proceso.
    IF INTEGER(SUBSTRING(Cmb_EncInstancias,1,5)) EQ 1 OR INTEGER(SUBSTRING(Cmb_EncInstancias,1,5)) EQ 3 THEN
    DO:
        /*cerrar la instancia actual*/
        FIND FIRST Mov_Instancias WHERE 
             Mov_Instancias.Instancia     EQ TTot.TCodIns AND
             Mov_Instancias.Num_Solicitud EQ TSol.TSolSol AND
             Mov_Instancias.Nit           EQ TSol.TNitSol AND
             Mov_Instancias.Usuario       EQ TUsu.TCodUsu AND
             NOT Mov_Instancias.Estado    NO-ERROR.
         IF AVAILABLE Mov_Instancias THEN DO:
            ASSIGN Mov_Instancias.Estado      = YES
                   Mov_Instancias.Fec_Retiro  = W_Fecha
                   Mov_Instancias.Hora_Retiro = TIME
                   Mov_Instancias.Descripcion = Mov_Instancias.Descripcion + 
                   "Asignación Forzada por usuario: " + W_Usuario.
            IF INTEGER(SUBSTRING(Cmb_EncInstancias,1,5)) EQ 3 AND INTEGER(SUBSTRING(Cmb_Proceso,1,5)) EQ 1 THEN DO:
               FIND Creditos WHERE Creditos.Nit EQ TSol.TNitSol AND Creditos.Num_Solicitud EQ TSol.TSolSol AND
                    Creditos.Monto EQ TSol.TMonSol NO-ERROR.
/*                DISABLE TRIGGERS FOR LOAD OF Creditos. */
               IF AVAILABLE Creditos THEN DO:
                  DELETE Creditos.
               END.
            END.
         END.

        FIND FIRST Mov_Instancias WHERE 
             Mov_Instancias.Agencia       EQ Cfg_Instancias.Agencia AND
             Mov_Instancias.Instancia     EQ INTEGER(SUBSTRING(Cmb_Instancias2:SCREEN-VALUE,1,5)) AND
             Mov_Instancias.Num_Solicitud EQ TSol.TSolSol AND
             Mov_Instancias.Nit           EQ TSol.TNitSol AND
             Mov_Instancias.Usuario       EQ Cfg_Instancias.Usuario NO-ERROR.
        IF AVAILABLE Mov_Instancias THEN
            ASSIGN Mov_Instancias.Estado       = NO
                   Mov_Instancias.Fec_Ingreso  = W_Fecha
                   Mov_Instancias.Hora_Ingreso = TIME
                   Mov_Instancias.Fec_Retiro   = ?
                   Mov_Instancias.Hora_Retiro  = 0.
        ELSE DO:
          CREATE Mov_Instancias.
          ASSIGN Mov_Instancias.Agencia       = Cfg_Instancias.Agencia
                 Mov_Instancias.Estado        = NO
                 Mov_Instancias.Fec_Ingreso   = W_Fecha
                 Mov_Instancias.Hora_Ingreso  = TIME
                 Mov_Instancias.Instancia     = INTEGER(SUBSTRING(Cmb_Instancias2:SCREEN-VALUE,1,5))
                 Mov_Instancias.Nit           = TSol.TNitSol
                 Mov_Instancias.Cuenta        = STRING(TSol.TNumSol)
                 Mov_Instancias.Num_Solicitud = TSol.TSolSol
                 Mov_Instancias.Usuario       = Cfg_Instancias.Usuario.
        END.
        IF INTEGER(SUBSTRING(Cmb_Proceso,1,5)) EQ 1 THEN DO:
           FIND Solicitud WHERE Solicitud.Nit EQ TSol.TNitSol AND Solicitud.Num_Solicitud EQ TSol.TSolSol NO-ERROR.
           IF AVAILABLE Solicitud THEN DO:
              IF Solicitud.Estado GE 2 THEN Solicitud.Estado = 1.
           END.
           ELSE DO:
               MESSAGE "No se encontro la solicitud para cambiarle el estado a EN ESTUDIO" VIEW-AS ALERT-BOX.
           END.
        END.
        APPLY "value-changed" TO Cmb_EncInstancias IN FRAME F_Consulta.
    END.
    ELSE DO:
       MESSAGE "Solo se puede hacer cambio entre instancias" SKIP
               "de solicitud y proceso de desembolso!" VIEW-AS ALERT-BOX ERROR.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Foto
&Scoped-define SELF-NAME BUTTON-230
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-230 wWin
ON CHOOSE OF BUTTON-230 IN FRAME F_Foto /* Cerrar */
DO:
  HIDE FRAME F_Foto.
  cFullPathName = string(cRaiz + "SOLICITUD ") + string(TSol.TSolSol, "99999") + ".tif".

  RUN ShellExecute{&A} IN hpApi 
                   (0,
                    "open",
                    cFullPathName,
                    "",
                    "",
                    1,
                    OUTPUT hInstance).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Agencias
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME B_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Agencias wWin
ON MOUSE-SELECT-CLICK OF B_Agencias IN FRAME F_Consulta
DO:
  RUN Totalizar_Usuario.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Instancias
&Scoped-define SELF-NAME B_Instancias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Instancias wWin
ON MOUSE-SELECT-CLICK OF B_Instancias IN FRAME F_Consulta
DO:
   FOR EACH TAge: DELETE TAge. END.
   FOR EACH TUsu: DELETE TUsu. END.
   FOR EACH TSol: DELETE TSol. END.
   RUN Tot_Instancias.
   /*IF W_Agencia EQ 24 THEN DO:*/
      OPEN QUERY B_Agencias   
           FOR EACH TAge WHERE TAge.TInsAge EQ TTot.TCodIns INDEXED-REPOSITION.
   /*END.
   ELSE DO:
     OPEN QUERY B_Agencias   
          FOR EACH TAge WHERE TAge.TInsAge EQ TTot.TCodIns
                          AND TAge.TCodAge EQ W_Agencia INDEXED-REPOSITION.
   END.*/
   RUN Totalizar_Usuario.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Pasa
&Scoped-define FRAME-NAME F_AInstancias
&Scoped-define SELF-NAME B_Pasa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Pasa wWin
ON ROW-DISPLAY OF B_Pasa IN FRAME F_AInstancias
DO:
  FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario NO-LOCK NO-ERROR.
  IF AVAILABLE Usuarios THEN
     NomUsu = Usuarios.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Solicitud
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME B_Solicitud
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Solicitud wWin
ON MOUSE-SELECT-DBLCLICK OF B_Solicitud IN FRAME F_Consulta
DO:

  RUN VerFoto.

  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Solicitud wWin
ON ROW-DISPLAY OF B_Solicitud IN FRAME F_Consulta
DO:
  IF R_Visualiza:SCREEN-VALUE IN FRAME F_Filtros EQ "1" THEN DO:
     IF TSol.TVigSol GT TTot.TTmiIns THEN DO:
        ASSIGN TSol.TCodSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TNomSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TNomcli:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TNitSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TNumSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TSolSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TMonSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TValDes:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TPlaSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TPerSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TFciSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TFcrSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TVigSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TCodSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TNomSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TNomcli:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TNitSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TNumSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TSolSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TMonSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TValDes:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TPlaSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TPerSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TFciSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TFcrSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TVigSol:FGCOL IN BROWSE B_Solicitud = 15.
     END.
     IF TSol.TVigSol GT (TTot.TTmiIns / 2) AND TSol.TVigSol LE TTot.TTmiIns THEN DO:
        ASSIGN TSol.TCodSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TNomSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TNomcli:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TNitSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TNumSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TSolSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TMonSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TValDes:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TPlaSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TPerSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TFciSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TFcrSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TVigSol:BGCOL IN BROWSE B_Solicitud = 14.
     END.
     IF TSol.TVigSol LE (TTot.TTmiIns / 2) THEN DO:
        ASSIGN TSol.TCodSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TNomSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TNomcli:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TNitSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TNumSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TSolSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TMonSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TValDes:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TPlaSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TPerSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TFciSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TFcrSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TVigSol:BGCOL IN BROWSE B_Solicitud = 10.
        ASSIGN TSol.TCodSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TNomSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TNomcli:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TNitSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TNumSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TSolSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TMonSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TValDes:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TPlaSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TPerSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TFciSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TFcrSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TVigSol:FGCOL IN BROWSE B_Solicitud = 0.
     END.
  END.
  ELSE DO:
     IF TSol.TVigSol GT TMG THEN DO:
        ASSIGN TSol.TCodSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TNomSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TNomcli:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TNitSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TNumSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TSolSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TMonSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TValDes:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TPlaSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TPerSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TFciSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TFcrSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TVigSol:BGCOL IN BROWSE B_Solicitud = 12
               TSol.TCodSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TNomSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TNomcli:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TNitSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TNumSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TSolSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TMonSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TValDes:FGCOL IN BROWSE B_Solicitud = 15 
               TSol.TPlaSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TPerSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TFciSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TFcrSol:FGCOL IN BROWSE B_Solicitud = 15
               TSol.TVigSol:FGCOL IN BROWSE B_Solicitud = 15.
     END.
     IF TSol.TVigSol GT (TMG / 2) AND TSol.TVigSol LE TMG THEN DO:
        ASSIGN TSol.TCodSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TNomSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TNomcli:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TNitSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TNumSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TSolSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TMonSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TValDes:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TPlaSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TPerSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TFciSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TFcrSol:BGCOL IN BROWSE B_Solicitud = 14
               TSol.TVigSol:BGCOL IN BROWSE B_Solicitud = 14.
     END.
     IF TSol.TVigSol LE (TMG / 2) THEN DO:
        ASSIGN TSol.TCodSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TNomSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TNomcli:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TNitSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TNumSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TSolSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TMonSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TValDes:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TPlaSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TPerSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TFciSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TFcrSol:BGCOL IN BROWSE B_Solicitud = 10
               TSol.TVigSol:BGCOL IN BROWSE B_Solicitud = 10.
        ASSIGN TSol.TCodSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TNomSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TNomcli:fGCOL IN BROWSE B_Solicitud = 0
               TSol.TNitSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TNumSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TSolSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TMonSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TValDes:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TPlaSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TPerSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TFciSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TFcrSol:FGCOL IN BROWSE B_Solicitud = 0
               TSol.TVigSol:FGCOL IN BROWSE B_Solicitud = 0.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Usuarios
&Scoped-define SELF-NAME B_Usuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Usuarios wWin
ON MOUSE-SELECT-CLICK OF B_Usuarios IN FRAME F_Consulta
DO:
  RUN Totalizar_Solicitud.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias2
&Scoped-define SELF-NAME Cmb_Agencias2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias2 wWin
ON VALUE-CHANGED OF Cmb_Agencias2 IN FRAME F_Instancias2 /* Agencia */
DO:
  APPLY "value-changed" TO Cmb_Instancias2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Cmb_EncInstancias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_EncInstancias wWin
ON VALUE-CHANGED OF Cmb_EncInstancias IN FRAME F_Consulta /* Procesos Disponibles */
DO:
  FOR EACH TTot: DELETE TTot. END.

  FOR EACH TAge: ASSIGN TAge.TNroAge = 0
                        TAge.TValAge = 0
                        TAge.TDesAge = 0.
  END.

  edw:SCREEN-VALUE IN FRAME F_Consulta = "".

  FIND Varios WHERE Varios.Tipo   EQ 9 AND
                    Varios.Codigo EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,5)) NO-LOCK NO-ERROR.
  IF AVAILABLE Varios THEN TMG = Varios.Val_Final.
  FOR EACH Instancias WHERE
           Instancias.Tipo_Instancia EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,5)) AND
           Instancias.Estado         EQ 1 NO-LOCK:
      
      CREATE TTot.
      ASSIGN TTot.TCodIns = Instancias.Instancia
             TTot.TNomIns = Instancias.Nom_Instancia
             TTot.TOrdIns = Instancias.Orden
             TTot.TTMIIns = Instancias.TMI
             ttot.ttipo   = instancias.tipo_instancia.      
  END.

  RUN Totalizar_Instancias.
  
  FIND FIRST TAge NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
     MESSAGE "No se encuentra ninguna información entre las fechas de trabajo."
             VIEW-AS ALERT-BOX INFORMATION.
  
  CLOSE QUERY B_Usuarios.            /*Para blanquear el browser siempre, Enero 17/06 GAER*/
  CLOSE QUERY B_Solicitud.
  FOR EACH TUsu: DELETE TUsu. END.
  FOR EACH TSol: DELETE TSol. END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias2
&Scoped-define SELF-NAME Cmb_instancias2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_instancias2 wWin
ON VALUE-CHANGED OF Cmb_instancias2 IN FRAME F_Instancias2 /* Instancias */
DO:
  ASSIGN FRAME F_Instancias2 Cmb_Agencias2 Cmb_Instancias2.
  OPEN QUERY BIns2 
  FOR EACH Cfg_Instancias WHERE 
      Cfg_Instancias.Agencia   EQ INTEGER(SUBSTRING(Cmb_Agencias2,1,3))  AND
      Cfg_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias2,1,5)) AND
      Cfg_Instancias.Estado    EQ 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Proceso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Proceso wWin
ON VALUE-CHANGED OF Cmb_Proceso IN FRAME F_Instancias2 /* Proceso */
DO:
  Cmb_Instancias2:LIST-ITEMS = "".
  ASSIGN FRAME F_Instancias2 Cmb_Proceso.
  FIND Varios WHERE Varios.Tipo   EQ 9 AND
                    Varios.Codigo EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,5)) NO-LOCK NO-ERROR.
  FOR EACH Instancias WHERE
           Instancias.Tipo_Instancia EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,5)) AND
           Instancias.Instancia      LE TTot.TCodIns AND
           Instancias.Estado         EQ 1 NO-LOCK:
      W_Ok = Cmb_Instancias2:ADD-LAST(STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia) IN FRAME F_Instancias2.
  END.
  Cmb_Instancias2:SCREEN-VALUE = Cmb_Instancias2:ENTRY(1).
  APPLY "value-changed" TO Cmb_Instancias2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
IF FRAME F_AInstancias:HIDDEN EQ YES AND T_Refresh:SCREEN-VALUE IN FRAME F_Consulta EQ "YES" THEN
  APPLY "value-changed" TO Cmb_EncInstancias IN FRAME F_Consulta.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME Fec_Fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fec_Fin wWin
ON LEAVE OF Fec_Fin IN FRAME F_Filtros /* Hasta */
DO:
   ASSIGN Fec_Fin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Asignar_a_otra_Instancia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Asignar_a_otra_Instancia wWin
ON CHOOSE OF MENU-ITEM m_Asignar_a_otra_Instancia /* Asignar a otra Instancia */
DO:
  DEFI VAR W_RowIdU AS ROWID.
  ASSIGN W_RowIdU = ROWID(Usuarios) WHEN AVAIL(Usuarios).
  FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.

  IF Usuarios.Grupo NE 1 THEN DO:
    IF Usuarios.Grupo NE 9 THEN DO:
     MESSAGE "Esta opción solo puede ser Operada" SKIP
             "por el Grupo-Usuario administrador.!" VIEW-AS ALERT-BOX.
     FIND FIRST Usuarios WHERE ROWID(Usuarios) EQ W_RowIdU NO-LOCK NO-ERROR.
     RETURN NO-APPLY.
    END.
  END.

  FIND FIRST Usuarios WHERE ROWID(Usuarios) EQ W_RowIdU NO-LOCK NO-ERROR.
  DO WITH FRAME F_Instancias2:
    Cmb_Proceso:SCREEN-VALUE = Cmb_Proceso:ENTRY(1).
    IF INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_Consulta,1,5)) EQ 1 THEN
       DISABLE Cmb_Proceso WITH FRAME F_Instancias2.
    ELSE
       ENABLE Cmb_Proceso WITH FRAME F_Instancias2.
    Cmb_Instancias2:LIST-ITEMS = "".
    FOR EACH Instancias WHERE Instancias.Tipo_Instancia EQ INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE,1,5))
             AND Instancias.Instancia LE TTot.TCodIns
             AND Instancias.Estado EQ 1 NO-LOCK:
        W_Ok = Cmb_Instancias2:ADD-LAST(STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia) IN FRAME F_Instancias2.
    END.
    Cmb_Instancias2:SCREEN-VALUE = Cmb_Instancias2:ENTRY(1).
    Cmb_Agencias2:SCREEN-VALUE = Cmb_Agencias2:ENTRY(TSol.TAgeSol).
    APPLY "value-changed" TO Cmb_Proceso.
    VIEW FRAME F_Instancias2.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Fecha_de_Ingreso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Fecha_de_Ingreso wWin
ON CHOOSE OF MENU-ITEM m_Fecha_de_Ingreso /* Fecha de Ingreso */
DO:
   OPEN QUERY B_Solicitud  FOR EACH TSol WHERE 
     TSol.TAgeSol EQ TAge.TCodAge AND
     TSol.TUsuSol EQ TUsu.TCodUsu BY TSol.TFciSol INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Fecha_de_Retiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Fecha_de_Retiro wWin
ON CHOOSE OF MENU-ITEM m_Fecha_de_Retiro /* Fecha de Retiro */
DO:
   OPEN QUERY B_Solicitud  FOR EACH TSol WHERE 
     TSol.TAgeSol EQ TAge.TCodAge AND
     TSol.TUsuSol EQ TUsu.TCodUsu BY TSol.TFcrSol INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Monto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Monto wWin
ON CHOOSE OF MENU-ITEM m_Monto /* Monto */
DO:
 OPEN QUERY B_Solicitud  FOR EACH TSol WHERE 
     TSol.TAgeSol EQ TAge.TCodAge AND
     TSol.TUsuSol EQ TUsu.TCodUsu BY TSol.TMonSol INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Mover_hacia_otro_Usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Mover_hacia_otro_Usuario wWin
ON CHOOSE OF MENU-ITEM m_Mover_hacia_otro_Usuario /* Asignar a otro Usuario */
DO:
  OPEN QUERY B_Pasa 
     FOR EACH Cfg_Instancias WHERE
              Cfg_Instancias.Tipo_Instancia EQ INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_Consulta,1,5)) AND
              Cfg_Instancias.Instancia      EQ TTot.TCodIns AND
              Cfg_Instancias.Usuario        NE TUsu.TCodUsu AND
              Cfg_Instancias.Estado         EQ 1 AND
              Cfg_Instancias.Fec_Retiro     EQ ? AND
              Cfg_Instancias.Agencia        EQ TSol.TAgeSol AND
              Cfg_Instancias.Plazo_Minimo   LE TSol.TPlaSol AND
              Cfg_Instancias.Plazo_Maximo   GE TSol.TPlaSol AND
              Cfg_Instancias.Monto_Minimo   LE TSol.TMonSol AND
              Cfg_Instancias.Monto_Maximo   GE TSol.TMonSol
     NO-LOCK BY Cfg_Instancias.Usuario INDEXED-REPOSITION.
  VIEW FRAME F_AInstancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_NumSolicitud
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_NumSolicitud wWin
ON CHOOSE OF MENU-ITEM m_NumSolicitud /* Num.Solicitud */
DO:
  OPEN QUERY B_Solicitud  FOR EACH TSol WHERE 
     TSol.TAgeSol EQ TAge.TCodAge AND
     TSol.TUsuSol EQ TUsu.TCodUsu BY TSol.TSolSol INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Plazo wWin
ON CHOOSE OF MENU-ITEM m_Plazo /* Plazo */
DO:
   OPEN QUERY B_Solicitud  FOR EACH TSol WHERE 
     TSol.TAgeSol EQ TAge.TCodAge AND
     TSol.TUsuSol EQ TUsu.TCodUsu BY TSol.TPlaSol INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Vigencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Vigencia wWin
ON CHOOSE OF MENU-ITEM m_Vigencia /* Vigencia */
DO:
  OPEN QUERY B_Solicitud  FOR EACH TSol WHERE 
     TSol.TAgeSol EQ TAge.TCodAge AND
     TSol.TUsuSol EQ TUsu.TCodUsu BY TSol.TVigSol DESCENDING INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Vigencia_Mayor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Vigencia_Mayor wWin
ON CHOOSE OF MENU-ITEM m_Vigencia_Mayor /* Vigencia */
DO:
   OPEN QUERY B_Solicitud  FOR EACH TSol WHERE 
     TSol.TAgeSol EQ TAge.TCodAge AND
     TSol.TUsuSol EQ TUsu.TCodUsu BY TSol.TVigSol INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Corr
&Scoped-define SELF-NAME W_CedNitCorr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CedNitCorr wWin
ON LEAVE OF W_CedNitCorr IN FRAME F_Corr
DO:
  CLOSE QUERY Br_ConM.

  OPEN QUERY Br_ConM FOR EACH Mov_Instancias WHERE 
                              Mov_Instancias.Nit EQ W_CedNitCorr:SCREEN-VALUE NO-LOCK
                         BY Fec_Ingreso BY Hora_Ingreso INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define BROWSE-NAME BIns2
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "W-Con_Instancias.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "W-Con_Instancias.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  DISPLAY Cmb_EncInstancias T_Refresh edw 
      WITH FRAME F_Consulta IN WINDOW wWin.
  ENABLE Cmb_EncInstancias T_Refresh BUTTON-162 B_Instancias BUTTON-167 
         BUTTON-171 B_Agencias B_Usuarios Btn_Eliminar Btn_Corr BUTTON-176 
         B_Solicitud BUTTON-169 BUTTON-168 
      WITH FRAME F_Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}
  ENABLE P_Foto BUTTON-230 
      WITH FRAME F_Foto IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Foto}
  DISPLAY R_Imp 
      WITH FRAME F_Imprimir IN WINDOW wWin.
  ENABLE R_Imp BUTTON-174 BUTTON-175 
      WITH FRAME F_Imprimir IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Imprimir}
  DISPLAY R_Estado Fec_Ini Fec_Fin R_Visualiza 
      WITH FRAME F_Filtros IN WINDOW wWin.
  ENABLE R_Estado Fec_Ini Fec_Fin R_Visualiza BUTTON-163 
      WITH FRAME F_Filtros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Filtros}
  DISPLAY W_CedNitCorr 
      WITH FRAME F_Corr IN WINDOW wWin.
  ENABLE W_CedNitCorr Br_ConM BUTTON-180 
      WITH FRAME F_Corr IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Corr}
  DISPLAY Cmb_Agencias2 Cmb_Proceso Cmb_instancias2 
      WITH FRAME F_Instancias2 IN WINDOW wWin.
  ENABLE Cmb_Proceso Cmb_instancias2 BIns2 BUTTON-182 Btn_OutIn 
      WITH FRAME F_Instancias2 IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Instancias2}
  ENABLE B_Pasa BUTTON-165 BUTTON-173 
      WITH FRAME F_AInstancias IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_AInstancias}
  DISPLAY PR_Nomral PR_Alta PR_Media 
      WITH FRAME F_Prioridades IN WINDOW wWin.
  ENABLE RECT-290 RECT-291 RECT-292 BUTTON-177 
      WITH FRAME F_Prioridades IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Prioridades}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Detallado wWin 
PROCEDURE Informe_Detallado :
DEFINE VAR TipIns LIKE Cfg_Instancias.Tipo_Instancia.
DEFINE VAR Encabezado AS CHARACTER FORMAT "X(132)".
DEFINE VAR Linea AS CHARACTER FORMAT "X(232)".

DEFINE VAR W_NomUsu AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_NomIns AS CHARACTER FORMAT "X(30)".

FOR EACH tiu: DELETE tiu.
END.

DEFINE VAR i AS INTEGER.
FOR EACH TOrdIns: DELETE TOrdIns. END.
FOR EACH TUsuDet: DELETE TUsuDet. END.
TipIns = INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_consulta,1,5)).
FOR EACH Instancias WHERE 
         Instancias.Tipo_Instancia EQ TipIns NO-LOCK
         BY Instancias.Orden:
    FOR EACH Mov_Instancias WHERE
             Mov_Instancias.Instancia EQ Instancias.Instancia AND
             Mov_Instancias.Fec_Retiro EQ ? NO-LOCK BREAK BY Mov_Instancias.Usuario:
         FIND TIU WHERE TIU.u_age EQ Mov_Instancias.Agencia AND
                        TIU.u_usu EQ Mov_Instancias.Usuario AND
                        TIU.u_ins EQ Mov_Instancias.Instancia NO-ERROR.
         IF NOT AVAILABLE TIU THEN DO:
            CREATE TIU.
            ASSIGN TIU.u_age = Mov_Instancias.Agencia
                   TIU.u_usu = Mov_Instancias.Usuario
                   TIU.u_ins = Mov_Instancias.Instancia.
         END.
         ASSIGN TIU.u_con = TIU.u_con + 1.
         IF TipIns EQ 1 THEN DO:
            FIND Solicitud WHERE Solicitud.Agencia       EQ Mov_Instancias.Agencia AND
                                 Solicitud.Num_Solicitud EQ Mov_Instancias.Num_Solicitud AND
                                 Solicitud.Nit           EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
            ASSIGN TIU.u_val = TIU.u_val + Solicitud.Monto.
         END.
         IF TipIns EQ 2 OR TipIns EQ 3 OR TipIns EQ 5  THEN DO:
            FIND Creditos WHERE Creditos.Agencia       EQ Mov_Instancias.Agencia AND
                                Creditos.Num_Credito   EQ INTEGER(Mov_Instancias.Cuenta) AND
                                Creditos.Num_Solicitud EQ Mov_Instancias.Num_Solicitud AND
                                Creditos.Nit           EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
            ASSIGN TIU.u_val = TIU.u_val + Creditos.Sdo_Capital.
         END.
    END.
END.
FOR EACH TIU BREAK BY TIU.u_usu BY tiu.u_age:
  IF FIRST-OF(TIU.u_usu) THEN DO:
     FIND Usuarios WHERE Usuarios.Usuario EQ TIU.u_usu NO-LOCK NO-ERROR.
     IF AVAILABLE Usuarios THEN
        DISPLAY SKIP(1) 
                "Usuario : "     AT 1
                Usuarios.Usuario AT 15
                Usuarios.Nombre  AT 25
        WITH FRAME F_NomUsu WIDTH 132 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
  END.
  FIND Instancias WHERE Instancias.Instancia EQ tiu.u_ins NO-LOCK NO-ERROR.
  W_NomIns = "Inconsistente".
  IF AVAILABLE Instancias THEN W_NomIns = Instancias.Nom_Instancia.
  DISPLAY tiu.u_age AT 1
          tiu.u_ins AT 5
          W_NomIns  AT 15
          tiu.u_con AT 50
          tiu.u_val AT 60 FORMAT ">>,>>>,>>>,>>9"
  WITH FRAME F_tiu WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Detalladoold2 wWin 
PROCEDURE Informe_Detalladoold2 :
DEFINE VAR TipIns LIKE Cfg_Instancias.Tipo_Instancia.
DEFINE VAR Encabezado AS CHARACTER FORMAT "X(132)".
DEFINE VAR Linea AS CHARACTER FORMAT "X(232)".
DEFINE VAR i AS INTEGER.
FOR EACH TOrdIns: DELETE TOrdIns. END.
FOR EACH TUsuDet: DELETE TUsuDet. END.
TipIns = INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_consulta,1,5)).
FOR EACH Instancias WHERE
         Instancias.Tipo_Instancia EQ TipIns
         NO-LOCK BREAK BY Instancias.Orden:
   i = i + 1.
   CREATE TOrdIns.      
   ASSIGN TOrdIns.TOrdIns = i
          TOrdIns.TCodIns = Instancias.Instancia
          TOrdIns.TNomIns = Instancias.Nom_Instancia
          Encabezado = Encabezado + " " + STRING(Instancias.Nom_Instancia,"X(15)").
END.

FOR EACH Usuario NO-LOCK:
  FIND FIRST Cfg_Instancias WHERE Cfg_Instancias.Tipo_Instancia EQ TipIns AND
             Cfg_Instancias.Usuario EQ Usuarios.Usuario NO-LOCK NO-ERROR.
  IF AVAILABLE Cfg_Instancias THEN DO:
     CREATE TUsuDet.
     ASSIGN TUsuDet.TAgeDet = Usuarios.Agencia
            TUsuDet.TCodDet = Usuarios.Usuario
            TUsuDet.TNomDet = Usuarios.Nombre.
  END.
END.

FOR EACH Cfg_Instancias WHERE 
         Cfg_Instancias.Tipo_Instancia EQ TipIns
         NO-LOCK BREAK BY Cfg_Instancias.Usuario BY Cfg_Instancias.Orden:
 
  FIND TOrdIns WHERE TOrdIns.TCodIns EQ Cfg_Instancias.Instancia NO-ERROR.
  IF AVAILABLE TOrdIns THEN i = TOrdIns.TOrdIns.
  FIND TUsuDet WHERE TUsuDet.TCodDet EQ Cfg_Instancias.Usuario NO-ERROR.
  ASSIGN TUsuDet.TInsDet[i] = TOrdIns.TCodIns
         TUsuDet.TNInDet[i] = TOrdIns.TNomIns.
  FOR EACH Mov_Instancias WHERE
           Mov_Instancias.Usuario     EQ Cfg_Instancias.Usuario AND
           Mov_Instancias.Instancia   EQ Cfg_Instancias.Instancia AND
           Mov_Instancias.Fec_Ingreso GE DATE(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) AND
           Mov_Instancias.Fec_Ingreso LE DATE(Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros)
           NO-LOCK BREAK BY Mov_Instancias.Usuario:
     FIND Solicitud WHERE Solicitud.Num_Solicitud EQ Mov_Instancias.Num_Solicitud NO-LOCK NO-ERROR.
     IF AVAILABLE Solicitud THEN DO:
        ASSIGN TUsuDet.TVInDet[i] = TUsuDet.TVInDet[i] + Solicitud.Monto
               TUsuDet.TCInDet[i] = TUsuDet.TCInDet[i] + 1.
     END.
  END.
END.

DISPLAY "Nombre         " AT 1
        Encabezado        AT 16
        WITH FRAME F_Enca WIDTH 232 USE-TEXT NO-BOX NO-LABELS.
        
FOR EACH TUsuDet:
    Linea = STRING(TUsuDet.TNomDet,"X(20)") + " " +
            STRING(TUsuDet.TVInDet[1],">>,>>>,>>>,>>9") + " " + 
            STRING(TUsuDet.TCInDet[1],">,>>9") + " " +
            STRING(TUsuDet.TVInDet[2],">>,>>>,>>>,>>9") + " " + 
            STRING(TUsuDet.TCInDet[2],">,>>9") + " " +
            STRING(TUsuDet.TVInDet[3],">>,>>>,>>>,>>9") + " " + 
            STRING(TUsuDet.TCInDet[3],">,>>9") + " " +
            STRING(TUsuDet.TVInDet[4],">>,>>>,>>>,>>9") + " " +
            STRING(TUsuDet.TCInDet[4],">,>>9") + " " +
            STRING(TUsuDet.TVInDet[5],">>,>>>,>>>,>>9") + " " +
            STRING(TUsuDet.TCInDet[5],">,>>9") + " " +
            STRING(TUsuDet.TVInDet[6],">>,>>>,>>>,>>9") + " " +
            STRING(TUsuDet.TCInDet[6],">,>>9") + " " +
            STRING(TUsuDet.TVInDet[7],">>,>>>,>>>,>>9") + " " +
            STRING(TUsuDet.TCInDet[7],">,>>9").
    DISPLAY linea AT 1
    WITH FRAME F_Deta NO-LABELS USE-TEXT NO-BOX WIDTH 232.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_estadistico wWin 
PROCEDURE Informe_estadistico :
DEFINE VAR TipIns LIKE Cfg_Instancias.Tipo_Instancia.
DEFINE VAR Encabezado AS CHARACTER FORMAT "X(132)".
DEFINE VAR Linea AS CHARACTER FORMAT "X(232)".
DEFINE VAR Listado     AS CHARACTER INITIAL "".
Listado = W_PathSpl + "SOLICITUD_ESTADISTICA.TXT".


DEFINE VAR W_NomUsu AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_NomIns AS CHARACTER FORMAT "X(30)".
DEFINE VAR Xnom     AS CHARACTER FORMAT "X(30)".
DEFINE VARIABLE xtiempo AS INTEGER INITIAL 0.

DEFINE VAR i AS INTEGER.
TipIns = 1.
OUTPUT TO VALUE(Listado).
PUT   "Oficina"          ";"
      "usuario"          ";"
      "Nombre Empleado"  ";"
      "instancia"        ";"
      "Nom_instancia"    ";"
      "Nit Asociado"     ";"
      "Nombre Asociado"  ";"
      "Monto "           ";"
      "Tasa    "        ";"
      "Plazo"            ";"  
      "Fec_ingreso"      ";"
      "Fec_Retiro"       ";"
      "Dias Respuesta "   SKIP.
FOR EACH Instancias WHERE 
         (Instancias.Tipo_Instancia EQ 1 OR
          Instancias.Tipo_Instancia EQ 3)  NO-LOCK
         BY Instancias.Orden:
    FOR EACH Mov_Instancias WHERE
             Mov_Instancias.Instancia EQ Instancias.Instancia 
             NO-LOCK BREAK BY Mov_Instancias.Usuario:

     IF Mov_Instancias.Fec_Ingreso GE DATE(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) AND
           Mov_Instancias.Fec_Ingreso LE DATE(Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros) THEN DO:
        W_NomIns = Instancias.Nom_Instancia.
        FIND Usuarios WHERE Usuarios.Usuario EQ Mov_instancias.usuario NO-LOCK NO-ERROR.
        FIND FIRST clientes WHERE clientes.nit = Mov_instancias.nit NO-LOCK NO-ERROR.
        IF AVAILABLE(clientes)  THEN 
            Xnom = TRIM(Clientes.nombre) + " " + TRIM(apellido1) + " " + TRIM(apellido2).
        ELSE xnom = "No registrado ".
        IF mov_instancia.fec_Retiro NE ? THEN
          Xtiempo = (mov_instancia.fec_Retiro - mov_instancia.fec_ingreso).
        ELSE Xtiempo = (W_fecha - mov_instancia.fec_ingreso).
        IF TipIns EQ 1 THEN DO:
           FIND Solicitud WHERE Solicitud.Agencia       EQ Mov_Instancias.Agencia AND
                                 Solicitud.Num_Solicitud EQ Mov_Instancias.Num_Solicitud AND
                                 Solicitud.Nit           EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
           PUT   solicitud.agencia          ";"
                 Mov_Instancia.usuario      ";"
                 Usuarios.Nombre            ";"
                 mov_instancia.instancia    ";"
                 W_NomIns                   ";"
                 Solicitud.nit              ";"
                 xnom                       ";"
                 solicitud.monto            ";"
                 solicitud.tasa             ";"
                 solicitud.plazo                      ";"  
                 mov_instancia.fec_ingreso  ";"
                 mov_instancia.fec_retiro   ";"
                 xtiempo SKIP.
         END.
         IF TipIns EQ 2 OR TipIns EQ 3 OR TipIns EQ 5  THEN DO:
            FIND Creditos WHERE Creditos.Agencia       EQ Mov_Instancias.Agencia AND
                                Creditos.Num_Credito   EQ INTEGER(Mov_Instancias.Cuenta) AND
                                Creditos.Num_Solicitud EQ Mov_Instancias.Num_Solicitud AND
                                Creditos.Nit           EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
            PUT   creditos.agencia           ";"
                  Mov_Instancia.usuario      ";"
                  Usuarios.Nombre            ";"
                  mov_instancia.instancia    ";"
                  W_NomIns                   ";"
                  Creditos.nit              ";"
                  xnom                       ";"
                  Creditos.monto            ";"
                  Creditos.tasa             ";"
                  Creditos.plazo                      ";"  
                  mov_instancia.fec_ingreso  ";"
                  mov_instancia.fec_retiro   ";"
                  xtiempo SKIP.
            END.
     END.
    END.
END.
MESSAGE "Informe Generado en : " + Listado VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Fabrica_Periodo wWin 
PROCEDURE Informe_Fabrica_Periodo :
DEFINE VAR TipIns LIKE Cfg_Instancias.Tipo_Instancia.
DEFINE VAR Encabezado AS CHARACTER FORMAT "X(132)".
DEFINE VAR Linea AS CHARACTER FORMAT "X(232)".

DEFINE VAR W_NomUsu AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_NomIns AS CHARACTER FORMAT "X(30)".
DEFINE VAR Xnom     AS CHARACTER FORMAT "X(30)".

DEFINE VAR  xIU LIKE estadistico.I20U.
DEFINE VAR xIFI LIKE estadistico.I20FI.
DEFINE VAR xIFF LIKE estadistico.I20FF.
DEFINE VAR Listado     AS CHARACTER INITIAL "".
Listado = W_PathSpl + "Gestion_fabrica.TXT".

DEFINE VARIABLE xtiempo AS INTEGER INITIAL 0.

EMPTY TEMP-TABLE ESTADISTICO.
DEFINE VAR i AS INTEGER.
TipIns = 1.
FOR EACH Instancias WHERE 
         (Instancias.Tipo_Instancia EQ 1 OR
          Instancias.Tipo_Instancia EQ 3)  NO-LOCK
         BY Instancias.Orden:
       FOR EACH Mov_Instancias WHERE
             Mov_Instancias.Instancia EQ Instancias.Instancia 
             NO-LOCK BREAK BY Mov_Instancias.Usuario:
        
        IF Mov_Instancias.Fec_Ingreso GE DATE(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) AND
           Mov_Instancias.Fec_Ingreso LE DATE(Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros) THEN DO:
           IF mov_instancia.fec_Retiro NE ? THEN
              Xtiempo = (mov_instancia.fec_Retiro - mov_instancia.fec_ingreso).
           ELSE Xtiempo = (W_fecha - mov_instancia.fec_ingreso).
           
           FIND Solicitud WHERE Solicitud.Agencia       EQ Mov_Instancias.Agencia AND
                                   Solicitud.Num_Solicitud EQ Mov_Instancias.Num_Solicitud AND
                                   Solicitud.Nit           EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
         
           IF Solicitud.Fec_Solicitud GE DATE(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) AND
              Solicitud.Fec_Solicitud LE DATE(Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros) THEN DO:
             FIND FIRST estadistico WHERE estadistico.tnit = mov_instancias.nit AND
                                          estadistico.tsol  = Mov_Instancias.Num_Solicitud NO-LOCK NO-ERROR.
             IF Instancias.Tipo_Instancia EQ 1 THEN DO:   
                IF AVAILABLE(solicitud) THEN DO:
                 IF NOT AVAILABLE(estadistico) THEN do:
                    CREATE estadistico.
                    ASSIGN Tage = solicitud.agencia
                           Tnit = solicitud.nit
                           Tsol = solicitud.num_solicitud
                           Tmon = solicitud.monto
                           ttas = solicitud.tasa
                           tpla = solicitud.plazo.
                 END.
                END.
             END.
             IF Instancias.Tipo_Instancia EQ 3  THEN DO:
                FIND Creditos WHERE Creditos.Agencia       EQ Mov_Instancias.Agencia AND
                                Creditos.Num_Credito   EQ INTEGER(Mov_Instancias.Cuenta) AND
                                Creditos.Num_Solicitud EQ Mov_Instancias.Num_Solicitud AND
                                Creditos.Nit           EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
                IF AVAILABLE(creditos) THEN DO:
                 IF NOT AVAILABLE(estadistico) THEN do:
                    CREATE estadistico.
                    ASSIGN Tage = creditos.agencia
                           Tnit = creditos.nit.
                 END.
                 IF creditos.estado = 2 THEN
                   ASSIGN Tnumc = creditos.num_credito
                        Tmonc = creditos.monto
                        ttasc = creditos.tasa
                        tplac = creditos.plazo.
                  ELSE ASSIGN Tnumc = 0
                              Tmonc = 0
                              ttasc = 0
                              tplac = 0.
                END.
             END.
           IF TRIM(mov_instancias.usuario) NE "" THEN
              ASSIGN xIU  = mov_instancias.usuario
                     xIFI = Mov_instancias.fec_ingreso
                     xIFF = MOV_INSTANCIAS.FEC_RETIRO.
           ELSE ASSIGN xIU  = ?
                       xIFI =  ?
                       xIFF =  ?.

           IF mov_instancias.instancia = 20 THEN 
              ASSIGN I20U  = xiu
                     I20FI = xifi
                     I20FF = xiff
                     i20t  = xtiempo .
           IF mov_instancias.instancia = 30 THEN 
              ASSIGN I30U  = xiu
                     I30FI = xifi
                     I30FF = xiff
                     i30t  = xtiempo .

           IF mov_instancias.instancia = 35 THEN 
              ASSIGN I35U  = xiu
                     I35FI = xifi
                     I35FF = xiff
                     i35t  = xtiempo .

           IF mov_instancias.instancia = 40 THEN 
              ASSIGN I40U  = xiu
                     I40FI = xifi
                     I40FF = xiff
                     i40t  = xtiempo .

           IF mov_instancias.instancia = 50 THEN 
             ASSIGN I50U  = xiu
                    I50FI = xifi
                    I50FF = xiff
                    i50t  = xtiempo .


           IF mov_instancias.instancia = 70 THEN 
             ASSIGN I70U  = xiu
                    I70FI = xifi
                    I70FF = xiff
                    i70t  = xtiempo .

           IF mov_instancias.instancia = 75 THEN 
              ASSIGN I75U  = xiu
                     I75FI = xifi
                     I75FF = xiff
                     i75t  = xtiempo .

           IF mov_instancias.instancia = 78 THEN 
              ASSIGN I78U  = xiu
                     I78FI = xifi
                     I78FF = xiff
                     i78t  = xtiempo .

           IF mov_instancias.instancia = 80 THEN 
              ASSIGN I80U  = xiu
                     I80FI = xifi
                     I80FF = xiff
                     i80t  = xtiempo .
           
           IF mov_instancias.instancia = 950 THEN 
              ASSIGN I950U  = xiu
                     I950FI = xifi
                     I950FF = xiff
                     i950t  = xtiempo .
      END.
    END.
    END.
END.

/*****
/* genera informe */
******/
OUTPUT TO VALUE(Listado).
PUT "agencia;"
     "Nit;"
     "nombre;"
     "num_solicitud;"
     "monto Solicitado;"
     "tasa Solicit;"
     "Plazo Solict;"
     "num_credito;"
     "monto Desembol;"
     "tasa Desembol;"
     "plazo Desembol;"
     "Rad_usuario_20;"
     "Rad_FI;"
     "Rad_FF;"
     "Rad_Tiempo;"
     "VerOf_usuario_30;"
     "VerOf_FI;"
     "VerOf_FF;"
     "VerOf_Tiempo;"
     "VerFc_usuario_35;"
     "VerFc_FI;"
     "VerFc_FF;"
     "VerFc_Tiempo;"
     "Analis_usuario_40;"
     "Analis_FI;"
     "Analis_FF;"
     "Analis_Tiempo;"
     "Concp_usuario_50;"
     "Concp_FI;"
     "Concp_FF;"
     "Concp_Tiempo;"
     "Negado_usuario_70;"
     "Negado_FI;"
     "Negado_FF;"
     "Negado_Tiempo;"
     "Formlz_usuario_75;"
     "Formlz_FI;"
     "Formlz_FF;"
     "Formlz_Tiempo;"
     "autoriz_usuario_78;"
     "autoriz_FI;"
     "autoriz_FF;"
     "autoriz_Tiempo;"
     "Giro_usuario_80;"
     "Giro_FI;"
     "Giro_FF;"
     "Giro_Tiempo;"
     "Cajero_usuario_950;"
     "Cajero_FI;"
     "Cajero_FF;"
     "Cajero_Tiempo;" SKIP.
 
FOR EACH estadistico:
   FIND FIRST CLIENTES WHERE CLIENTES.NIT = ESTADISTICO.tNIT NO-LOCK NO-ERROR.
   IF AVAILABLE(CLIENTES)  THEN estadistico.tNOM = TRIM(APELLIDO1) + " " + TRIM(APELLIDO2) + " " + TRIM(NOMBRE).
   ELSE estadistico.tNOM = "Nombre no Encontrado".
   EXPORT DELIMITER ";" estadistico .
END.
MESSAGE "Informe Generado en : " + Listado VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Solicitud wWin 
PROCEDURE Informe_Solicitud :
DEFINE VAR TotEstC     AS DECIMAL FORMAT "999".
DEFINE VAR TotAprC     AS DECIMAL FORMAT "999".
DEFINE VAR TotCanC     AS DECIMAL FORMAT "999".
DEFINE VAR TotEstV     AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotAprV     AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotCanV     AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotEstP     AS DECIMAL FORMAT ">>9.99".
DEFINE VAR TotAprP     AS DECIMAL FORMAT ">>9.99".
DEFINE VAR TotCanP     AS DECIMAL FORMAT ">>9.99".
DEFINE VAR TotTot     AS DECIMAL FORMAT "9999".

 FOR EACH Solicitud WHERE
          Solicitud.Fec_Solicitud GE DATE(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) AND
          Solicitud.Fec_Solicitud LE DATE(Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros)
          NO-LOCK BREAK BY Solicitud.Agencia:
          IF FIRST-OF(Solicitud.Agencia) THEN DO:
             FIND Agencias WHERE Agencias.Agencia EQ Solicitud.Agencia NO-LOCK NO-ERROR.
             IF AVAILABLE Agencias THEN DO:
                CREATE TInfoA.
                ASSIGN TInfoA.TAgencia = Agencias.Agencia
                       TInfoA.TNomAgen = Agencias.Nombre.
             END.
          END.
          CASE Solicitud.Estado:
            WHEN 2 THEN
               ASSIGN TInfoA.TCon_Aprobada = TInfoA.TCon_Aprobada + 1
                      TInfoA.TVal_Aprobada = TInfoA.TVal_Aprobada + Solicitud.Monto
                      TotAprC = TotAprC + 1
                      TotAPrV = TotAPrV + Solicitud.Monto.
            WHEN 3 THEN 
               ASSIGN TInfoA.TCon_Cancelada = TInfoA.TCon_Cancelada + 1
                      TInfoA.TVal_Cancelada = TInfoA.TVal_Cancelada + Solicitud.Monto
                      TotCanC = TotCanC + 1
                      TotCanV = TotCanV + Solicitud.Monto.
            OTHERWISE 
               ASSIGN TInfoA.TCon_Estudio = TInfoA.TCon_Estudio + 1
                      TInfoA.TVal_Estudio = TInfoA.TVal_Estudio + Solicitud.Monto
                      TotEstC = TotEstC + 1
                      TotEstV = TotEstV + Solicitud.Monto.
          END CASE.
 END.
 TotTot = TotEstC + TotCanC + TotAprC.
 /*                1         2         3         4         5         6         7         8         9         1         2        2
          123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
          
 DISPLAY "----------------------------------------|APROBADAS|------------------|CANCELADAS|-----------------|EN ESTUDIO|---------" AT 1 SKIP(1)
         "Age  Nombre Agencia                    Valor  Nro    Por             Valor  Nro   Por             Valor   Nro  Por" AT 1
    WITH FRAME F_EncAgen WIDTH 132 USE-TEXT NO-BOX.
 FOR EACH TInfoA:
   ASSIGN TInfoA.TPor_Aprobada  = ((TInfoA.TCon_Aprobada * 100) / TotTot)
          TInfoA.TPor_Cancelada = ((TInfoA.TCon_Cancelada * 100) / TotTot)
          TInfoA.TPor_Estudio   = ((TInfoA.TCon_Estudio * 100) / TotTot)
          TotAprP = TotAprP + TInfoA.TPor_Aprobada
          TotCanP = TotCanP + TInfoA.TPor_Cancelada
          TotestP = (TotEstP + TInfoA.TPor_Estudio).
   DISPLAY TInfoA.TAgencia         AT 1 FORMAT "999"
           TInfoA.TNomAgen         AT 5 FORMAT "X(25)"
           TInfoA.TVal_Aprobada    AT 32 FORMAT ">>,>>>,>>>,>>9"
           TInfoA.TCon_Aprobada    AT 48 FORMAT ">>9"
           TInfoA.TPor_AProbada    AT 53 FORMAT ">9.99%"
           TInfoA.TVal_Cancelada   AT 61 FORMAT ">>,>>>,>>>,>>9"
           TInfoA.TCon_Cancelada   AT 77 FORMAT ">>9"
           TInfoA.TPor_Cancelada   AT 83 FORMAT ">9.99%"
           TInfoA.TVal_Estudio     AT 91 FORMAT ">>,>>>,>>>,>>9"
           TInfoA.TCon_Estudio     AT 107 FORMAT ">>9"
           TInfoA.TPor_Estudio     AT 112 FORMAT ">9.99%"
           WITH FRAME F_Agencias WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
 END.
 DISPLAY Skip(1)
           "-----------------------------------------------------------------------------------------------------------------------" AT 1 SKIP(1)
           "Totales Entidad" AT 1
           TotAprV    AT 32 FORMAT ">,>>>,>>>,>>9"
           TotAprC    AT 47 FORMAT ">>9"
           TotAprP    AT 52 FORMAT ">>9.99%"
           TotCanV    AT 61 FORMAT ">,>>>,>>>,>>9"
           TotCanC    AT 77 FORMAT ">>9"
           TotCanP    AT 82 FORMAT ">>9.99%"
           TotEstV    AT 91 FORMAT ">,>>>,>>>,>>9"
           TotEstC    AT 107 FORMAT ">>9"
           TotEstP    AT 111 FORMAT ">>9.99%"
    WITH FRAME F_PieAgen WIDTH 132 USE-TEXT NO-BOX NO-LABELS.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_xsolicitud wWin 
PROCEDURE Informe_xsolicitud :
DEFINE VAR TipIns LIKE Cfg_Instancias.Tipo_Instancia.
DEFINE VAR Encabezado AS CHARACTER FORMAT "X(132)".
DEFINE VAR Linea AS CHARACTER FORMAT "X(232)".

DEFINE VAR W_NomUsu AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_NomIns AS CHARACTER FORMAT "X(30)".
DEFINE VAR Xnom     AS CHARACTER FORMAT "X(30)".

DEFINE VAR  xIU LIKE estadistico.I20U.
DEFINE VAR xIFI LIKE estadistico.I20FI.
DEFINE VAR xIFF LIKE estadistico.I20FF.

DEFINE VARIABLE xtiempo AS INTEGER INITIAL 0.
DEFINE VAR Listado     AS CHARACTER INITIAL "".
Listado = W_PathSpl + "Gestion_Solicitud.txt".


EMPTY TEMP-TABLE ESTADISTICO.
DEFINE VAR i AS INTEGER.
TipIns = 1.
FOR EACH Instancias WHERE 
         (Instancias.Tipo_Instancia EQ 1 OR
          Instancias.Tipo_Instancia EQ 3)  NO-LOCK
         BY Instancias.Orden:
    FOR EACH Mov_Instancias WHERE
             Mov_Instancias.Instancia EQ Instancias.Instancia 
             NO-LOCK BREAK BY Mov_Instancias.Usuario:
        
        IF Mov_Instancias.Fec_Ingreso GE DATE(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) AND
        Mov_Instancias.Fec_Ingreso LE DATE(Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros) THEN DO:
           IF mov_instancia.fec_Retiro NE ? THEN
              Xtiempo = (mov_instancia.fec_Retiro - mov_instancia.fec_ingreso).
           ELSE Xtiempo = (W_fecha - mov_instancia.fec_ingreso).

           FIND FIRST estadistico WHERE estadistico.tnit = mov_instancias.nit AND
                                        estadistico.tsol  = Mov_Instancias.Num_Solicitud NO-LOCK NO-ERROR.
           IF Instancias.Tipo_Instancia EQ 1 THEN DO:
              FIND Solicitud WHERE Solicitud.Agencia       EQ Mov_Instancias.Agencia AND
                                   Solicitud.Num_Solicitud EQ Mov_Instancias.Num_Solicitud AND
                                   Solicitud.Nit           EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
              IF AVAILABLE(solicitud) THEN DO:
                 IF NOT AVAILABLE(estadistico) THEN do:
                    CREATE estadistico.
                    ASSIGN Tage = solicitud.agencia
                           Tnit = solicitud.nit
                           Tsol = solicitud.num_solicitud
                           Tmon = solicitud.monto
                           ttas = solicitud.tasa
                           tpla = solicitud.plazo.
                 END.
              END.
           END.
           IF Instancias.Tipo_Instancia EQ 3  THEN DO:
            FIND Creditos WHERE Creditos.Agencia       EQ Mov_Instancias.Agencia AND
                                Creditos.Num_Credito   EQ INTEGER(Mov_Instancias.Cuenta) AND
                                Creditos.Num_Solicitud EQ Mov_Instancias.Num_Solicitud AND
                                Creditos.Nit           EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
            IF AVAILABLE(creditos) THEN DO:
                 IF NOT AVAILABLE(estadistico) THEN do:
                    CREATE estadistico.
                    ASSIGN Tage = creditos.agencia
                           Tnit = creditos.nit.
                 END.
                 IF creditos.estado = 2 THEN
                   ASSIGN Tnumc = creditos.num_credito
                        Tmonc = creditos.monto
                        ttasc = creditos.tasa
                        tplac = creditos.plazo.
                  ELSE ASSIGN Tnumc = 0
                              Tmonc = 0
                              ttasc = 0
                              tplac = 0.
            END.
           END.
           IF TRIM(mov_instancias.usuario) NE " " THEN
              ASSIGN xIU  = mov_instancias.usuario
                     xIFI = Mov_instancias.fec_ingreso
                     xIFF = MOV_INSTANCIAS.FEC_RETIRO.
           ELSE ASSIGN xIU  = ?
                       xIFI =  ?
                       xIFF =  ?.

           IF mov_instancias.instancia = 20 THEN 
              ASSIGN I20U  = xiu
                     I20FI = xifi
                     I20FF = xiff
                     i20t  = xtiempo .
           IF mov_instancias.instancia = 30 THEN 
              ASSIGN I30U  = xiu
                     I30FI = xifi
                     I30FF = xiff
                     i30t  = xtiempo .

           IF mov_instancias.instancia = 35 THEN 
              ASSIGN I35U  = xiu
                     I35FI = xifi
                     I35FF = xiff
                     i35t  = xtiempo .

           IF mov_instancias.instancia = 40 THEN 
              ASSIGN I40U  = xiu
                     I40FI = xifi
                     I40FF = xiff
                     i40t  = xtiempo .

           IF mov_instancias.instancia = 50 THEN 
             ASSIGN I50U  = xiu
                    I50FI = xifi
                    I50FF = xiff
                    i50t  = xtiempo .


           IF mov_instancias.instancia = 70 THEN 
             ASSIGN I70U  = xiu
                    I70FI = xifi
                    I70FF = xiff
                    i70t  = xtiempo .

           IF mov_instancias.instancia = 75 THEN 
              ASSIGN I75U  = xiu
                     I75FI = xifi
                     I75FF = xiff
                     i75t  = xtiempo .

           IF mov_instancias.instancia = 78 THEN 
              ASSIGN I78U  = xiu
                     I78FI = xifi
                     I78FF = xiff
                     i78t  = xtiempo .

           IF mov_instancias.instancia = 80 THEN 
              ASSIGN I80U  = xiu
                     I80FI = xifi
                     I80FF = xiff
                     i80t  = xtiempo .
           
           IF mov_instancias.instancia = 950 THEN 
              ASSIGN I950U  = xiu
                     I950FI = xifi
                     I950FF = xiff
                     i950t  = xtiempo .
    END.
    END.
END.
/*****
/* genera informe */
******/
OUTPUT TO VALUE(Listado).
PUT "agencia;"
     "Nit;"
     "nombre;"
     "num_solicitud;"
     "monto Solicitado;"
     "tasa Solicit;"
     "Plazo Solict;"
     "num_credito;"
     "monto Desembol;"
     "tasa Desembol;"
     "plazo Desembol;"
     "Rad_usuario_20;"
     "Rad_FI;"
     "Rad_FF;"
     "Rad_Tiempo;"
     "VerOf_usuario_30;"
     "VerOf_FI;"
     "VerOf_FF;"
     "VerOf_Tiempo;"
     "VerFc_usuario_35;"
     "VerFc_FI;"
     "VerFc_FF;"
     "VerFc_Tiempo;"
     "Analis_usuario_40;"
     "Analis_FI;"
     "Analis_FF;"
     "Analis_Tiempo;"
     "Concp_usuario_50;"
     "Concp_FI;"
     "Concp_FF;"
     "Concp_Tiempo;"
     "Negado_usuario_70;"
     "Negado_FI;"
     "Negado_FF;"
     "Negado_Tiempo;"
     "Formlz_usuario_75;"
     "Formlz_FI;"
     "Formlz_FF;"
     "Formlz_Tiempo;"
     "autoriz_usuario_78;"
     "autoriz_FI;"
     "autoriz_FF;"
     "autoriz_Tiempo;"
     "Giro_usuario_80;"
     "Giro_FI;"
     "Giro_FF;"
     "Giro_Tiempo;"
     "Cajero_usuario_950;"
     "Cajero_FI;"
     "Cajero_FF;"
     "Cajero_Tiempo;" SKIP.
 
FOR EACH estadistico:
   FIND FIRST CLIENTES WHERE CLIENTES.NIT = ESTADISTICO.tNIT NO-LOCK NO-ERROR.
   IF AVAILABLE(CLIENTES)  THEN estadistico.tNOM = TRIM(APELLIDO1) + " " + TRIM(APELLIDO2) + " " + TRIM(NOMBRE).
   ELSE estadistico.tNOM = "Nombre no Encontrado".
   EXPORT DELIMITER ";" estadistico .
END.
MESSAGE "Informe Generado en : " + Listado VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*
  ----------------------------------------------------------------------*/
 /* IF W_Agencia EQ 10 THEN*/
     FOR EACH Varios WHERE Varios.Tipo       EQ 9 AND
                           Varios.Estado     EQ 1 AND 
                           Varios.Id_Gestion EQ YES NO-LOCK.
      W_Ok = Cmb_EncInstancias:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME F_Consulta.
      IF Varios.Codigo EQ 1 OR Varios.Codigo EQ 3 THEN
         W_Ok = Cmb_Proceso:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME F_Instancias2.
  END.
  FOR EACH Agencias NO-LOCK:
      W_Ok = Cmb_Agencias2:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME F_Instancias2.
  END.
 /* ELSE FOR EACH Varios WHERE Varios.Tipo       EQ 9 AND
                           ( Varios.Codigo     EQ 1 OR Varios.Codigo EQ 3) AND
                             Varios.Estado     EQ 1 AND 
                             Varios.Id_Gestion EQ YES NO-LOCK.
      W_Ok = Cmb_EncInstancias:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME F_Consulta.
  END.*/
 
  RUN SUPER.
  
  DO WITH FRAME F_Filtros:
    ASSIGN Fec_Ini:SCREEN-VALUE = STRING("01/01/" + STRING(YEAR(TODAY)))
           Fec_Fin:SCREEN-VALUE = STRING(TODAY).
  END.
  Cmb_EncInstancias:SCREEN-VALUE = Cmb_EncInstancias:ENTRY(1).
  APPLY "value-changed" TO Cmb_EncInstancias.
  HIDE FRAME F_AInstancias.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inst_Creditos wWin 
PROCEDURE Inst_Creditos :
FIND FIRST Creditos WHERE     creditos.agencia       EQ mov_instancias.agencia
                          AND Creditos.Nit           EQ Mov_Instancias.Nit
                          AND Creditos.Num_credito EQ integer(Mov_Instancias.cuenta) 
                          NO-LOCK NO-ERROR.
FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ creditos.Cod_Credito NO-LOCK NO-ERROR.
IF NOT AVAIL(Pro_Creditos) THEN 
   MESSAGE "Creditos.Nit Creditos.Num_Credito Creditos.Cod_Credito " SKIP
            Creditos.Nit Creditos.Num_Credito Creditos.Cod_Credito SKIP
           "Falta Producto de Credito"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
 IF AVAIL(Creditos) THEN DO:
    /*IF Creditos.Dias_Atraso LE 0 THEN DO: 
       FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Mov_Instancias.Usuario NO-LOCK NO-ERROR.
       IF  AVAIL(Usuarios) AND Usuarios.Id_CobroJuridico NE 1
       AND INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_Consulta,1,5)) NE 3 THEN
          RETURN.
    END.*/

    CREATE TSol.                                         
    ASSIGN TSol.TAgeSol = Mov_Instancias.Agencia               
           TSol.TCodSol = Creditos.Cod_Credito                        
           TSol.TNomSol = Pro_Creditos.Nom_Producto                    
           TSol.TNitSol = Creditos.Nit                                
           TSol.TNumSol = DEC(Mov_Instancias.cuenta)
           TSol.TSolSol = Mov_Instancias.Num_Solicitud
           TSol.TMonSol = Creditos.Monto              /*Creditos.Sdo_capital */
           TSol.TPlaSol = Creditos.Plazo
           TSol.TFciSol = Mov_Instancias.Fec_Ingreso                   
           TSol.TFcrSol = Mov_Instancias.Fec_Retiro                    
           TSol.TUsuSol = Mov_Instancias.Usuario.                      
                                                               
    CASE Creditos.Per_Pago:                                           
       WHEN 1 THEN TSol.TPerSol = "Semanal".                           
       WHEN 2 THEN TSol.TPerSol = "Decadal".                           
       WHEN 3 THEN TSol.TPerSol = "Quincenal".                         
       WHEN 4 THEN TSol.TPerSol = "Mensual".                           
    END CASE.                                                  
 END.
 ELSE 
    MESSAGE "No se hallo el Credito, en esta instancia."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inst_Creditos3 wWin 
PROCEDURE Inst_Creditos3 :
/* Este Procedure no se esta utilizando*/
 FIND FIRST Creditos WHERE Creditos.Num_Credito EQ INT(Tmi.Cuenta) NO-LOCK NO-ERROR.
 IF AVAIL(Creditos) THEN DO:
    CREATE TSol.                                         
    ASSIGN TSol.TAgeSol = TMI.Agencia               
           TSol.TCodSol = Creditos.Cod_Credito                        
           TSol.TNomSol = Pro_Creditos.Nom_Producto                    
           TSol.TNitSol = Creditos.Nit                                
           TSol.TNumSol = DEC(Tmi.cuenta)
           TSol.TSolSol = Tmi.Num_Solicitud
           TSol.TMonSol = Solicitud.Monto   /*Creditos.Sdo_capital */
           TSol.TPlaSol = Creditos.Cuo_Atraso
           TSol.TFciSol = Tmi.Fec_Ingreso                   
           TSol.TFcrSol = Tmi.Fec_Retiro                    
           TSol.TUsuSol = Tmi.Usuario.                      
                                                               
    CASE Creditos.Per_Pago:                                           
       WHEN 1 THEN TSol.TPerSol = "Semanal".                           
       WHEN 2 THEN TSol.TPerSol = "Decadal".                           
       WHEN 3 THEN TSol.TPerSol = "Quincenal".                         
       WHEN 4 THEN TSol.TPerSol = "Mensual".                           
    END CASE.                                                  
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inst_Solicitud wWin 
PROCEDURE Inst_Solicitud :
DEFINE VARIABLE viValDes AS INTEGER     NO-UNDO. /*valor desembolso*/
    
    FIND FIRST clientes WHERE clientes.nit = solicitud.nit NO-LOCK NO-ERROR.
    IF AVAILABLE(clientes) THEN 
        ASSIGN wx_nombre = TRIM(nombre) + " " + TRIM(apellido1) + " " + TRIM(apellido2).
    ELSE    
        ASSIGN wx_nombre = "No Encontrado".

    /*GCamacho - Feb-08-2011 . implementa columna valor desembolsar*/
    DO viCnt = 1 TO 5:
        IF Solicitud.Num_CredACanc[viCnt] GT 0 THEN DO: 
            ASSIGN viValDes = viValDes + getValTotCred(INPUT solicitud.nit, INPUT Solicitud.Num_CredACanc[viCnt]).
        END.
    END.
/*     ASSIGN viValDes = Solicitud.Monto - viValDes. */
    /****************************/
    CREATE TSol.                                          
    ASSIGN TSol.TAgeSol = Mov_Instancias.Agencia          
            TSol.TCodSol = Solicitud.Cod_Credito           
            TSol.TNomSol = Pro_Creditos.Nom_Producto       
            TSol.TNitSol = Solicitud.Nit                   
            TSol.TNumSol = DECIMAL(Mov_Instancias.Cuenta)  
            TSol.TSolSol = Mov_Instancias.Num_Solicitud    
            TSol.TMonSol = Solicitud.Monto                 
            TSol.TValDes = TSol.TMonSol - viValDes
            TSol.TPlaSol = Solicitud.Plazo                 
            TSol.TFciSol = Mov_Instancias.Fec_Ingreso      
            TSol.TFcrSol = Mov_Instancias.Fec_Retiro       
            TSol.TUsuSol = Mov_Instancias.Usuario
            TSol.TNomCli = Wx_nombre.         
                                                           
    CASE Solicitud.Per_Pago:                              
        WHEN 1 THEN TSol.TPerSol = "Semanal".              
        WHEN 2 THEN TSol.TPerSol = "Decadal".              
        WHEN 3 THEN TSol.TPerSol = "Quincenal".            
        WHEN 4 THEN TSol.TPerSol = "Mensual".              
    END CASE.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inst_Solicitud3 wWin 
PROCEDURE Inst_Solicitud3 :
/* Este Procedure no se esta utilizando*/
 CREATE TSol.                                          
 ASSIGN TSol.TAgeSol = Tmi.Agencia          
        TSol.TCodSol = Solicitud.Cod_Credito           
        TSol.TNomSol = Pro_Creditos.Nom_Producto       
        TSol.TNitSol = Solicitud.Nit                   
        TSol.TNumSol = DECIMAL(Tmi.Cuenta)  
        TSol.TSolSol = Tmi.Num_Solicitud    
        TSol.TMonSol = Solicitud.Monto                 
        TSol.TPlaSol = Solicitud.Plazo                 
        TSol.TFciSol = Tmi.Fec_Ingreso      
        TSol.TFcrSol = Tmi.Fec_Retiro       
        TSol.TUsuSol = Tmi.Usuario.         
                                                       
 CASE Solicitud.Per_Pago:                              
    WHEN 1 THEN TSol.TPerSol = "Semanal".              
    WHEN 2 THEN TSol.TPerSol = "Decadal".              
    WHEN 3 THEN TSol.TPerSol = "Quincenal".            
    WHEN 4 THEN TSol.TPerSol = "Mensual".              
 END CASE.                                             

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    
    ASSIGN FRAME F_Imprimir R_Imp.
    W_Reporte    = "REPORTE   : Créditos - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna =     "Referencia : " + Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_Consulta +
                       "      - Entre: " + STRING(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) +
                       "  y  " + STRING(Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros).
          
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  IF R_Imp EQ 1 THEN DO:
    CASE INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_Consulta,1,5)):
      WHEN 1 THEN RUN Informe_Solicitud.
    END CASE.
  END.
  IF R_Imp EQ 2 THEN DO:
    CASE INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_Consulta,1,5)):
      WHEN 1 THEN RUN Informe_Detallado.
    END CASE.
  END.
  IF R_Imp EQ 3 THEN DO:
     RUN Informe_Estadistico.
  END.
  IF R_Imp EQ 4 THEN DO:
     RUN Informe_xsolicitud.
  END.
  IF R_Imp EQ 5 THEN DO:
     RUN Informe_Fabrica_Periodo.
  END.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Totalizar_Instancias wWin 
PROCEDURE Totalizar_Instancias :
/*
  ----------------------------------------------------------------*/
  DEFINE VAR P_Ins AS ROWID.

  FOR EACH TAge: DELETE TAge. END.

  FOR EACH TTot:
      ASSIGN TTot.TNroIns = 0
             TTot.TValIns = 0
             TTot.TDesIns = 0.
      RUN Tot_Instancias.
  END.

     OPEN QUERY B_Instancias 
          FOR EACH TTot WHERE TTot.TNroIns GT 0 BY TTot.TOrdIns INDEXED-REPOSITION.
     OPEN QUERY B_Agencias   
          FOR EACH TAge WHERE TAge.TInsAge EQ TTot.TCodIns INDEXED-REPOSITION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Totalizar_Solicitud wWin 
PROCEDURE Totalizar_Solicitud :
DEFINE VAR W_FecW AS CHARACTER FORMAT "X(10)".
  FOR EACH TSol: DELETE TSol. END.
  FOR EACH Mov_Instancias WHERE 
           Mov_Instancias.Agencia       EQ TAge.TCodAge AND
           Mov_Instancias.Instancia     EQ TTot.TCodIns AND   
           Mov_Instancias.Usuario       EQ TUsu.TCodUsu AND
           Mov_Instancias.Estado        EQ LOGICAL(R_Estado:SCREEN-VALUE IN FRAME F_Filtros) AND
           Mov_Instancias.Fec_Ingreso   GE DATE(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) AND
           Mov_Instancias.Fec_Ingreso   LE DATE(Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros) NO-LOCK:
         
         IF TTot.ttipo NE 2 THEN DO:
            FIND Solicitud WHERE Solicitud.Num_Solicitud EQ Mov_Instancias.Num_Solicitud AND
                              Solicitud.Nit           EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Solicitud THEN NEXT.
            FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.

            IF INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_Consulta,1,5)) EQ 1 THEN
               RUN Inst_Solicitud.            
            ELSE 
              RUN Inst_Creditos.
         END.
         ELSE DO:
            RUN Inst_Creditos.
         END.
         IF TSol.TFcrSol EQ ? THEN ASSIGN TSol.TVigSol = TODAY - TSol.TFciSol.
         ELSE TSol.TVigSol = TSol.TFcrSol - TSol.TFciSol.
         
         /*totales*/
  END.

 OPEN QUERY B_Solicitud  FOR EACH TSol WHERE 
     TSol.TAgeSol EQ TAge.TCodAge AND
     TSol.TUsuSol EQ TUsu.TCodUsu BY TSol.TVigSol DESCENDING INDEXED-REPOSITION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Totalizar_Solicitud3 wWin 
PROCEDURE Totalizar_Solicitud3 :
/* Este Procedure no se esta utilizando*/

DEFINE VAR W_FecW AS CHARACTER FORMAT "X(10)".
  FOR EACH TSol: DELETE TSol. END.
  FOR EACH Tmi USE-INDEX Xs WHERE 
           Tmi.Agencia       EQ TAge.TCodAge AND
           Tmi.Instancia     EQ TTot.TCodIns AND   
           Tmi.Usuario       EQ TUsu.TCodUsu AND
           Tmi.Estado        EQ LOGICAL(R_Estado:SCREEN-VALUE IN FRAME F_Filtros) AND
           Tmi.Fec_Ingreso   GE DATE(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) AND
           Tmi.Fec_Ingreso   LE DATE(Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros) NO-LOCK:
         FIND Solicitud WHERE Solicitud.Num_Solicitud EQ Tmi.Num_Solicitud NO-LOCK NO-ERROR.
         FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.

         /*IF INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_Consulta,1,5)) NE 2 
         AND INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE,1,5)) NE 5 THEN 
         Oct.11/05 GAER se reemplazo x la sgte*/

         IF INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_Consulta,1,5)) EQ 1 THEN
            RUN Inst_Solicitud3.            
         ELSE 
            RUN Inst_Creditos3.

         IF TSol.TFcrSol EQ ? THEN ASSIGN TSol.TVigSol = TODAY - TSol.TFciSol.
         ELSE TSol.TVigSol = TSol.TFcrSol - TSol.TFciSol.
         
         /*totales*/
  END.

 OPEN QUERY B_Solicitud  FOR EACH TSol WHERE 
     TSol.TAgeSol EQ TAge.TCodAge AND
     TSol.TUsuSol EQ TUsu.TCodUsu BY TSol.TVigSol DESCENDING INDEXED-REPOSITION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Totalizar_Usuario wWin 
PROCEDURE Totalizar_Usuario :
DEFINE VAR TotUsu AS INTEGER.
  DEFINE VAR W_FecW AS CHARACTER FORMAT "X(10)".
  DEFINE VAR i AS DECIMAL.
  
  DEFINE VARIABLE viValDes AS INTEGER     NO-UNDO.

  FOR EACH TUsu: DELETE TUsu. END.
  FOR EACH TSol: DELETE TSol. END.

  FOR EACH Mov_Instancias WHERE 
           Mov_Instancias.Agencia     EQ TAge.TCodAge   AND
           Mov_Instancias.Instancia   EQ TTot.TCodIns   AND   
           Mov_Instancias.Estado      EQ LOGICAL(R_Estado:SCREEN-VALUE IN FRAME F_Filtros) AND
           Mov_Instancias.Fec_Ingreso GE DATE(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) AND
           Mov_Instancias.Fec_Ingreso LE DATE(Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros) NO-LOCK
           BREAK BY Mov_Instancias.Agencia BY Mov_Instancias.Instancia BY Mov_Instancias.Usuario:
         IF FIRST-OF(Mov_Instancias.Usuario) THEN DO:
             FIND Usuarios WHERE Usuarios.Usuario EQ Mov_Instancias.Usuario NO-LOCK NO-ERROR.
             IF AVAILABLE Usuarios AND FIRST-OF(Mov_Instancias.Usuario) THEN DO:
                CREATE TUsu.
                ASSIGN TUsu.TAgeUsu = Usuarios.Agencia
                       TUsu.TASlUsu = Mov_Instancias.Agencia
                       TUsu.TCodUsu = Usuarios.Usuario
                       TUsu.TNomUsu = Usuarios.Nombre.
             END.
         END.
         i = i + 1.
         edw:SCREEN-VALUE IN FRAME F_Consulta = STRING(i).
         IF TTot.Ttipo NE 2 THEN DO:
            FIND FIRST Solicitud WHERE Solicitud.Num_Solicitud EQ Mov_Instancias.Num_Solicitud AND
                                       Solicitud.Nit           EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Solicitud THEN NEXT.
            FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.
            IF  INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_Consulta,1,5)) EQ 1 
                AND AVAIL (Solicitud) THEN
                RUN Inst_Solicitud.            
            ELSE 
                RUN Inst_Creditos.              
            
         END.
         ELSE DO:
              RUN Inst_Creditos.              
         END.
         IF AVAILABLE TSol THEN DO:
            IF TSol.TFcrSol EQ ? THEN ASSIGN TSol.TVigSol = TODAY - TSol.TFciSol.
            ELSE TSol.TVigSol = TSol.TFcrSol - TSol.TFciSol.

            /*GCamacho - Feb-08-2011 . implementa columna valor desembolsar*/
            DO viCnt = 1 TO 5:
                IF Solicitud.Num_CredACanc[viCnt] GT 0 THEN DO: 
                    ASSIGN viValDes = viValDes + getValTotCred(INPUT solicitud.nit, 
                                                               INPUT Solicitud.Num_CredACanc[viCnt]).
                END.
            END.
/*             ASSIGN viValDes = Solicitud.Monto - viValDes. */
        /*totales*/
            ASSIGN TUsu.TNroUsu = TUsu.TNroUsu + 1
                   TUsu.TValUsu = TUsu.TValUsu + Solicitud.Monto
                   TUsu.TDesUsu = TUsu.TValUsu - viValDes
/*                    TUsu.TDesUsu = TUsu.TValUsu - viValDes */
                   TTot.TNroIns = TTot.TNroIns + 1
                   TTot.TValIns = TTot.TValIns + Solicitud.Monto
                   TTot.TDesIns = TTot.TValIns - viValDes
                   TotUsu = TotUsu + 1.
         END.
 END.
 FOR EACH TUsu WHERE 
          TUsu.TAslUsu EQ TAge.TCodAge:
     TUsu.TPorUsu = ((TUsu.TNroUsu * 100) / TotUsu) * 10.
 END.            
 /*MESSAGE "FinalTotUsuarios" VIEW-AS ALERT-BOX.*/
 OPEN QUERY B_Usuarios   FOR EACH TUsu WHERE 
      TUsu.TASlUsu EQ TAge.TCodAge INDEXED-REPOSITION.
 FIND FIRST TUsu NO-ERROR.
 OPEN QUERY B_Solicitud  FOR EACH TSol WHERE 
     TSol.TAgeSol EQ TAge.TCodAge AND
     TSol.TUsuSol EQ TUsu.TCodUsu BY TSol.TVigSol DESCENDING INDEXED-REPOSITION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Totalizar_Usuario3 wWin 
PROCEDURE Totalizar_Usuario3 :
/* Este Procedure no se esta utilizando*/
  DEFINE VAR TotUsu AS INTEGER.
  DEFINE VAR W_FecW AS CHARACTER FORMAT "X(10)".
  DEFINE VAR i AS DECIMAL.
  FOR EACH TUsu: DELETE TUsu. END.
  FOR EACH TSol: DELETE TSol. END.
  FOR EACH TMI USE-INDEX Xu WHERE 
           TMI.Agencia     EQ TAge.TCodAge   AND
           TMI.Instancia   EQ TTot.TCodIns AND
           TMI.Fec_Ingreso GE DATE(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) AND
           TMI.Fec_Ingreso LE DATE(Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros) AND 
           TMI.Estado      EQ LOGICAL(R_Estado:SCREEN-VALUE IN FRAME F_Filtros)
           NO-LOCK
           BREAK BY TMI.Usuario:
         i = i + 1.
         edw:SCREEN-VALUE IN FRAME F_consulta = STRING(i).
         IF FIRST-OF(Tmi.Usuario) THEN DO:
             FIND Usuarios WHERE Usuarios.Usuario EQ Tmi.Usuario NO-LOCK NO-ERROR.
             IF AVAILABLE Usuarios AND FIRST-OF(Tmi.Usuario) THEN DO:
                CREATE TUsu.
                ASSIGN TUsu.TAgeUsu = Usuarios.Agencia
                       TUsu.TASlUsu = Tmi.Agencia
                       TUsu.TCodUsu = Usuarios.Usuario
                       TUsu.TNomUsu = Usuarios.Nombre.
             END.
         END.

         FIND Solicitud WHERE Solicitud.Num_Solicitud EQ Tmi.Num_Solicitud NO-LOCK NO-ERROR.
         FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Solicitud.Cod_Credito NO-LOCK NO-ERROR.

         /*
         IF  INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_Consulta,1,5)) NE 2            
         AND INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE,1,5)) NE 5 THEN 
         Oct.11/05 GAER se reemplazo x la sgte*/

         IF  INTEGER(SUBSTRING(Cmb_EncInstancias:SCREEN-VALUE IN FRAME F_Consulta,1,5)) EQ 1 THEN
             RUN Inst_Solicitud3.            
         ELSE 
             RUN Inst_Creditos3.         
         
         IF TSol.TFcrSol EQ ? THEN ASSIGN TSol.TVigSol = TODAY - TSol.TFciSol.
         ELSE TSol.TVigSol = TSol.TFcrSol - TSol.TFciSol.
         
         /*totales*/
         ASSIGN TUsu.TNroUsu = TUsu.TNroUsu + 1
                TUsu.TValUsu = TUsu.TValUsu + Solicitud.Monto
                TTot.TNroIns = TTot.TNroIns + 1
                TTot.TValIns = TTot.TValIns + Solicitud.Monto
                TotUsu = TotUsu + 1.
        /* IF tusu.tcodusu EQ "6" THEN
            MESSAGE Tusu.TcodUsu Tusu.TnroUsu.*/
                
  END.
 FOR EACH TUsu WHERE 
          TUsu.TAslUsu EQ TAge.TCodAge:
     TUsu.TPorUsu = ((TUsu.TNroUsu * 100) / TotUsu) * 10.
 END.            
 
 OPEN QUERY B_Usuarios   FOR EACH TUsu WHERE 
      TUsu.TASlUsu EQ TAge.TCodAge INDEXED-REPOSITION.
 FIND FIRST TUsu NO-ERROR.
 OPEN QUERY B_Solicitud  FOR EACH TSol WHERE 
     TSol.TAgeSol EQ TAge.TCodAge AND
     TSol.TUsuSol EQ TUsu.TCodUsu BY TSol.TVigSol DESCENDING INDEXED-REPOSITION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tot_Instancias wWin 
PROCEDURE Tot_Instancias :
DEFINE VAR TotSol AS INTEGER.
DEFINE VARIABLE viValDes AS INTEGER     NO-UNDO.

FOR EACH Mov_Instancias WHERE 
          Mov_Instancias.Instancia     EQ TTot.TCodIns   AND   
          Mov_Instancias.Estado        EQ LOGICAL(R_Estado:SCREEN-VALUE IN FRAME F_Filtros) AND
          Mov_Instancias.Fec_Ingreso   GE DATE(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) AND
          Mov_Instancias.Fec_Ingreso   LE DATE(Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros)
          NO-LOCK BREAK BY Mov_Instancias.Agencia:
      IF FIRST-OF(Mov_Instancias.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Mov_Instancias.Agencia NO-LOCK NO-ERROR.
         CREATE TAge.
         ASSIGN TAge.TCodAge = Mov_Instancias.Agencia
                TAge.TNomAge = Agencias.Nombre
                TAge.TInsAge = Mov_Instancias.Instancia.
      END.

      IF ttot.ttipo NE 2 THEN DO:
        FIND Solicitud WHERE 
             Solicitud.Nit           EQ Mov_Instancias.Nit AND
             Solicitud.Num_Solicitud EQ Mov_Instancias.Num_Solicitud NO-LOCK NO-ERROR.
        IF AVAIL(Solicitud) THEN DO:

            /*GCamacho - Feb-08-2011 . implementa columna valor desembolsar*/
            DO viCnt = 1 TO 5:
                IF Solicitud.Num_CredACanc[viCnt] GT 0 THEN DO: 
                    ASSIGN viValDes = viValDes + getValTotCred(INPUT solicitud.nit, 
                                                               INPUT Solicitud.Num_CredACanc[viCnt]).
                END.
            END.
/*             ASSIGN viValDes = viValDes. */

           ASSIGN   TAge.TNroAge = TAge.TNroAge + 1
                    TAge.TValAge = TAge.TValAge + Solicitud.Monto 
                    TAge.TDesAge = TAge.TValAge - viValDes
                    TTot.TNroIns = TTot.TNroIns + 1
                    TTot.TValIns = TTot.TValIns + Solicitud.Monto
                    TTot.TDesIns = TTot.TValIns - viValDes
                    TotSol       = TotSol + 1. 

           FIND Creditos WHERE 
                Creditos.Nit           EQ Mov_Instancias.Nit AND
                Creditos.Cod_Credito   EQ Solicitud.Cod_Credito AND
                Creditos.Num_Solicitud EQ Mov_Instancias.Num_Solicitud /*AND
           Creditos.Num_Credito   EQ INTEG(Mov_Instancias.Cuenta)*/   NO-LOCK NO-ERROR.

         ASSIGN TTot.TValSdo = TTot.TValSdo + Creditos.Monto WHEN AVAIL(Creditos).
        END.
      END.
      ELSE DO:
          FIND Creditos WHERE 
               creditos.agencia       EQ mov_instancias.agencia AND
               Creditos.Nit           EQ Mov_Instancias.Nit AND
               Creditos.Num_Credito   EQ INTEG(Mov_Instancias.Cuenta)   NO-LOCK NO-ERROR.

            /*GCamacho - Feb-08-2011 . implementa columna valor desembolsar*/
            DO viCnt = 1 TO 5:
                IF Solicitud.Num_CredACanc[viCnt] GT 0 THEN DO: 
                    ASSIGN viValDes = viValDes + getValTotCred(INPUT solicitud.nit, 
                                                               INPUT Solicitud.Num_CredACanc[viCnt]).
                END.
            END.
/*             ASSIGN viValDes = viValDes + viValDes. */

          IF AVAIL(Creditos) THEN DO:
            ASSIGN  TAge.TNroAge = TAge.TNroAge + 1
                    TAge.TValAge = TAge.TValAge + Solicitud.Monto 
                    TAge.TDesAge = TAge.TValAge - viValDes
                    TTot.TNroIns = TTot.TNroIns + 1
                    TTot.TValIns = TTot.TValIns + Solicitud.Monto
                    TTot.TDesIns = TTot.TValIns - viValDes
                    TotSol       = TotSol + 1. 

             ASSIGN TTot.TValSdo = TTot.TValSdo + Creditos.Monto WHEN AVAIL(Creditos).
          END.
      END.
 END.

 FOR EACH TAge WHERE TAge.TNroAge NE 0 AND
     TAge.TInsAge EQ TTot.TCodIns:
     TAge.TPorAge = ((TAge.TNroAge * 100) / TotSol) * 10.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VerFoto wWin 
PROCEDURE VerFoto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN RutaFoto = "imagenes\fotos\0.jpg".
P_Foto:LOAD-IMAGE(RutaFoto) IN FRAME F_Foto.
IF TRIM(TSol.TNitSol) NE "" THEN
DO:

   ASSIGN RutaFoto = "imagenes\fotos\" + TRIM(TSol.TNitSol) + ".jpg".
   P_Foto:LOAD-IMAGE(RutaFoto) IN FRAME F_Foto.

   /*  
   IF ERROR-STATUS:ERROR THEN
     MESSAGE "No Existe la foto del asociado" VIEW-AS ALERT-BOX.
  ELSE
  */
     VIEW FRAME F_Foto.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValTotCred wWin 
FUNCTION getValTotCred RETURNS INTEGER
  ( INPUT ipcNit        AS CHARACTER, 
    INPUT ipiNumCre     AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  Devuelve el valor total de la deuda del credito
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE viValDeuda AS INTEGER     NO-UNDO.

    FIND FIRST creditos WHERE creditos.nit EQ ipcNit AND creditos.num_credito EQ ipiNumCre NO-LOCK NO-ERROR.
    IF AVAILABLE creditos THEN DO:
        ASSIGN viValDeuda = Creditos.Sdo_Capital + Creditos.Int_Corrientes + 
                            Creditos.Int_DifCobro + Creditos.Int_MoraDifCob + 
                            Creditos.Int_MorCobrar + Creditos.Costas + Creditos.Polizas +
                            Creditos.Honorarios - Creditos.Int_Anticipado.
    END.
    RETURN viValDeuda.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

