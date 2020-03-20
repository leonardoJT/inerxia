&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

{incluido/VARIABLE.i "SHARED"}.

DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR W_Inf AS CHARACTER FORMAT "X(10)".
DEFINE VAR wEdad AS INTEGER.
DEFINE VAR gTexto AS CHARACTER FORMAT "X(50)".

/* oakley */

DEF VAR Wk_Edad AS INT FORMAT "999" NO-UNDO.
DEF VAR Tel_Numerico AS DEC FORMAT "99999999999999" NO-UNDO.
DEF VAR W_NUbicacion AS CHA FORMAT "X(50)" NO-UNDO.
DEF VAR P_Ubi LIKE Ubicacion.Ubicacion NO-UNDO.
DEF VAR P_NUbi AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEF VAR P_Nit LIKE Act_Pasivos.Nit NO-UNDO.
DEF VAR P_VVeh LIKE Clientes.Act_vehiculo NO-UNDO.
DEF VAR P_VBie LIKE Clientes.Act_casa NO-UNDO.
DEF VAR P_VOtros LIKE Clientes.Act_inversion NO-UNDO.
DEF VAR P_Pas AS DECIMAL NO-UNDO.
DEF VAR V_Nom LIKE Varios.Descripcion NO-UNDO.
DEF VAR V_Cod LIKE Varios.Codigo NO-UNDO.
DEF VAR P_cod LIKE Empresas.Cod_Empresa NO-UNDO.
DEF VAR P_AgeEmp AS INTEGER NO-UNDO.
DEF VAR P_Nom AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEF VAR cCdgoCiiu AS CHARACTER NO-UNDO.
DEF VAR P_Nombre AS CHARACTER NO-UNDO.
DEF VAR P_Apellido AS CHARACTER NO-UNDO.
DEF VAR P_AgeCli AS INTEGER NO-UNDO.
DEF VAR W_Sarlaft AS LOG INIT FALSE.

/* oakley */

DEF TEMP-TABLE T_Relaciones NO-UNDO
    FIELD R_Relacion LIKE Varios.Descripcion
    FIELD R_AgeObjeto LIKE Clientes.Agencia
    FIELD R_NitObjeto LIKE Clientes.Nit
    FIELD R_NomObjeto AS CHARACTER FORMAT "X(35)"
    FIELD R_TelObjeto AS CHARACTER FORMAT "X(30)"
    FIELD R_TelObjetoIndctvo AS INTEGER
    FIELD R_NomDescri AS CHARACTER FORMAT "X(15)"
    FIELD R_TelComerc AS CHARACTER FORMAT "X(30)"
    FIELD R_TelComercIndctvo AS INTEGER.

/* Tabla que identifica los campos necesarios que no se han llenado */
DEF TEMP-TABLE TFalta NO-UNDO
    FIELD TCampo AS CHARACTER FORMAT "X(55)"
    FIELD TDonde AS CHARACTER FORMAT "X(35)".

DEF BUFFER BClientes FOR Clientes.
DEF BUFFER CClientes FOR Clientes.

DEF VAR cDgtos AS CHAR NO-UNDO INITIAL "1,2,3,4,5,6,7,8,9,0".
DEF VAR cAlfbto AS CHAR NO-UNDO INIT " ,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,ñ".
DEF VAR cDrccion AS CHAR NO-UNDO.
DEF VAR hDrccion AS HANDLE NO-UNDO.
DEF VAR SWER1 AS LOG NO-UNDO.
DEF VAR SWER2 AS LOG NO-UNDO.
DEF VAR XDigi AS CHAR NO-UNDO EXTENT 10 INIT [1,2,3,4,5,6,7,8,9,0].
DEF VAR cOrden AS CHAR NO-UNDO.

DEF TEMP-TABLE t NO-UNDO
    FIELD tTtlo AS CHAR
    FIELD tFla AS INTEGER
    FIELD tClmna AS INTEGER
    FIELD th AS HANDLE
    FIELD tLbel AS CHAR
    FIELD tCmpo AS CHAR
    FIELD tfrmeHndle AS HANDLE
    FIELD tVlor AS CHAR
    FIELD tOrden AS CHAR
    INDEX pk tOrden tFla tClmna.

DEF TEMP-TABLE TClientes NO-UNDO LIKE Clientes.
DEF TEMP-TABLE TAnexos_Clientes NO-UNDO LIKE Anexos_Clientes.

DEF TEMP-TABLE vinculantes
    FIELD nro AS INTEGER
    FIELD texto AS CHAR
    INDEX idx1 IS WORD-INDEX texto.

DEF VAR i AS INTEGER.
DEF VAR lg AS LOGICAL.
DEF VAR GHPARENT AS HANDLE NO-UNDO.

/*Variables Para Imprimir en Excel*/
DEF VAR Dato AS CHA NO-UNDO.
DEF VAR ValCol AS CHA NO-UNDO.
DEF VAR SwExiste AS CHA NO-UNDO.
DEF VAR InputFile AS CHA NO-UNDO.
DEF VAR PrinterName AS CHA NO-UNDO.
DEF VAR chExcelApp AS COM-HANDLE NO-UNDO.
DEF VAR hWorkBooks AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet AS COM-HANDLE NO-UNDO.
DEFINE VAR gerror AS LOGICAL INIT FALSE.
DEFINE VAR codFacultad AS CHARACTER.
DEFINE VAR codAgencia AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Aportes
&Scoped-define BROWSE-NAME Br_Relaciones

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES T_Relaciones TFalta Anexos_Clientes Clientes ~
MicroEmpresas

/* Definitions for BROWSE Br_Relaciones                                 */
&Scoped-define FIELDS-IN-QUERY-Br_Relaciones T_Relaciones.R_Relacion T_Relaciones.R_AgeObjeto T_Relaciones.R_NitObjeto T_Relaciones.R_NomObjeto T_Relaciones.R_NomDescri t_relaciones.R_TelObjetoIndctvo T_Relaciones.R_TelObjeto T_Relaciones.R_TelComercIndctvo T_Relaciones.R_TelComerc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Relaciones   
&Scoped-define SELF-NAME Br_Relaciones
&Scoped-define QUERY-STRING-Br_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Relaciones OPEN QUERY {&SELF-NAME} FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Relaciones T_Relaciones
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Relaciones T_Relaciones


/* Definitions for BROWSE B_Falta                                       */
&Scoped-define FIELDS-IN-QUERY-B_Falta TFalta.TCampo TFalta.TDonde   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Falta   
&Scoped-define SELF-NAME B_Falta
&Scoped-define QUERY-STRING-B_Falta FOR EACH TFalta NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Falta OPEN QUERY {&SELF-NAME} FOR EACH TFalta NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Falta TFalta
&Scoped-define FIRST-TABLE-IN-QUERY-B_Falta TFalta


/* Definitions for FRAME F_Clientes                                     */
&Scoped-define QUERY-STRING-F_Clientes FOR EACH Anexos_Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Clientes OPEN QUERY F_Clientes FOR EACH Anexos_Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Clientes Anexos_Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-F_Clientes Anexos_Clientes


/* Definitions for FRAME F_Conyuge                                      */
&Scoped-define QUERY-STRING-F_Conyuge FOR EACH Clientes NO-LOCK, ~
      EACH Anexos_Clientes OF Clientes NO-LOCK
&Scoped-define OPEN-QUERY-F_Conyuge OPEN QUERY F_Conyuge FOR EACH Clientes NO-LOCK, ~
      EACH Anexos_Clientes OF Clientes NO-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Conyuge Clientes Anexos_Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-F_Conyuge Clientes
&Scoped-define SECOND-TABLE-IN-QUERY-F_Conyuge Anexos_Clientes


/* Definitions for FRAME F_Falta                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Falta ~
    ~{&OPEN-QUERY-B_Falta}

/* Definitions for FRAME F_Juridicas                                    */
&Scoped-define FIELDS-IN-QUERY-F_Juridicas Clientes.Celular ~
Clientes.Fec_expedicion Clientes.Dir_Residencia Clientes.Tel_Residencia ~
Clientes.Tel_Arrendatario Clientes.Email Clientes.Tipo_Empresa ~
Clientes.Salario Clientes.Ing_Otros Clientes.Sdo_Obligaciones ~
Clientes.Act_vehiculo Clientes.Id_Mon_Ext 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Juridicas Clientes.Celular ~
Clientes.Fec_expedicion Clientes.Tel_Residencia Clientes.Tel_Arrendatario ~
Clientes.Email Clientes.Tipo_Empresa Clientes.Salario Clientes.Ing_Otros ~
Clientes.Sdo_Obligaciones Clientes.Act_vehiculo Clientes.Id_Mon_Ext 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Juridicas Clientes
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Juridicas Clientes
&Scoped-define QUERY-STRING-F_Juridicas FOR EACH Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Juridicas OPEN QUERY F_Juridicas FOR EACH Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Juridicas Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-F_Juridicas Clientes


/* Definitions for FRAME F_Microempresas                                */
&Scoped-define FIELDS-IN-QUERY-F_Microempresas MicroEmpresas.NOMBRE ~
MicroEmpresas.DIRECCION MicroEmpresas.TELEFONO MicroEmpresas.ESTRATO ~
MicroEmpresas.cod_actividad MicroEmpresas.Codigo_CIIU ~
MicroEmpresas.UBICACION MicroEmpresas.HOMBRES MicroEmpresas.MUJERES ~
MicroEmpresas.NINOS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Microempresas MicroEmpresas.NOMBRE ~
MicroEmpresas.TELEFONO MicroEmpresas.ESTRATO MicroEmpresas.HOMBRES ~
MicroEmpresas.MUJERES MicroEmpresas.NINOS 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Microempresas MicroEmpresas
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Microempresas MicroEmpresas
&Scoped-define QUERY-STRING-F_Microempresas FOR EACH MicroEmpresas SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Microempresas OPEN QUERY F_Microempresas FOR EACH MicroEmpresas SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Microempresas MicroEmpresas
&Scoped-define FIRST-TABLE-IN-QUERY-F_Microempresas MicroEmpresas


/* Definitions for FRAME F_Relaciones                                   */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Bt_FinAporte 
&Scoped-Define DISPLAYED-OBJECTS Edit_msaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fchar wWin 
FUNCTION fchar RETURNS LOGICAL
  (c AS CHAR  /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDgto wWin 
FUNCTION fDgto RETURNS LOGICAL
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDptoCiudad wWin 
FUNCTION fDptoCiudad RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fImprmeDtos wWin 
FUNCTION fImprmeDtos RETURNS LOGICAL
  (h AS HANDLE /* el frame principal */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIncialza wWin 
FUNCTION fIncialza RETURNS LOGICAL
  (h AS HANDLE /* el frame principal */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPrfsion wWin 
FUNCTION fPrfsion RETURNS CHARACTER
  (iv AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fTxtoCmpo wWin 
FUNCTION fTxtoCmpo RETURNS CHARACTER
  (h AS HANDLE /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fVldaDtos wWin 
FUNCTION fVldaDtos RETURNS LOGICAL
  (h AS HANDLE /* el frame principal */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD vinculantes wWin 
FUNCTION vinculantes RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_wgnrdordrccion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_wsolprofina AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Lugar_Comercial-2 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Lugar_Comercial" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_SC 
     LABEL "SC" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-205 
     LABEL "Salvar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-206 
     LABEL "Ocultar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON R_Btn_Direccion-3 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "R_Btn_Direccion" 
     SIZE 3 BY .54.

DEFINE VARIABLE R_Relacion AS CHARACTER 
     LABEL "Relación O Parentesco" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 30
     LIST-ITEMS "Abuelo(a)","Bisabuelo(a)","Bisnieto(a)","Conyuge","Compañero(a)","Cuñado(a)","Hermano(a)","Hermano(a) del Abuelo(a)","Hijo(a)","Hijo(a) Adoptivo(a)","Hijastro(a)","Hijo(a) del Conyuge","Hijo(a) del Compañero(a)","Madre","Madre Adoptante","Madrastra","Nieto(a)","Nuera","Padre","Padre Adoptante","Padrastro","Primo(a)","Primo(a) Hermano(a)","Sobrino(a)","Suegro(a)","Tatarabuelo(a)","Tataranieto(a)","Tio(a)","Yerno" 
     DROP-DOWN MAX-CHARS 20
     SIZE 30 BY 1 TOOLTIP "Parentesco O Relación"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fechaNacimientoRelacion AS DATE FORMAT "99/99/9999" 
     LABEL "Fecha de nacimiento" 
     VIEW-AS FILL-IN 
     SIZE 15.14 BY .81 TOOLTIP "Fecha de nacimiento"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rela_Nit AS CHARACTER FORMAT "X(12)":U 
     LABEL "Número de identificación" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Apellido1 AS CHARACTER FORMAT "X(15)" 
     LABEL "Primer Apellido" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Apellido2 AS CHARACTER FORMAT "X(15)" 
     LABEL "Segundo Apellido" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Ciu_Dpt AS CHARACTER FORMAT "X(80)" 
     LABEL "Ciudad-Dpt" 
     VIEW-AS FILL-IN 
     SIZE 37.72 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Direccion AS CHARACTER FORMAT "X(255)" 
     LABEL "Dirección" 
     VIEW-AS FILL-IN 
     SIZE 37.72 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Nombre AS CHARACTER FORMAT "X(40)" 
     LABEL "Nombre-Establecimiento-Entidad" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Nombre2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre2" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE R_Numero AS CHARACTER FORMAT "X(20)" 
     LABEL "Numero" 
     VIEW-AS FILL-IN 
     SIZE 15.14 BY .81
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE R_Producto AS CHARACTER FORMAT "X(20)" 
     LABEL "Producto" 
     VIEW-AS FILL-IN 
     SIZE 16.72 BY .81
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE R_Sucursal AS CHARACTER FORMAT "X(20)" 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE R_Tel_Comercial AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 20.14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Tel_ComercialIndctvo AS INTEGER FORMAT "ZZZZ":U INITIAL 0 
     LABEL "Teléfono Comercial" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81 TOOLTIP "Indicativo Teléfono Comercial" NO-UNDO.

DEFINE VARIABLE R_Tel_Residencia AS CHARACTER FORMAT "X(12)" 
     VIEW-AS FILL-IN 
     SIZE 20.14 BY .81 TOOLTIP "Número"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Tel_ResidenciaIndctvo AS INTEGER FORMAT "ZZZZ":U INITIAL 0 
     LABEL "Teléfono de la Residencia" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81 TOOLTIP "Indicativo" NO-UNDO.

DEFINE RECTANGLE RECT-303
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 4.35.

DEFINE RECTANGLE RECT-331
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 4.31.

DEFINE RECTANGLE RECT-333
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 6.04.

DEFINE BUTTON Bt_FinAporte 
     LABEL "Fin Creación Aportes" 
     SIZE 22 BY 1.12.

DEFINE VARIABLE Edit_msaje AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 42.29 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-304
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 111 BY 3.62.

DEFINE RECTANGLE RECT-305
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 9.42.

DEFINE BUTTON Btn_Borrar  NO-FOCUS
     LABEL "Borrar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Cancelar  NO-FOCUS
     LABEL "Cancelar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U NO-FOCUS
     LABEL "Button 3" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Deshacer  NO-FOCUS
     LABEL "Limpiar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Ingresar  NO-FOCUS
     LABEL "Ingresar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Salir DEFAULT  NO-FOCUS
     LABEL "&Salir" 
     SIZE 11 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Salvar  NO-FOCUS
     LABEL "Grabar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BtonInfoCnyge 
     LABEL "Conyuge" 
     SIZE 11 BY 1.62 TOOLTIP "Ver Información Conyuge".

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U NO-FOCUS
     LABEL "Button 11" 
     SIZE 5 BY 1.08.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U NO-FOCUS
     LABEL "Button 2" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-207 
     LABEL "Button 207" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE Cmb_Agencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 40.86 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE Foto
     FILENAME "imagenes/fotos/0.jpg":U
     SIZE 18 BY 5.38.

DEFINE VARIABLE RSeleccion AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "General", 1,
"Económica", 2,
"Financiera", 3,
"Relaciones", 4,
"Autorizaciones", 5,
"Documentación", 7,
"Controles", 6
     SIZE 112 BY 1.08
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 115.86 BY 1.35
     BGCOLOR 18 .

DEFINE RECTANGLE RECT-216
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13 BY 5.38.

DEFINE RECTANGLE RECT-218
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13 BY 13.31.

DEFINE RECTANGLE RECT-317
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.14 BY 2.15.

DEFINE RECTANGLE RECT-318
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 10.29 BY 2.08.

DEFINE BUTTON Btn_Grabar_conyuge 
     LABEL "Grabar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_Ocultar_conyuge 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON R_Btn_Direccion 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "R_Btn_Direccion" 
     SIZE 3 BY 1.

DEFINE BUTTON R_Btn_Empresa 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Empresa" 
     SIZE 3 BY .92.

DEFINE BUTTON R_Btn_Nacimiento 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "R_Btn_nacimiento" 
     SIZE 3 BY .81.

DEFINE BUTTON R_Btn_Profesion 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "R_Btn_profesion" 
     SIZE 3 BY .92.

DEFINE VARIABLE R_Niv_Educativo AS CHARACTER FORMAT "X(14)" INITIAL "Ninguno" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "Ninguno","Primaria","Secundaria","Tecnologo","Tecnico","Profesional","PosGrado","Especializacion","Maestria","Doctorado" 
     DROP-DOWN-LIST
     SIZE 24.43 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Ocupacion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Empleado Público","Empleado Privado","Independiente","Comerciante","Pensionado","Político","Rentista de Capital","Ama de Casa","Estudiante","Otro" 
     DROP-DOWN-LIST
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Tipo_Identificacion AS CHARACTER FORMAT "X(4)" INITIAL "C.C" 
     LABEL "Tipo" 
     VIEW-AS COMBO-BOX 
     LIST-ITEMS "C.C","C.E","T.I","C.D","NIT","R.C","NUIP","PPTE" 
     DROP-DOWN-LIST
     SIZE 8.29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CnygeExtncion AS INTEGER FORMAT "999999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.72 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE R_Cargo_Actividad AS CHARACTER FORMAT "X(30)":U 
     LABEL "Cargo Actual/Actividad" 
     VIEW-AS FILL-IN 
     SIZE 59 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Cod_Profesion LIKE Clientes.Cod_Profesion
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .81
     BGCOLOR 17 FGCOLOR 17  NO-UNDO.

DEFINE VARIABLE R_Dir_Empresa LIKE Clientes.Dir_comercial
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .81
     BGCOLOR 17 FGCOLOR 17  NO-UNDO.

DEFINE VARIABLE R_Empresa AS CHARACTER FORMAT "X(40)":U 
     LABEL "Empresa Trabaja" 
     VIEW-AS FILL-IN 
     SIZE 59 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Especifique AS CHARACTER FORMAT "X(90)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .81
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE R_Fec_Nacimiento LIKE Clientes.Fec_Nacimiento
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Lugar_Nacimiento LIKE Clientes.Lugar_Nacimiento
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .81
     BGCOLOR 17 FGCOLOR 17  NO-UNDO.

DEFINE VARIABLE R_Nacionalidad AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 28.43 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_TEgresos AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Egresos $" 
     VIEW-AS FILL-IN 
     SIZE 25.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Tel_Empresa AS CHARACTER FORMAT "x(8)":U 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Tel_Empresa-Indctvo AS INTEGER FORMAT "zzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Tercero AS CHARACTER FORMAT "99999xxxxxx" 
     LABEL "No. Identificación" 
     VIEW-AS FILL-IN 
     SIZE 25.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_TIngreso AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Ingresos $" 
     VIEW-AS FILL-IN 
     SIZE 25.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WR_CiuEmpresa AS CHARACTER FORMAT "X(80)":U 
     VIEW-AS FILL-IN 
     SIZE 56.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WR_CiuNacimiento AS CHARACTER FORMAT "X(80)":U 
     VIEW-AS FILL-IN 
     SIZE 64 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WR_NomProfesion AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Sexo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Hombre", 1,
"Mujer", 2
     SIZE 16.57 BY .77
     FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-231
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 1.88.

DEFINE RECTANGLE RECT-232
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 1.88.

DEFINE RECTANGLE RECT-233
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 2.15.

DEFINE RECTANGLE RECT-234
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 113 BY 5.92.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20.57 BY 1.12.

DEFINE VARIABLE Cmb_Contrato AS CHARACTER FORMAT "X(15)":U 
     LABEL "CONTRATO" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "Asalariado","Independiente" 
     DROP-DOWN-LIST
     SIZE 20.29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Linea AS CHARACTER FORMAT "X(256)":U 
     LABEL "LINEA" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "AFILIACION","CTA DE AHORROS","A TÉRMINO","CRÉDITO" 
     DROP-DOWN-LIST
     SIZE 26 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_Grabar_Act_Pas 
     LABEL "Grabar" 
     SIZE 19 BY 1.08
     FONT 5.

DEFINE VARIABLE Tot_Activos AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Total Activos" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Tot_Egresos AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Total Egresos" 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Tot_Ingresos AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Total Ingresos" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-213
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 7.04.

DEFINE RECTANGLE RECT-214
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 6.62.

DEFINE RECTANGLE RECT-219
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 6.19.

DEFINE RECTANGLE RECT-237
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 7.5.

DEFINE BUTTON BUTTON-179 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 179" 
     SIZE 9 BY 1.62.

DEFINE BUTTON Btn_Act_Ppal 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Act_Ppal" 
     SIZE 3 BY .81.

DEFINE BUTTON Btn_DirEmpresa 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_DirEmpresa" 
     SIZE 3 BY .81.

DEFINE BUTTON Btn_UbiEmpresa 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_UbiEmpresa" 
     SIZE 3 BY .85.

DEFINE VARIABLE W_Nom_Act_Ppal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Act. Economica Ppal" 
     VIEW-AS FILL-IN 
     SIZE 27.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Nom_Ciu_Emp AS CHARACTER FORMAT "X(256)":U 
     LABEL "CIIU" 
     VIEW-AS FILL-IN 
     SIZE 28 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Ubi_Empresa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dpto-Ciu-Barrio" 
     VIEW-AS FILL-IN 
     SIZE 66 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-327
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91.57 BY 1.62.

DEFINE RECTANGLE RECT-328
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42.72 BY 1.5.

DEFINE RECTANGLE RECT-329
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 1.5.

DEFINE RECTANGLE RECT-330
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90.43 BY 2.15.

DEFINE BUTTON Btn_Actividad_Micro 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Actividad" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Direccion-2 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Direccion" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Ubi_Micro 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Ubi_Micro" 
     SIZE 3 BY .54.

DEFINE BUTTON Grabar_MicroEmpresa 
     LABEL "Salvar" 
     SIZE 13 BY 1.5
     FONT 5.

DEFINE BUTTON Salir_MicroEmpresa 
     LABEL "Salir" 
     SIZE 13 BY 1.5
     FONT 5.

DEFINE VARIABLE Nom_Actividad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Actividad" 
     VIEW-AS FILL-IN 
     SIZE 55.14 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Nom_Ciiu AS CHARACTER FORMAT "X(80)":U 
     LABEL "Ciiu" 
     VIEW-AS FILL-IN 
     SIZE 55.14 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Nom_Ubicacion AS CHARACTER FORMAT "X(80)":U 
     LABEL "Ubicacion" 
     VIEW-AS FILL-IN 
     SIZE 55.29 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-311
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY 3.23.

DEFINE RECTANGLE RECT-312
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69.72 BY 3.77.

DEFINE RECTANGLE RECT-313
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.96.

DEFINE BUTTON Btn_Codseg 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_coding 2" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-180 
     LABEL "Fecha fallecimiento" 
     SIZE 18 BY 1.12
     FONT 4.

DEFINE VARIABLE W_NomRetiro AS CHARACTER FORMAT "X(30)":U 
     LABEL "Causal de Retiro" 
     VIEW-AS FILL-IN 
     SIZE 39 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomSegmento AS CHARACTER FORMAT "X(30)":U 
     LABEL "Segmentación Interna" 
     VIEW-AS FILL-IN 
     SIZE 39 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomUsuario AS CHARACTER FORMAT "X(60)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 39 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-221
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 1.31.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 1.35.

DEFINE RECTANGLE RECT-320
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 7.

DEFINE RECTANGLE RECT-321
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 6.46.

DEFINE RECTANGLE RECT-323
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59.14 BY 3.23.

DEFINE RECTANGLE RECT-324
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 5.92.

DEFINE RECTANGLE RECT-325
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 2.69.

DEFINE RECTANGLE RECT-326
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 2.15.

DEFINE BUTTON Btn_Activas 
     LABEL "Borrar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_CanRel 
     LABEL "Cancelar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_CreRel 
     LABEL "Crear" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_SalRel 
     LABEL "Grabar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-116 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 116" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE Cmb_Relaciones AS CHARACTER FORMAT "X(50)":U 
     LABEL "Consulta Relaciones" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W_MenRel AS CHARACTER FORMAT "X(256)":U INITIAL "Escoja del Combo de Relaciones, el tipo de relación que quiere consultar" 
     VIEW-AS FILL-IN 
     SIZE 91 BY .81
     FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE VARIABLE RActivas AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activas", 1,
"Borradas", 2
     SIZE 23.43 BY 1 NO-UNDO.

DEFINE BUTTON Btn_Colegio 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Colegio" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Direccion 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Direccion" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Direccion_Ante 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "R_Btn_Direccion" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Direccion_Arendatario 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "R_Btn_Direccion" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Documento 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 109" 
     SIZE 3 BY .77.

DEFINE BUTTON Btn_Nacionalidad 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Nacionalidad" 
     SIZE 3 BY .81.

DEFINE BUTTON Btn_Profesion 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 13" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Residencia 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 103" 
     SIZE 3 BY .54.

DEFINE VARIABLE Nom_Colegio AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36.43 BY .77
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CiuExpedicion AS CHARACTER FORMAT "X(80)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY .81
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_NomProfesion AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 36.43 BY .77
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_UbicacionResidencia AS CHARACTER FORMAT "X(70)":U 
     LABEL "Dpto-Ciu-Barrio" 
     VIEW-AS FILL-IN 
     SIZE 55.57 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 5.04.

DEFINE RECTANGLE RECT-228
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 2.96.

DEFINE RECTANGLE RECT-306
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 3.

DEFINE RECTANGLE RECT-309
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81.72 BY 2.96.

DEFINE RECTANGLE RECT-334
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 4.04.

DEFINE BUTTON btnFacultadDpto 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_empresa 2" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Cargo 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 12" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Ciiu 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 99" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Ciiu-2 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Ciiu 2" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Ciiu-3 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Ciiu 3" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_DireccionComercial 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "R_Btn_Direccion" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Empresa 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 98" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Lugar_Comercial 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Btn_Lugar_Comercial" 
     SIZE 3 BY .54.

DEFINE VARIABLE Cmb_Est_Cargo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estado Cargo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Libre nombramiento y remocion","Carrera administrativa","Provisionalidad abierta","Provisionalidad cerrada","Supernumerario","Temporal","Docente regular","Docente ocasional tiempo completo","Docente ocasional medio tiempo","Docente de catedra","Docente ad honorem" 
     DROP-DOWN-LIST
     SIZE 40 BY 1
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Cmb_Situacion_Actual AS CHARACTER FORMAT "X(30)":U INITIAL "01 - Diario" 
     LABEL "Situacion Actual" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "0 - Normal","1 - Concordato","2 - Liquidacion Forzosa","3 - Liquidacion Voluntaria","4 - Proceso de Reorganizacion","5 - Ley 550","6 - Ley 1116","7 - Otra" 
     DROP-DOWN-LIST
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_TipAct AS CHARACTER FORMAT "X(35)":U 
     LABEL "Ocupación Principal" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "Cesante","Empleado Público","Empleado Privado","Estudiante","Hogar","Independiente o Empleado Socio","Pensionado","Otro" 
     DROP-DOWN-LIST
     SIZE 29.86 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FacultadDepartamento AS CHARACTER FORMAT "X(50)":U 
     LABEL "Facultad/Depto" 
     VIEW-AS FILL-IN 
     SIZE 74 BY .81
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE W_NomCargo AS CHARACTER FORMAT "X(50)":U 
     LABEL "Cargo" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .81
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE W_NomCiiu AS CHARACTER FORMAT "X(80)":U 
     LABEL "Ciiu" 
     VIEW-AS FILL-IN 
     SIZE 39 BY .81
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE W_NomCiiu-2 AS CHARACTER FORMAT "X(80)":U 
     LABEL "Actividad Económica" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .81
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE W_NomCiiu-3 AS CHARACTER FORMAT "X(80)":U 
     LABEL "Actividad Permanente" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .81
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE W_NomEmpresa AS CHARACTER FORMAT "X(50)":U 
     LABEL "Empresa~\Entidad" 
     VIEW-AS FILL-IN 
     SIZE 74 BY .81
     BGCOLOR 18  NO-UNDO.

DEFINE VARIABLE W_UbicacionComercial AS CHARACTER FORMAT "X(80)":U 
     LABEL "Dpto-Ciu" 
     VIEW-AS FILL-IN 
     SIZE 74 BY .81
     BGCOLOR 18  NO-UNDO.

DEFINE RECTANGLE RECT-217
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 2.54.

DEFINE RECTANGLE RECT-235
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 3.23.

DEFINE RECTANGLE RECT-236
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 2.15.

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 4.58.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Relaciones FOR 
      T_Relaciones SCROLLING.

DEFINE QUERY B_Falta FOR 
      TFalta SCROLLING.

DEFINE QUERY F_Clientes FOR 
      Anexos_Clientes SCROLLING.

DEFINE QUERY F_Conyuge FOR 
      Clientes, 
      Anexos_Clientes SCROLLING.

DEFINE QUERY F_Juridicas FOR 
      Clientes SCROLLING.

DEFINE QUERY F_Microempresas FOR 
      MicroEmpresas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Relaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Relaciones wWin _FREEFORM
  QUERY Br_Relaciones NO-LOCK DISPLAY
      T_Relaciones.R_Relacion           FORMAT "X(18)" LABEL "Relacion"
      T_Relaciones.R_AgeObjeto          FORMAT "999"   LABEL "Age"
      T_Relaciones.R_NitObjeto          FORMAT "X(14)" LABEL "Nit"
      T_Relaciones.R_NomObjeto          FORMAT "X(35)" LABEL "Nombre"
      T_Relaciones.R_NomDescri          FORMAT "X(15)" LABEL "Descripción"
      t_relaciones.R_TelObjetoIndctvo   FORMAT "zz99"  LABEL "Indicativo"
      T_Relaciones.R_TelObjeto          FORMAT "X(15)" LABEL "Tel.Residencia"
      T_Relaciones.R_TelComercIndctvo   FORMAT "zz99" LABEL "Indicativo"
      T_Relaciones.R_TelComerc          FORMAT "X(15)" LABEL "Tel.Comercial"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 84 BY 8.62
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.

DEFINE BROWSE B_Falta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Falta wWin _FREEFORM
  QUERY B_Falta NO-LOCK DISPLAY
      TFalta.TCampo COLUMN-LABEL "Campo"
  TFalta.TDonde COLUMN-LABEL "Donde encontrar para llenar"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 111 BY 11.85
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Aportes
     Edit_msaje AT ROW 1.46 COL 2.57 NO-LABEL
     Bt_FinAporte AT ROW 5.62 COL 12.29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 7.85
         SIZE 45.14 BY 6.69
         BGCOLOR 17 
         TITLE "Creación de Aportes".

DEFINE FRAME F_Clientes
     Clientes.Nombre AT ROW 4.62 COL 27.86 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 67 BY .81
          BGCOLOR 15 
     Btn_Salvar AT ROW 10.15 COL 119.14
     Btn_Consulta AT ROW 4.77 COL 119.14
     Btn_Borrar AT ROW 15 COL 119.14
     Btn_Cancelar AT ROW 16.62 COL 119.14
     Btn_Deshacer AT ROW 11.77 COL 119.14
     Btn_Ingresar AT ROW 13.38 COL 119.14
     BUTTON-207 AT ROW 6.92 COL 119.14 WIDGET-ID 2
     Btn_Salir AT ROW 18.23 COL 119.14
     BUTTON-11 AT ROW 20.38 COL 122.14
     Cmb_Agencia AT ROW 1.08 COL 28.14 COLON-ALIGNED
     BUTTON-1 AT ROW 1.54 COL 119.14
     Clientes.Estado AT ROW 1.88 COL 87.57 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Inactivo", 2
          SIZE 9.43 BY 1.35
          FONT 4
     BUTTON-2 AT ROW 3.15 COL 119.14
     Clientes.Nit AT ROW 1.96 COL 39.57 COLON-ALIGNED
          LABEL "Nº"
          VIEW-AS FILL-IN 
          SIZE 26 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 5
     Clientes.Tipo_Identificacion AT ROW 1.92 COL 28.14 COLON-ALIGNED
          LABEL "Tipo DocId" FORMAT "X(4)"
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "C.C","C.E","T.I","NIT","R.C","NUIP","PPTE" 
          DROP-DOWN-LIST
          SIZE 8.29 BY 1
          BGCOLOR 15 
     Clientes.Apellido1 AT ROW 2.92 COL 28 COLON-ALIGNED
          LABEL "Apellido 1"
          VIEW-AS FILL-IN 
          SIZE 41 BY .81
          BGCOLOR 15 FONT 5
     Clientes.Apellido2 AT ROW 3.73 COL 28 COLON-ALIGNED
          LABEL "Apellido 2"
          VIEW-AS FILL-IN 
          SIZE 41 BY .81
          BGCOLOR 15 FONT 5
     Clientes.Tipo_Cliente AT ROW 1.85 COL 72.86 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Natural", 1,
"Jurídica", 2
          SIZE 10.14 BY 1.31
          FONT 4
     RSeleccion AT ROW 7.08 COL 4 NO-LABEL
     BtonInfoCnyge AT ROW 8.54 COL 119.14 WIDGET-ID 4
     Clientes.dv AT ROW 1.96 COL 66.14 COLON-ALIGNED NO-LABEL WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 2.72 BY .81
          BGCOLOR 3 
     "Estado" VIEW-AS TEXT
          SIZE 5.57 BY .54 AT ROW 1.12 COL 88.57
          FGCOLOR 7 FONT 4
     "Tipo de persona" VIEW-AS TEXT
          SIZE 12.14 BY .54 AT ROW 1.12 COL 72.72 WIDGET-ID 30
          FGCOLOR 7 FONT 4
     Foto AT ROW 1.19 COL 2.43
     RECT-2 AT ROW 6.92 COL 1.14
     RECT-216 AT ROW 1.27 COL 118.14
     RECT-317 AT ROW 1.27 COL 71.86 WIDGET-ID 24
     RECT-318 AT ROW 1.31 COL 87 WIDGET-ID 26
     RECT-218 AT ROW 6.81 COL 118.14 WIDGET-ID 28
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 130.72 BY 24.31
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON Btn_Salvar.

DEFINE FRAME F_Relaciones
     RActivas AT ROW 1.12 COL 67.57 NO-LABEL
     Cmb_Relaciones AT ROW 1.27 COL 19.57 COLON-ALIGNED
     Btn_CreRel AT ROW 1.27 COL 104.14
     Btn_CanRel AT ROW 2.92 COL 104.14
     Btn_Activas AT ROW 4.58 COL 104.14
     Br_Relaciones AT ROW 4.77 COL 3
     Btn_SalRel AT ROW 6.23 COL 104.14
     BUTTON-116 AT ROW 7.88 COL 104.29
     W_MenRel AT ROW 13.65 COL 1 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.23
         SIZE 115.57 BY 17
         BGCOLOR 17 FONT 4
         TITLE "REFERENCIAS o RELACIONES".

DEFINE FRAME FRelNva
     Btn_SC AT ROW 1.12 COL 67.14
     R_Relacion AT ROW 1.19 COL 17.43 COLON-ALIGNED WIDGET-ID 294
     Rela_Nit AT ROW 2.38 COL 24 COLON-ALIGNED WIDGET-ID 308
     BUTTON-205 AT ROW 2.77 COL 67
     R_Nombre AT ROW 3.27 COL 24 COLON-ALIGNED HELP
          "Nombre del cliente"
     R_Nombre2 AT ROW 4.23 COL 24 COLON-ALIGNED WIDGET-ID 316
     BUTTON-206 AT ROW 4.42 COL 67
     R_Apellido1 AT ROW 5.19 COL 24 COLON-ALIGNED HELP
          "Primer apellido del cliente"
     R_Apellido2 AT ROW 6.12 COL 24 COLON-ALIGNED HELP
          "Segundo apellido del cliente"
     fechaNacimientoRelacion AT ROW 7.04 COL 23.86 COLON-ALIGNED HELP
          "Teléfono de la residencia del cliente" WIDGET-ID 318
     R_Btn_Direccion-3 AT ROW 8.92 COL 48.43 WIDGET-ID 290
     R_Direccion AT ROW 9 COL 9 COLON-ALIGNED HELP
          "Teléfono de la residencia del cliente"
     R_Sucursal AT ROW 9.81 COL 58.72 COLON-ALIGNED HELP
          "Teléfono de la residencia del cliente" WIDGET-ID 2
     R_Ciu_Dpt AT ROW 9.96 COL 8.86 COLON-ALIGNED HELP
          "Teléfono de la residencia del cliente"
     Btn_Lugar_Comercial-2 AT ROW 9.96 COL 48.43 WIDGET-ID 292
     R_Tel_ResidenciaIndctvo AT ROW 10.88 COL 19.86 COLON-ALIGNED WIDGET-ID 298
     R_Tel_Residencia AT ROW 10.88 COL 28.86 COLON-ALIGNED HELP
          "Teléfono de la residencia del cliente" NO-LABEL
     R_Producto AT ROW 10.88 COL 58.72 COLON-ALIGNED HELP
          "Teléfono de la residencia del cliente" WIDGET-ID 4
     R_Tel_ComercialIndctvo AT ROW 11.73 COL 19.86 COLON-ALIGNED WIDGET-ID 302
     R_Tel_Comercial AT ROW 11.77 COL 28.86 COLON-ALIGNED NO-LABEL
     R_Numero AT ROW 12.04 COL 58.86 COLON-ALIGNED HELP
          "Teléfono de la residencia del cliente"
     "Financiera" VIEW-AS TEXT
          SIZE 9.29 BY .81 AT ROW 8.42 COL 62 WIDGET-ID 166
          FGCOLOR 7 FONT 5
     RECT-303 AT ROW 8.73 COL 53.14 WIDGET-ID 8
     RECT-331 AT ROW 8.73 COL 2 WIDGET-ID 310
     RECT-333 AT ROW 2.23 COL 2 WIDGET-ID 314
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 6 ROW 3.15
         SIZE 78 BY 13.19
         BGCOLOR 17 FONT 4
         TITLE "Nueva Relacion".

DEFINE FRAME F_Segmentacion
     Clientes.Est_Civil AT ROW 1.35 COL 98.29 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 6
          LIST-ITEMS "Soltero","Casado","Viudo","Divorciado","Separado","Union Libre" 
          DROP-DOWN-LIST
          SIZE 14.29 BY 1
          BGCOLOR 15 FONT 4
     Clientes.Sexo AT ROW 1.42 COL 64.43 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Hombre", 1,
"Mujer", 2
          SIZE 11.14 BY 1.54
          FONT 5
     Clientes.Tipo_Vinculo AT ROW 1.96 COL 2.72 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Asociado", 1,
"Cliente No Asociado", 2,
"Tercero", 3,
"Proveedor", 4,
"Empleado", 5
          SIZE 16.57 BY 3.35
          FONT 15
     W_CiuExpedicion AT ROW 2.12 COL 21.14 NO-LABEL
     Btn_Documento AT ROW 2.12 COL 45.14 WIDGET-ID 206
     Clientes.Fec_expedicion AT ROW 2.12 COL 47.57 COLON-ALIGNED NO-LABEL FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     Clientes.Fec_Nacimiento AT ROW 3.27 COL 47.57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     Clientes.Num_Hijos AT ROW 3.88 COL 67.43 COLON-ALIGNED WIDGET-ID 306
          LABEL "1-10"
          VIEW-AS COMBO-BOX INNER-LINES 11
          LIST-ITEMS "0","1","2","3","4","5","6","7","8","9","10" 
          DROP-DOWN-LIST
          SIZE 5.57 BY 1
          BGCOLOR 15 
     Anexos_Clientes.Num_Hijos_11 AT ROW 3.88 COL 79.72 COLON-ALIGNED WIDGET-ID 308
          LABEL "11-18"
          VIEW-AS COMBO-BOX INNER-LINES 11
          LIST-ITEMS "0","1","2","3","4","5","6","7","8","9","10" 
          DROP-DOWN-LIST
          SIZE 5.57 BY 1
          BGCOLOR 15 
     Anexos_Clientes.Num_Hijos_18 AT ROW 3.88 COL 90.29 COLON-ALIGNED WIDGET-ID 310
          LABEL ">18"
          VIEW-AS COMBO-BOX INNER-LINES 11
          LIST-ITEMS "0","1","2","3","4","5","6","7","8","9","10" 
          DROP-DOWN-LIST
          SIZE 5.57 BY 1
          BGCOLOR 15 
     Clientes.Per_Acargo AT ROW 3.88 COL 102.86 COLON-ALIGNED NO-LABEL WIDGET-ID 314
          VIEW-AS COMBO-BOX INNER-LINES 16
          LIST-ITEMS "0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15" 
          DROP-DOWN-LIST
          SIZE 5.57 BY 1
          BGCOLOR 15 
     Btn_Nacionalidad AT ROW 5.08 COL 97 WIDGET-ID 324
     Anexos_Clientes.Nacionalidad AT ROW 5.15 COL 75 COLON-ALIGNED WIDGET-ID 102
          VIEW-AS FILL-IN 
          SIZE 20.14 BY .69 TOOLTIP "nacionalidad"
          BGCOLOR 15 
     Clientes.Niv_Educativo AT ROW 7.81 COL 1.86 NO-LABEL FORMAT "X(20)"
          VIEW-AS COMBO-BOX INNER-LINES 10
          LIST-ITEMS "Ninguno","Primaria","Secundaria","Tecnologo","Tecnico","Profesional","PosGrado","Especializacion","Maestria","Doctorado" 
          DROP-DOWN-LIST
          SIZE 24 BY 1
          BGCOLOR 15 
     Clientes.Niv_EduCop AT ROW 7.81 COL 27 NO-LABEL WIDGET-ID 358
          VIEW-AS COMBO-BOX INNER-LINES 6
          LIST-ITEMS "Ninguno","Basico","Medio","Superior","Especializado","Otro" 
          DROP-DOWN-LIST
          SIZE 24 BY 1
          BGCOLOR 15 
     Clientes.Num_ActaCop AT ROW 7.81 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 360
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.27
         SIZE 115.57 BY 16.96
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Segmentacion
     Clientes.Tipo_Vivienda AT ROW 8 COL 83.72 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Propia", 1,
"Familiar", 3,
"Asignada Emp", 4,
"Arrendada", 2
          SIZE 16 BY 2.38
     Clientes.Jornada AT ROW 8.73 COL 67.72 COLON-ALIGNED WIDGET-ID 332
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "N/A","Mañana","Tarde","Noche","Sabado" 
          DROP-DOWN-LIST
          SIZE 12.14 BY 1
          BGCOLOR 15 
     Nom_Colegio AT ROW 8.77 COL 9.72 COLON-ALIGNED NO-LABEL WIDGET-ID 340
     Clientes.Grado AT ROW 8.81 COL 56.14 COLON-ALIGNED WIDGET-ID 330
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .81
          BGCOLOR 15 
     Btn_Colegio AT ROW 8.88 COL 48.14 WIDGET-ID 344
     W_NomProfesion AT ROW 9.62 COL 9.72 COLON-ALIGNED NO-LABEL
     Btn_Profesion AT ROW 9.69 COL 48.29
     Clientes.Lugar_Nacimiento AT ROW 9.85 COL 67.57 COLON-ALIGNED NO-LABEL WIDGET-ID 216
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Lugar_expedicion AT ROW 9.85 COL 70.14 COLON-ALIGNED NO-LABEL WIDGET-ID 244
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Colegio AT ROW 9.85 COL 72.57 COLON-ALIGNED NO-LABEL WIDGET-ID 328
          VIEW-AS FILL-IN 
          SIZE 2.14 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Cod_Profesion AT ROW 9.85 COL 75.29 COLON-ALIGNED NO-LABEL WIDGET-ID 242
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Lugar_Residencia AT ROW 9.85 COL 77.86 COLON-ALIGNED NO-LABEL WIDGET-ID 240
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Dir_Residencia AT ROW 10.58 COL 13.43 COLON-ALIGNED WIDGET-ID 230
          LABEL "Direc Residenc" FORMAT "X(90)"
          VIEW-AS FILL-IN 
          SIZE 55.57 BY .81
          BGCOLOR 15 
     Clientes.Estrato AT ROW 10.62 COL 80.57 COLON-ALIGNED WIDGET-ID 218
          LABEL "Estrato"
          VIEW-AS COMBO-BOX INNER-LINES 8
          LIST-ITEMS "0","1","2","3","4","5","6","7" 
          DROP-DOWN-LIST
          SIZE 5.43 BY 1
          BGCOLOR 15 
     Btn_Direccion AT ROW 10.77 COL 71.14 WIDGET-ID 290
     W_UbicacionResidencia AT ROW 11.46 COL 13.43 COLON-ALIGNED
     Anexos_Clientes.Ind_cli AT ROW 11.58 COL 80.72 COLON-ALIGNED WIDGET-ID 320
          LABEL "Teléfono" FORMAT "zzz9"
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .69 TOOLTIP "Indicativo"
     Clientes.Tel_Residencia AT ROW 11.58 COL 86 COLON-ALIGNED NO-LABEL WIDGET-ID 234 FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 11.86 BY .69 TOOLTIP "Teléfono"
          BGCOLOR 15 
     Btn_Residencia AT ROW 11.65 COL 71 WIDGET-ID 238
     Clientes.Celular AT ROW 12.31 COL 80.72 COLON-ALIGNED WIDGET-ID 228 FORMAT "xxx-xxxxxxx"
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .69 TOOLTIP "Celular"
          BGCOLOR 15 
     Clientes.Email AT ROW 12.35 COL 13.43 COLON-ALIGNED WIDGET-ID 232
          LABEL "Correo Electrón"
          VIEW-AS FILL-IN 
          SIZE 55.57 BY .81
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.27
         SIZE 115.57 BY 16.96
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Segmentacion
     Anexos_Clientes.Val_arriendo AT ROW 13.23 COL 13.57 COLON-ALIGNED WIDGET-ID 174 FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 19.43 BY .81 TOOLTIP "Valor Del Arriendo"
          BGCOLOR 18 FONT 4
     Clientes.Nom_Arrendatario AT ROW 13.27 COL 44.29 COLON-ALIGNED WIDGET-ID 220
          LABEL "Arrendador" FORMAT "X(70)"
          VIEW-AS FILL-IN 
          SIZE 34.72 BY .81
          BGCOLOR 18 
     Anexos_Clientes.Dir_Correspondencia AT ROW 14.46 COL 83.29 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Residencia", 1,
"Oficina", 2,
"A.A.", 3
          SIZE 13 BY 2.42
          FONT 5
     Anexos_Clientes.Per_Anos AT ROW 14.65 COL 6 COLON-ALIGNED WIDGET-ID 176
          LABEL "Años"
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 15 FONT 4
     Anexos_Clientes.Per_Meses AT ROW 14.65 COL 17 COLON-ALIGNED WIDGET-ID 178
          LABEL "Meses"
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 15 FONT 4
     Anexos_Clientes.Dir_Arrendatario AT ROW 14.65 COL 31 COLON-ALIGNED WIDGET-ID 154
          LABEL "Dirección" FORMAT "X(90)"
          VIEW-AS FILL-IN 
          SIZE 28 BY .81
          BGCOLOR 18 FONT 4
     Anexos_Clientes.Ind_Arr AT ROW 14.73 COL 62.57 COLON-ALIGNED NO-LABEL WIDGET-ID 322 FORMAT "zzz9"
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .81 TOOLTIP "Indicativo"
          BGCOLOR 18 
     Clientes.Tel_Arrendatario AT ROW 14.73 COL 68 COLON-ALIGNED NO-LABEL WIDGET-ID 222 FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 
     Btn_Direccion_Arendatario AT ROW 14.81 COL 61.29 WIDGET-ID 296
     Anexos_Clientes.AA_Cliente AT ROW 15.92 COL 87.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 
     Anexos_Clientes.Per_Ant_Anos AT ROW 16.15 COL 66.29 COLON-ALIGNED WIDGET-ID 198
          LABEL "Años"
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 18 FONT 4
     Anexos_Clientes.Per_Ant_Meses AT ROW 16.15 COL 76.43 COLON-ALIGNED WIDGET-ID 194
          LABEL "Meses"
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 18 FONT 4
     Anexos_Clientes.Dir_Ant_Residencia AT ROW 16.19 COL 9.72 COLON-ALIGNED
          LABEL "Dirección" FORMAT "X(90)"
          VIEW-AS FILL-IN 
          SIZE 36.29 BY .81
          BGCOLOR 18 
     Anexos_Clientes.Tel_Ant_Residencia AT ROW 16.19 COL 49.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 
     Btn_Direccion_Ante AT ROW 16.38 COL 48 WIDGET-ID 298
     "Teléfono" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 14.19 COL 65.57
          FGCOLOR 7 FONT 4
     "Envío" VIEW-AS TEXT
          SIZE 6.57 BY .77 AT ROW 13.19 COL 84 WIDGET-ID 190
          FGCOLOR 7 FONT 5
     "DD/MM/AAAA" VIEW-AS TEXT
          SIZE 11 BY .69 AT ROW 1.46 COL 49.57 WIDGET-ID 144
          FONT 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.27
         SIZE 115.57 BY 16.96
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Segmentacion
     "Estado Civil" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.54 COL 89.29 WIDGET-ID 354
     "Nivel Cooperativo" VIEW-AS TEXT
          SIZE 16 BY .5 AT ROW 7.27 COL 27.43 WIDGET-ID 362
     "Teléfono" VIEW-AS TEXT
          SIZE 6.29 BY .5 AT ROW 15.69 COL 51.72 WIDGET-ID 246
          FGCOLOR 7 FONT 4
     "Correspondencia" VIEW-AS TEXT
          SIZE 15.57 BY .69 AT ROW 13.81 COL 84 WIDGET-ID 186
          FGCOLOR 7 FONT 5
     "Nro. de Acta" VIEW-AS TEXT
          SIZE 11 BY .5 AT ROW 8 COL 52 WIDGET-ID 364
     "             Número de Hijos por Edades          Personas a cargo" VIEW-AS TEXT
          SIZE 50.57 BY .62 AT ROW 3.23 COL 64.29 WIDGET-ID 100
          BGCOLOR 18 FGCOLOR 7 FONT 5
     "Colegio:" VIEW-AS TEXT
          SIZE 7.72 BY .62 AT ROW 8.88 COL 4.14 WIDGET-ID 338
     "Fecha Nacimiento (DD/MM/AAAA)" VIEW-AS TEXT
          SIZE 24.72 BY .69 AT ROW 3.42 COL 21.57 WIDGET-ID 106
          FONT 4
     "Permanencia" VIEW-AS TEXT
          SIZE 12.14 BY .81 AT ROW 14 COL 2.72 WIDGET-ID 180
          FGCOLOR 7 FONT 5
     "Nivel Educación" VIEW-AS TEXT
          SIZE 15 BY .5 AT ROW 7.27 COL 2.43 WIDGET-ID 172
          FGCOLOR 7 FONT 5
     "Tipo de Vivienda" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 7.38 COL 84 WIDGET-ID 142
          BGCOLOR 18 FGCOLOR 7 FONT 5
     "Tipo de Vínculo" VIEW-AS TEXT
          SIZE 15 BY .77 AT ROW 1.27 COL 3
          BGCOLOR 18 FGCOLOR 7 FONT 5
     "Permanencia" VIEW-AS TEXT
          SIZE 12.14 BY .81 AT ROW 15.5 COL 63.72 WIDGET-ID 188
          FGCOLOR 7 FONT 5
     "Profesion" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 9.69 COL 2.86 WIDGET-ID 356
     "Vivienda Actual Menos Dos años" VIEW-AS TEXT
          SIZE 29 BY .69 AT ROW 15.5 COL 3.14 WIDGET-ID 184
          FGCOLOR 7 FONT 5
     "Lugar y Fecha Expedición Documento" VIEW-AS TEXT
          SIZE 27.57 BY .69 AT ROW 1.42 COL 21.43 WIDGET-ID 104
          FONT 4
     RECT-1 AT ROW 1.15 COL 63.72
     RECT-228 AT ROW 14 COL 82 WIDGET-ID 150
     RECT-306 AT ROW 7.5 COL 83 WIDGET-ID 346
     RECT-309 AT ROW 7.54 COL 1.29 WIDGET-ID 348
     RECT-334 AT ROW 1.54 COL 2 WIDGET-ID 366
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.27
         SIZE 115.57 BY 16.96
         BGCOLOR 17 FONT 5
         TITLE "2. INFORMACIÓN PERSONAL".

DEFINE FRAME F_Otros
     Clientes.Fec_UltActualiza AT ROW 1.38 COL 82.14 COLON-ALIGNED WIDGET-ID 50
          LABEL "Ultima Actualización"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Cod_Retiro AT ROW 1.81 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Reestructurado AT ROW 1.92 COL 4 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "No aplica", 0,
"Reestructurado", 1,
"No ReEstructurado", 2
          SIZE 50 BY .69
     Clientes.Fec_Ingreso AT ROW 2.27 COL 82.29 COLON-ALIGNED WIDGET-ID 44
          LABEL "Ingreso a la Cooperativa"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Cod_Ingreso AT ROW 2.35 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Usuario AT ROW 2.88 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Fec_Retiro AT ROW 3.15 COL 82.29 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Dias_Sancion AT ROW 3.23 COL 39 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.57 BY .81
          BGCOLOR 15 
     Clientes.Sancionado AT ROW 3.31 COL 4
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .77
     Clientes.Cod_Segmento AT ROW 3.42 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Fec_Calificacion AT ROW 4.04 COL 82.29 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Id_PuedeCodeudar AT ROW 4.73 COL 5 HELP
          "Id_PuedeCodeudar"
          LABEL "Id_PuedeCodeudar"
          VIEW-AS TOGGLE-BOX
          SIZE 18.14 BY .65
     Clientes.Fec_IniSancion AT ROW 4.92 COL 82.29 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Reportado_Super AT ROW 5.38 COL 5
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY .77
     Clientes.Id_Privilegiado AT ROW 5.5 COL 40.72 HELP
          "Id_Privilegiado" NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "No Privilegiado", 0,
"Si Privilegiado", 1
          SIZE 13.43 BY 1.96
     BUTTON-180 AT ROW 5.77 COL 66 WIDGET-ID 38
     Clientes.Fec_fallecido AT ROW 5.92 COL 82.29 COLON-ALIGNED NO-LABEL WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Clientes.Aut_CentralRiesgo AT ROW 6.19 COL 5
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .77
     Clientes.Id_Preexistentes AT ROW 7 COL 5
          LABEL "Ha tenido enfermedades preexistentes?"
          VIEW-AS TOGGLE-BOX
          SIZE 29 BY .77
     Clientes.fecPagare AT ROW 7 COL 82.29 COLON-ALIGNED WIDGET-ID 88
          VIEW-AS FILL-IN 
          SIZE 9.86 BY .81
     Clientes.Reportado_fiscalia AT ROW 7.81 COL 5
          LABEL "Reportado a Fiscalia"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .77
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.57 ROW 8.27
         SIZE 115.43 BY 16.96
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Otros
     Clientes.Reportado_Procredito AT ROW 8.62 COL 5
          LABEL "Reportado Procrédito"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .77
     Clientes.id_AsistioAsamblea AT ROW 9.35 COL 5 HELP
          "" WIDGET-ID 86
          LABEL "Asisitió a la última Asamblea?"
          VIEW-AS TOGGLE-BOX
          SIZE 29 BY .77
     Clientes.Fec_Ult_Act[1] AT ROW 9.62 COL 85.86 COLON-ALIGNED WIDGET-ID 56
          LABEL "del Estado Civil"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Clientes.Fec_Ult_Act[2] AT ROW 10.42 COL 85.86 COLON-ALIGNED WIDGET-ID 58
          LABEL "del Nivel de Estudios"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Clientes.Calificacion AT ROW 10.85 COL 21.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     Clientes.Fec_Ult_Act[3] AT ROW 11.23 COL 85.86 COLON-ALIGNED WIDGET-ID 60
          LABEL "de los Ingresos"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Clientes.Con_Sospechosas AT ROW 11.81 COL 21.86 COLON-ALIGNED
          LABEL "Operaciones Sospechosas"
          VIEW-AS FILL-IN 
          SIZE 2.86 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Clientes.Fec_Ult_Act[4] AT ROW 12.04 COL 85.86 COLON-ALIGNED WIDGET-ID 62
          LABEL "del Nit del Empleador"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Clientes.Fec_Ult_Act[5] AT ROW 12.85 COL 85.86 COLON-ALIGNED WIDGET-ID 64
          LABEL "del Tipo de Contrato Laboral"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     W_NomSegmento AT ROW 13.42 COL 16.14 COLON-ALIGNED
     Btn_Codseg AT ROW 13.5 COL 57.43
     Clientes.Fec_Ult_Act[6] AT ROW 13.65 COL 85.86 COLON-ALIGNED WIDGET-ID 66
          LABEL "de las Operaciones Internacionales"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     W_NomRetiro AT ROW 14.38 COL 16.14 COLON-ALIGNED WIDGET-ID 34
     Clientes.Fec_Ult_Act[7] AT ROW 14.46 COL 85.86 COLON-ALIGNED WIDGET-ID 68
          LABEL "de la Fuerza Mayor"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     W_NomUsuario AT ROW 15.31 COL 16.14 COLON-ALIGNED WIDGET-ID 36
     "  Reestructurado" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 1.19 COL 6
          FGCOLOR 7 FONT 5
     " Fecha de la Ultima Actualizacion" VIEW-AS TEXT
          SIZE 24 BY .81 AT ROW 8.69 COL 70 WIDGET-ID 74
          BGCOLOR 18 FGCOLOR 0 
     RECT-221 AT ROW 3.04 COL 3
     RECT-3 AT ROW 1.54 COL 3
     RECT-320 AT ROW 1.12 COL 62 WIDGET-ID 54
     RECT-321 AT ROW 9.08 COL 62.29 WIDGET-ID 76
     RECT-323 AT ROW 13.15 COL 1.86 WIDGET-ID 78
     RECT-324 AT ROW 4.5 COL 3.14 WIDGET-ID 80
     RECT-325 AT ROW 4.96 COL 39.29 WIDGET-ID 82
     RECT-326 AT ROW 10.73 COL 3.14 WIDGET-ID 84
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.57 ROW 8.27
         SIZE 115.43 BY 16.96
         BGCOLOR 17 FONT 4
         TITLE "OTROS".

DEFINE FRAME F_Microempresas
     MicroEmpresas.NOMBRE AT ROW 1.23 COL 10.72 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 55.29 BY .81
          BGCOLOR 15 FONT 5
     MicroEmpresas.DIRECCION AT ROW 2.08 COL 10.72 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 55.29 BY .81
          BGCOLOR 15 FONT 5
     Btn_Direccion-2 AT ROW 2.23 COL 68.29 WIDGET-ID 290
     Nom_Ubicacion AT ROW 2.92 COL 10.72 COLON-ALIGNED WIDGET-ID 294
     Btn_Ubi_Micro AT ROW 3.08 COL 68.29 WIDGET-ID 206
     MicroEmpresas.TELEFONO AT ROW 4.23 COL 10.57 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 15 FONT 5
     Nom_Actividad AT ROW 5.04 COL 10.57 COLON-ALIGNED WIDGET-ID 26
     Btn_Actividad_Micro AT ROW 5.12 COL 68 WIDGET-ID 306
     Nom_Ciiu AT ROW 5.85 COL 10.57 COLON-ALIGNED WIDGET-ID 292
     MicroEmpresas.ESTRATO AT ROW 6.69 COL 10.57 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 2.57 BY .81
          BGCOLOR 15 FONT 5
     MicroEmpresas.cod_actividad AT ROW 6.73 COL 14 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 17 
     MicroEmpresas.Codigo_CIIU AT ROW 6.73 COL 17.43 COLON-ALIGNED NO-LABEL WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 17 FGCOLOR 17 
     MicroEmpresas.UBICACION AT ROW 6.73 COL 23 NO-LABEL WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 17 FGCOLOR 17 
     Grabar_MicroEmpresa AT ROW 8.08 COL 54.86 WIDGET-ID 308
     MicroEmpresas.HOMBRES AT ROW 8.58 COL 9.86 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 3.72 BY .81
          BGCOLOR 15 FONT 5
     MicroEmpresas.MUJERES AT ROW 9.42 COL 9.86 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 3.72 BY .81
          BGCOLOR 15 FONT 5
     Salir_MicroEmpresa AT ROW 9.81 COL 54.86 WIDGET-ID 310
     MicroEmpresas.NINOS AT ROW 10.27 COL 9.86 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 3.72 BY .81
          BGCOLOR 15 FONT 5
     "Personas que Favorece" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 7.81 COL 2.86 WIDGET-ID 28
          BGCOLOR 17 FGCOLOR 18 
     RECT-311 AT ROW 8.12 COL 2 WIDGET-ID 312
     RECT-312 AT ROW 4 COL 2 WIDGET-ID 314
     RECT-313 AT ROW 1 COL 2 WIDGET-ID 316
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 29 ROW 8.31
         SIZE 71.86 BY 11.31
         BGCOLOR 17 FONT 5
         TITLE "Microempresas" WIDGET-ID 400.

DEFINE FRAME F_Juridicas
     Clientes.Celular AT ROW 1.15 COL 22.29 COLON-ALIGNED WIDGET-ID 2
          LABEL "Camara de Comercio"
          VIEW-AS FILL-IN 
          SIZE 21.43 BY .81
          BGCOLOR 15 
     Clientes.Fec_expedicion AT ROW 1.12 COL 75 COLON-ALIGNED WIDGET-ID 4
          LABEL "Fecha de Constitucion"
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     Btn_DirEmpresa AT ROW 2.04 COL 90 WIDGET-ID 292
     Clientes.Dir_Residencia AT ROW 2.08 COL 22.14 COLON-ALIGNED WIDGET-ID 6
          LABEL "Direccion de la Empresa"
          VIEW-AS FILL-IN 
          SIZE 66 BY .81
          BGCOLOR 15 
     Btn_UbiEmpresa AT ROW 2.96 COL 90 WIDGET-ID 208
     Clientes.Lugar_Residencia AT ROW 1.54 COL 108.14 COLON-ALIGNED NO-LABEL WIDGET-ID 296
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     W_Ubi_Empresa AT ROW 2.96 COL 22.29 COLON-ALIGNED WIDGET-ID 294
     Clientes.Tel_Residencia AT ROW 3.81 COL 22.43 COLON-ALIGNED WIDGET-ID 302
          LABEL "Telefono Fijo"
          VIEW-AS FILL-IN 
          SIZE 21.43 BY .81
          BGCOLOR 15 
     Clientes.Tel_Arrendatario AT ROW 3.85 COL 51.86 COLON-ALIGNED WIDGET-ID 300
          LABEL "Fax"
          VIEW-AS FILL-IN 
          SIZE 15.43 BY .81
          BGCOLOR 15 
     Anexos_Clientes.AA_Cliente AT ROW 3.85 COL 78.57 COLON-ALIGNED WIDGET-ID 304
          LABEL "AA"
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .81
          BGCOLOR 15 
     Clientes.Email AT ROW 4.77 COL 22.29 COLON-ALIGNED WIDGET-ID 298
          LABEL "Pagina Web"
          VIEW-AS FILL-IN 
          SIZE 66 BY .81
          BGCOLOR 15 
     Clientes.Tipo_Empresa AT ROW 6.04 COL 22.43 COLON-ALIGNED WIDGET-ID 306
          LABEL "Tipo de Empresa"
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "Privada","Publica","Mixta" 
          DROP-DOWN-LIST
          SIZE 12.14 BY 1
          BGCOLOR 15 
     Btn_Act_Ppal AT ROW 6.96 COL 51.72 WIDGET-ID 310
     Clientes.cod_actividad AT ROW 2.62 COL 108 COLON-ALIGNED NO-LABEL WIDGET-ID 314
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     W_Nom_Act_Ppal AT ROW 7 COL 22.29 COLON-ALIGNED WIDGET-ID 308
     Anexos_Clientes.Acti_Economica_emp AT ROW 3.69 COL 108 COLON-ALIGNED NO-LABEL WIDGET-ID 316
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     W_Nom_Ciu_Emp AT ROW 6.92 COL 61.29 COLON-ALIGNED WIDGET-ID 318
     Clientes.Salario AT ROW 9.19 COL 3 COLON-ALIGNED WIDGET-ID 322
          LABEL "$"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Clientes.Ing_Otros AT ROW 9.19 COL 34.29 COLON-ALIGNED WIDGET-ID 320
          VIEW-AS FILL-IN 
          SIZE 15.57 BY .81
          BGCOLOR 15 
     Anexos_Clientes.Especifique_OtrosIng AT ROW 9.19 COL 62.57 COLON-ALIGNED WIDGET-ID 324
          LABEL "Especificar"
          VIEW-AS FILL-IN 
          SIZE 28 BY .81
          BGCOLOR 15 
     Clientes.Sdo_Obligaciones AT ROW 10.65 COL 20.29 COLON-ALIGNED WIDGET-ID 332
          LABEL "Egresos Mensuales"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Clientes.Act_vehiculo AT ROW 10.65 COL 48 COLON-ALIGNED WIDGET-ID 330
          LABEL "Total Activos"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 8.27
         SIZE 115 BY 16.96
         BGCOLOR 17 FONT 5 WIDGET-ID 500.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Juridicas
     Anexos_Clientes.Val_Pasivos AT ROW 10.65 COL 76 COLON-ALIGNED WIDGET-ID 334
          LABEL "Total Pasivos"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 
     Clientes.Id_Mon_Ext AT ROW 12.58 COL 1.14 COLON-ALIGNED NO-LABEL WIDGET-ID 336
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 15 
     Anexos_Clientes.Otra_Transac_extr AT ROW 12.54 COL 13.14 COLON-ALIGNED WIDGET-ID 340
          LABEL "Cuales"
          VIEW-AS FILL-IN 
          SIZE 29 BY .81
          BGCOLOR 15 
     Anexos_Clientes.Maneja_Cta_Extanjera AT ROW 12.62 COL 45.86 COLON-ALIGNED NO-LABEL WIDGET-ID 338
          VIEW-AS FILL-IN 
          SIZE 4 BY .81
          BGCOLOR 15 
     Anexos_Clientes.Tipo_Moneda_Divisa AT ROW 12.54 COL 58.86 COLON-ALIGNED WIDGET-ID 342
          LABEL "Moneda"
          VIEW-AS FILL-IN 
          SIZE 29 BY .81
          BGCOLOR 15 
     Anexos_Clientes.Num_cta_extranjera AT ROW 13.85 COL 8.57 COLON-ALIGNED WIDGET-ID 358
          LABEL "Nro Cta"
          VIEW-AS FILL-IN 
          SIZE 25 BY .81
          BGCOLOR 15 
     Anexos_Clientes.Nom_Banco_extranjero AT ROW 13.85 COL 52 COLON-ALIGNED WIDGET-ID 356
          LABEL "Banco"
          VIEW-AS FILL-IN 
          SIZE 36.57 BY .81
          BGCOLOR 15 
     Anexos_Clientes.Ciudad_Pais_Bco_Extranjero AT ROW 14.85 COL 52.14 COLON-ALIGNED WIDGET-ID 354
          LABEL "Ciudad y Pais"
          VIEW-AS FILL-IN 
          SIZE 36.43 BY .81
          BGCOLOR 15 
     "Posee Cuentas en Moneda Extranjera" VIEW-AS TEXT
          SIZE 33 BY .62 AT ROW 11.77 COL 51.14 WIDGET-ID 348
          BGCOLOR 18 FGCOLOR 0 
     "Ingresos Mensuales Derivados de su Actividad Principal" VIEW-AS TEXT
          SIZE 48.57 BY .62 AT ROW 8.35 COL 3 WIDGET-ID 328
          BGCOLOR 18 
     "Realiza Operaciones en Moneda Extranjera" VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 11.81 COL 4.72 WIDGET-ID 346
          BGCOLOR 18 FGCOLOR 0 
     RECT-327 AT ROW 8.69 COL 2.43 WIDGET-ID 326
     RECT-328 AT ROW 12.12 COL 2.29 WIDGET-ID 350
     RECT-329 AT ROW 12.12 COL 47 WIDGET-ID 352
     RECT-330 AT ROW 5.85 COL 2.57 WIDGET-ID 360
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 8.27
         SIZE 115 BY 16.96
         BGCOLOR 17 FONT 5
         TITLE "INFORMACION GENERAL ASOCIADO - PERSONA JURIDICA" WIDGET-ID 500.

DEFINE FRAME F_Falta
     B_Falta AT ROW 2.08 COL 3
     BUTTON-179 AT ROW 14.19 COL 105
     "La siguiente información se necesita para salvar el registro" VIEW-AS TEXT
          SIZE 51 BY .62 AT ROW 1.27 COL 4
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 8.27
         SIZE 115 BY 16.96
         BGCOLOR 17 
         TITLE "Información que falta por llenar".

DEFINE FRAME F_Economica
     Anexos_Clientes.Transac_Mod_Ext AT ROW 8.73 COL 96 NO-LABEL WIDGET-ID 18
          VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
          LIST-ITEMS "","Importaciones","Exportaciones","Inversiones","Giros","Préstamos en moneda Extranjera","Ordenes de pago","Remesas","Transferencias","Cambio de Divisas","Otros" 
          SIZE 17.72 BY 1.96 TOOLTIP "Transacciones Que Normalmente Realiza"
     Anexos_Clientes.Gto_TargetaCredito AT ROW 5.04 COL 77.86
          LABEL "Cuota Tarjeta Crédito" FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
     Clientes.Salario AT ROW 1.77 COL 23 COLON-ALIGNED
          LABEL "Salario / Pensión" FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Ing_Honorarios AT ROW 2.65 COL 23 COLON-ALIGNED
          LABEL "Honorarios" FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Ing_financieros AT ROW 3.58 COL 23 COLON-ALIGNED
          LABEL "Comisiones" FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Ing_arriendos AT ROW 4.46 COL 23 COLON-ALIGNED WIDGET-ID 2
          LABEL "Arriendos" FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Clientes.Ing_Otros AT ROW 5.35 COL 23 COLON-ALIGNED FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     Anexos_Clientes.Especifique_OtrosIng AT ROW 6.23 COL 11 COLON-ALIGNED
          LABEL "Especifique" FORMAT "X(70)"
          VIEW-AS FILL-IN 
          SIZE 29 BY .81
          BGCOLOR 19 
     Tot_Ingresos AT ROW 7.19 COL 23 COLON-ALIGNED
     Clientes.Gto_obligacion AT ROW 2.54 COL 91 COLON-ALIGNED
          LABEL "Descuentos Nomina" FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 15 
     Clientes.Gto_Arriendo AT ROW 4.23 COL 91 COLON-ALIGNED
          LABEL "Arriendo / Cuota Hip."
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81 TOOLTIP "Valor Arriendo"
          BGCOLOR 33 
     Clientes.Sdo_Obligaciones AT ROW 3.38 COL 91 COLON-ALIGNED
          LABEL "Cuotas Préstamos" FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 15 
     Clientes.Gto_Familiar AT ROW 1.69 COL 90.86 COLON-ALIGNED
          LABEL "Gastos Familiares" FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 15 
     Anexos_Clientes.Gto_Otros AT ROW 5.88 COL 91 COLON-ALIGNED
          LABEL "Otras Deudas"
          VIEW-AS FILL-IN 
          SIZE 14.29 BY .81
          BGCOLOR 15 
     Tot_Egresos AT ROW 6.81 COL 91.29 COLON-ALIGNED
     Btn_Grabar_Act_Pas AT ROW 10.42 COL 34.29 WIDGET-ID 16
     Clientes.Act_vehiculo AT ROW 10.42 COL 14 COLON-ALIGNED
          LABEL "Valor Vehículo" FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 8 
     Clientes.Act_casa AT ROW 11.31 COL 14 COLON-ALIGNED FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 8 
     Clientes.Act_inversion AT ROW 12.19 COL 14 COLON-ALIGNED
          LABEL "Otros Activos" FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 8 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.23
         SIZE 115.57 BY 17
         BGCOLOR 17 FGCOLOR 0 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Economica
     Tot_Activos AT ROW 13.08 COL 14 COLON-ALIGNED
     Anexos_Clientes.Val_Pasivos AT ROW 14.27 COL 14 COLON-ALIGNED
          LABEL "Total Pasivos"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Anexos_Clientes.Otra_Transac_extr AT ROW 10.96 COL 90 COLON-ALIGNED
          LABEL "Otra ¿Cual?" FORMAT "X(70)"
          VIEW-AS FILL-IN 
          SIZE 21.86 BY .81
          BGCOLOR 18 
     Anexos_Clientes.Maneja_Cta_Extanjera AT ROW 12.15 COL 97.29
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 4 BY .77 TOOLTIP "Cuenta Moneda Extranjera"
     Anexos_Clientes.Tipo_Moneda_Divisa AT ROW 13.08 COL 76 COLON-ALIGNED
          LABEL "Tipo Moneda" FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FONT 5
     Anexos_Clientes.Nom_Banco_extranjero AT ROW 13.08 COL 94.29 COLON-ALIGNED WIDGET-ID 10
          LABEL "Banco" FORMAT "X(50)"
          VIEW-AS FILL-IN 
          SIZE 18 BY .81
          BGCOLOR 18 FONT 5
     Anexos_Clientes.Num_cta_extranjera AT ROW 13.92 COL 76 COLON-ALIGNED WIDGET-ID 12
          LABEL "Numero Cuenta" FORMAT "X(25)"
          VIEW-AS FILL-IN 
          SIZE 36 BY .81
          BGCOLOR 18 FONT 5
     Anexos_Clientes.Ciudad_Pais_Bco_Extranjero AT ROW 14.77 COL 76 COLON-ALIGNED
          LABEL "Ciudad y País" FORMAT "X(50)"
          VIEW-AS FILL-IN 
          SIZE 36 BY .81
          BGCOLOR 18 FONT 5
     Clientes.Id_Mon_Ext AT ROW 9.08 COL 90 COLON-ALIGNED NO-LABEL WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 2.86 BY .81
          BGCOLOR 15 
     "  Egresos Mensuales" VIEW-AS TEXT
          SIZE 19 BY .77 AT ROW 1 COL 68.29
          FGCOLOR 7 FONT 5
     "  Ingresos Mensuales" VIEW-AS TEXT
          SIZE 19 BY 1.04 AT ROW 1 COL 6
          FGCOLOR 7 FONT 5
     " Activos y Pasivos" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 9.19 COL 7
          FGCOLOR 7 FONT 5
     "Transacciones Moneda Extranjera" VIEW-AS TEXT
          SIZE 31 BY .81 AT ROW 7.92 COL 67.72
          FGCOLOR 7 FONT 5
     "Posee cuenta en moneda extranjera" VIEW-AS TEXT
          SIZE 27.29 BY .81 AT ROW 12.15 COL 68 WIDGET-ID 8
          FONT 4
     "Realiza Tx en moneda extranjera?" VIEW-AS TEXT
          SIZE 24 BY .81 AT ROW 9.12 COL 67 WIDGET-ID 24
          FONT 4
     RECT-213 AT ROW 1.27 COL 3
     RECT-214 AT ROW 1.23 COL 66
     RECT-219 AT ROW 9.62 COL 3
     RECT-237 AT ROW 8.27 COL 66 WIDGET-ID 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.23
         SIZE 115.57 BY 17
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "INFORMACIÓN FINANCIERA".

DEFINE FRAME F_Documentacion
     Cmb_Linea AT ROW 1.19 COL 32.14 COLON-ALIGNED
     Cmb_Contrato AT ROW 1.27 COL 74 COLON-ALIGNED
     Anexos_Clientes.Docum_Requer[1] AT ROW 2.12 COL 5
          LABEL "   Formato unico de vinculación"
          VIEW-AS TOGGLE-BOX
          SIZE 29 BY .77
     Anexos_Clientes.Docum_Requer[2] AT ROW 2.81 COL 5 WIDGET-ID 182
          LABEL "   Fotocopia Ampliada del Documento de Identidad"
          VIEW-AS TOGGLE-BOX
          SIZE 49.57 BY .77
     Anexos_Clientes.Docum_Requer[3] AT ROW 3.54 COL 5 WIDGET-ID 192
          LABEL "   Fotocopia de las dos (2) últimas nóminas o mesada pensional"
          VIEW-AS TOGGLE-BOX
          SIZE 59 BY .77
     Anexos_Clientes.Docum_Requer[4] AT ROW 4.23 COL 5
          LABEL "   Certificado Laboral Expedicion menor a 30 días"
          VIEW-AS TOGGLE-BOX
          SIZE 49.57 BY .77
     Anexos_Clientes.Docum_Requer[5] AT ROW 4.96 COL 5
          LABEL "   Fotocopia de Tarjeta de propiedad de (los) vehiculos(s) o moto(s)"
          VIEW-AS TOGGLE-BOX
          SIZE 61 BY .77
     Anexos_Clientes.Docum_Requer[6] AT ROW 5.73 COL 5 WIDGET-ID 188
          LABEL "   Factura proforma para vehiculo o moto nuevo"
          VIEW-AS TOGGLE-BOX
          SIZE 49.57 BY .77
     Anexos_Clientes.Docum_Requer[7] AT ROW 6.46 COL 5 WIDGET-ID 186
          LABEL "   Peritaje para vehiculo usado"
          VIEW-AS TOGGLE-BOX
          SIZE 49.57 BY .77
     Anexos_Clientes.Docum_Requer[8] AT ROW 7.19 COL 5 WIDGET-ID 190
          LABEL "   Certificado original de tradición y libertad del inmuebleno mayor a 30 días"
          VIEW-AS TOGGLE-BOX
          SIZE 67 BY .77
     Anexos_Clientes.Docum_Requer[9] AT ROW 7.92 COL 5
          LABEL "   Declaración de renta o constancia si no esta obligado (2 últimos años)"
          VIEW-AS TOGGLE-BOX
          SIZE 67 BY .77
     Anexos_Clientes.Docum_Requer[10] AT ROW 8.65 COL 5 WIDGET-ID 194
          LABEL "   Copia del contrato de promesa de compra venta o la minuta"
          VIEW-AS TOGGLE-BOX
          SIZE 67 BY .77
     Anexos_Clientes.Docum_Requer[11] AT ROW 9.42 COL 5
          LABEL "   Paz y salvo de impuesto predial"
          VIEW-AS TOGGLE-BOX
          SIZE 67 BY .77
     Anexos_Clientes.Docum_Requer[12] AT ROW 10.15 COL 5
          LABEL "   Avalúo Comercial y estudio de titulos"
          VIEW-AS TOGGLE-BOX
          SIZE 67 BY .77
     Anexos_Clientes.Docum_Requer[13] AT ROW 10.88 COL 5
          LABEL "   Factura de compra venta que acrediten las transacciones comerciales"
          VIEW-AS TOGGLE-BOX
          SIZE 67 BY .77
     Anexos_Clientes.Docum_Requer[14] AT ROW 11.62 COL 5
          LABEL "   Certificado de ingresos, retenciones otros ingresos u honorarios"
          VIEW-AS TOGGLE-BOX
          SIZE 67 BY .77
     Anexos_Clientes.Docum_Requer[15] AT ROW 12.35 COL 5
          LABEL "   Cert. Cámara de comercio, Licencia de Sanidad o Bomberos."
          VIEW-AS TOGGLE-BOX
          SIZE 67 BY .77
     Anexos_Clientes.Docum_Requer[16] AT ROW 13.08 COL 5
          LABEL "   Extractos Bancarios de los últimos tres (3) meses"
          VIEW-AS TOGGLE-BOX
          SIZE 67 BY .77
     Anexos_Clientes.Docum_Requer[17] AT ROW 13.81 COL 5
          LABEL "   Estados Financieros certificados de 2 últimos años y al corte más reciente"
          VIEW-AS TOGGLE-BOX
          SIZE 67 BY .77
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.23
         SIZE 115.57 BY 17
         BGCOLOR 17 FONT 5 WIDGET-ID 200.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Documentacion
     Anexos_Clientes.Docum_Requer[18] AT ROW 14.54 COL 5
          LABEL "   Prueba de destinación para el crédito educativo"
          VIEW-AS TOGGLE-BOX
          SIZE 67 BY .77
     Anexos_Clientes.Docum_Requer[19] AT ROW 15.23 COL 5 WIDGET-ID 196
          LABEL "   Flujo de Caja del Proyecto a Financiar"
          VIEW-AS TOGGLE-BOX
          SIZE 67 BY .77
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.23
         SIZE 115.57 BY 17
         BGCOLOR 17 FONT 5
         TITLE "DOCUMENTACION REQUERIDA" WIDGET-ID 200.

DEFINE FRAME F_Conyuge
     CnygeExtncion AT ROW 12.38 COL 103 COLON-ALIGNED HELP
          "Extencion del Teléfono Comercial" NO-LABEL WIDGET-ID 286
     R_Tipo_Identificacion AT ROW 1.42 COL 5.72 COLON-ALIGNED WIDGET-ID 2
     R_Tercero AT ROW 1.42 COL 31.14 COLON-ALIGNED HELP
          "Número documento de identificación" WIDGET-ID 110
     R_Sexo AT ROW 1.46 COL 61.86 NO-LABEL WIDGET-ID 114
     R_Apellido1 AT ROW 3.46 COL 1 COLON-ALIGNED HELP
          "Primer apellido del cliente" NO-LABEL WIDGET-ID 22 FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 25 BY .81
          BGCOLOR 15 
     R_Apellido2 AT ROW 3.46 COL 26.86 COLON-ALIGNED HELP
          "Segundo apellido del cliente" NO-LABEL WIDGET-ID 24 FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 25 BY .81
          BGCOLOR 15 
     R_Nombre AT ROW 3.42 COL 53 COLON-ALIGNED HELP
          "Nombre del cliente" NO-LABEL WIDGET-ID 26 FORMAT "X(90)"
          VIEW-AS FILL-IN 
          SIZE 59 BY .81
          BGCOLOR 15 
     WR_CiuNacimiento AT ROW 5.62 COL 3 NO-LABEL WIDGET-ID 264
     R_Btn_Nacimiento AT ROW 5.54 COL 67 WIDGET-ID 262
     R_Lugar_Nacimiento AT ROW 14.19 COL 61 COLON-ALIGNED HELP
          "Barrio, Ciudad y Departamento donde nació el cliente" NO-LABEL WIDGET-ID 216
          BGCOLOR 17 FGCOLOR 17 
     R_Fec_Nacimiento AT ROW 5.62 COL 69 COLON-ALIGNED HELP
          "Fecha de nacimiento del cliente" NO-LABEL
          BGCOLOR 15 
     R_Nacionalidad AT ROW 5.62 COL 112.43 RIGHT-ALIGNED NO-LABEL WIDGET-ID 102
     R_Niv_Educativo AT ROW 7.85 COL 3 NO-LABEL
     WR_NomProfesion AT ROW 7.85 COL 26 COLON-ALIGNED NO-LABEL
     R_Btn_Profesion AT ROW 7.77 COL 57.86 WIDGET-ID 244
     R_Cod_Profesion AT ROW 14.19 COL 57 COLON-ALIGNED HELP
          "Código de la profesión del cliente" NO-LABEL WIDGET-ID 242
          BGCOLOR 17 FGCOLOR 17 
     R_Ocupacion AT ROW 7.77 COL 62.57 NO-LABEL WIDGET-ID 4
     R_Especifique AT ROW 7.81 COL 83.43 COLON-ALIGNED NO-LABEL WIDGET-ID 252
     R_Cargo_Actividad AT ROW 11.12 COL 24 COLON-ALIGNED
     R_Empresa AT ROW 10.23 COL 24 COLON-ALIGNED WIDGET-ID 254
     R_Direccion AT ROW 12 COL 26 NO-LABEL FORMAT "X(90)":U
          VIEW-AS FILL-IN 
          SIZE 57 BY .81
          BGCOLOR 15 
     WR_CiuEmpresa AT ROW 12.92 COL 26 NO-LABEL WIDGET-ID 270
     R_Btn_Empresa AT ROW 12.88 COL 82.43 WIDGET-ID 266
     R_Dir_Empresa AT ROW 14.19 COL 55 HELP
          "Dirección comercial donde labora el cliente" NO-LABEL WIDGET-ID 268
          BGCOLOR 17 FGCOLOR 17 
     R_Btn_Direccion AT ROW 11.85 COL 82.43 WIDGET-ID 290
     R_Tel_Empresa AT ROW 12.38 COL 90.57 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     R_TIngreso AT ROW 13.92 COL 11.43
     R_TEgresos AT ROW 14.65 COL 11.57
     Btn_Grabar_conyuge AT ROW 1.31 COL 84.14 WIDGET-ID 282
     Btn_Ocultar_conyuge AT ROW 1.27 COL 100 WIDGET-ID 284
     R_Tel_Empresa-Indctvo AT ROW 12.38 COL 85.86 COLON-ALIGNED NO-LABEL WIDGET-ID 292
     "Segundo Apellido" VIEW-AS TEXT
          SIZE 16.86 BY .77 AT ROW 2.69 COL 29.72 WIDGET-ID 106
          FGCOLOR 7 FONT 5
     "Primer Apellido" VIEW-AS TEXT
          SIZE 14 BY .77 AT ROW 2.65 COL 3 WIDGET-ID 104
          FGCOLOR 7 FONT 5
     "Ciudad y Departamento" VIEW-AS TEXT
          SIZE 21 BY .77 AT ROW 12.88 COL 4 WIDGET-ID 276
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.23
         SIZE 115.57 BY 17
         BGCOLOR 17 FONT 5 WIDGET-ID 300.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Conyuge
     "NACIONALIDAD" VIEW-AS TEXT
          SIZE 12.14 BY .77 AT ROW 4.85 COL 86 WIDGET-ID 222
          FGCOLOR 7 FONT 4
     "Teléfono" VIEW-AS TEXT
          SIZE 14 BY .77 AT ROW 11.58 COL 88.57 WIDGET-ID 278
          FGCOLOR 7 FONT 5
     "Dirección de la Empresa" VIEW-AS TEXT
          SIZE 22 BY .77 AT ROW 12.04 COL 3.14 WIDGET-ID 274
          FGCOLOR 7 FONT 5
     "Ocupación Principal" VIEW-AS TEXT
          SIZE 18.29 BY .77 AT ROW 7 COL 63.14 WIDGET-ID 250
          FGCOLOR 7 FONT 5
     "Nivel Educación" VIEW-AS TEXT
          SIZE 15 BY .77 AT ROW 7.08 COL 3 WIDGET-ID 280
          FGCOLOR 7 FONT 5
     "Ext" VIEW-AS TEXT
          SIZE 7 BY .77 AT ROW 11.58 COL 105.29 WIDGET-ID 288
          FGCOLOR 7 FONT 5
     "Profesión" VIEW-AS TEXT
          SIZE 9.29 BY .77 AT ROW 7.04 COL 30 WIDGET-ID 166
          FGCOLOR 7 FONT 5
     "Datos Financieros" VIEW-AS TEXT
          SIZE 16.29 BY .77 AT ROW 9.46 COL 3 WIDGET-ID 172
          FGCOLOR 7 FONT 5
     "Lugar y Fecha de Nacimiento" VIEW-AS TEXT
          SIZE 26 BY .77 AT ROW 4.85 COL 3 WIDGET-ID 220
          FGCOLOR 7 FONT 5
     "DDMMAAAA" VIEW-AS TEXT
          SIZE 9.43 BY .77 AT ROW 4.85 COL 71 WIDGET-ID 146
          FGCOLOR 7 FONT 4
     "Género" VIEW-AS TEXT
          SIZE 7.14 BY .54 AT ROW 1.08 COL 60.86 WIDGET-ID 118
          FGCOLOR 7 FONT 4
     "Nombre" VIEW-AS TEXT
          SIZE 9.14 BY .77 AT ROW 2.65 COL 55.72 WIDGET-ID 108
          FGCOLOR 7 FONT 5
     RECT-231 AT ROW 4.77 COL 2 WIDGET-ID 68
     RECT-5 AT ROW 1.31 COL 60.29 WIDGET-ID 112
     RECT-232 AT ROW 2.62 COL 2 WIDGET-ID 218
     RECT-233 AT ROW 6.92 COL 2 WIDGET-ID 168
     RECT-234 AT ROW 9.88 COL 2 WIDGET-ID 272
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.23
         SIZE 115.57 BY 17
         BGCOLOR 17 FONT 5
         TITLE "Información Cónyuge o Compañero (a) permanente" WIDGET-ID 300.

DEFINE FRAME F_Autorizaciones
     Anexos_Clientes.Represetacion AT ROW 2.04 COL 19.57 COLON-ALIGNED
          LABEL "Representacion de" FORMAT "X(100)"
          VIEW-AS FILL-IN 
          SIZE 91 BY .81
          BGCOLOR 15 
     Anexos_Clientes.Proviene AT ROW 2.88 COL 19.57 COLON-ALIGNED
          LABEL "Proviene de" FORMAT "X(100)"
          VIEW-AS FILL-IN 
          SIZE 91 BY .81
          BGCOLOR 15 
     Anexos_Clientes.Declaracion_Conocimiento AT ROW 4.04 COL 5.43
          LABEL "Declaracion de Conocimiento de Origen de Fondos"
          VIEW-AS TOGGLE-BOX
          SIZE 49.57 BY .77
     Anexos_Clientes.Autorizaciones[1] AT ROW 6.15 COL 5 WIDGET-ID 180
          LABEL "1. Consulta y Reporte a Centros de Riesgos"
          VIEW-AS TOGGLE-BOX
          SIZE 49.57 BY .77
     Anexos_Clientes.Declaracion_aceptacion AT ROW 6.69 COL 111 WIDGET-ID 194
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .77 TOOLTIP "Declaracion Aceptacion"
          BGCOLOR 17 
     Anexos_Clientes.Autorizaciones[2] AT ROW 6.88 COL 5 WIDGET-ID 182
          LABEL "2. Actualiza y Verifica la información"
          VIEW-AS TOGGLE-BOX
          SIZE 49.57 BY .77
     Anexos_Clientes.Autorizaciones[3] AT ROW 7.62 COL 5
          LABEL "3. Suministro de Informacion"
          VIEW-AS TOGGLE-BOX
          SIZE 49.57 BY .77
     Anexos_Clientes.Autorizaciones[4] AT ROW 8.35 COL 5 WIDGET-ID 184
          LABEL "4. Cláusula de Aceptación de Reglamentos"
          VIEW-AS TOGGLE-BOX
          SIZE 49.57 BY .77
     Anexos_Clientes.Autorizaciones[5] AT ROW 9.08 COL 5
          LABEL "5. Destrucción de Documentos"
          VIEW-AS TOGGLE-BOX
          SIZE 49.57 BY .77
     Anexos_Clientes.Fec_Diligenciamiento AT ROW 9.77 COL 80.14 COLON-ALIGNED WIDGET-ID 210
          LABEL "DD/MM/AAAA"
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     Anexos_Clientes.Autorizaciones[6] AT ROW 9.81 COL 5 WIDGET-ID 188
          LABEL "6. Cláusula de Aceptación de Términos"
          VIEW-AS TOGGLE-BOX
          SIZE 49.57 BY .77
     Anexos_Clientes.Autorizaciones[7] AT ROW 10.54 COL 5 WIDGET-ID 186
          LABEL "7. Cobro de Centrales de Riesgo"
          VIEW-AS TOGGLE-BOX
          SIZE 49.57 BY .77
     Anexos_Clientes.Autorizaciones[8] AT ROW 11.27 COL 5 WIDGET-ID 190
          LABEL "8. Compromiso de Cumplimiento de actualizacion de Información"
          VIEW-AS TOGGLE-BOX
          SIZE 58 BY .77
     Anexos_Clientes.Autorizaciones[9] AT ROW 12 COL 5.14 WIDGET-ID 208
          LABEL "9. Requisitos de Asegurabilidad"
          VIEW-AS TOGGLE-BOX
          SIZE 56.86 BY .77 TOOLTIP "Autorizacion 9"
     Anexos_Clientes.Autorizaciones[10] AT ROW 12.73 COL 5 WIDGET-ID 216
          LABEL "10. Desembolso"
          VIEW-AS TOGGLE-BOX
          SIZE 33 BY .77
     Anexos_Clientes.Autorizaciones[11] AT ROW 13.46 COL 5 WIDGET-ID 218
          LABEL "11. Débito Automàtico"
          VIEW-AS TOGGLE-BOX
          SIZE 43 BY .77
     Anexos_Clientes.Cam_log1 AT ROW 13.96 COL 65.14 COLON-ALIGNED NO-LABEL WIDGET-ID 224
          VIEW-AS COMBO-BOX INNER-LINES 1
          LIST-ITEM-PAIRS "FIRMA SOLICITANTE Y HUELLA INDICE DERECHO","FIRMA SOLICITANTE Y HUELLA INDICE DERECHO"
          DROP-DOWN-LIST
          SIZE 46.86 BY 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.23
         SIZE 115.57 BY 17
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Autorizaciones
     Anexos_Clientes.Autorizaciones[12] AT ROW 14.19 COL 5 WIDGET-ID 220
          LABEL "12. Prioridad de Descuento"
          VIEW-AS TOGGLE-BOX
          SIZE 34 BY .77
     "Fecha de Diligenciamiento" VIEW-AS TEXT
          SIZE 30 BY .65 AT ROW 8.85 COL 68 WIDGET-ID 212
          FGCOLOR 7 FONT 7
     "DECLARACION VOLUNTARIA DE ORIGEN DE FONDOS" VIEW-AS TEXT
          SIZE 50 BY .77 AT ROW 1.12 COL 25.72 WIDGET-ID 172
          FGCOLOR 7 FONT 7
     "contratos y reglamentos vigentes en FODUN" VIEW-AS TEXT
          SIZE 43 BY 1.35 AT ROW 6.62 COL 66 WIDGET-ID 202
          FONT 7
     "AUTORIZACIONES" VIEW-AS TEXT
          SIZE 18 BY .77 AT ROW 5.35 COL 23.14 WIDGET-ID 178
          FGCOLOR 7 FONT 7
     "Declaro conocer y aceptar los términos de los" VIEW-AS TEXT
          SIZE 43.14 BY 1.35 AT ROW 5.73 COL 65.86 WIDGET-ID 200
          FONT 7
     RECT-304 AT ROW 1.42 COL 3 WIDGET-ID 4
     RECT-305 AT ROW 5.62 COL 3 WIDGET-ID 176
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.23
         SIZE 115.57 BY 17
         BGCOLOR 17 FONT 5
         TITLE "DECLARACION DE CONOCIMIENTO  Y AUTORIZACIONES".

DEFINE FRAME F_Ubicacion
     Clientes.departamento AT ROW 4.81 COL 70.57 COLON-ALIGNED NO-LABEL WIDGET-ID 326
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 14 FGCOLOR 14 
     Clientes.facultad AT ROW 4.81 COL 72.86 COLON-ALIGNED NO-LABEL WIDGET-ID 324
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 14 FGCOLOR 14 
     Anexos_Clientes.Cam_int1 AT ROW 14.69 COL 56 COLON-ALIGNED HELP
          "Indicativo del telefono del cliente" NO-LABEL WIDGET-ID 298 FORMAT "99"
          VIEW-AS COMBO-BOX INNER-LINES 13
          LIST-ITEMS "0","1","2","3","4","5","6","7","8","9","10","11","12" 
          DROP-DOWN-LIST
          SIZE 7 BY 1 TOOLTIP "Meses Actividad"
     Anexos_Clientes.Tel_Fax_Comercial AT ROW 11.96 COL 62.86 COLON-ALIGNED WIDGET-ID 292
          LABEL "Fax"
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 18 
     Anexos_Clientes.Exento_retencion AT ROW 1.15 COL 13
          LABEL "Retencion Fuente"
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY .81 TOOLTIP "Exento Retencion fuente"
          FONT 5
     Anexos_Clientes.Exento_GMF AT ROW 1.15 COL 36.14
          LABEL "GMF"
          VIEW-AS TOGGLE-BOX
          SIZE 8 BY .81 TOOLTIP "Exento GMF"
     Anexos_Clientes.Declara_Renta AT ROW 1.15 COL 60.86
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .77 TOOLTIP "Declara Renta"
     Anexos_Clientes.Adm_Recursos_Publ AT ROW 1.15 COL 95.14 WIDGET-ID 8
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3 BY .77 TOOLTIP "Administra Recursos Publicos"
     Cmb_TipAct AT ROW 2 COL 3
     Anexos_Clientes.Especifique_Ocupacion AT ROW 2 COL 65 COLON-ALIGNED WIDGET-ID 252
          LABEL "Especifique" FORMAT "X(70)"
          VIEW-AS FILL-IN 
          SIZE 33.29 BY .81
          BGCOLOR 18 
     W_NomEmpresa AT ROW 5.73 COL 18 COLON-ALIGNED
     Btn_Empresa AT ROW 5.85 COL 94 WIDGET-ID 260
     Clientes.Cod_Empresa AT ROW 4.77 COL 82.72 COLON-ALIGNED NO-LABEL WIDGET-ID 264
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Anexos_Clientes.Acti_Economica_emp AT ROW 4.81 COL 75 COLON-ALIGNED NO-LABEL WIDGET-ID 272 FORMAT "X(90)"
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Dir_comercial AT ROW 7.35 COL 18 COLON-ALIGNED
          LABEL "Dirección" FORMAT "X(90)"
          VIEW-AS FILL-IN 
          SIZE 74 BY .81
          BGCOLOR 18 
     W_UbicacionComercial AT ROW 8.19 COL 18 COLON-ALIGNED
     Btn_Lugar_Comercial AT ROW 8.31 COL 94
     Clientes.Lugar_comercial AT ROW 4.81 COL 80.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Tel_comercial AT ROW 9.04 COL 23.43 COLON-ALIGNED NO-LABEL FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 19.57 BY .81
          BGCOLOR 18 
     Anexos_Clientes.Extencion_Comercial AT ROW 9.08 COL 48 COLON-ALIGNED WIDGET-ID 280
          LABEL "Ext" FORMAT ">>>>9"
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
          BGCOLOR 18 
     W_NomCargo AT ROW 9.08 COL 65 COLON-ALIGNED WIDGET-ID 44
     Btn_Cargo AT ROW 9.15 COL 94 WIDGET-ID 38
     Clientes.Cod_Cargo AT ROW 4.81 COL 87.86 COLON-ALIGNED NO-LABEL WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.27
         SIZE 115.57 BY 16.96
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Ubicacion
     Clientes.Fec_IngEmpresa AT ROW 10.88 COL 23.86 COLON-ALIGNED WIDGET-ID 266
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 0 
     Clientes.Carnet AT ROW 11.04 COL 56 COLON-ALIGNED WIDGET-ID 42
          LABEL "Carnet"
          VIEW-AS FILL-IN 
          SIZE 26 BY .81
          BGCOLOR 18 
     Cmb_Est_Cargo AT ROW 11.85 COL 15 COLON-ALIGNED
     Clientes.Tip_Contrato AT ROW 12.88 COL 17.29 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Término Indefinido", 1,
"Término Fijo", 2
          SIZE 66.72 BY .81
          BGCOLOR 18 FONT 5
     Anexos_Clientes.Nit_Independiente AT ROW 14.73 COL 6.29 COLON-ALIGNED
          LABEL "Nit" FORMAT "9(5)x(7)"
          VIEW-AS FILL-IN 
          SIZE 28.43 BY .81
          BGCOLOR 18 
     Anexos_Clientes.Tiempo_Actividad AT ROW 14.77 COL 51.57 COLON-ALIGNED
          LABEL "Tiempo Actividad"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .81
          BGCOLOR 18 
     W_NomCiiu AT ROW 15.77 COL 7 COLON-ALIGNED WIDGET-ID 30
     Btn_Ciiu AT ROW 15.85 COL 48 WIDGET-ID 20
     Clientes.Codigo_CIIU AT ROW 4.81 COL 92.14 NO-LABEL WIDGET-ID 268
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Grupo AT ROW 4.81 COL 92.57 COLON-ALIGNED NO-LABEL WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Subgrupo AT ROW 4.81 COL 95 COLON-ALIGNED NO-LABEL WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Anexos_Clientes.Fec_Pensionado AT ROW 14.96 COL 83 COLON-ALIGNED WIDGET-ID 278
          LABEL "Pensionado Desde"
          VIEW-AS FILL-IN 
          SIZE 10.57 BY .81
          BGCOLOR 18 
     Btn_DireccionComercial AT ROW 7.42 COL 94 WIDGET-ID 290
     Anexos_Clientes.Ind_Comercial AT ROW 9.04 COL 18 COLON-ALIGNED WIDGET-ID 294
          LABEL "Teléfono"
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .81
          BGCOLOR 18 
     Btn_Ciiu-2 AT ROW 4.77 COL 48 WIDGET-ID 300
     W_NomCiiu-2 AT ROW 4.62 COL 19 COLON-ALIGNED WIDGET-ID 302
     W_NomCiiu-3 AT ROW 3.77 COL 19 COLON-ALIGNED WIDGET-ID 304
     Btn_Ciiu-3 AT ROW 3.92 COL 48 WIDGET-ID 306
     Clientes.cod_actividad AT ROW 4.81 COL 77.57 COLON-ALIGNED NO-LABEL WIDGET-ID 308
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
     Clientes.Id_Micro AT ROW 2.92 COL 19 COLON-ALIGNED WIDGET-ID 310
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .81
          BGCOLOR 15 
     Cmb_Situacion_Actual AT ROW 2.85 COL 65 COLON-ALIGNED WIDGET-ID 312
     Clientes.Fuerza_Mayor AT ROW 3.77 COL 65 COLON-ALIGNED WIDGET-ID 314
          VIEW-AS FILL-IN 
          SIZE 3 BY .81
          BGCOLOR 15 
     Clientes.Situacion_Actual AT ROW 4.81 COL 85.29 COLON-ALIGNED NO-LABEL WIDGET-ID 316
          VIEW-AS FILL-IN 
          SIZE 2 BY .5
          BGCOLOR 17 FGCOLOR 17 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.27
         SIZE 115.57 BY 16.96
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Ubicacion
     FacultadDepartamento AT ROW 6.54 COL 18 COLON-ALIGNED WIDGET-ID 320
     btnFacultadDpto AT ROW 6.62 COL 94 WIDGET-ID 322
     "Declara Renta" VIEW-AS TEXT
          SIZE 12.72 BY .81 TOOLTIP "Declara Renta" AT ROW 1.15 COL 47.29 WIDGET-ID 6
          FGCOLOR 7 FONT 5
     "Independiente o Empleado Socio" VIEW-AS TEXT
          SIZE 29 BY .69 AT ROW 14.04 COL 5.29 WIDGET-ID 270
          FGCOLOR 7 FONT 5
     " Exento" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 1.15 COL 4
          FGCOLOR 7 FONT 5
     "Pensionado" VIEW-AS TEXT
          SIZE 10.57 BY .81 AT ROW 14 COL 68.43 WIDGET-ID 276
          FGCOLOR 7 FONT 5
     "Contrato:" VIEW-AS TEXT
          SIZE 8.57 BY .81 AT ROW 12.85 COL 8.72 WIDGET-ID 60
          FGCOLOR 7 FONT 5
     "Empleado" VIEW-AS TEXT
          SIZE 10.14 BY .69 AT ROW 10.38 COL 4.72
          FGCOLOR 7 FONT 5
     " Administra Recursos Públicos" VIEW-AS TEXT
          SIZE 27 BY .81 AT ROW 1.19 COL 68 WIDGET-ID 4
          FGCOLOR 7 FONT 5
     RECT-235 AT ROW 10.65 COL 3 WIDGET-ID 258
     RECT-217 AT ROW 14.42 COL 3 WIDGET-ID 50
     RECT-236 AT ROW 14.54 COL 67 WIDGET-ID 274
     RECT-319 AT ROW 5.58 COL 3 WIDGET-ID 318
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.43 ROW 8.27
         SIZE 115.57 BY 16.96
         BGCOLOR 17 FONT 5
         TITLE "3. INFORMACION ACTIVIDAD ECONÓMICA".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Afiliaciones"
         HEIGHT             = 24.31
         WIDTH              = 130.72
         MAX-HEIGHT         = 36.54
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 36.54
         VIRTUAL-WIDTH      = 182.86
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
ASSIGN FRAME FRelNva:FRAME = FRAME F_Relaciones:HANDLE
       FRAME F_Autorizaciones:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Conyuge:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Documentacion:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Economica:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Falta:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Juridicas:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Microempresas:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Otros:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Relaciones:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Segmentacion:FRAME = FRAME F_Clientes:HANDLE
       FRAME F_Ubicacion:FRAME = FRAME F_Clientes:HANDLE.

/* SETTINGS FOR FRAME FRelNva
   NOT-VISIBLE L-To-R                                                   */
ASSIGN 
       FRAME FRelNva:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN R_Ciu_Dpt IN FRAME FRelNva
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN R_Direccion IN FRAME FRelNva
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Aportes
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_Aportes:HIDDEN           = TRUE
       FRAME F_Aportes:MOVABLE          = TRUE.

/* SETTINGS FOR EDITOR Edit_msaje IN FRAME F_Aportes
   NO-ENABLE                                                            */
ASSIGN 
       Edit_msaje:READ-ONLY IN FRAME F_Aportes        = TRUE.

/* SETTINGS FOR FRAME F_Autorizaciones
                                                                        */
ASSIGN 
       FRAME F_Autorizaciones:PRIVATE-DATA     = 
                "08 DECLARACION DE CONOCIMIENTO  Y AUTORIZACIONES".

/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Autorizaciones[10] IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Autorizaciones[11] IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Autorizaciones[12] IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Autorizaciones[1] IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Autorizaciones[2] IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Autorizaciones[3] IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Autorizaciones[4] IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Autorizaciones[5] IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Autorizaciones[6] IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Autorizaciones[7] IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Autorizaciones[8] IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Autorizaciones[9] IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Declaracion_aceptacion IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Declaracion_Conocimiento IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Fec_Diligenciamiento IN FRAME F_Autorizaciones
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Proviene IN FRAME F_Autorizaciones
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       Anexos_Clientes.Proviene:PRIVATE-DATA IN FRAME F_Autorizaciones     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Anexos_Clientes.Represetacion IN FRAME F_Autorizaciones
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FRAME F_Clientes
   Custom                                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Microempresas:MOVE-BEFORE-TAB-ITEM (Clientes.Nombre:HANDLE IN FRAME F_Clientes)
       XXTABVALXX = FRAME F_Relaciones:MOVE-BEFORE-TAB-ITEM (FRAME F_Microempresas:HANDLE)
       XXTABVALXX = FRAME F_Documentacion:MOVE-BEFORE-TAB-ITEM (FRAME F_Relaciones:HANDLE)
       XXTABVALXX = FRAME F_Otros:MOVE-BEFORE-TAB-ITEM (FRAME F_Documentacion:HANDLE)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME F_Clientes:PRIVATE-DATA     = 
                "00) Identificación Cliente".

/* SETTINGS FOR FILL-IN Clientes.Apellido1 IN FRAME F_Clientes
   EXP-LABEL                                                            */
ASSIGN 
       Clientes.Apellido1:PRIVATE-DATA IN FRAME F_Clientes     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Apellido2 IN FRAME F_Clientes
   EXP-LABEL                                                            */
/* SETTINGS FOR BUTTON Btn_Borrar IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Deshacer IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-207 IN FRAME F_Clientes
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-207:HIDDEN IN FRAME F_Clientes           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Agencia IN FRAME F_Clientes
   NO-ENABLE                                                            */
ASSIGN 
       Cmb_Agencia:PRIVATE-DATA IN FRAME F_Clientes     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.dv IN FRAME F_Clientes
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET Clientes.Estado IN FRAME F_Clientes
   NO-ENABLE                                                            */
ASSIGN 
       Clientes.Estado:PRIVATE-DATA IN FRAME F_Clientes     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Nit IN FRAME F_Clientes
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       Clientes.Nit:PRIVATE-DATA IN FRAME F_Clientes     = 
                "OBLIGATORIO".

ASSIGN 
       RSeleccion:PRIVATE-DATA IN FRAME F_Clientes     = 
                "OBLIGATORIO".

ASSIGN 
       Clientes.Tipo_Cliente:PRIVATE-DATA IN FRAME F_Clientes     = 
                "OBLIGATORIO".

/* SETTINGS FOR COMBO-BOX Clientes.Tipo_Identificacion IN FRAME F_Clientes
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       Clientes.Tipo_Identificacion:PRIVATE-DATA IN FRAME F_Clientes     = 
                "OBLIGATORIO".

/* SETTINGS FOR FRAME F_Conyuge
   Custom                                                               */
ASSIGN 
       FRAME F_Conyuge:HIDDEN           = TRUE
       FRAME F_Conyuge:PRIVATE-DATA     = 
                "02 Información Cónyuge o Compañero (a) permanente".

ASSIGN 
       CnygeExtncion:PRIVATE-DATA IN FRAME F_Conyuge     = 
                "22 abril 2008, Se amplió a 6 dígitos por solicitud del usuario.".

/* SETTINGS FOR FILL-IN R_Cod_Profesion IN FRAME F_Conyuge
   NO-ENABLE LIKE = bdcentral.Clientes.Cod_Profesion EXP-SIZE           */
/* SETTINGS FOR FILL-IN R_Direccion IN FRAME F_Conyuge
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN R_Dir_Empresa IN FRAME F_Conyuge
   NO-ENABLE ALIGN-L LIKE = bdcentral.Clientes.Dir_comercial EXP-SIZE   */
/* SETTINGS FOR FILL-IN R_Fec_Nacimiento IN FRAME F_Conyuge
   LIKE = bdcentral.Clientes.Fec_Nacimiento EXP-SIZE                    */
/* SETTINGS FOR FILL-IN R_Lugar_Nacimiento IN FRAME F_Conyuge
   NO-ENABLE LIKE = bdcentral.Clientes.Lugar_Nacimiento EXP-SIZE        */
/* SETTINGS FOR FILL-IN R_Nacionalidad IN FRAME F_Conyuge
   ALIGN-R                                                              */
/* SETTINGS FOR COMBO-BOX R_Niv_Educativo IN FRAME F_Conyuge
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX R_Ocupacion IN FRAME F_Conyuge
   ALIGN-L                                                              */
/* SETTINGS FOR RADIO-SET R_Sexo IN FRAME F_Conyuge
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN R_TEgresos IN FRAME F_Conyuge
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN R_TIngreso IN FRAME F_Conyuge
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN WR_CiuEmpresa IN FRAME F_Conyuge
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN WR_CiuNacimiento IN FRAME F_Conyuge
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN WR_NomProfesion IN FRAME F_Conyuge
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Documentacion
                                                                        */
ASSIGN 
       FRAME F_Documentacion:PRIVATE-DATA     = 
                "08 DOCUMENTACION REQUERIDA".

/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[10] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[11] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[12] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[13] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[14] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[15] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[16] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[17] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[18] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[19] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[1] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[2] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[3] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[4] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[5] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[6] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[7] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[8] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Docum_Requer[9] IN FRAME F_Documentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME F_Economica
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_Economica:HIDDEN           = TRUE
       FRAME F_Economica:PRIVATE-DATA     = 
                "04 INFORMACIÓN FINANCIERA".

/* SETTINGS FOR FILL-IN Clientes.Act_casa IN FRAME F_Economica
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Act_inversion IN FRAME F_Economica
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Clientes.Act_vehiculo IN FRAME F_Economica
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Ciudad_Pais_Bco_Extranjero IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Especifique_OtrosIng IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Gto_Arriendo IN FRAME F_Economica
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Gto_Familiar IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       Clientes.Gto_Familiar:PRIVATE-DATA IN FRAME F_Economica     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Gto_obligacion IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Gto_Otros IN FRAME F_Economica
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Gto_TargetaCredito IN FRAME F_Economica
   ALIGN-L EXP-LABEL EXP-FORMAT                                         */
/* SETTINGS FOR FILL-IN Clientes.Ing_arriendos IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Ing_financieros IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Ing_Honorarios IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Ing_Otros IN FRAME F_Economica
   EXP-FORMAT                                                           */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Maneja_Cta_Extanjera IN FRAME F_Economica
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Nom_Banco_extranjero IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Num_cta_extranjera IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Otra_Transac_extr IN FRAME F_Economica
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Clientes.Salario IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       Clientes.Salario:PRIVATE-DATA IN FRAME F_Economica     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Sdo_Obligaciones IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Tipo_Moneda_Divisa IN FRAME F_Economica
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Tot_Activos IN FRAME F_Economica
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tot_Egresos IN FRAME F_Economica
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Tot_Ingresos IN FRAME F_Economica
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Val_Pasivos IN FRAME F_Economica
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FRAME F_Falta
   NOT-VISIBLE                                                          */
/* BROWSE-TAB B_Falta TEXT-37 F_Falta */
ASSIGN 
       FRAME F_Falta:HIDDEN           = TRUE
       FRAME F_Falta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Juridicas
   Custom                                                               */
ASSIGN 
       FRAME F_Juridicas:HIDDEN           = TRUE
       FRAME F_Juridicas:PRIVATE-DATA     = 
                "03 INFORMACION ACTIVIDAD ECONÓMICA".

/* SETTINGS FOR FILL-IN Anexos_Clientes.AA_Cliente IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Acti_Economica_emp IN FRAME F_Juridicas
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Clientes.Act_vehiculo IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Celular IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Ciudad_Pais_Bco_Extranjero IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.cod_actividad IN FRAME F_Juridicas
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Clientes.Dir_Residencia IN FRAME F_Juridicas
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Email IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Especifique_OtrosIng IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Fec_expedicion IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Lugar_Residencia IN FRAME F_Juridicas
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Nom_Banco_extranjero IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Num_cta_extranjera IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Otra_Transac_extr IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Salario IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Sdo_Obligaciones IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Tel_Arrendatario IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Tel_Residencia IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX Clientes.Tipo_Empresa IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Tipo_Moneda_Divisa IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Val_Pasivos IN FRAME F_Juridicas
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN W_Nom_Act_Ppal IN FRAME F_Juridicas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Nom_Ciu_Emp IN FRAME F_Juridicas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Ubi_Empresa IN FRAME F_Juridicas
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Microempresas
                                                                        */
ASSIGN 
       FRAME F_Microempresas:HIDDEN           = TRUE
       FRAME F_Microempresas:PRIVATE-DATA     = 
                "03 INFORMACION ACTIVIDAD ECONÓMICA".

/* SETTINGS FOR FILL-IN MicroEmpresas.Codigo_CIIU IN FRAME F_Microempresas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN MicroEmpresas.cod_actividad IN FRAME F_Microempresas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN MicroEmpresas.DIRECCION IN FRAME F_Microempresas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Actividad IN FRAME F_Microempresas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom_Ubicacion IN FRAME F_Microempresas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN MicroEmpresas.UBICACION IN FRAME F_Microempresas
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME F_Otros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Otros:HIDDEN           = TRUE
       FRAME F_Otros:PRIVATE-DATA     = 
                "09 OTROS".

ASSIGN 
       Clientes.Aut_CentralRiesgo:PRIVATE-DATA IN FRAME F_Otros     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Cod_Ingreso IN FRAME F_Otros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Cod_Retiro IN FRAME F_Otros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Cod_Segmento IN FRAME F_Otros
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Clientes.Con_Sospechosas IN FRAME F_Otros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Fec_Calificacion IN FRAME F_Otros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Fec_fallecido IN FRAME F_Otros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Fec_Ingreso IN FRAME F_Otros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Fec_IniSancion IN FRAME F_Otros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Fec_Retiro IN FRAME F_Otros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Clientes.Fec_UltActualiza IN FRAME F_Otros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Fec_Ult_Act[1] IN FRAME F_Otros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Fec_Ult_Act[2] IN FRAME F_Otros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Fec_Ult_Act[3] IN FRAME F_Otros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Fec_Ult_Act[4] IN FRAME F_Otros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Fec_Ult_Act[5] IN FRAME F_Otros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Fec_Ult_Act[6] IN FRAME F_Otros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Clientes.Fec_Ult_Act[7] IN FRAME F_Otros
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX Clientes.id_AsistioAsamblea IN FRAME F_Otros
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX Clientes.Id_Preexistentes IN FRAME F_Otros
   EXP-LABEL                                                            */
/* SETTINGS FOR RADIO-SET Clientes.Id_Privilegiado IN FRAME F_Otros
   EXP-HELP                                                             */
/* SETTINGS FOR TOGGLE-BOX Clientes.Id_PuedeCodeudar IN FRAME F_Otros
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR TOGGLE-BOX Clientes.Reportado_fiscalia IN FRAME F_Otros
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Clientes.Reportado_Procredito IN FRAME F_Otros
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Usuario IN FRAME F_Otros
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN W_NomRetiro IN FRAME F_Otros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomSegmento IN FRAME F_Otros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomUsuario IN FRAME F_Otros
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Relaciones
   NOT-VISIBLE                                                          */
ASSIGN XXTABVALXX = FRAME FRelNva:MOVE-AFTER-TAB-ITEM (Btn_CanRel:HANDLE IN FRAME F_Relaciones)
       XXTABVALXX = FRAME FRelNva:MOVE-BEFORE-TAB-ITEM (Btn_Activas:HANDLE IN FRAME F_Relaciones)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB Br_Relaciones Btn_Activas F_Relaciones */
ASSIGN 
       FRAME F_Relaciones:HIDDEN           = TRUE
       FRAME F_Relaciones:PRIVATE-DATA     = 
                "05 REFERENCIAS".

/* SETTINGS FOR BUTTON Btn_CanRel IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_SalRel IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Relaciones IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_MenRel IN FRAME F_Relaciones
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Segmentacion
   L-To-R                                                               */
ASSIGN 
       FRAME F_Segmentacion:HIDDEN           = TRUE
       FRAME F_Segmentacion:PRIVATE-DATA     = 
                "02 Información Cónyuge o Compañero (a) permanente".

/* SETTINGS FOR FILL-IN Clientes.Celular IN FRAME F_Segmentacion
   EXP-FORMAT                                                           */
ASSIGN 
       Clientes.Celular:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Cod_Profesion IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
ASSIGN 
       Clientes.Cod_Profesion:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Colegio IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Dir_Ant_Residencia IN FRAME F_Segmentacion
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Dir_Arrendatario IN FRAME F_Segmentacion
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       Anexos_Clientes.Dir_Correspondencia:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Dir_Residencia IN FRAME F_Segmentacion
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       Clientes.Dir_Residencia:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Email IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX Clientes.Estrato IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
ASSIGN 
       Clientes.Estrato:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

ASSIGN 
       Clientes.Est_Civil:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Fec_expedicion IN FRAME F_Segmentacion
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       Clientes.Fec_expedicion:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Fec_Nacimiento IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
ASSIGN 
       Clientes.Fec_Nacimiento:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Anexos_Clientes.Ind_Arr IN FRAME F_Segmentacion
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Ind_cli IN FRAME F_Segmentacion
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       Anexos_Clientes.Ind_cli:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Lugar_expedicion IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
ASSIGN 
       Clientes.Lugar_expedicion:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Lugar_Nacimiento IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
ASSIGN 
       Clientes.Lugar_Nacimiento:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Lugar_Residencia IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
ASSIGN 
       Clientes.Lugar_Residencia:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Anexos_Clientes.Nacionalidad IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
ASSIGN 
       Anexos_Clientes.Nacionalidad:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR COMBO-BOX Clientes.Niv_Educativo IN FRAME F_Segmentacion
   ALIGN-L EXP-FORMAT                                                   */
ASSIGN 
       Clientes.Niv_Educativo:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR COMBO-BOX Clientes.Niv_EduCop IN FRAME F_Segmentacion
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Clientes.Nom_Arrendatario IN FRAME F_Segmentacion
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Nom_Colegio IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Clientes.Num_Hijos IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX Anexos_Clientes.Num_Hijos_11 IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX Anexos_Clientes.Num_Hijos_18 IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX Clientes.Per_Acargo IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Per_Anos IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Per_Ant_Anos IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Per_Ant_Meses IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Per_Meses IN FRAME F_Segmentacion
   EXP-LABEL                                                            */
ASSIGN 
       Clientes.Sexo:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Tel_Arrendatario IN FRAME F_Segmentacion
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Clientes.Tel_Residencia IN FRAME F_Segmentacion
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       Clientes.Tipo_Vinculo:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Anexos_Clientes.Val_arriendo IN FRAME F_Segmentacion
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN W_CiuExpedicion IN FRAME F_Segmentacion
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       W_CiuExpedicion:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN W_NomProfesion IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
ASSIGN 
       W_NomProfesion:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN W_UbicacionResidencia IN FRAME F_Segmentacion
   NO-ENABLE                                                            */
ASSIGN 
       W_UbicacionResidencia:PRIVATE-DATA IN FRAME F_Segmentacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FRAME F_Ubicacion
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_Ubicacion:HIDDEN           = TRUE
       FRAME F_Ubicacion:PRIVATE-DATA     = 
                "03 INFORMACION ACTIVIDAD ECONÓMICA".

/* SETTINGS FOR FILL-IN Anexos_Clientes.Acti_Economica_emp IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE EXP-LABEL EXP-FORMAT                            */
ASSIGN 
       Anexos_Clientes.Acti_Economica_emp:PRIVATE-DATA IN FRAME F_Ubicacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Adm_Recursos_Publ IN FRAME F_Ubicacion
   EXP-LABEL                                                            */
/* SETTINGS FOR BUTTON Btn_Ciiu-2 IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Anexos_Clientes.Cam_int1 IN FRAME F_Ubicacion
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN Clientes.Carnet IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE EXP-LABEL                                       */
ASSIGN 
       Clientes.Carnet:HIDDEN IN FRAME F_Ubicacion           = TRUE.

ASSIGN 
       Cmb_Est_Cargo:PRIVATE-DATA IN FRAME F_Ubicacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR COMBO-BOX Cmb_TipAct IN FRAME F_Ubicacion
   ALIGN-L                                                              */
ASSIGN 
       Cmb_TipAct:PRIVATE-DATA IN FRAME F_Ubicacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Codigo_CIIU IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       Clientes.Codigo_CIIU:PRIVATE-DATA IN FRAME F_Ubicacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.cod_actividad IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Clientes.Cod_Cargo IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Clientes.Cod_Cargo:PRIVATE-DATA IN FRAME F_Ubicacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Clientes.Cod_Empresa IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE EXP-LABEL                                       */
ASSIGN 
       Clientes.Cod_Empresa:PRIVATE-DATA IN FRAME F_Ubicacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Declara_Renta IN FRAME F_Ubicacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.departamento IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Clientes.Dir_comercial IN FRAME F_Ubicacion
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Especifique_Ocupacion IN FRAME F_Ubicacion
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Exento_GMF IN FRAME F_Ubicacion
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX Anexos_Clientes.Exento_retencion IN FRAME F_Ubicacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Extencion_Comercial IN FRAME F_Ubicacion
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.facultad IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FacultadDepartamento IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
ASSIGN 
       FacultadDepartamento:PRIVATE-DATA IN FRAME F_Ubicacion     = 
                "OBLIGATORIO".

ASSIGN 
       Clientes.Fec_IngEmpresa:PRIVATE-DATA IN FRAME F_Ubicacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Anexos_Clientes.Fec_Pensionado IN FRAME F_Ubicacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Grupo IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Ind_Comercial IN FRAME F_Ubicacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Clientes.Lugar_comercial IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Clientes.Lugar_comercial:PRIVATE-DATA IN FRAME F_Ubicacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN Anexos_Clientes.Nit_Independiente IN FRAME F_Ubicacion
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Clientes.Situacion_Actual IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Clientes.Subgrupo IN FRAME F_Ubicacion
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN Clientes.Tel_comercial IN FRAME F_Ubicacion
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Tel_Fax_Comercial IN FRAME F_Ubicacion
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Anexos_Clientes.Tiempo_Actividad IN FRAME F_Ubicacion
   EXP-LABEL                                                            */
ASSIGN 
       Clientes.Tip_Contrato:PRIVATE-DATA IN FRAME F_Ubicacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN W_NomCargo IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCiiu IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCiiu-2 IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCiiu-3 IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomEmpresa IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
ASSIGN 
       W_NomEmpresa:PRIVATE-DATA IN FRAME F_Ubicacion     = 
                "OBLIGATORIO".

/* SETTINGS FOR FILL-IN W_UbicacionComercial IN FRAME F_Ubicacion
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Relaciones
/* Query rebuild information for BROWSE Br_Relaciones
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Relaciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Falta
/* Query rebuild information for BROWSE B_Falta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TFalta NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Falta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRelNva
/* Query rebuild information for FRAME FRelNva
     _Query            is NOT OPENED
*/  /* FRAME FRelNva */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Aportes
/* Query rebuild information for FRAME F_Aportes
     _Query            is NOT OPENED
*/  /* FRAME F_Aportes */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Autorizaciones
/* Query rebuild information for FRAME F_Autorizaciones
     _Query            is NOT OPENED
*/  /* FRAME F_Autorizaciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Clientes
/* Query rebuild information for FRAME F_Clientes
     _TblList          = "bdcentral.Anexos_Clientes"
     _Query            is NOT OPENED
*/  /* FRAME F_Clientes */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Conyuge
/* Query rebuild information for FRAME F_Conyuge
     _TblList          = "bdcentral.Clientes,bdcentral.Anexos_Clientes OF bdcentral.Clientes"
     _Options          = "NO-LOCK"
     _TblOptList       = ","
     _Query            is OPENED
*/  /* FRAME F_Conyuge */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Documentacion
/* Query rebuild information for FRAME F_Documentacion
     _Query            is NOT OPENED
*/  /* FRAME F_Documentacion */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Economica
/* Query rebuild information for FRAME F_Economica
     _Query            is NOT OPENED
*/  /* FRAME F_Economica */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Falta
/* Query rebuild information for FRAME F_Falta
     _Query            is NOT OPENED
*/  /* FRAME F_Falta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Juridicas
/* Query rebuild information for FRAME F_Juridicas
     _TblList          = "bdcentral.Clientes"
     _Query            is OPENED
*/  /* FRAME F_Juridicas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Microempresas
/* Query rebuild information for FRAME F_Microempresas
     _TblList          = "bdcentral.MicroEmpresas"
     _Query            is OPENED
*/  /* FRAME F_Microempresas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Otros
/* Query rebuild information for FRAME F_Otros
     _Query            is NOT OPENED
*/  /* FRAME F_Otros */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Relaciones
/* Query rebuild information for FRAME F_Relaciones
     _Query            is NOT OPENED
*/  /* FRAME F_Relaciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Segmentacion
/* Query rebuild information for FRAME F_Segmentacion
     _Query            is NOT OPENED
*/  /* FRAME F_Segmentacion */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Ubicacion
/* Query rebuild information for FRAME F_Ubicacion
     _Query            is NOT OPENED
*/  /* FRAME F_Ubicacion */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME F_Clientes:HANDLE
       ROW             = 1
       COLUMN          = 1
       HEIGHT          = 5.65
       WIDTH           = .14
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {93330F00-7CA6-101B-874B-0020AF109266} type: CSComboBox */
      CtrlFrame:MOVE-AFTER(Clientes.Nombre:HANDLE IN FRAME F_Clientes).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Afiliaciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Afiliaciones */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Segmentacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Segmentacion wWin
ON LEAVE OF FRAME F_Segmentacion /* 2. INFORMACIÓN PERSONAL */
DO:
  IF Cliente.Est_Civil:SCREEN-VALUE = "Casado" OR cliente.Est_Civil:SCREEN-VALUE = "Union Libre"
    THEN DO:

    FIND FIRST CClientes WHERE CClientes.nit = Cliente.nit:SCREEN-VALUE IN FRAME F_Clientes NO-LOCK NO-ERROR.
  END.
  ELSE DO: 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Anexos_Clientes.AA_Cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.AA_Cliente wWin
ON LEAVE OF Anexos_Clientes.AA_Cliente IN FRAME F_Segmentacion /* AA */
DO:
  Anexos_Clientes.AA_Cliente:BGCOLOR = 18.
  Anexos_Clientes.AA_Cliente:FGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Anexos_Clientes.Acti_Economica_emp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Acti_Economica_emp wWin
ON ANY-PRINTABLE OF Anexos_Clientes.Acti_Economica_emp IN FRAME F_Ubicacion /* Actividad Económica */
DO:
    DEF VAR cCrctresEspcialesPrmtdos AS CHAR NO-UNDO.
    cCrctresEspcialesPrmtdos = "-".
    DEF VAR cAlfbto AS CHAR NO-UNDO.
    cAlfbto = "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z".
    DEF VAR cDgtos AS CHAR NO-UNDO.
    cDgtos = "0,1,2,3,4,5,6,7,8,9".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Acti_Economica_emp wWin
ON LEAVE OF Anexos_Clientes.Acti_Economica_emp IN FRAME F_Ubicacion /* Actividad Económica */
DO:
  Anexos_Clientes.Acti_Economica_Emp:BGCOLOR = 18.
  Anexos_Clientes.Acti_Economica_Emp:FGCOLOR = 15.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Clientes.Act_casa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Act_casa wWin
ON LEAVE OF Clientes.Act_casa IN FRAME F_Economica /* Valor Propiedad */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Act_casa wWin
ON VALUE-CHANGED OF Clientes.Act_casa IN FRAME F_Economica /* Valor Propiedad */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Act_inversion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Act_inversion wWin
ON LEAVE OF Clientes.Act_inversion IN FRAME F_Economica /* Otros Activos */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Act_inversion wWin
ON VALUE-CHANGED OF Clientes.Act_inversion IN FRAME F_Economica /* Otros Activos */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Act_vehiculo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Act_vehiculo wWin
ON LEAVE OF Clientes.Act_vehiculo IN FRAME F_Economica /* Valor Vehículo */
DO:
  RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Act_vehiculo wWin
ON VALUE-CHANGED OF Clientes.Act_vehiculo IN FRAME F_Economica /* Valor Vehículo */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Clientes.Apellido1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Apellido1 wWin
ON LEAVE OF Clientes.Apellido1 IN FRAME F_Clientes /* Apellido 1 */
DO:
    IF LENGTH(trim(SELF:SCREEN-VALUE)) = 0 THEN DO:
       MESSAGE "Nombre Incorrecto"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN NO-APPLY.
    END.
    ASSIGN SELF:SCREEN-VALUE IN FRAME F_Clientes = CAPS(SELF:SCREEN-VALUE IN FRAME F_Clientes).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Apellido1 wWin
ON VALUE-CHANGED OF Clientes.Apellido1 IN FRAME F_Clientes /* Apellido 1 */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Apellido2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Apellido2 wWin
ON LEAVE OF Clientes.Apellido2 IN FRAME F_Clientes /* Apellido 2 */
DO:
  ASSIGN SELF:SCREEN-VALUE IN FRAME F_Clientes = CAPS(SELF:SCREEN-VALUE IN FRAME F_Clientes).
         
/*FIND FIRST ListaNegra WHERE 
       ListaNegra.Apellido1 EQ Ap1LN AND
       ListaNegra.Apellido2 EQ Ap2LN NO-LOCK NO-ERROR.
  IF AVAILABLE ListaNegra THEN DO:
     MESSAGE "Este nombre se encuentra matriculado en" SKIP
             "Las listas negras de la cooperativa." SKIP
             "identificado con el número: " Listanegra.Nit SKIP(1)
             "La operación se cancela." SKIP
             "Reporte este incidente al deparamento de CI" 
             VIEW-AS ALERT-BOX WARNING TITLE "Lista Negra".
     APPLY "choose" TO Btn_Cancelar.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Apellido2 wWin
ON VALUE-CHANGED OF Clientes.Apellido2 IN FRAME F_Clientes /* Apellido 2 */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Aut_CentralRiesgo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Aut_CentralRiesgo wWin
ON VALUE-CHANGED OF Clientes.Aut_CentralRiesgo IN FRAME F_Otros /* Autoriza Consulta C.Riesgo */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME btnFacultadDpto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFacultadDpto wWin
ON CHOOSE OF btnFacultadDpto IN FRAME F_Ubicacion /* Btn_empresa 2 */
DO:
    DEFINE VAR vAgencia AS INTEGER.

    ENABLE Btn_Salvar
           Btn_Deshacer
        WITH FRAME F_Clientes.

    ASSIGN WWin:SENSITIVE = FALSE.

    RUN C-Facultades.r (OUTPUT codAgencia,
                        OUTPUT codFacultad).

    ASSIGN WWin:SENSITIVE = TRUE.

    WWin:MOVE-TO-TOP().

    vAgencia = INTEGER(SUBSTRING(cmb_agencia:SCREEN-VALUE,1,3)).

    IF codFacultad <> "" THEN DO:
        FIND FIRST facultades WHERE facultades.agencia = vAgencia
                                AND facultades.codigo = SUBSTRING(codFacultad,1,2)
                                AND facultades.tipo = "F" NO-LOCK NO-ERROR.
        IF AVAILABLE facultades THEN DO:
            FacultadDepartamento:SCREEN-VALUE = facultades.nombre.
            clientes.facultad:SCREEN-VALUE = SUBSTRING(codFacultad,1,2).

            FIND FIRST facultades WHERE facultades.agencia = vAgencia
                                    AND facultades.codigo = codFacultad
                                    AND facultades.tipo = "D" NO-LOCK NO-ERROR.
            IF AVAILABLE facultades THEN DO:
                facultadDepartamento:SCREEN-VALUE = facultadDepartamento:SCREEN-VALUE + " - " + facultades.nombre.
                clientes.departamento:SCREEN-VALUE = SUBSTRING(codFacultad,3,3).
            END.
        END.
        ELSE DO:
            MESSAGE "Esta Facultad/Departamento no está disponible para la agencia en la cual se encuentra" SKIP
                    "matriculado el Asociado. Revise por favor"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.

    facultadDepartamento:BGCOLOR = 18.
    facultadDepartamento:FGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_Activas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Activas wWin
ON CHOOSE OF Btn_Activas IN FRAME F_Relaciones /* Borrar */
DO:
  DO WITH FRAME F_Relaciones:
  W_MenRel:SCREEN-VALUE = "Escoja del Combo de Relaciones, el tipo de relación que quiere consultar".
  IF Br_Relaciones:NUM-SELECTED-ROWS EQ 0 THEN DO:
     MESSAGE "Debe posicionarse en la relación a Borrar" SKIP
             "mediante el mouse. Rectifique!!!" VIEW-AS ALERT-BOX INFORMATION.
  END.
  ELSE
  DO:
    FIND Relaciones WHERE Relaciones.Nit EQ Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes AND
                          Relaciones.Cod_Relacion EQ INTEGER(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5)) AND
                          Relaciones.Nit_Relacion EQ T_Relaciones.R_NitObjeto NO-ERROR.
    IF AVAILABLE(Relaciones) THEN DO:
       IF RActivas:SCREEN-VALUE EQ "1" THEN
          ASSIGN Relaciones.Estado = 2
                 Relaciones.Fec_Inactividad = W_Fecha.
       ELSE
          ASSIGN Relaciones.Estado = 1
                 Relaciones.Fec_Inactividad = ?.
    END.
    APPLY 'value-changed' TO Cmb_Relaciones.
    RETURN NO-APPLY.
  END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Microempresas
&Scoped-define SELF-NAME Btn_Actividad_Micro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Actividad_Micro wWin
ON CHOOSE OF Btn_Actividad_Micro IN FRAME F_Microempresas /* Btn_Actividad */
DO:
    ASSIGN WWin:SENSITIVE = FALSE.
    RUN C-Varios.r (INPUT 35, OUTPUT V_Cod, OUTPUT V_Nom).
    ASSIGN WWin:SENSITIVE = TRUE.
    WWin:MOVE-TO-TOP().

    IF V_Cod EQ 0 THEN
       MESSAGE "No se ha elegido ninguna Actividad Permanente" VIEW-AS ALERT-BOX.
    ELSE DO:
       ASSIGN Nom_Actividad:SCREEN-VALUE = V_Nom
              MicroEmpresas.Cod_Actividad:SCREEN-VALUE IN FRAME F_MicroEmpresas = STRING(V_Cod).  
       FIND Varios WHERE Varios.tipo = 35 AND Varios.Codigo = v_cod NO-LOCK NO-ERROR.
       IF AVAIL Varios THEN DO:
          FIND FIRST Ciiu WHERE Ciiu.Codigo_Ciiu = Varios.Val_Inicial NO-LOCK NO-ERROR.
          IF AVAIL Ciiu THEN DO:
             ASSIGN MicroEmpresas.Codigo_Ciiu:SCREEN-VALUE = 
                    STRING(Ciiu.grupo) + "-" + STRING(Ciiu.subgrupo)
                                       + "-" + STRING(Ciiu.Codigo_Ciiu)
                    Nom_Ciiu:SCREEN-VALUE = Ciiu.Descripcion.
          END.
          ELSE ASSIGN Nom_Ciiu:SCREEN-VALUE = STRING(Varios.Val_Inicial)
                      MicroEmpresas.Codigo_Ciiu:SCREEN-VALUE = "".
       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Juridicas
&Scoped-define SELF-NAME Btn_Act_Ppal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Act_Ppal wWin
ON CHOOSE OF Btn_Act_Ppal IN FRAME F_Juridicas /* Btn_Act_Ppal */
DO:
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
    ASSIGN WWin:SENSITIVE = FALSE.
    RUN C-Varios.r (INPUT 35, OUTPUT V_Cod, OUTPUT V_Nom).
    ASSIGN WWin:SENSITIVE = TRUE.
    WWin:MOVE-TO-TOP().
    IF V_Cod EQ 0 THEN
       MESSAGE "No se ha elegido ninguna Actividad Permanente" VIEW-AS ALERT-BOX.
    ELSE DO:
       ASSIGN W_Nom_Act_Ppal:SCREEN-VALUE = V_Nom
              Clientes.Cod_Actividad:SCREEN-VALUE IN FRAME F_Juridicas = STRING(V_Cod).  
       FIND Varios WHERE Varios.tipo = 35 AND Varios.Codigo = v_cod NO-LOCK NO-ERROR.
       IF AVAIL Varios THEN DO:
          FIND FIRST Ciiu WHERE Ciiu.Codigo_Ciiu = Varios.Val_Inicial NO-LOCK NO-ERROR.
          IF AVAIL Ciiu THEN DO:
             ASSIGN Anexos_Clientes.Acti_Economica_Emp:SCREEN-VALUE = STRING(Ciiu.Codigo_Ciiu)
           /*STRING(Ciiu.Grupo) + "-" + STRING(Ciiu.subGrupo) + "-" + STRING(Ciiu.Codigo_Ciiu)*/
                    W_Nom_Ciu_Emp:SCREEN-VALUE = Ciiu.Descripcion.
          END.
          ELSE DO:
             MESSAGE "Ciiu " Varios.Val_Inicial " de la Actividad Economica "
                     V_Cod " No Existe" VIEW-AS ALERT-BOX.
             ASSIGN W_Nom_Ciu_Emp:SCREEN-VALUE = STRING(Varios.Val_Inicial)
                    Anexos_Clientes.Acti_Economica_Emp:SCREEN-VALUE = "".
          END.
       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_Clientes /* Cancelar */
DO:
    DISABLE Cmb_Agencia WITH FRAME F_Clientes.
    ENABLE Btn_Consulta Btn_Ingresar Btn_Borrar WITH FRAME F_Clientes.
    DISABLE Btn_Salvar Btn_Cancelar Btn_Deshacer Btn_Borrar WITH FRAME F_Clientes.  
    W_Ok = RSeleccion:ENABLE("Relaciones").
    Clientes.Nit:BGCOLOR = 18.
    Clientes.Nit:FGCOLOR = 15.
   
    /* emm */
    RUN inicializar_campos.
    /*
     DISABLE Clientes.Nit Clientes.Tipo_Identificacion WITH FRAME F_Clientes.
    FIND Clientes NO-LOCK WHERE Clientes.Agencia EQ
         INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Clientes,1,3)) AND Clientes.Nit EQ W_NitAnt NO-ERROR.
    IF AVAILABLE(Clientes) THEN DO:
       IF NOT CAN-FIND(FIRST Anexos_Clientes NO-LOCK WHERE Anexos_Clientes.nit = Clientes.nit) THEN DO:
          CREATE Anexos_Clientes.
          Anexos_Clientes.nit = Clientes.nit.
       END.
       FIND FIRST Anexos_Clientes NO-LOCK WHERE Anexos_Clientes.nit = Clientes.nit NO-ERROR.
       RUN Mostrar_Cliente.  
    END.
    ELSE MESSAGE "No se encontro ningún Cliente para mostrar" VIEW-AS ALERT-BOX INFORMATION.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_CanRel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CanRel wWin
ON CHOOSE OF Btn_CanRel IN FRAME F_Relaciones /* Cancelar */
DO:
  DO WITH FRAME F_Relaciones:
      W_MenRel:SCREEN-VALUE = "Escoja del Combo de Relaciones, el tipo de relación que quiere consultar".
      ENABLE Btn_CreRel Btn_Activas.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Btn_Cargo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cargo wWin
ON CHOOSE OF Btn_Cargo IN FRAME F_Ubicacion /* Button 12 */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Varios.r (INPUT 2, OUTPUT V_Cod, OUTPUT V_Nom).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  IF V_Cod EQ 0 THEN
     MESSAGE "No se ha elegido ningun CARGO" VIEW-AS ALERT-BOX.
  ELSE
     ASSIGN W_NomCargo:SCREEN-VALUE = V_Nom
            Clientes.Cod_Cargo:SCREEN-VALUE = STRING(V_Cod).

  W_NomCargo:BGCOLOR = 18.
  W_NomCargo:FGCOLOR = 15.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ciiu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ciiu wWin
ON CHOOSE OF Btn_Ciiu IN FRAME F_Ubicacion /* Button 99 */
DO:
  DEFINE VAR P_Gru LIKE Ciiu.Grupo NO-UNDO.
  DEFINE VAR P_Sub LIKE Ciiu.Subgrupo NO-UNDO.
  DEFINE VAR P_Cod LIKE Ciiu.Codigo NO-UNDO.
  DEFINE VAR P_Nom AS CHARACTER FORMAT "X(50)" NO-UNDO.
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ciiu.r (OUTPUT P_Gru, OUTPUT P_Sub, OUTPUT P_Cod, OUTPUT P_Nom).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  IF P_Gru EQ 0 THEN MESSAGE "No fue escogido ningún código" VIEW-AS ALERT-BOX.
  ELSE
    ASSIGN Clientes.Codigo_Ciiu:SCREEN-VALUE = STRING(P_Cod)
           Clientes.Grupo:SCREEN-VALUE       = STRING(P_Gru)
           Clientes.Subgrupo:SCREEN-VALUE    = STRING(P_Sub)
           W_NomCiiu:SCREEN-VALUE = LOWER(P_Nom).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ciiu-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ciiu-2 wWin
ON CHOOSE OF Btn_Ciiu-2 IN FRAME F_Ubicacion /* Btn_Ciiu 2 */
DO:
    DEFINE VAR P_Gru LIKE Ciiu.Grupo NO-UNDO.
    DEFINE VAR P_Sub LIKE Ciiu.Subgrupo NO-UNDO.
    DEFINE VAR P_Cod LIKE Ciiu.Codigo NO-UNDO.
    DEFINE VAR P_Nom AS CHARACTER FORMAT "X(50)" NO-UNDO.
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
    
    ASSIGN WWin:SENSITIVE = FALSE.
    RUN C-Ciiu.r (OUTPUT P_Gru, OUTPUT P_Sub, OUTPUT P_Cod, OUTPUT P_Nom).
    ASSIGN WWin:SENSITIVE = TRUE.
    WWin:MOVE-TO-TOP().
    
    IF P_Gru EQ 0 THEN MESSAGE "No fue escogido ningún código" VIEW-AS ALERT-BOX.
    ELSE
    ASSIGN Anexos_Clientes.Acti_Economica_Emp:SCREEN-VALUE = STRING(P_Cod)
               /*STRING(P_Gru) + "-" + STRING(P_Sub) + "-" + STRING(P_Cod)*/
           W_NomCiiu-2:SCREEN-VALUE = LOWER(P_Nom).    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ciiu-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ciiu-3 wWin
ON CHOOSE OF Btn_Ciiu-3 IN FRAME F_Ubicacion /* Btn_Ciiu 3 */
DO:
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

    ASSIGN WWin:SENSITIVE = FALSE.
    RUN C-Varios.r (INPUT 35, OUTPUT V_Cod, OUTPUT V_Nom).
    ASSIGN WWin:SENSITIVE = TRUE.
    WWin:MOVE-TO-TOP().

    IF V_Cod EQ 0 THEN
       MESSAGE "No se ha elegido ninguna Actividad Permanente" VIEW-AS ALERT-BOX.
    ELSE DO:
       ASSIGN W_NomCiiu-3:SCREEN-VALUE = V_Nom
              Clientes.Cod_Actividad:SCREEN-VALUE IN FRAME f_Ubicacion = STRING(V_Cod).  
       FIND varios WHERE varios.tipo = 35 AND varios.Codigo = v_cod NO-LOCK NO-ERROR.
       IF AVAIL varios THEN DO:
          FIND FIRST Ciiu WHERE Ciiu.Codigo_Ciiu = Varios.Val_Inicial NO-LOCK NO-ERROR.
          IF AVAIL Ciiu THEN DO:
             ASSIGN Anexos_Clientes.Acti_Economica_Emp:SCREEN-VALUE = STRING(Ciiu.Codigo_Ciiu)
           /*STRING(Ciiu.grupo) + "-" + STRING(Ciiu.subgrupo) + "-" + STRING(Ciiu.Codigo_Ciiu)*/
                    W_NomCiiu-2:SCREEN-VALUE = Ciiu.Descripcion.
          END.
          ELSE DO:
             MESSAGE "Ciiu " Varios.Val_Inicial " de la Actividad Economica "
                     V_Cod " no Existe" VIEW-AS ALERT-BOX.
             ASSIGN W_NomCiiu-2:SCREEN-VALUE = STRING(Varios.Val_Inicial)
                    Anexos_Clientes.Acti_Economica_Emp:SCREEN-VALUE = "".
          END.
       END.

       ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Btn_Codseg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Codseg wWin
ON CHOOSE OF Btn_Codseg IN FRAME F_Otros /* Btn_coding 2 */
DO:
DO WITH FRAME F_Otros:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  RUN C-Varios.r (INPUT 6, OUTPUT V_Cod, OUTPUT V_Nom).
  IF V_Cod EQ 0 THEN
     MESSAGE "No se ha elegido ningun Segmento" VIEW-AS ALERT-BOX.
  ELSE
     ASSIGN W_Nomsegmento:SCREEN-VALUE = V_Nom
            Clientes.Cod_Segmento:SCREEN-VALUE = STRING(V_Cod).
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Btn_Colegio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Colegio wWin
ON CHOOSE OF Btn_Colegio IN FRAME F_Segmentacion /* Btn_Colegio */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  
  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Varios.r (INPUT 37, OUTPUT V_Cod, OUTPUT V_Nom).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  IF V_Cod EQ 0 THEN
     MESSAGE "No se ha elegido ningun Colegio" VIEW-AS ALERT-BOX.
  ELSE
     ASSIGN Nom_Colegio:SCREEN-VALUE = V_Nom
            Clientes.Colegio:SCREEN-VALUE IN FRAME f_segmentacion = STRING(V_Cod).  

  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME F_Clientes /* Button 3 */
DO:
    DO WITH FRAME F_Clientes:
        WWin:SENSITIVE = FALSE.

        RUN C-Clientes.R(INPUT 2,
                         INPUT W_Agencia,
                         OUTPUT P_Nit,
                         OUTPUT P_Nombre,
                         OUTPUT P_Apellido,
                         OUTPUT P_AgeCli).

        WWin:SENSITIVE = TRUE.
        WWin:MOVE-TO-TOP().

        IF P_Nit NE "" THEN DO:
            FIND FIRST Clientes NO-LOCK WHERE Clientes.Agencia EQ P_AgeCli
                                          AND Clientes.Nit EQ P_Nit
                                          AND Clientes.Tipo_Vinculo LT 3 NO-ERROR.
            IF AVAIL(Clientes) THEN DO:
                Wk_Edad = (DECIMAL(TODAY) - DECIMAL(Clientes.Fec_Nacimiento)) / 365.

                 RUN Mostrar_Cliente.

                 FIND FIRST Agencias WHERE Agencias.Agencia EQ P_AgeCli NO-LOCK NO-ERROR.
              IF AVAIL(Agencias) THEN Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
           END.
           ELSE DO:
              MESSAGE "Este persona no es un cliente de la cooperativa" SKIP(1)
                      "Si se desea cambiar su estado a cliente, deberá" SKIP
                      "hacerse como un ingreso normal" VIEW-AS ALERT-BOX INFORMATION TITLE "Persona No vinculada".
           END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_CreRel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CreRel wWin
ON CHOOSE OF Btn_CreRel IN FRAME F_Relaciones /* Crear */
DO:
    FOR EACH Relaciones NO-LOCK
                    WHERE 
                        Relaciones.Nit    EQ Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes 
                    AND Relaciones.Estado = 1:
        IF  cod_relacion = INT(substring(cmb_relaciones:SCREEN-VALUE IN FRAME F_Relaciones,1,5)) AND 
            (cod_relacion = 3 OR cod_relacion = 5 ) THEN DO:
            MESSAGE "Ya existe una persona con esa refrencia!" VIEW-AS ALERT-BOX INFORMATION.
            RETURN NO-APPLY.
        END.

    END.

  r_relacion:SCREEN-VALUE IN FRAME FRelNva = "" .
  DO WITH FRAME F_Relaciones:
      IF Cmb_Relaciones:SCREEN-VALUE BEGINS "00000" OR 
         Cmb_Relaciones:SCREEN-VALUE BEGINS "99999" THEN DO:
         MESSAGE "Para crear una relacion se debe escoger el tipo" VIEW-AS ALERT-BOX.
         APPLY "entry" TO Cmb_Relaciones.
         RETURN NO-APPLY.
      END.
      W_MenRel:SCREEN-VALUE = "Escoja del Combo de consulta la relación que se le asignará a la persona o empresa".
     RActivas:SCREEN-VALUE = "1".
     ENABLE /*Cmb_Relaciones*/ Btn_SalRel.
     DISABLE Btn_CreRel Btn_Activas.

     DISABLE R_Sucursal
             R_Producto
             R_Numero
     WITH FRAME FRelNva.
     
     IF Cmb_Relaciones:SCREEN-VALUE = "00008 - Referencias Financieras" THEN DO WITH FRAME FRelNva:
       ENABLE R_Sucursal
              R_Producto
              R_Numero
       WITH FRAME FRelNva.
       R_Sucursal:BGCOLOR = 15.
       R_Producto:BGCOLOR = 15.
       R_Numero:BGCOLOR = 15.
     END.
     ELSE DO WITH FRAME FRelNva:
       R_Sucursal:BGCOLOR = 18.
       R_Producto:BGCOLOR = 18.
       R_Numero:BGCOLOR = 18.
     END.

     VIEW FRAME FRelNva.
             IF substring(Cmb_Relaciones:SCREEN-VALUE,1,5) NE "00003" OR
                substring(Cmb_Relaciones:SCREEN-VALUE,1,5) NE "00005" THEN 
                        ENABLE R_Relacion  btn_SC WITH FRAME FRelNva.
                
                    IF  substring(Cmb_Relaciones:SCREEN-VALUE,1,5) EQ "00005" THEN 
                           DISABLE R_Relacion btn_SC WITH FRAME FRelNva.   
                  
                          IF  substring(Cmb_Relaciones:SCREEN-VALUE,1,5) EQ "00003" THEN  
                           DISABLE  btn_SC WITH FRAME FRelNva.    
                               
                
     APPLY "entry" TO R_Relacion.
     RETURN NO-APPLY.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME F_Clientes /* Limpiar */
DO:
    /*
    DISABLE Cmb_Agencia WITH FRAME F_Clientes.
    ENABLE Btn_Consulta Btn_Ingresar Btn_Borrar WITH FRAME F_Clientes.
    DISABLE Btn_Salvar Btn_Cancelar Btn_Deshacer WITH FRAME F_Clientes.  
    Clientes.Nit:BGCOLOR = 18.
    Clientes.Nit:FGCOLOR = 15.
    DISABLE Clientes.Nit Clientes.Tipo_Identificacion WITH FRAME F_Clientes.
    FIND FIRST Clientes WHERE Clientes.Agencia = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Clientes,1,3)) 
           AND (Clientes.Nit EQ W_NitAnt OR Clientes.Nit EQ Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes) NO-ERROR.
    IF AVAILABLE(Clientes) THEN RUN Mostrar_Cliente.
    */
      
    RUN Inicializar_campos.
      DISABLE Btn_Salvar Btn_Cancelar Btn_Deshacer WITH FRAME F_Clientes.
      ENABLE Btn_consulta Btn_ingresar  WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Btn_Direccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Direccion wWin
ON CHOOSE OF Btn_Direccion IN FRAME F_Segmentacion /* Btn_Direccion */
DO:
    hDrccion = Clientes.Dir_Residencia:HANDLE.
    WWin:SENSITIVE = FALSE.
    RUN viewObject IN h_wgnrdordrccion.
    RUN CmbiarDrccion IN h_wgnrdordrccion(Clientes.Dir_Residencia:SCREEN-VALUE).

    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Microempresas
&Scoped-define SELF-NAME Btn_Direccion-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Direccion-2 wWin
ON CHOOSE OF Btn_Direccion-2 IN FRAME F_Microempresas /* Btn_Direccion */
DO:
    hDrccion = MicroEmpresas.Direccion:HANDLE.
    WWin:SENSITIVE = FALSE.
    RUN viewObject IN h_wgnrdordrccion.
    RUN CmbiarDrccion IN h_wgnrdordrccion(MicroEmpresas.Direccion:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Btn_DireccionComercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_DireccionComercial wWin
ON CHOOSE OF Btn_DireccionComercial IN FRAME F_Ubicacion /* R_Btn_Direccion */
DO:
    hDrccion = Clientes.Dir_comercial:HANDLE.
    WWin:SENSITIVE = FALSE.
    RUN viewObject IN h_wgnrdordrccion.
    RUN CmbiarDrccion IN h_wgnrdordrccion(Clientes.Dir_comercial:SCREEN-VALUE).
    Clientes.Dir_comercial:BGCOLOR = 18.
    Clientes.Dir_comercial:FGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Btn_Direccion_Ante
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Direccion_Ante wWin
ON CHOOSE OF Btn_Direccion_Ante IN FRAME F_Segmentacion /* R_Btn_Direccion */
DO:

    hDrccion = Anexos_Clientes.Dir_Ant_Residencia:HANDLE.
    WWin:SENSITIVE = FALSE.
    RUN viewObject IN h_wgnrdordrccion.
    RUN CmbiarDrccion IN h_wgnrdordrccion(Anexos_Clientes.Dir_Ant_Residencia:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Direccion_Arendatario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Direccion_Arendatario wWin
ON CHOOSE OF Btn_Direccion_Arendatario IN FRAME F_Segmentacion /* R_Btn_Direccion */
DO:
    hDrccion = Anexos_Clientes.Dir_Arrendatario:HANDLE.
    WWin:SENSITIVE = FALSE.
    RUN viewObject IN h_wgnrdordrccion.
    RUN CmbiarDrccion IN h_wgnrdordrccion(Anexos_Clientes.Dir_Arrendatario:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Juridicas
&Scoped-define SELF-NAME Btn_DirEmpresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_DirEmpresa wWin
ON CHOOSE OF Btn_DirEmpresa IN FRAME F_Juridicas /* Btn_DirEmpresa */
DO:
    hDrccion = Clientes.Dir_Residencia:HANDLE.
    WWin:SENSITIVE = FALSE.
    RUN viewObject IN h_wgnrdordrccion.
    RUN CmbiarDrccion IN h_wgnrdordrccion(Clientes.Dir_Residencia:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Btn_Documento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Documento wWin
ON CHOOSE OF Btn_Documento IN FRAME F_Segmentacion /* Button 109 */
DO :
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
  
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  ASSIGN WWin:SENSITIVE = TRUE.
  
  WWin:MOVE-TO-TOP().

  ASSIGN W_CiuExpedicion:SCREEN-VALUE = LC(P_NUbi)
         Clientes.Lugar_Expedicion:SCREEN-VALUE   = P_Ubi.

  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Btn_Empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Empresa wWin
ON CHOOSE OF Btn_Empresa IN FRAME F_Ubicacion /* Button 98 */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Empresas.w (INPUT W_Agencia, OUTPUT P_Cod, OUTPUT P_Nit, OUTPUT P_AgeEmp, OUTPUT P_Nom).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().
  IF NOT p_cod = ? 
  THEN DO:
 /*  IF P_Cod NE 0 THEN DO: */
     ASSIGN W_NomEmpresa:SCREEN-VALUE = P_Nom
            Clientes.Cod_Empresa:SCREEN-VALUE = STRING(P_Cod).
 /* END.
  ELSE MESSAGE "No se ha elegido ninguna EMPRESA" VIEW-AS ALERT-BOX. */
  END.
  ELSE P_Cod= 0.

    W_NomEmpresa:BGCOLOR = 18.
    W_NomEmpresa:FGCOLOR = 15.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Btn_Grabar_Act_Pas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar_Act_Pas wWin
ON CHOOSE OF Btn_Grabar_Act_Pas IN FRAME F_Economica /* Grabar */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes. 

  ASSIGN WWin:SENSITIVE = FALSE.
  ASSIGN P_Nit = Clientes.nit:SCREEN-VALUE IN FRAME f_Clientes.
 
  ASSIGN WWin:SENSITIVE = FALSE.
  RUN w_ClientesPasivosBienes.w (INPUT P_Nit, OUTPUT P_Pas, OUTPUT P_VVeh, OUTPUT P_VBie, OUTPUT P_VOtros).
  WWin:MOVE-TO-TOP().

  IF P_VVeh NE 0 THEN DO:
     ASSIGN Clientes.Act_vehiculo:SCREEN-VALUE = STRING(DECIMAL(P_VVeh))
            Clientes.Act_inversion:SCREEN-VALUE = STRING(DECIMAL(P_VOtros))
            Clientes.Act_casa:SCREEN-VALUE = STRING(DECIMAL(P_VBie))
            Anexos_Clientes.Val_Pasivos:SCREEN-VALUE = STRING(DECIMAL(P_Pas)).     .
           

     DISPLAY Clientes.Act_vehiculo:SCREEN-VALUE = STRING(DECIMAL(P_VVeh))
             Clientes.Act_inversion:SCREEN-VALUE = STRING(DECIMAL(P_VOtros))
             Clientes.Act_casa:SCREEN-VALUE = STRING(DECIMAL(P_VBie))
             Anexos_Clientes.Val_Pasivos:SCREEN-VALUE = STRING(DECIMAL(P_Pas)).     
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Conyuge
&Scoped-define SELF-NAME Btn_Grabar_conyuge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar_conyuge wWin
ON CHOOSE OF Btn_Grabar_conyuge IN FRAME F_Conyuge /* Grabar */
DO:
    /* grabando conyuge */
    ASSIGN FRAME F_Conyuge R_Tipo_Identificacion R_Sexo R_Tercero R_Nombre R_Apellido1 R_Apellido2 R_Lugar_Nacimiento
           R_Fec_Nacimiento R_Nacionalidad R_Niv_Educativo R_Cod_Profesion R_Ocupacion R_Especifique R_Cargo_Actividad
           R_Empresa R_Direccion R_Dir_Empresa  R_Tel_Empresa R_Tel_Empresa-Indctvo R_TIngreso R_TEgresos cnygeextncion.
    R_Relacion = "Conyuge".
    IF R_tercero EQ "" OR R_Nombre EQ "" OR R_Apellido1 EQ "" OR R_Relacion EQ "" 
    THEN DO:
        MESSAGE "Debe entrarse minimo: Nit, Nombre, apellido1 y el tipo de relacion" SKIP
                "o parentesco de la persona. rectifique la informacion" VIEW-AS ALERT-BOX.
        APPLY "entry" TO R_tercero.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND Clientes EXCLUSIVE-LOCK WHERE Clientes.Nit EQ R_tercero NO-ERROR NO-WAIT. /* emm */
        IF LOCKED(Clientes) THEN DO:
           MESSAGE "ERROR: Registro en uso por otros usuario." SKIP
                   "Intente Más Tarde" VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN NO-APPLY.
        END.
        IF NOT AVAILABLE Clientes 
        THEN DO:
            CREATE Clientes.
            ASSIGN  Clientes.Agencia = W_Agencia
                    Clientes.Nit     = R_tercero.
        END.
        ASSIGN  Clientes.Nombre              = R_Nombre
                Clientes.Apellido1           = R_Apellido1
                Clientes.Apellido2           = R_Apellido2
                Clientes.Tipo_Identificacion = R_Tipo_Identificacion
                Clientes.sexo                = R_Sexo
                Clientes.Lugar_Nacimiento    = R_Lugar_Nacimiento
                Clientes.Fec_Nacimiento      = R_Fec_Nacimiento
                Clientes.Niv_Educativo       = R_Niv_Educativo
                Clientes.Cod_Profesion       = R_Cod_Profesion
                Clientes.Tipo_Actividad      = R_Ocupacion
                Clientes.Dir_comercial       = R_Direccion
                Clientes.Lugar_comercial     = R_Dir_Empresa
                Clientes.Est_Civil           = Clientes.Est_Civil:SCREEN-VALUE IN FRAME F_segmentacion
                Clientes.Tel_Comercial       = R_Tel_Empresa
                Clientes.Estado              = 1
                Clientes.Tipo_Cliente        = 1 /*Mayor de edad*/
                Clientes.Tipo_Vinculo        = 3 /*Como tercero 2 es no asociado, uno asociado*/.
        FIND Anexos_Clientes WHERE Anexos_Clientes.Nit EQ R_tercero NO-ERROR.
        IF NOT AVAILABLE Anexos_Clientes 
        THEN DO:
            CREATE Anexos_Clientes.
            ASSIGN Anexos_Clientes.Nit     = R_tercero.
        END.
        ASSIGN  Anexos_Clientes.Nacionalidad           = R_Nacionalidad
                Anexos_Clientes.Especifique_Ocupacion  = R_Especifique
                Anexos_Clientes.Ind_cli                = R_Tel_Empresa-indctvo
                Anexos_Clientes.extencion_comercial    = INTEGER(CnygeExtncion:SCREEN-VALUE).

        FIND CURRENT Anexos_Clientes NO-LOCK.
    END. /* IF NOT AVAILABLE Clientes  */
    DO WITH FRAME F_Relaciones:
        FIND LAST Relaciones 
            WHERE 
                Relaciones.nit = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes
            AND Relaciones.Cod_Relacion = 00001
            AND Relaciones.Nit_Relacion = R_tercero
            NO-ERROR.
        IF NOT AVAILABLE Relaciones 
        THEN CREATE Relaciones.
        ASSIGN  Relaciones.Nit                 = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes
                Relaciones.Cod_Relacion        = 00001
                Relaciones.Nit_Relacion        = R_tercero
                Relaciones.Usuario             = W_Usuario
                Relaciones.Fec_Ingreso         = W_Fecha
                Relaciones.Descripcion         = R_Relacion
                Relaciones.Estado              = 1
                Relaciones.Rel_Cargo_Acti      = R_Cargo_Actividad
                Relaciones.Rel_Nom_Empresa     = R_Empresa
                Relaciones.Tot_Ing_Conyuge     = R_TIngreso
                Relaciones.Tot_Egr_Conyuge     = R_TEgresos.
    
    END. /* DO WITH FRAME F_Relaciones: */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME F_Clientes /* Ingresar */
DO:
    IF AVAILABLE Clientes THEN DO:
        FIND CClientes WHERE CClientes.nit = Cliente.nit:SCREEN-VALUE IN FRAME F_Clientes NO-LOCK NO-ERROR.
        RELEASE Clientes.
    END.

    DISABLE Btn_Consulta
            Btn_Ingresar
            Btn_Borrar
        WITH FRAME F_Clientes.

    ENABLE Btn_Salvar
           Btn_Cancelar
           Btn_Deshacer
        WITH FRAME F_Clientes.

    W_Ok = RSeleccion:ENABLE("Relaciones").

    ENABLE Clientes.Nit
           Clientes.Tipo_Identificacion
        WITH FRAME F_Clientes.

    FIND FIRST Agencias WHERE Agencias.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
    
    RUN Inicializar_Campos.

    DISABLE Clientes.Dir_Residencia
            Anexos_Clientes.Dir_Arrendatario
            Anexos_Clientes.Dir_Ant_Residencia
        WITH FRAME F_segmentacion.

    ASSIGN Clientes.Nit:BGCOLOR = 15
           Clientes.Nit:FGCOLOR = 0
           Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.

    fIncialza(FRAME F_Conyuge:FIRST-CHILD).
    BtonInfoCnyge:SENSITIVE = FALSE.

    DISABLE Anexos_Clientes.Otra_Transac_Ext Anexos_Clientes.Tipo_Moneda_Divisa Anexos_Clientes.Num_Cta_Extranjera
            Anexos_Clientes.Nom_Banco_Extranjero Anexos_Clientes.Ciudad_Pais_Bco_Extranjero WITH FRAME F_Juridicas.
 /* APPLY 'entry' TO Clientes.Tipo_Identificacion IN FRAME F_Clientes.*/
    APPLY "entry" TO Clientes.Nit IN FRAME F_Clientes.
    RETURN NO-APPLY.
/*  APPLY 'VALUE-CHANGED' TO Clientes.Tipo_Identificacion IN FRAME F_Clientes. */
/*  RETURN NO-APPLY.                                                           */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Btn_Lugar_Comercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Lugar_Comercial wWin
ON CHOOSE OF Btn_Lugar_Comercial IN FRAME F_Ubicacion /* Btn_Lugar_Comercial */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  ASSIGN W_UbicacionComercial:SCREEN-VALUE = LC(P_NUbi)
         Clientes.Lugar_Comercial:SCREEN-VALUE = P_Ubi.  

   W_UbicacionComercial:BGCOLOR = 18. 
   W_UbicacionComercial:FGCOLOR = 15. 
  


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME Btn_Lugar_Comercial-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Lugar_Comercial-2 wWin
ON CHOOSE OF Btn_Lugar_Comercial-2 IN FRAME FRelNva /* Btn_Lugar_Comercial */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().
/*  ASSIGN    R_Ciu_Dpt:SCREEN-VALUE = LC(P_NUbi)
            R_Ciu_DptCdgo:SCREEN-VALUE = P_Ubi.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Btn_Nacionalidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Nacionalidad wWin
ON CHOOSE OF Btn_Nacionalidad IN FRAME F_Segmentacion /* Btn_Nacionalidad */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  
  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Varios.r (INPUT 36, OUTPUT V_Cod, OUTPUT V_Nom).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  IF V_Cod EQ 0 THEN
     MESSAGE "No se ha elegido ninguna Nacionalidad" VIEW-AS ALERT-BOX.
  ELSE
     Anexos_Clientes.Nacionalidad:SCREEN-VALUE IN FRAME f_segmentacion = STRING(V_Nom).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Conyuge
&Scoped-define SELF-NAME Btn_Ocultar_conyuge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ocultar_conyuge wWin
ON CHOOSE OF Btn_Ocultar_conyuge IN FRAME F_Conyuge /* Ocultar */
DO:
    HIDE FRAME f_conyuge.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Btn_Profesion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Profesion wWin
ON CHOOSE OF Btn_Profesion IN FRAME F_Segmentacion /* Button 13 */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  
  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Varios.r (INPUT 1, OUTPUT V_Cod, OUTPUT V_Nom).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  IF V_Cod EQ 0 THEN
     MESSAGE "No se ha elegido ninguna PROFESIÓN" VIEW-AS ALERT-BOX.
  ELSE
     ASSIGN Clientes.Cod_Profesion:SCREEN-VALUE IN FRAME f_segmentacion = STRING(V_Cod)
            W_NomProfesion:SCREEN-VALUE = V_Nom.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Residencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Residencia wWin
ON CHOOSE OF Btn_Residencia IN FRAME F_Segmentacion /* Button 103 */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ubicacion.r (OUTPUT P_Ubi,OUTPUT P_NUbi).
  
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().
  /*
  IF INTEGER(P_Ubi) EQ 0 THEN DO:
     MESSAGE "No fue encontrada ninguna ubicación" VIEW-AS ALERT-BOX.
  END.
  */

  ASSIGN W_UbicacionResidencia:SCREEN-VALUE = LC(P_NUbi)
         Clientes.Lugar_Residencia:SCREEN-VALUE = P_Ubi.

  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_Clientes /* Salir */
DO:
    IF NOT GHPARENT = ? 
    THEN DO:
        RUN hideObject IN THIS-PROCEDURE.    
        RETURN NO-APPLY.
    END.
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


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Btn_SalRel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalRel wWin
ON CHOOSE OF Btn_SalRel IN FRAME F_Relaciones /* Grabar */
DO:
    DO WITH FRAME F_Relaciones:
        W_MenRel:SCREEN-VALUE = "Escoja del Combo de Relaciones, el tipo de relación que quiere consultar".
        FIND Clientes NO-LOCK
            WHERE 
                Clientes.Nit EQ Relaciones.Nit_Relacion NO-ERROR.
        IF AVAILABLE(Clientes) 
        THEN DO:
            FIND Anexos_cliente NO-LOCK
                WHERE
                    Anexos_Clientes.nit = Clientes.nit NO-ERROR.
            CREATE  T_Relaciones.
            UPDATE  T_Relaciones.R_Relacion         =  SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,8,15)
                    T_Relaciones.R_AgeObjeto        = Clientes.Agencia
                    T_Relaciones.R_NitObjeto        = Relaciones.Nit_Relacion
                    T_Relaciones.R_NomDescri        = Relaciones.Descripcion
                    T_Relaciones.R_NomObjeto        = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                    T_Relaciones.R_TelObjeto        = Clientes.Tel_Residencia
                    T_Relaciones.R_TelObjetoIndctvo = IF AVAILABLE Anexos_Clientes THEN Anexos_Clientes.ind_cli ELSE 0
                    T_Relaciones.R_TelComerc        = Clientes.Tel_Comercial
                    t_relaciones.r_telcomercIndctvo = IF AVAILABLE Anexos_Clientes THEN Anexos_Clientes.ind_com ELSE 0.
        END.
        DISABLE Btn_SalRel.
        OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
        ENABLE Btn_CreRel Btn_Activas.
    END.                                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Clientes /* Grabar */
DO:
    DEFINE VAR flagError AS LOGICAL.

    /* Validar integridad del documento ID */
    RUN validaNIT.r(INPUT clientes.nit:SCREEN-VALUE,
                    INPUT clientes.tipo_identificacion:SCREEN-VALUE,
                    OUTPUT flagError).

    IF flagError = TRUE THEN DO:
        MESSAGE "El documento ingresado no es correcto de" SKIP
                "acuerdo al tipo de identificación seleccionado." SKIP
                "Revise por favor..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    gerror = FALSE.

    IF clientes.nombre:SCREEN-VALUE = ? OR clientes.nombre:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Por favor digite Nombres y Apellidos" clientes.nombre:SCREEN-VALUE
            VIEW-AS ALERT-BOX.

        RETURN NO-APPLY.
    END.

    EMPTY TEMP-TABLE TFalta.

    RUN Validacion_Informacion.
    RUN Totales_Economica.

    wEdad = TODAY - date(Clientes.Fec_Nacimiento:SCREEN-VALUE IN FRAME f_segmentacion).

    IF wEdad >= 6570 AND (INT(Tot_Ingresos:SCREEN-VALUE IN FRAME F_Economica) = 0  OR INT(Tot_Egresos:SCREEN-VALUE IN FRAME F_Economica) = 0) THEN DO:
        MESSAGE "Ingresos y/o Egresos no pueden ser $0" SKIP
                "Revise la información financiera"
            VIEW-AS ALERT-BOX.

        RETURN NO-APPLY.
    END.
    ELSE
        RUN Verifica_Tutor.

    IF Clientes.Estado:SCREEN-VALUE IN FRAME F_Clientes EQ "2" OR (AVAIL Clientes AND Clientes.Tipo_Vinculo NE 1) THEN DO:
        MESSAGE "El Cliente NO es Asociado..." SKIP
                "Continue sólo si desea actualizarlo..."
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "SI ASOCIAR" UPDATE W_SiAsoc AS LOGICAL.

        IF NOT W_SiAsoc THEN DO:
            DISABLE Btn_Salvar
                    Btn_Deshacer
                WITH FRAME F_Clientes.

            APPLY "entry" TO Btn_Salir IN FRAME F_Clientes.
            RETURN NO-APPLY.
        END.
    END.

    IF clientes.nit:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Debe ingresar un documento de identificación válido"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    DISABLE Cmb_Agencia WITH FRAME F_Clientes.

    ENABLE Btn_Consulta
           Btn_Ingresar
        WITH FRAME F_Clientes.

    DISABLE Btn_Salvar
            Btn_Deshacer
        WITH FRAME F_Clientes.

    FIND FIRST Clientes EXCLUSIVE-LOCK WHERE Clientes.Nit EQ Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes NO-ERROR NO-WAIT.
    IF LOCKED(Clientes) THEN DO:
        MESSAGE "ERROR: Registro en uso por otro Usuario. Intente Grabar la Información de nuevo en un momento"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    IF NOT AVAIL(Clientes) THEN DO:
        W_Ok = RSeleccion:ENABLE("Relaciones").

        CREATE Clientes.
        ASSIGN Clientes.Agencia = W_Agencia
               Clientes.Tipo_Vinculo = 2.
    END.
    ELSE DO:
        FIND FIRST Ahorros WHERE Ahorros.Tip_Ahorro EQ 4
                             AND Ahorros.Nit EQ Clientes.Nit
                             AND ahorros.estado = 1 NO-LOCK NO-ERROR.
        IF AVAIL Ahorros AND Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje GT 0 THEN DO:
            Clientes.Estado = 1.

            IF clientes.tipo_vinculo <> 1 THEN DO:
                MESSAGE "Este Asociado posee cuenta de Aportes vigente y con Saldo," SKIP
                        "y no se encuentra marcado como tal en el sistema. Será" SKIP
                        "marcado como Asociado!"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                Clientes.Tipo_Vinculo = 1.
            END.
        END.
        ELSE
            Clientes.Estado = 1.
    END.

    FIND FIRST Anexos_Clientes WHERE Anexos_Clientes.Nit EQ Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes NO-ERROR.
    IF NOT AVAIL(Anexos_Clientes) THEN DO:
        CREATE Anexos_Clientes.
        Anexos_Clientes.Nit = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes.
    END.

    Clientes.Nombre = Clientes.Nombre:SCREEN-VALUE IN FRAME F_Clientes.

    
    IF Clientes.Agencia = 0 OR clientes.agencia = ? THEN
        Clientes.Agencia = W_Agencia.

    IF Clientes.Tipo_Identificacion = "NIT" THEN DO:
        DO WITH FRAME f_juridicas:
            IF Clientes.Celular:SCREEN-VALUE = "" OR
               Clientes.Fec_Expedicion:SCREEN-VALUE = "" OR
               Clientes.DIR_Residencia:SCREEN-VALUE = "" OR
               W_Ubi_Empresa:SCREEN-VALUE = "" OR
               W_Nom_Act_Ppal:SCREEN-VALUE = "" THEN DO:
                MESSAGE "Faltan datos obligatorios:" SKIP
                        "Cámara de Comercio:" Clientes.Celular SKIP
                        "Fecha Constitución:" Clientes.Fec_Expedicion SKIP
                        "Dirección:" Clientes.DIR_Residencia SKIP
                        "Ciudad:" W_Ubi_Empresa SKIP
                        "Actividad económica principal:" W_Nom_Act_Ppal:SCREEN-VALUE
                    VIEW-AS ALERT-BOX.

                ENABLE Btn_Salvar
                       Btn_Deshacer
                    WITH FRAME F_Clientes.

                RETURN NO-APPLY.
            END.
            ELSE DO:
                IF DECIMAL(clientes.salario:SCREEN-VALUE ) LE 0 OR DECIMAL(clientes.sdo_Obligaciones:SCREEN-VALUE ) LE 0 THEN DO:
                    MESSAGE "Faltan datos importantes:" SKIP
                            "Ingresos:" clientes.salario SKIP
                            "Egresos:" clientes.Sdo_Obligaciones
                        VIEW-AS ALERT-BOX.

                    ENABLE Btn_Salvar
                           Btn_Deshacer
                        WITH FRAME F_Clientes.

                    RETURN NO-APPLY.
                END.
            END.

            IF decimal(clientes.Act_vehiculo:SCREEN-VALUE) LE 0 OR decimal(clientes.Act_vehiculo:SCREEN-VALUE) LE 0 THEN DO:
                MESSAGE "Faltan datos obligatorios:" SKIP
                        "Activos:" clientes.Act_vehiculo:SCREEN-VALUE SKIP
                        "Pasivos:" Anexos_Clientes.Val_Pasivos:SCREEN-VALUE
                    VIEW-AS ALERT-BOX.

                ENABLE Btn_Salvar
                       Btn_Deshacer
                    WITH FRAME F_Clientes.

                RETURN NO-APPLY.
            END.
        END.

        RUN Verifica_RepLegal.
    END.
    ELSE DO:
        IF Clientes.Tipo_Identificacion <> "NIT" THEN DO:
            IF Cmb_TipAct:SCREEN-VALUE IN FRAME F_Ubicacion = "" OR Cmb_TipAct:SCREEN-VALUE IN FRAME F_Ubicacion = ? THEN DO:
                MESSAGE "Debes escoger una ocupación!"
                    VIEW-AS ALERT-BOX INFORMATION.

                HIDE FRAME F_segmentacion
                     FRAME F_Autorizaciones
                     FRAME F_Relaciones
                     FRAME F_Documentacion
                     FRAME F_Otros
                     FRAME F_Economica
                     FRAME F_Juridicas.

                VIEW FRAME F_Ubicacion.

                RETURN NO-APPLY.
            END.

            IF Cmb_TipAct:SCREEN-VALUE IN FRAME F_Ubicacion NE "Estudiante" OR Cmb_TipAct:SCREEN-VALUE IN FRAME F_Ubicacion NE "Hogar" THEN DO:
                IF W_nomEmpresa:SCREEN-VALUE IN FRAME F_Ubicacion = ? OR
                   W_nomempresa:SCREEN-VALUE IN FRAME F_Ubicacion = "" OR
                   w_nomciiu-3:SCREEN-VALUE IN FRAME F_Ubicacion = "" OR
                   w_nomciiu-3:SCREEN-VALUE IN FRAME F_Ubicacion = ? OR
                   w_nomProfesion:SCREEN-VALUE IN FRAME F_segmentacion = "" OR
                   w_nomProfesion:SCREEN-VALUE IN FRAME F_Segmentacion = ? OR
                   facultadDepartamento:SCREEN-VALUE IN FRAME F_ubicacion = "" OR
                   facultadDepartamento:SCREEN-VALUE IN FRAME F_ubicacion = ? THEN DO:
                    MESSAGE "Faltan datos obligatorios:" SKIP
                            "Actividad permanente:" Clientes.Cod_Actividad SKIP
                            "Empresa/Entidad:" W_nomEmpresa:SCREEN-VALUE IN FRAME F_Ubicacion SKIP
                            "Profesión:" W_nomProfesion:SCREEN-VALUE IN FRAME F_segmentacion SKIP
                            "Facultad/Departamento:" facultadDepartamento:SCREEN-VALUE
                        VIEW-AS ALERT-BOX.

                    DISABLE Btn_Salvar
                            Btn_Deshacer
                        WITH FRAME F_Clientes.

                    RETURN NO-APPLY.
                END.
            END.
        END.
    END.

    IF gerror = TRUE THEN
        RETURN NO-APPLY.

    FIND FIRST Ahorros WHERE Ahorros.Tip_Ahorro EQ 4
                         AND Ahorros.Cod_Ahorro EQ 2
                         AND Ahorros.Nit EQ Clientes.Nit:SCREEN-VALUE IN FRAME F_clientes NO-LOCK NO-ERROR.
    IF NOT AVAIL Ahorros OR (AVAILABLE ahorros AND ahorros.estado <> 1) THEN DO:
        MESSAGE "Desea Asignar/Reactivar la cuenta de Aportes en forma automática?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE SwDecide AS LOGICAL.

        IF SwDecide THEN DO:
            RUN Grabar_Aportes NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                MESSAGE "El producto de Aportes, debe estar configurado" SKIP
                        "para la creación automática de la cuenta." SKIP
                        "La cuenta se deberá crear manualmente"
                    VIEW-AS ALERT-BOX WARNING.
            ELSE DO:
                FRAME F_Clientes:SENSITIVE = FALSE.

                VIEW FRAME F_Aportes.

                ASSIGN FRAME F_Aportes:SENSITIVE = TRUE
                             Edit_Msaje:SCREEN-VALUE = " ".

                IF Pro_Ahorros.Id_Consecutivo THEN
                    Edit_Msaje:SCREEN-VALUE = "La cuenta de aportes fue creada de forma correcta".
            END.
        END.
    END.

    DISABLE Clientes.Fec_Fallecido WITH FRAME F_Fechas.

    CREATE hoja_vida.
    ASSIGN hoja_vida.tipo = 40
           hoja_vida.codigo = 1
           hoja_vida.nit = clientes.nit:SCREEN-VALUE
           hoja_vida.usuario = w_usuario
           hoja_vida.fec_grabacion = w_fecha
           hoja_vida.hora_grabacion = TIME
           hoja_vida.fec_limite = TODAY.

    IF clientes.tipo_identificacion <> clientes.tipo_identificacion:SCREEN-VALUE THEN
        hoja_vida.observacion = "Tipo de identificación de " + clientes.tipo_identificacion + " a " + clientes.tipo_identificacion:SCREEN-VALUE.
        
    RUN Grabar.

    FIND CURRENT Clientes NO-LOCK NO-ERROR.

    MESSAGE "Grabado!"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME Btn_SC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SC wWin
ON CHOOSE OF Btn_SC IN FRAME FRelNva /* SC */
DO:
  Rela_Nit:SCREEN-VALUE = "SC_" + STRING(NEXT-VALUE(Sec_NitAuto)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Juridicas
&Scoped-define SELF-NAME Btn_UbiEmpresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_UbiEmpresa wWin
ON CHOOSE OF Btn_UbiEmpresa IN FRAME F_Juridicas /* Btn_UbiEmpresa */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ubicacion.r (OUTPUT P_Ubi, OUTPUT P_NUbi).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  ASSIGN W_Ubi_Empresa:SCREEN-VALUE IN FRAME F_Juridicas = LC(P_NUbi)
         Clientes.Lugar_Residencia:SCREEN-VALUE = P_Ubi.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Microempresas
&Scoped-define SELF-NAME Btn_Ubi_Micro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ubi_Micro wWin
ON CHOOSE OF Btn_Ubi_Micro IN FRAME F_Microempresas /* Btn_Ubi_Micro */
DO :
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ubicacion.w (OUTPUT P_Ubi,OUTPUT P_NUbi).
  
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  ASSIGN WWin:SENSITIVE = TRUE.
  
  WWin:MOVE-TO-TOP().

  ASSIGN Nom_Ubicacion:SCREEN-VALUE = LC(P_NUbi)
         MicroEmpresas.Ubicacion:SCREEN-VALUE = P_Ubi.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME BtonInfoCnyge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtonInfoCnyge wWin
ON CHOOSE OF BtonInfoCnyge IN FRAME F_Clientes /* Conyuge */
DO:
    RSeleccion:SCREEN-VALUE = "1".        
    APPLY "value-changed" TO RSeleccion.
    VIEW FRAME f_conyuge.
    FRAME f_conyuge:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Aportes
&Scoped-define SELF-NAME Bt_FinAporte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bt_FinAporte wWin
ON CHOOSE OF Bt_FinAporte IN FRAME F_Aportes /* Fin Creación Aportes */
DO:
  /*IF Pro_Ahorros.Id_Consecutivo  THEN 
     MESSAGE "La cuenta de aportes fue creada" SKIP
             "el monto de apertura debe ser: $ " Ahorros.Monto_Apertura SKIP
             "La cuota mensual para el " YEAR(TODAY) " es " Ahorros.Cuota
             VIEW-AS ALERT-BOX INFORMATION.*/

  HIDE FRAME F_Aportes.
  FRAME F_Clientes:SENSITIVE = TRUE.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Clientes /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME BUTTON-116
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-116 wWin
ON CHOOSE OF BUTTON-116 IN FRAME F_Relaciones /* Button 116 */
DO:
  W_Inf = "Relaciones".     
  IF Br_Relaciones:NUM-ENTRIES EQ 0 THEN
  DO:
     MESSAGE "La lista Actual no contiene ninguna información para imprimir" SKIP
             "Rectifique la consulta por medio del combo: CONSULTA DE RELACIONES" 
             VIEW-AS ALERT-BOX INFORMATION.
  END.
  ELSE DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "" NO-UNDO.
    Listado = W_Pathspl + "Lst_Relaciones.lst".
    {incluido/imprimir.i "Listado"}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Falta
&Scoped-define SELF-NAME BUTTON-179
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-179 wWin
ON CHOOSE OF BUTTON-179 IN FRAME F_Falta /* Button 179 */
DO:
  HIDE FRAME F_Falta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME BUTTON-180
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-180 wWin
ON CHOOSE OF BUTTON-180 IN FRAME F_Otros /* Fecha fallecimiento */
DO:
    ENABLE Clientes.Fec_Fallecido WITH FRAME F_otros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME F_Clientes /* Button 2 */
DO:
  IF NOT AVAIL Clientes THEN DO:
     MESSAGE "Cliente No Existe. Salve el Registro Para Poder Imprimirlo" VIEW-AS ALERT-BOX.
     RETURN.
  END.
  InputFile = SEARCH("Formatos\AS - 301.xls").
  RUN Abrir_Excel.
  RUN Imprime_Vinculacion.
  /* RUN Cerrar_Excel. */

/*     DEFINE VAR Listado AS CHARACTER INITIAL "" NO-UNDO.        */
/*     Listado = W_Pathspl + "Lst_Relaciones.lst".                */
/*     {incluido/imprimir.i "Listado"}.                           */
/*                                                                */
/*     RUN pRcbeDtos IN h_wsolprofina(Clientes.nit:SCREEN-VALUE). */
/*     wwin:SENSITIVE = FALSE.                                    */
/*     RUN viewObject IN h_wsolprofina.                           */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME BUTTON-205
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-205 wWin
ON CHOOSE OF BUTTON-205 IN FRAME FRelNva /* Salvar */
DO:
    IF SUBSTRING(Rela_Nit:SCREEN-VALUE,1,3) <> "SC_" THEN DO:
        SWER1 = TRUE.

        DO I = 1 TO 9:
            IF SUBSTRING(Rela_Nit:SCREEN-VALUE,1,1) = Xdigi[i] THEN
                SWER1 = FALSE.
        END.

        IF SWER1 THEN DO:
            MESSAGE "Primer Dígito del documento de identificación no es válido"
                VIEW-AS ALERT-BOX INFORMATION.

            RETURN NO-APPLY.
        END.
    END.

    ASSIGN FRAME FRelNva
        Rela_Nit
        R_Nombre
        R_nombre2
        R_Apellido1
        R_Apellido2
        fechaNacimientoRelacion
        r_direccion
        r_ciu_dpt
        R_Tel_Residencia
        R_Tel_ResidenciaIndctvo
        R_Relacion
        R_Tel_Comercial
        R_Tel_ComercialIndctvo.

    DO WITH FRAME F_Relaciones:
        IF Rela_Nit = "" OR (IF NOT R_Nombre:HIDDEN THEN R_Nombre = "" ELSE TRUE) THEN DO:
            MESSAGE "Debe entrarse minimo: número de identificación, nombre, primer apellido y el tipo de relación" SKIP
                    "o parentesco de la persona. Rectifique la información"
                VIEW-AS ALERT-BOX.

            RETURN NO-APPLY.
        END.

        IF INTEGER(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5)) = 5 AND (R_Nombre = "" OR R_Direccion = "" OR r_ciu_dpt = "" OR R_Tel_Residencia = "") THEN DO:
            MESSAGE "Faltan datos importantes del Representante Legal" SKIP(1)
                    "Nombre:" R_Nombre SKIP
                    "Apellido:" R_Apellido1 SKIP
                    "Direccion:" R_Direccion SKIP
                    "Ciudad:" r_ciu_dpt SKIP
                    "Teléfono:" R_Tel_Residencia
                VIEW-AS ALERT-BOX.

            RETURN NO-APPLY.
        END.
    END.

    /* oakley */

    DO:
        IF CAN-FIND(FIRST Clientes WHERE Clientes.Nit EQ Rela_Nit) THEN do:
            FIND FIRST Clientes EXCLUSIVE-LOCK WHERE Clientes.Nit EQ Rela_Nit NO-ERROR.
            DO WHILE LOCKED(Clientes):
                MESSAGE "Cliente En Uso Por Otro Usuario. Espere O CTRL-C Para Abortar"
                         VIEW-AS ALERT-BOX INFO BUTTONS OK.
                FIND FIRST Clientes EXCLUSIVE-LOCK WHERE Clientes.Nit EQ Rela_Nit NO-ERROR.
            END.
        END.
        ELSE DO:
            CREATE Clientes.
        END. /*IF CAN-FIND(FIRST Clientes WHERE Clientes.Nit EQ R_Nit) */
        ASSIGN Clientes.Agencia          = W_Agencia
               Clientes.Nit              = Rela_Nit
               Clientes.Nombre           = R_Nombre + R_Nombre2
               Clientes.Apellido1        = R_Apellido1
               Clientes.Apellido2        = R_Apellido2
               Clientes.Dir_Residencia   = R_Direccion
              /* Clientes.Lugar_Residencia = R_Ciu_DptCdgo */
               Clientes.Tel_Residencia   = R_Tel_Residencia
               Clientes.Tel_Comercial    = R_Tel_Comercial.
        IF CAN-FIND(FIRST Anexos_Clientes WHERE Anexos_Clientes.nit = Rela_Nit) THEN DO:
           FIND FIRST Anexos_Clientes EXCLUSIVE-LOCK WHERE Anexos_Clientes.nit = Rela_Nit NO-ERROR.
           DO WHILE LOCKED(Anexos_Clientes):
              MESSAGE "Registro En Uso Por Otro Usuario. Espere O CTRL-C Para Abortar"
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.
              FIND FIRST Anexos_Clientes EXCLUSIVE-LOCK WHERE Anexos_Clientes.nit = Rela_Nit NO-ERROR.
           END.
        END.
        ELSE DO:
            CREATE Anexos_Clientes.
        END. /*IF CAN-FIND(FIRST Anexos_Clientes WHERE Anexos_Clientes.nit = r_nit)*/
        ASSIGN Anexos_Clientes.nit     = Rela_Nit
               Anexos_Clientes.Ind_cli = R_Tel_ResidenciaIndctvo
               Anexos_Clientes.ind_com = R_Tel_ComercialIndctvo.
        DO WITH FRAME F_Relaciones:
            FIND LAST Relaciones WHERE 
                      Relaciones.nit = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes 
                  AND Relaciones.Cod_Relacion = INTEGER(SUBSTR(Cmb_Relaciones:SCREEN-VALUE,1,5)) 
                  AND Relaciones.Nit_Relacion = Rela_Nit NO-ERROR.
            IF NOT AVAIL Relaciones THEN CREATE Relaciones.
            ASSIGN Relaciones.Nit           = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes
                   Relaciones.Cod_Relacion  = INTEGER(SUBSTR(Cmb_Relaciones:SCREEN-VALUE,1,5))
                   Relaciones.Nit_Relacion  = Rela_Nit
                   Relaciones.Usuario       = W_Usuario
                   Relaciones.Fec_Ingreso   = W_Fecha
                   Relaciones.Descripcion   = R_Relacion:SCREEN-VALUE
                   Relaciones.Sucursal      = R_Sucursal:SCREEN-VALUE
                   Relaciones.Producto      = R_Producto:SCREEN-VALUE
                   Relaciones.Ref_Numero    = R_Numero:SCREEN-VALUE
                   Relaciones.Estado        = 1.
        END.
        DO WITH FRAME FNvaRel:
            ASSIGN  Rela_Nit:SCREEN-VALUE = ""
                    R_Nombre:SCREEN-VALUE = ""
                    R_Nombre2:SCREEN-VALUE = ""
                    R_Apellido1:SCREEN-VALUE = ""
                    R_Apellido2:SCREEN-VALUE = ""
                    R_Direccion:SCREEN-VALUE = ""
                    R_Ciu_Dpt:SCREEN-VALUE = ""
                    R_Tel_Residencia:SCREEN-VALUE = ""
                    R_Tel_ResidenciaIndctvo:SCREEN-VALUE = ""
                    R_Tel_Comercial:SCREEN-VALUE  = ""
                    R_Tel_Comercial:SCREEN-VALUE  = ""
                    R_Sucursal:SCREEN-VALUE  = ""
                    R_Producto:SCREEN-VALUE  = ""
                    R_Numero:SCREEN-VALUE  = ""
                    fechaNacimientoRelacion:SCREEN-VALUE = "".
            ENABLE R_Nombre R_nombre2 R_Apellido1 R_Apellido2 /* R_Direccion R_Ciu_Dpt */ R_Tel_ResidenciaIndctvo
                   R_Tel_Residencia R_Tel_ComercialIndctvo R_Tel_Comercial /* R_Ciu_DptCdgo */
                   Btn_Lugar_Comercial-2 R_Btn_Direccion-3 fechaNacimientoRelacion WITH FRAME FRelNva.
        END.
        HIDE FRAME FRelNva.
        APPLY "choose" TO Btn_SalRel IN FRAME F_Relaciones.
        FIND CURRENT Clientes        NO-LOCK NO-ERROR.    
        FIND CURRENT Anexos_Clientes NO-LOCK NO-ERROR.
        FIND CURRENT Relaciones      NO-LOCK NO-ERROR.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-206
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-206 wWin
ON CHOOSE OF BUTTON-206 IN FRAME FRelNva /* Ocultar */
DO:
    ASSIGN  Rela_Nit:SCREEN-VALUE = ""
            R_Nombre:SCREEN-VALUE = ""
            R_Apellido1:SCREEN-VALUE = ""
            R_Apellido2:SCREEN-VALUE = ""
            R_Tel_Residencia:SCREEN-VALUE = ""
            R_Tel_ResidenciaIndctvo:SCREEN-VALUE = ""
            R_Tel_Comercial:SCREEN-VALUE = ""
            R_Tel_ComercialIndctvo:SCREEN-VALUE = ""
            fechaNacimientoRelacion:SCREEN-VALUE = "".
    ENABLE R_Nombre R_Apellido1 R_Apellido2 /* R_Direccion R_Ciu_Dpt  */ R_Tel_ResidenciaIndctvo
           R_Tel_Residencia R_Tel_ComercialIndctvo R_Tel_Comercial /* R_Ciu_DptCdgo */
           Btn_Lugar_Comercial-2 R_Btn_Direccion-3 fechaNacimientoRelacion WITH FRAME FRelNva.
    APPLY "choose" TO Btn_CanRel IN FRAME F_Relaciones.
  /*  APPLY "choose" TO Btn_SalRel IN FRAME F_Relaciones. */
    HIDE FRAME FRelNva.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME BUTTON-207
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-207 wWin
ON CHOOSE OF BUTTON-207 IN FRAME F_Clientes /* Button 207 */
DO:
    DEF VAR cl AS CHAR NO-UNDO.
    fImprmeDtos(FRAME F_Clientes:FIRST-CHILD).
    OUTPUT TO c:\tmp\exito.txt.
    FOR EACH t
        BREAK
            BY torden 
            BY tFla 
            BY tClmna:
        IF FIRST-OF(tfla)
        THEN cl = "".
        OVERLAY(cl,tclmna) = tLbel.
        IF LAST-OF(tfla)
        THEN PUT UNFORMATTED cl SKIP.
    END.
    OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Calificacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Calificacion wWin
ON VALUE-CHANGED OF Clientes.Calificacion IN FRAME F_Otros /* Calificación del Cliente */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Anexos_Clientes.Cam_int1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Cam_int1 wWin
ON VALUE-CHANGED OF Anexos_Clientes.Cam_int1 IN FRAME F_Ubicacion /* IND Tel CLi */
DO:
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Carnet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Carnet wWin
ON LEAVE OF Clientes.Carnet IN FRAME F_Ubicacion /* Carnet */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

   
   /*Clientes.Carnet:BGCOLOR = 18.
   Clientes.Carnet:FGCOLOR = 15.*/
   


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Celular
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Celular wWin
ON LEAVE OF Clientes.Celular IN FRAME F_Segmentacion /* Celular */
DO:
    IF NOT trim(SELF:SCREEN-VALUE,"-") = ""
    THEN
    IF NOT can-do("300,301,302,303,304,310,311,312,313,314,315,316,317,318,320,321,322,323",SUBSTRING(SELF:SCREEN-VALUE,1,3)) THEN DO:
       MESSAGE "Indicativo De Celular Incorrecto" VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN NO-APPLY.
    END.
/*
  DEFI VAR Tel_Nume AS DEC FORM "999999999999".
  ASSIGN Tel_Nume = DEC(Clientes.Celular:SCREEN-VALUE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "Solo Numèrico...Corrija por favor."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "ENTRY" TO SELF.
     RETURN NO-APPLY.
  END.

  IF LENGTH(Clientes.Celular:SCREEN-VALUE) < 10 THEN DO:
    MESSAGE "Debe contener minimo 10 caracteres.... Corrija por Favor."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "ENTRY" TO SELF.
    RETURN NO-APPLY.
  END.
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Celular wWin
ON VALUE-CHANGED OF Clientes.Celular IN FRAME F_Segmentacion /* Celular */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Anexos_Clientes.Ciudad_Pais_Bco_Extranjero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Ciudad_Pais_Bco_Extranjero wWin
ON LEAVE OF Anexos_Clientes.Ciudad_Pais_Bco_Extranjero IN FRAME F_Economica /* Ciudad y País */
DO:
  Anexos_Clientes.Ciudad_Pais_Bco_Extranjero:BGCOLOR = 18.
  Anexos_Clientes.Ciudad_Pais_Bco_Extranjero:FGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Cmb_Est_Cargo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Est_Cargo wWin
ON VALUE-CHANGED OF Cmb_Est_Cargo IN FRAME F_Ubicacion /* Estado Cargo */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME Cmb_Relaciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Relaciones wWin
ON VALUE-CHANGED OF Cmb_Relaciones IN FRAME F_Relaciones /* Consulta Relaciones */
DO:
    DO WITH FRAME F_Relaciones:
        EMPTY TEMP-TABLE T_Relaciones.

        CASE Cmb_Relaciones:SCREEN-VALUE:
            WHEN "99999 - Todas las Relaciones" THEN DO:
                FOR EACH Relaciones WHERE Relaciones.Nit = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes
                                      AND Relaciones.Estado EQ INTEGER(RActivas:SCREEN-VALUE) NO-LOCK BREAK BY Relaciones.Cod_Relacion:
                    IF FIRST-OF(Relaciones.Cod_Relacion) THEN
                        FIND Varios WHERE Varios.Tipo = 3
                                      AND Varios.Codigo = Relaciones.Cod_Relacion NO-LOCK NO-ERROR.

                    FIND Clientes WHERE Clientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
                    IF AVAILABLE(Clientes) THEN DO:
                        FIND Anexos_Clientes WHERE Anexos_Clientes.nit = Clientes.nit NO-LOCK NO-ERROR.

                        CREATE T_Relaciones.
                        UPDATE T_Relaciones.R_Relacion = Varios.Descripcion
                               T_Relaciones.R_AgeObjeto = Clientes.Agencia
                               T_Relaciones.R_NitObjeto = Relaciones.Nit_Relacion
                               T_Relaciones.R_NomObjeto = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                               T_Relaciones.R_NomDescri = Relaciones.Descripcion
                               T_Relaciones.R_TelObjeto = Clientes.Tel_Residencia
                               T_Relaciones.R_TelObjetoIndctvo = IF AVAILABLE Anexos_Clientes THEN Anexos_Clientes.ind_cli ELSE 0
                               T_Relaciones.R_TelComerc = Clientes.Tel_Comercial.
                    END.
                END.
            END.

            WHEN "00000 - Ninguna Relacion" THEN DO:
                EMPTY TEMP-TABLE T_Relaciones.
            END.

            OTHERWISE DO:
                FIND FIRST Varios WHERE Varios.Tipo = 3
                                    AND Varios.Codigo = INTEGER(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5)) NO-LOCK NO-ERROR.

                FOR EACH Relaciones WHERE Relaciones.Nit = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes
                                      AND Relaciones.Cod_Relacion = INTEGER(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5))
                                      AND Relaciones.Estado = INTEGER(RActivas:SCREEN-VALUE) NO-LOCK:
                    FIND FIRST Clientes WHERE Clientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
                    IF AVAILABLE(Clientes) THEN DO:
                        FIND FIRST Anexos_Clientes WHERE Anexos_Clientes.nit = Clientes.nit NO-LOCK NO-ERROR.

                        CREATE T_Relaciones.
                        T_Relaciones.R_Relacion = Varios.Descripcion.
                        T_Relaciones.R_AgeObjeto = Clientes.Agencia.
                        T_Relaciones.R_NitObjeto = Relaciones.Nit_Relacion.
                        T_Relaciones.R_NomObjeto = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
                        T_Relaciones.R_NomDescri = Relaciones.Descripcion.
                        T_Relaciones.R_TelObjeto = Clientes.Tel_Residencia.
                        T_Relaciones.R_TelObjetoIndctvo = IF AVAILABLE Anexos_Clientes THEN Anexos_Clientes.ind_cli ELSE 0.
                        T_Relaciones.R_TelComerc = Clientes.Tel_Comercial.
                    END.
                END.

                DO WITH FRAME FRelNva:
                    CASE substring(Cmb_Relaciones:SCREEN-VALUE,1,5):
                        WHEN "00002" THEN DO: /* Comerciales */
                            r_apellido1:HIDDEN = TRUE.
                            r_apellido2:HIDDEN = TRUE.
                            r_relacion:HIDDEN = TRUE.
                            r_tel_residencia:HIDDEN = TRUE.
                            r_tel_residenciaIndctvo:HIDDEN = TRUE.
                        END.

                        WHEN "00008" THEN DO:
                            r_apellido1:HIDDEN = TRUE.
                            r_apellido2:HIDDEN = TRUE.
                            r_relacion:HIDDEN = TRUE.
                            r_tel_residencia:HIDDEN = TRUE.
                            r_tel_residenciaIndctvo:HIDDEN = TRUE.
                        END.

                        OTHERWISE DO:
                            r_nombre:HIDDEN = FALSE.
                            r_apellido1:HIDDEN = FALSE.
                            r_apellido2:HIDDEN = FALSE.
                            r_relacion:HIDDEN = FALSE.
                            r_tel_residencia:HIDDEN = FALSE.
                            r_tel_residenciaIndctvo:HIDDEN = FALSE.
                            r_relacion:HIDDEN = IF substring(Cmb_Relaciones:SCREEN-VALUE,1,5) = "00009" THEN TRUE ELSE r_relacion:HIDDEN.
                            fechaNacimientoRelacion:HIDDEN = FALSE.
                        END.
                    END CASE.

                    IF SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5) <> "00003" AND SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5) <> "00005" THEN
                        ENABLE R_Relacion btn_SC WITH FRAME FRelNva.

                    IF SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5) = "00005" THEN
                        DISABLE R_Relacion btn_SC WITH FRAME FRelNva.

                    IF SUBSTRING(Cmb_Relaciones:SCREEN-VALUE,1,5) = "00003" THEN
                        DISABLE  btn_SC WITH FRAME FRelNva.
                END.
            END.
        END CASE.

        OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Cmb_Situacion_Actual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Situacion_Actual wWin
ON LEAVE OF Cmb_Situacion_Actual IN FRAME F_Ubicacion /* Situacion Actual */
DO:
  APPLY "value-changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_TipAct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TipAct wWin
ON LEAVE OF Cmb_TipAct IN FRAME F_Ubicacion /* Ocupación Principal */
DO:
  ASSIGN Cmb_TipAct.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TipAct wWin
ON VALUE-CHANGED OF Cmb_TipAct IN FRAME F_Ubicacion /* Ocupación Principal */
DO:
    IF Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes NE "2" OR (Cmb_TipAct:SCREEN-VALUE IN FRAME F_Ubicacion = "Estudiante" OR Cmb_TipAct:SCREEN-VALUE IN FRAME F_Ubicacion = "Hogar") THEN DO:
        ENABLE Btn_Salvar
               Btn_Deshacer
            WITH FRAME F_Clientes.

        DO WITH FRAME {&FRAME-NAME}:
            W_NomEmpresa:SCREEN-VALUE = "".
            Clientes.cod_empresa:SCREEN-VALUE = "".
            facultadDepartamento:SCREEN-VALUE = "".
            clientes.facultad:SCREEN-VALUE = "".
            clientes.departamento:SCREEN-VALUE = "".
            W_NomCiiu-2:SCREEN-VALUE = "".
            W_NomCiiu-3:SCREEN-VALUE  = "".
            Clientes.DIR_comercial:SCREEN-VALUE = "".
            W_UbicacionComercial:SCREEN-VALUE = "".
            Anexos_Clientes.ind_comercial:SCREEN-VALUE = "".
            Clientes.tel_comercial:SCREEN-VALUE = "".
            Anexos_Clientes.extencion_comercial:SCREEN-VALUE = "".
            W_NomCargo:SCREEN-VALUE = "".
            Clientes.lugar_comercial:SCREEN-VALUE = "".
            Clientes.cod_cargo:SCREEN-VALUE = "".
            Clientes.fec_ingempresa:SCREEN-VALUE = "".
            Clientes.carnet:SCREEN-VALUE = "".
            Cmb_Est_Cargo:SCREEN-VALUE = ?.
            Anexos_Clientes.tel_fax_comercial:SCREEN-VALUE = "".
            Anexos_Clientes.nit_independiente:SCREEN-VALUE = "".
            Anexos_Clientes.tiempo_actividad:SCREEN-VALUE = "".
            Anexos_Clientes.cam_int1:SCREEN-VALUE = "".
            Anexos_Clientes.fec_pensionado:SCREEN-VALUE = ?.
            W_NomCiiu:SCREEN-VALUE = "".
            Clientes.Codigo_Ciiu:SCREEN-VALUE = "".
            Clientes.grupo:SCREEN-VALUE = "".
            Clientes.subgrupo:SCREEN-VALUE = "".
        END.

        DO WITH FRAME F_Ubicacion:
            IF SELF:SCREEN-VALUE = "Otro" THEN DO:
                ENABLE Anexos_Clientes.Especifique_Ocupacion. 
                Anexos_Clientes.Especifique_Ocupacion:BGCOLOR = 15.
                Anexos_Clientes.Especifique_Ocupacion:FGCOLOR = 0.
            END.
            ELSE DO:
                ASSIGN Anexos_Clientes.Especifique_Ocupacion:SCREEN-VALUE = "".
                DISABLE Anexos_Clientes.Especifique_Ocupacion.
                Anexos_Clientes.Especifique_Ocupacion:BGCOLOR = 18.
                Anexos_Clientes.Especifique_Ocupacion:FGCOLOR = 15.
            END.

            DISABLE Btn_Empresa
                    btnFacultadDpto
                    Anexos_Clientes.Acti_Economica_Emp
                    Clientes.Dir_comercial
                    Btn_DireccionComercial
                    Clientes.Lugar_comercial
                    Clientes.Tel_comercial
                    Anexos_Clientes.Extencion_Comercial
                    Btn_Cargo
                    Clientes.Fec_IngEmpresa
                    Clientes.Carnet
                    Cmb_Est_Cargo
                    Anexos_Clientes.Tel_Fax_Comercial
                    Clientes.Tip_Contrato
                    Anexos_Clientes.Nit_Independiente
                    Anexos_Clientes.Tiempo_Actividad
                    Anexos_Clientes.cam_int1
                    Btn_Ciiu
                    Anexos_Clientes.Fec_Pensionado.

            IF SELF:SCREEN-VALUE = "Empleado Público" THEN DO:
                ENABLE Cmb_Est_Cargo
                       Btn_Empresa
                       btnFacultadDpto
                       Btn_Cargo
                       Clientes.Fec_IngEmpresa
                       Anexos_Clientes.Acti_Economica_Emp
                       Clientes.Tel_comercial
                       Anexos_Clientes.Extencion_Comercial
                       Btn_DireccionComercial
                       Clientes.Lugar_comercial
                       Anexos_Clientes.Tel_Fax_Comercial.

                W_NomEmpresa:BGCOLOR = 15.
                facultadDepartamento:BGCOLOR = 15.
                W_NomCargo:BGCOLOR = 15.

                Clientes.Fec_IngEmpresa:BGCOLOR = 15.
                Anexos_Clientes.Acti_Economica_Emp:BGCOLOR = 15.
                Clientes.Tel_comercial:BGCOLOR = 15.
                Anexos_Clientes.Extencion_Comercial:BGCOLOR = 15.
                Clientes.Dir_comercial:BGCOLOR = 15.
                W_UbicacionComercial:BGCOLOR = 15.
                Anexos_Clientes.Tel_Fax_Comercial:BGCOLOR = 15.
            END.

            IF SELF:SCREEN-VALUE = "Empleado Privado" THEN DO:
                ENABLE Clientes.Tip_Contrato
                       Btn_Empresa
                       btnFacultadDpto
                       Btn_Cargo
                       Clientes.Fec_IngEmpresa
                       Anexos_Clientes.Acti_Economica_Emp
                       Clientes.Tel_comercial
                       Anexos_Clientes.Extencion_Comercial
                       Btn_DireccionComercial
                       Clientes.Lugar_comercial
                       Anexos_Clientes.Tel_Fax_Comercial.

                W_NomEmpresa:BGCOLOR = 15.
                facultadDepartamento:BGCOLOR = 15.
                W_NomCargo:BGCOLOR = 15.
                Clientes.Fec_IngEmpresa:BGCOLOR = 15.
                Anexos_Clientes.Acti_Economica_Emp:BGCOLOR = 15.
                Clientes.Tel_comercial:BGCOLOR = 15.
                Anexos_Clientes.Extencion_Comercial:BGCOLOR = 15.
                Clientes.Dir_comercial:BGCOLOR = 15.
                W_UbicacionComercial:BGCOLOR = 15. 
                Anexos_Clientes.Tel_Fax_Comercial:BGCOLOR = 15.
                Clientes.Tip_Contrato:BGCOLOR = 15.
            END.

            IF SELF:SCREEN-VALUE = "Independiente o Empleado Socio" THEN DO:
                ENABLE Btn_Empresa
                       btnFacultadDpto
                       Anexos_Clientes.Nit_Independiente
                       Btn_Ciiu
                       Anexos_Clientes.Tiempo_Actividad
                       Anexos_Clientes.Cam_int1
                       Btn_Cargo
                       Anexos_Clientes.Acti_Economica_Emp
                       Btn_DireccionComercial
                       Clientes.Lugar_comercial
                       Clientes.Tel_comercial
                       Anexos_Clientes.Tel_Fax_Comercial.

                W_NomEmpresa:BGCOLOR = 15.
                facultadDepartamento:BGCOLOR = 15.
                Anexos_Clientes.Nit_Independiente:BGCOLOR = 15.
                W_NomCiiu:BGCOLOR = 15.
                W_NomCiiu-2:BGCOLOR = 15.
                Anexos_Clientes.Tiempo_Actividad:BGCOLOR = 15.
                Anexos_Clientes.cam_int1:BGCOLOR = 15.
                W_NomCargo:BGCOLOR = 15.
                Anexos_Clientes.Acti_Economica_Emp:BGCOLOR = 15.
                Clientes.Dir_comercial:BGCOLOR = 15.
                W_UbicacionComercial:BGCOLOR = 15. 
                Clientes.Tel_comercial:BGCOLOR = 15.
                Anexos_Clientes.Tel_Fax_Comercial:BGCOLOR = 15.
            END.

            IF SELF:SCREEN-VALUE = "Pensionado" THEN DO:
                ENABLE Btn_Empresa
                       btnFacultadDpto
                       Anexos_Clientes.Fec_Pensionado.

                W_NomEmpresa:BGCOLOR = 15.
                facultadDepartamento:BGCOLOR = 15.
                Anexos_Clientes.Fec_Pensionado:BGCOLOR = 15.
            END.

            IF SELF:SCREEN-VALUE = "Otro" THEN DO:
            END.
        END.
    END.
    ELSE DO:
        MESSAGE "No se pueden asignar a un menor una ocupación de adulto!"
            VIEW-AS ALERT-BOX.

        DISABLE Btn_Salvar
                Btn_Deshacer
            WITH FRAME F_Clientes.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Dias_Sancion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Dias_Sancion wWin
ON VALUE-CHANGED OF Clientes.Dias_Sancion IN FRAME F_Otros /* Número de Días de Sanción */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Anexos_Clientes.Dir_Ant_Residencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Dir_Ant_Residencia wWin
ON LEAVE OF Anexos_Clientes.Dir_Ant_Residencia IN FRAME F_Segmentacion /* Dirección */
DO:
  Anexos_Clientes.Dir_Ant_Residencia:BGCOLOR = 18.
  Anexos_Clientes.Dir_Ant_Residencia:FGCOLOR = 15.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Anexos_Clientes.Dir_Arrendatario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Dir_Arrendatario wWin
ON LEAVE OF Anexos_Clientes.Dir_Arrendatario IN FRAME F_Segmentacion /* Dirección */
DO:
  Anexos_Clientes.Dir_Arrendatario:BGCOLOR = 18.
  Anexos_Clientes.Dir_Arrendatario:FGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Clientes.Dir_comercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Dir_comercial wWin
ON VALUE-CHANGED OF Clientes.Dir_comercial IN FRAME F_Ubicacion /* Dirección */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Anexos_Clientes.Dir_Correspondencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Dir_Correspondencia wWin
ON VALUE-CHANGED OF Anexos_Clientes.Dir_Correspondencia IN FRAME F_Segmentacion /* Envio Correspondencia */
DO:
  DO WITH FRAME F_Segmentacion:
      IF INT(Anexos_Clientes.dir_correspondencia:SCREEN-VALUE) = 3 THEN DO:
         ENABLE Anexos_Clientes.AA_Cliente. 
         Anexos_Clientes.AA_Cliente:BGCOLOR = 15.
      END.
      ELSE DO:
         ASSIGN Anexos_Clientes.AA_Cliente:SCREEN-VALUE = "".
         DISABLE Anexos_Clientes.AA_Cliente.
         Anexos_Clientes.AA_Cliente:BGCOLOR = 18.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Dir_Residencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Dir_Residencia wWin
ON VALUE-CHANGED OF Clientes.Dir_Residencia IN FRAME F_Segmentacion /* Direc Residenc */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Email wWin
ON ANY-PRINTABLE OF Clientes.Email IN FRAME F_Segmentacion /* Correo Electrón */
DO:
    DEF VAR c AS CHAR NO-UNDO.
    c = KEYLABEL(LASTKEY).
    IF not(fchar(c) OR fdgto(c) OR NOT INDEX("_.-@",c) = 0) THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Email wWin
ON LEAVE OF Clientes.Email IN FRAME F_Segmentacion /* Correo Electrón */
DO:
    DEF VAR c AS CHAR NO-UNDO.
    c = SELF:SCREEN-VALUE.
    IF NOT trim(c) = ""
    THEN DO:
        IF NUM-ENTRIES(c,"@") <> 2
        OR NUM-ENTRIES(c,".") < 2 
        OR INDEX(c,"..") <> 0
        OR INDEX(c,"@@") <> 0
        OR INDEX(c,".@") <> 0
        OR INDEX(c,"@.") <> 0
        OR LENGTH(c) < 6 THEN DO:
           MESSAGE "Dirección De Correo Incorrecta" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Anexos_Clientes.Especifique_Ocupacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Especifique_Ocupacion wWin
ON LEAVE OF Anexos_Clientes.Especifique_Ocupacion IN FRAME F_Ubicacion /* Especifique */
DO:
  Anexos_Clientes.Especifique_Ocupacion:BGCOLOR = 18.
  Anexos_Clientes.Especifique_Ocupacion:FGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Anexos_Clientes.Especifique_OtrosIng
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Especifique_OtrosIng wWin
ON LEAVE OF Anexos_Clientes.Especifique_OtrosIng IN FRAME F_Economica /* Especifique */
DO:
  Anexos_Clientes.Especifique_OtrosIng:BGCOLOR = 18.
  Anexos_Clientes.Especifique_OtrosIng:FGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Estrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Estrato wWin
ON VALUE-CHANGED OF Clientes.Estrato IN FRAME F_Segmentacion /* Estrato */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Est_Civil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Est_Civil wWin
ON VALUE-CHANGED OF Clientes.Est_Civil IN FRAME F_Segmentacion /* Estado Civil */
DO:
    CASE SELF:SCREEN-VALUE:
         WHEN "casado" OR WHEN "union libre" THEN
             BtonInfoCnyge:SENSITIVE IN  FRAME f_Clientes = TRUE.
         OTHERWISE DO:
               BtonInfoCnyge:SENSITIVE IN  FRAME f_Clientes =  FALSE.
               fIncialza(FRAME F_Conyuge:FIRST-CHILD). /* inicializa los campos del frame conyuge */
         END.
    END CASE.
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Anexos_Clientes.Exento_GMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Exento_GMF wWin
ON VALUE-CHANGED OF Anexos_Clientes.Exento_GMF IN FRAME F_Ubicacion /* GMF */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Anexos_Clientes.Exento_retencion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Exento_retencion wWin
ON VALUE-CHANGED OF Anexos_Clientes.Exento_retencion IN FRAME F_Ubicacion /* Retencion Fuente */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Anexos_Clientes.Extencion_Comercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Extencion_Comercial wWin
ON LEAVE OF Anexos_Clientes.Extencion_Comercial IN FRAME F_Ubicacion /* Ext */
DO:
  DEFI VAR K AS INTEG FORM 99 NO-UNDO .

/*   IF ERROR-STATUS:ERROR THEN DO:                                                            */
/*      MESSAGE "Solo Numérico ...Corrija por favor."                                          */
/*                      VIEW-AS ALERT-BOX INFO BUTTONS OK.                                     */
/*      APPLY "ENTRY" TO SELF.                                                                 */
/*      RETURN NO-APPLY.                                                                       */
/*   END.                                                                                      */

  Anexos_Clientes.Extencion_Comercial:BGCOLOR = 18.
  Anexos_Clientes.Extencion_Comercial:FGCOLOR = 15.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FacultadDepartamento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacultadDepartamento wWin
ON MOUSE-SELECT-DBLCLICK OF FacultadDepartamento IN FRAME F_Ubicacion /* Facultad/Depto */
DO:
    APPLY "choose" TO btnFacultadDpto.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Fec_expedicion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_expedicion wWin
ON LEAVE OF Clientes.Fec_expedicion IN FRAME F_Segmentacion /* Fecha Expedicion */
DO:
  IF YEAR(DATE(Clientes.Fec_Expedicion:SCREEN-VALUE)) LT 1900 OR
     YEAR(DATE(Clientes.Fec_Expedicion:SCREEN-VALUE)) GT YEAR(TODAY) THEN DO:
     MESSAGE "Año fuera de Rango" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  IF DATE(SELF:SCREEN-VALUE) GT W_Fecha THEN DO:
     MESSAGE "La Fecha de Expedición" SKIP
             "no puede ser mayor a la fecha actual" SKIP
             "Rectifique de nuevo la fecha" VIEW-AS ALERT-BOX ERROR.
     SELF:SCREEN-VALUE = STRING(W_Fecha).
     APPLY "ENTRY" TO Clientes.Fec_Expedicion IN FRAME F_Segmentacion.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_expedicion wWin
ON VALUE-CHANGED OF Clientes.Fec_expedicion IN FRAME F_Segmentacion /* Fecha Expedicion */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Fec_fallecido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_fallecido wWin
ON LEAVE OF Clientes.Fec_fallecido IN FRAME F_Otros /* Fecha de Fallecido */
DO:
  IF YEAR(DATE(Clientes.Fec_fallecido:SCREEN-VALUE)) LT 1900 OR
     YEAR(DATE(Clientes.Fec_fallecido:SCREEN-VALUE)) GT YEAR(TODAY) THEN DO:
            MESSAGE "Año fuera de Rango" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
  END.
  IF DATE(SELF:SCREEN-VALUE) GT W_Fecha THEN DO:
     MESSAGE "La Fecha de Fallecimiento" SKIP
             "no puede ser mayor a la fecha actual" SKIP
             "Rectifique de nuevo la fecha" VIEW-AS ALERT-BOX ERROR.
     SELF:SCREEN-VALUE = STRING(W_Fecha).
     APPLY "ENTRY" TO Clientes.Fec_fallecido IN FRAME F_Otros.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_fallecido wWin
ON VALUE-CHANGED OF Clientes.Fec_fallecido IN FRAME F_Otros /* Fecha de Fallecido */
DO:
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Clientes.Fec_IngEmpresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_IngEmpresa wWin
ON LEAVE OF Clientes.Fec_IngEmpresa IN FRAME F_Ubicacion /* Fecha Ingreso Empresa */
DO:
    /*      
        IF YEAR(DATE(Clientes.Fec_IngEmpresa:SCREEN-VALUE)) LT 1900 
            OR YEAR(DATE(Clientes.Fec_IngEmpresa:SCREEN-VALUE)) GT YEAR(TODAY) 
        THEN DO:
            MESSAGE "Año fuera de Rango" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        IF DATE(SELF:SCREEN-VALUE) GT W_Fecha 
        THEN DO:
            MESSAGE     "La Fecha de Ingreso a la Empresa" SKIP
                        "no puede ser mayor a la fecha actual" SKIP
                        "Rectifique de nuevo la fecha" VIEW-AS ALERT-BOX ERROR.
            SELF:SCREEN-VALUE = STRING(W_Fecha).
            APPLY "ENTRY" TO Clientes.Fec_IngEmpresa IN FRAME F_Ubicacion.
            RETURN NO-APPLY.
        END.
    */    
    
    IF DATE(SELF:SCREEN-VALUE) < (DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE IN
       FRAME f_segmentacion) + 15 * 360) OR DATE(SELF:SCREEN-VALUE) > TODAY THEN DO:
       MESSAGE "Fecha de Ingreso Incorrecta" SKIP(1)
               "Fecha de Nacimiento - Fecha de Ingreso Debe Ser > 15 Años" skip(1)
               "Fecha de Nacimiento: "Clientes.Fec_Nacimiento:SCREEN-VALUE
                IN FRAME f_segmentacion VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN NO-APPLY.
    END.
    Clientes.Fec_IngEmpresa:BGCOLOR = 18.
    Clientes.Fec_IngEmpresa:FGCOLOR = 15.

   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_IngEmpresa wWin
ON VALUE-CHANGED OF Clientes.Fec_IngEmpresa IN FRAME F_Ubicacion /* Fecha Ingreso Empresa */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Fec_Nacimiento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_Nacimiento wWin
ON LEAVE OF Clientes.Fec_Nacimiento IN FRAME F_Segmentacion /* Fecha de Nacimiento */
DO:
  IF YEAR(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)) LT 1900 OR
     YEAR(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)) GT YEAR(TODAY) THEN DO:
     MESSAGE "Año fuera de Rango" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  IF DATE(SELF:SCREEN-VALUE) GT W_Fecha THEN DO:
     MESSAGE "La Fecha de Nacimiento o de Constitución" SKIP
             "no puede ser mayor a la fecha actual" SKIP
             "Rectifique de nuevo la fecha" VIEW-AS ALERT-BOX ERROR.
     SELF:SCREEN-VALUE = STRING(W_Fecha).
     APPLY "ENTRY" TO Clientes.Fec_Nacimiento IN FRAME F_Segmentacion.
     RETURN NO-APPLY.
  END.
  IF DATE(SELF:SCREEN-VALUE) GT DATE(Clientes.Fec_Expedicion:SCREEN-VALUE) THEN DO:
     MESSAGE "La Fecha de Nacimiento no puede ser mayor a la Fecha" SKIP
             "de expedición del documento de identidad." SKIP(1)
             "Rectifique de nuevo la fecha" VIEW-AS ALERT-BOX WARNING.
     APPLY "ENTRY" TO Clientes.Fec_Nacimiento IN FRAME F_Segmentacion.
     RETURN NO-APPLY.
  END.

   IF   ( TODAY - DATE(SELF:SCREEN-VALUE) LT 6570)  AND   
      (clientes.tipo_cliente:SCREEN-VALUE IN FRAME F_clientes = "1" ) THEN  DO:
       MESSAGE "Compañero !" SKIP
               "  Revise...,la edad no concuerda con el tipo de documento" VIEW-AS ALERT-BOX.
       APPLY "ENTRY" TO Clientes.Fec_Nacimiento IN FRAME F_Segmentacion.
       RETURN NO-APPLY.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Fec_Nacimiento wWin
ON VALUE-CHANGED OF Clientes.Fec_Nacimiento IN FRAME F_Segmentacion /* Fecha de Nacimiento */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
   APPLY "value-changed" TO Clientes.Fec_Nacimiento.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Anexos_Clientes.Fec_Pensionado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Fec_Pensionado wWin
ON LEAVE OF Anexos_Clientes.Fec_Pensionado IN FRAME F_Ubicacion /* Pensionado Desde */
DO:
    IF NOT SELF:SCREEN-VALUE = ? THEN DO:
       IF DATE(SELF:SCREEN-VALUE) < (DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE IN
          FRAME f_segmentacion)) OR DATE(SELF:SCREEN-VALUE) > TODAY THEN DO:
          MESSAGE "Fecha  Incorrecta" VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
       END.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Microempresas
&Scoped-define SELF-NAME Grabar_MicroEmpresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Grabar_MicroEmpresa wWin
ON CHOOSE OF Grabar_MicroEmpresa IN FRAME F_Microempresas /* Salvar */
DO:
  FIND MicroEmpresa WHERE MicroEmpresa.Nit = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL MicroEmpresa THEN CREATE MicroEmpresa.
  ASSIGN MicroEmpresa.Nit = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes
         MicroEmpresas.NOMBRE
         MicroEmpresas.ESTRATO
         MicroEmpresas.cod_actividad
         MicroEmpresas.HOMBRES
         MicroEmpresas.MUJERES
         MicroEmpresas.NINOS
         MicroEmpresas.Codigo_Ciiu
         MicroEmpresas.DIRECCION
         MicroEmpresas.TELEFONO
         MicroEmpresas.UBICACION.
  HIDE  FRAME F_Microempresas.
  CLEAR FRAME F_Microempresas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.id_AsistioAsamblea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.id_AsistioAsamblea wWin
ON VALUE-CHANGED OF Clientes.id_AsistioAsamblea IN FRAME F_Otros /* Asisitió a la última Asamblea? */
DO:
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Clientes.Id_Micro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Id_Micro wWin
ON LEAVE OF Clientes.Id_Micro IN FRAME F_Ubicacion /* Es MicroEmpresario */
DO:
  DEF VAR ubicacionx AS cha NO-UNDO.
  IF Clientes.Id_Micro:SCREEN-VALUE = "Si" THEN DO:
     VIEW FRAME F_Microempresas.
     FIND MicroEmpresa WHERE MicroEmpresa.Nit = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes NO-LOCK NO-ERROR.
     IF AVAIL MicroEmpresa THEN DO WITH FRAME F_Microempresas:
        Ubicacionx = "".
        FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = MicroEmpresa.Ubicacion NO-LOCK NO-ERROR.
        IF   AVAIL Ubicacion THEN  Ubicacionx     =  Ubicacion.Nombre +
             IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
        FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion BEGINS SUBSTR(MicroEmpresa.Ubicacion,1,5) NO-LOCK NO-ERROR.
        IF   AVAIL Ubicacion THEN  Ubicacionx     =  Ubicacionx + " - " + Ubicacion.Nombre.
        FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "D" AND Ubicacion.Ubicacion BEGINS SUBSTR(MicroEmpresa.Ubicacion,1,2) NO-LOCK NO-ERROR.
        IF   AVAIL Ubicacion THEN  Ubicacionx     =  Ubicacionx + " - " + Ubicacion.Nombre.
        Nom_Ubicacion:SCREEN-VALUE IN FRAME F_Microempresas = LC(Ubicacionx).
        FIND Varios WHERE Varios.tipo = 35 AND Varios.Codigo = int(MicroEmpresa.Cod_Actividad) NO-LOCK NO-ERROR. 
        IF AVAIL Varios THEN Nom_Actividad = Varios.Descripcion.
        FIND FIRST Ciiu WHERE Ciiu.Codigo_Ciiu = Microempresa.Codigo_Ciiu NO-LOCK NO-ERROR.
        IF AVAIL Ciiu THEN Nom_Ciiu = Ciiu.Descripcion.
        DISP Microempresa.Nombre Microempresa.Direccion Microempresa.Telefono Microempresa.Ubicacion
             Microempresa.Cod_Actividad Nom_Actividad Nom_Ciiu Microempresa.Estrato
             Microempresa.Hombres Microempresa.Mujeres Microempresa.Ninos Microempresa.Codigo_Ciiu.
     END.
     ELSE
     CLEAR FRAME F_Microempresas.
     RUN Crear_Microempresario.
  END.
  ELSE
  HIDE FRAME F_Microempresas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Juridicas
&Scoped-define SELF-NAME Clientes.Id_Mon_Ext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Id_Mon_Ext wWin
ON LEAVE OF Clientes.Id_Mon_Ext IN FRAME F_Juridicas /* Id Mon Ext */
DO:
  IF string(Clientes.Id_Mon_Ext:SCREEN-VALUE) = "Si" THEN DO:
     ENABLE Anexos_Clientes.Otra_Transac_Ext WITH FRAME F_Juridicas.
     APPLY 'entry' TO Anexos_Clientes.Otra_Transac_Ext IN FRAME F_Juridicas.
     RETURN NO-APPLY.
  END.
  ELSE
  DISABLE Anexos_Clientes.Otra_Transac_Ext WITH FRAME F_Juridicas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Id_Preexistentes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Id_Preexistentes wWin
ON VALUE-CHANGED OF Clientes.Id_Preexistentes IN FRAME F_Otros /* Ha tenido enfermedades preexistentes? */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Id_Privilegiado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Id_Privilegiado wWin
ON VALUE-CHANGED OF Clientes.Id_Privilegiado IN FRAME F_Otros /* Id_Privilegiado */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Id_PuedeCodeudar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Id_PuedeCodeudar wWin
ON VALUE-CHANGED OF Clientes.Id_PuedeCodeudar IN FRAME F_Otros /* Id_PuedeCodeudar */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Anexos_Clientes.Ind_Comercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Ind_Comercial wWin
ON VALUE-CHANGED OF Anexos_Clientes.Ind_Comercial IN FRAME F_Ubicacion /* Teléfono */
DO:
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Juridicas
&Scoped-define SELF-NAME Anexos_Clientes.Maneja_Cta_Extanjera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Maneja_Cta_Extanjera wWin
ON LEAVE OF Anexos_Clientes.Maneja_Cta_Extanjera IN FRAME F_Juridicas /* Cuenta Moneda Extranjera */
DO:
  IF string(Anexos_Clientes.Maneja_Cta_Extanjera:SCREEN-VALUE) = "Si" THEN DO:
     ENABLE Anexos_Clientes.Tipo_Moneda_Divisa Anexos_Clientes.Num_Cta_Extranjera
            Anexos_Clientes.Nom_Banco_Extranjero Anexos_Clientes.Ciudad_Pais_Bco_Extranjero WITH FRAME F_Juridicas.
     APPLY 'entry' TO Anexos_Clientes.Tipo_Moneda_Divisa IN FRAME F_Juridicas.
     RETURN NO-APPLY.
  END.
  ELSE
  DISABLE Anexos_Clientes.Tipo_Moneda_Divisa Anexos_Clientes.Num_Cta_Extranjera
          Anexos_Clientes.Nom_Banco_Extranjero Anexos_Clientes.Ciudad_Pais_Bco_Extranjero WITH FRAME F_Juridicas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Anexos_Clientes.Maneja_Cta_Extanjera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Maneja_Cta_Extanjera wWin
ON VALUE-CHANGED OF Anexos_Clientes.Maneja_Cta_Extanjera IN FRAME F_Economica
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  DO WITH FRAME F_Economica:
      IF Anexos_Clientes.Maneja_Cta_Extanjera:SCREEN-VALUE <> "" THEN DO:
         ENABLE Anexos_Clientes.Tipo_Moneda_Divisa
                Anexos_Clientes.Nom_Banco_extranjero 
                Anexos_Clientes.Num_cta_extranjera 
                Anexos_Clientes.Ciudad_Pais_Bco_Extranjero.

         Anexos_Clientes.Tipo_Moneda_Divisa:BGCOLOR = 15.
         Anexos_Clientes.Nom_Banco_extranjero:BGCOLOR = 15.
         Anexos_Clientes.Num_cta_extranjera:BGCOLOR = 15. 
         Anexos_Clientes.Ciudad_Pais_Bco_Extranjero:BGCOLOR = 15.
                
      END.
      ELSE DO:
         ASSIGN Anexos_Clientes.Tipo_Moneda_Divisa:SCREEN-VALUE = ""
                Anexos_Clientes.Nom_Banco_extranjero:SCREEN-VALUE = "" 
                Anexos_Clientes.Num_cta_extranjera:SCREEN-VALUE = "" 
                Anexos_Clientes.Ciudad_Pais_Bco_Extranjero:SCREEN-VALUE = "".
    
         DISABLE Anexos_Clientes.Tipo_Moneda_Divisa
                 Anexos_Clientes.Nom_Banco_extranjero 
                 Anexos_Clientes.Num_cta_extranjera 
                 Anexos_Clientes.Ciudad_Pais_Bco_Extranjero.

         Anexos_Clientes.Tipo_Moneda_Divisa:BGCOLOR = 18.
         Anexos_Clientes.Nom_Banco_extranjero:BGCOLOR = 18.
         Anexos_Clientes.Num_cta_extranjera:BGCOLOR = 18. 
         Anexos_Clientes.Ciudad_Pais_Bco_Extranjero:BGCOLOR = 18.
         Anexos_Clientes.Tipo_Moneda_Divisa:FGCOLOR = 15.
         Anexos_Clientes.Nom_Banco_extranjero:FGCOLOR = 15.
         Anexos_Clientes.Num_cta_extranjera:FGCOLOR = 15. 
         Anexos_Clientes.Ciudad_Pais_Bco_Extranjero:FGCOLOR = 15.

      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Clientes.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nit wWin
ON LEAVE OF Clientes.Nit IN FRAME F_Clientes /* Nº */
DO:
    DEFINE VAR digitoVerificacion AS INTEGER.
    DEFINE VAR tempDec AS DECIMAL.

    ASSIGN tempDec = DECIMAL(Clientes.Nit:SCREEN-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "El formato para el número de documento es inválido."
                "Revise por favor..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    DO WITH FRAME F_Clientes:
        IF Clientes.Nit:SCREEN-VALUE = "" THEN DO:
            MESSAGE "Debe entrar el Número de Identifcación del Cliente"
                VIEW-AS ALERT-BOX.

            RETURN NO-APPLY.
        END.

        IF clientes.tipo_identificacion:SCREEN-VALUE = "NIT" THEN DO:
            RUN digitoVerificacion.r(INPUT clientes.nit:SCREEN-VALUE,
                                     OUTPUT digitoVerificacion).

            clientes.dv:SCREEN-VALUE = STRING(digitoVerificacion).
        END.
        ELSE
            clientes.dv:SCREEN-VALUE = "".


        IF SUBSTR(Clientes.Nit:SCREEN-VALUE,LENGTH(Clientes.Nit:SCREEN-VALUE) - 1,1) = "-" THEN DO:
            Clientes.Tipo_Identificacion:SCREEN-VALUE = "NIT".

            APPLY 'VALUE-CHANGED' TO Clientes.Tipo_Identificacion IN FRAME F_Clientes.
        END.

        RUN Validar_Tamano NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Validar_Tamano"
                VIEW-AS ALERT-BOX.

            APPLY "entry" TO Clientes.Nit IN FRAME F_Clientes.

            RETURN NO-APPLY.
        END.

        FIND FIRST ListaNegra WHERE ListaNegra.Nit EQ Clientes.Nit:SCREEN-VALUE NO-ERROR.
        IF AVAIL ListaNegra THEN DO:
            ASSIGN ListaNegra.Id_HaVenido = YES
                   ListaNegra.Fec_HaVenido = W_Fecha.

            MESSAGE "Este número de identificación se encuentra en las listas de suspendidos de la Cooperativa con el nombre" ListaNegra.Nombre ListaNegra.Apellido1 ListaNegra.Apellido2
                VIEW-AS ALERT-BOX WARNING TITLE "Activo en Lista de Suspendidos".

            RETURN NO-APPLY.
        END.

        /*RUN validarUsuarioSarlaft.R (INPUT Clientes.Nit:SCREEN-VALUE, OUTPUT W_Sarlaft) NO-ERROR.
        IF W_Sarlaft THEN
            RETURN NO-APPLY.*/
      
        FIND FIRST Clientes WHERE Clientes.Nit EQ Clientes.Nit:SCREEN-VALUE NO-ERROR.
        IF AVAIL(Clientes) THEN DO:
            Clientes.Estado = 1.

            MESSAGE "Este Cliente ya Existe. A continuacion se mostrara la informacion Existente"
                VIEW-AS ALERT-BOX.

            FIND FIRST Agencias WHERE Agencias.Agencia EQ Clientes.Agencia NO-LOCK NO-ERROR.
            IF NOT AVAILABLE agencias THEN
                FIND FIRST agencias WHERE agencias.agencia = w_agencia NO-LOCK NO-ERROR.

            Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
            
            DISABLE Clientes.Nit.
            DISABLE Btn_Salvar Btn_Deshacer WITH  FRAME F_Clientes.

            Clientes.Nit:BGCOLOR = 18.
            Clientes.Nit:FGCOLOR = 15.

            RUN Mostrar_Cliente.
        END.

        DISABLE Clientes.Dir_Residencia Anexos_Clientes.Dir_Arrendatario Anexos_Clientes.Dir_Ant_Residencia WITH FRAME F_segmentacion.
        DISABLE Clientes.dir_comercial WITH FRAME F_ubicacion.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Niv_Educativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Niv_Educativo wWin
ON VALUE-CHANGED OF Clientes.Niv_Educativo IN FRAME F_Segmentacion /* Nivel Educativo */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Nom_Arrendatario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nom_Arrendatario wWin
ON VALUE-CHANGED OF Clientes.Nom_Arrendatario IN FRAME F_Segmentacion /* Arrendador */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
   Clientes.Nom_Arrendatario:BGCOLOR = 18.
   Clientes.Nom_Arrendatario:FGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Anexos_Clientes.Nom_Banco_extranjero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Nom_Banco_extranjero wWin
ON LEAVE OF Anexos_Clientes.Nom_Banco_extranjero IN FRAME F_Economica /* Banco */
DO:
  Anexos_Clientes.Nom_Banco_extranjero:BGCOLOR = 18.
  Anexos_Clientes.Nom_Banco_extranjero:FGCOLOR = 15.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Anexos_Clientes.Num_cta_extranjera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Num_cta_extranjera wWin
ON LEAVE OF Anexos_Clientes.Num_cta_extranjera IN FRAME F_Economica /* Numero Cuenta */
DO:
  Anexos_Clientes.Num_cta_extranjera:BGCOLOR = 18. 
  Anexos_Clientes.Num_cta_extranjera:FGCOLOR = 15. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Anexos_Clientes.Otra_Transac_extr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Otra_Transac_extr wWin
ON LEAVE OF Anexos_Clientes.Otra_Transac_extr IN FRAME F_Economica /* Otra ¿Cual? */
DO:
  Anexos_Clientes.Otra_Transac_extr:BGCOLOR = 18.
  Anexos_Clientes.Otra_Transac_extr:FGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Anexos_Clientes.Per_Ant_Anos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Per_Ant_Anos wWin
ON LEAVE OF Anexos_Clientes.Per_Ant_Anos IN FRAME F_Segmentacion /* Años */
DO:
  Anexos_Clientes.Per_Ant_Anos:BGCOLOR = 18. 
  Anexos_Clientes.Per_Ant_Anos:FGCOLOR = 15. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Anexos_Clientes.Per_Ant_Meses
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Per_Ant_Meses wWin
ON LEAVE OF Anexos_Clientes.Per_Ant_Meses IN FRAME F_Segmentacion /* Meses */
DO:
  Anexos_Clientes.Per_Ant_Meses:BGCOLOR = 18.
  Anexos_Clientes.Per_Ant_Meses:FGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Anexos_Clientes.Per_Meses
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Per_Meses wWin
ON VALUE-CHANGED OF Anexos_Clientes.Per_Meses IN FRAME F_Segmentacion /* Meses */
DO:
  DEF VAR vdias AS INT.

  DO WITH FRAME F_Segmentacion:
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
    /*Si el calculo de los anos y los meses son menos de dos años 
      habilite la direccion anterior telefono y permanencia anterior*/ 
                                                                          
    ASSIGN vdias = (INT(Anexos_Clientes.Per_Anos:SCREEN-VALUE) * 365) 
                 + (INT(Anexos_Clientes.Per_meses:SCREEN-VALUE) * 30).

    IF vdias < 730 THEN DO:
      ENABLE Anexos_Clientes.Tel_Ant_Residencia 
             Anexos_Clientes.Per_Ant_Anos 
             Anexos_Clientes.Per_Ant_Meses.

      Anexos_Clientes.Dir_Ant_Residencia:BGCOLOR = 15.
      Anexos_Clientes.Tel_Ant_Residencia:BGCOLOR = 15. 
      Anexos_Clientes.Per_Ant_Anos:BGCOLOR = 15. 
      Anexos_Clientes.Per_Ant_Meses:BGCOLOR = 15.
      
         
    END.
    ELSE DO:
      ASSIGN Anexos_Clientes.Dir_Ant_Residencia:SCREEN-VALUE = "" 
             Anexos_Clientes.Tel_Ant_Residencia:SCREEN-VALUE = ""
             Anexos_Clientes.Per_Ant_Anos:SCREEN-VALUE = "0"   
             Anexos_Clientes.Per_Ant_Meses:SCREEN-VALUE = "0". 
          
          
      DISABLE Anexos_Clientes.Dir_Ant_Residencia 
              Anexos_Clientes.Tel_Ant_Residencia 
              Anexos_Clientes.Per_Ant_Anos 
              Anexos_Clientes.Per_Ant_Meses.

      Anexos_Clientes.Dir_Ant_Residencia:BGCOLOR = 18.
      Anexos_Clientes.Tel_Ant_Residencia:BGCOLOR = 18. 
      Anexos_Clientes.Per_Ant_Anos:BGCOLOR = 18. 
      Anexos_Clientes.Per_Ant_Meses:BGCOLOR = 18.
    
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Relaciones
&Scoped-define SELF-NAME RActivas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RActivas wWin
ON VALUE-CHANGED OF RActivas IN FRAME F_Relaciones
DO:
  DO WITH FRAME F_Relaciones:
  IF RActivas:SCREEN-VALUE EQ "1" THEN
    Btn_Activas:LABEL = "Borrar".
  ELSE
    Btn_Activas:LABEL = "Activar".
  END.
  APPLY 'value-changed' TO Cmb_Relaciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Reestructurado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Reestructurado wWin
ON VALUE-CHANGED OF Clientes.Reestructurado IN FRAME F_Otros /* Reestructurado */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME Rela_Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rela_Nit wWin
ON LEAVE OF Rela_Nit IN FRAME FRelNva /* Número de identificación */
DO:
    DEFINE VAR Ubicacionx AS CHARACTER.

    DEFINE BUFFER BClientes FOR Clientes.
    DEFINE BUFFER BAnexos FOR Anexos_Clientes.

    FIND FIRST BClientes WHERE BClientes.Nit = Rela_Nit:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE BClientes THEN DO:
        Ubicacionx = "".

        FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = BClientes.Lugar_Residencia NO-LOCK NO-ERROR.
        IF AVAILABLE Ubicacion THEN
            Ubicacionx = Ubicacion.Nombre + (IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna)).

        FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(BClientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
        IF AVAILABLE Ubicacion THEN
            Ubicacionx =  Ubicacionx + " - " + Ubicacion.Nombre.

        FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(BClientes.Lugar_Residencia,1,2) NO-LOCK NO-ERROR.
        IF AVAILABLE Ubicacion THEN
            Ubicacionx = Ubicacionx + " - " + Ubicacion.Nombre.

        FIND BAnexos WHERE BAnexos.Nit = BClientes.Nit NO-LOCK NO-ERROR.
        IF AVAILABLE BAnexos THEN DO:
            R_Nombre = BAnexos.Nombre1.
            R_Nombre2 = BAnexos.Nombre2.
            R_Tel_ResidenciaIndctvo = BAnexos.Ind_Cli.
            R_Tel_ComercialIndctvo = BAnexos.Ind_Comercial.
        END.




        /* oakley */





     ASSIGN  R_Apellido1      = BClientes.Apellido1
             R_Apellido2      = BClientes.Apellido2
             R_Direccion      = BClientes.Dir_Residencia
             R_Ciu_Dpt        = Ubicacionx
             R_Tel_Residencia = BClientes.Tel_Residencia
             R_Tel_Comercial  = BClientes.Tel_Comercial.
            /* R_Ciu_DptCdgo    = BClientes.Lugar_Residencia.*/
     DISPLAY R_Nombre R_nombre2 R_Apellido1 R_Apellido2 R_Direccion R_Ciu_Dpt R_Tel_ResidenciaIndctvo
             R_Tel_Residencia R_Tel_ComercialIndctvo R_Tel_Comercial /*R_Ciu_DptCdgo*/ fechaNacimientoRelacion WITH FRAME FRelNva.
     DISABLE R_Nombre R_Nombre2 R_Apellido1 R_Apellido2 R_Direccion R_Ciu_Dpt R_Tel_ResidenciaIndctvo
             R_Tel_Residencia R_Tel_ComercialIndctvo R_Tel_Comercial /*R_Ciu_DptCdgo*/
             Btn_Lugar_Comercial-2 R_Btn_Direccion-3 fechaNacimientoRelacion WITH FRAME FRelNva.
  END.
  ELSE DO:
     ASSIGN R_Nombre:SCREEN-VALUE = ""
            R_Nombre2:SCREEN-VALUE = ""
            R_Apellido1:SCREEN-VALUE = ""
            R_Apellido2:SCREEN-VALUE = ""
            R_Direccion:SCREEN-VALUE = ""
            R_Ciu_Dpt:SCREEN-VALUE = ""
            R_Tel_ResidenciaIndctvo:SCREEN-VALUE = ""
            R_Tel_Residencia:SCREEN-VALUE = ""
            R_Tel_ComercialIndctvo:SCREEN-VALUE = ""
            R_Tel_Comercial:SCREEN-VALUE = ""
            fechaNacimientoRelacion:SCREEN-VALUE = "".
     ENABLE R_Nombre R_Nombre2 R_Apellido1 R_Apellido2 /* R_Direccion R_Ciu_Dpt */ R_Tel_ResidenciaIndctvo
            R_Tel_Residencia R_Tel_ComercialIndctvo R_Tel_Comercial /*R_Ciu_DptCdgo*/
            Btn_Lugar_Comercial-2 R_Btn_Direccion-3 fechaNacimientoRelacion WITH FRAME FRelNva.
     IF Cmb_Relaciones:SCREEN-VALUE IN FRAME F_Relaciones = "00003 - Rep.Legal Infantil" OR
        R_Relacion:SCREEN-VALUE = "Padre" OR  R_Relacion:SCREEN-VALUE = "Madre" THEN DO:
        MESSAGE "Nit no ha Sido Matriculado como Cliente ni Como Asociado" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Reportado_fiscalia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Reportado_fiscalia wWin
ON VALUE-CHANGED OF Clientes.Reportado_fiscalia IN FRAME F_Otros /* Reportado a Fiscalia */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Reportado_Procredito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Reportado_Procredito wWin
ON VALUE-CHANGED OF Clientes.Reportado_Procredito IN FRAME F_Otros /* Reportado Procrédito */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Reportado_Super
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Reportado_Super wWin
ON VALUE-CHANGED OF Clientes.Reportado_Super IN FRAME F_Otros /* Reportado Superbancaria */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME RSeleccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RSeleccion wWin
ON VALUE-CHANGED OF RSeleccion IN FRAME F_Clientes
DO:
    ASSIGN FRAME F_Clientes RSeleccion.

    wEdad = TODAY - date(Clientes.Fec_Nacimiento:SCREEN-VALUE IN FRAME f_segmentacion).

    ENABLE clientes.Tel_comercial
           Btn_Empresa
           Btn_DireccionComercial
           Btn_Lugar_Comercial
           Btn_cargo
           Anexos_clientes.Ind_Comercial
           Anexos_Clientes.Extencion_Comercial
           Clientes.Fec_IngEmpresa
           Clientes.Carnet
           Cmb_Est_Cargo
           Anexos_clientes.Tel_Fax_Comercial
           Clientes.Tip_Contrato
           Anexos_clientes.Nit_Independiente
           Btn_Ciiu
           Anexos_clientes.Tiempo_Actividad
           Anexos_clientes.Cam_int1
           Anexos_clientes.Fec_Pensionado
        WITH FRAME F_Ubicacion.

    CASE RSeleccion:
        WHEN 1 THEN DO:
            HIDE FRAME F_Ubicacion
                 FRAME F_Economica
                 FRAME F_Relaciones
                 FRAME F_Autorizaciones
                 FRAME F_Documentacion
                 FRAME F_Otros.

            IF Clientes.Tipo_Identificacion:SCREEN-VALUE = "NIT" THEN DO:
                HIDE FRAME F_segmentacion.
                VIEW FRAME F_Juridicas.
            END.
            ELSE DO:
                HIDE FRAME F_Juridicas.
                VIEW FRAME F_Segmentacion.
            END.
        END.

        /* oakley */
        WHEN 2 THEN DO:
            IF Clientes.Tipo_Identificacion:SCREEN-VALUE <> "NIT" THEN DO:
               HIDE FRAME F_segmentacion FRAME F_Autorizaciones FRAME F_Relaciones
                    FRAME F_Documentacion FRAME F_Otros FRAME F_Economica FRAME F_Juridicas.
                VIEW FRAME F_Ubicacion.
          

            END.
            ELSE 
                MESSAGE "Ventana No Disponible Para Personas Juridicas" VIEW-AS ALERT-BOX.
        END.
        WHEN 3 THEN DO:
            IF Clientes.Tipo_Identificacion:SCREEN-VALUE <> "NIT" AND 
                clientes.tipo_cliente:SCREEN-VALUE IN FRAME F_clientes = "1"  THEN DO: /* (5110 / 365) = 14 años */
               HIDE FRAME F_segmentacion FRAME F_Ubicacion FRAME F_Documentacion
                    FRAME F_Autorizaciones FRAME F_Otros FRAME F_Relaciones FRAME F_Juridicas.
               RUN Totales_Economica.
               VIEW FRAME F_Economica.
            END.
            ELSE 
                MESSAGE "Ventana No Disponible Para Menores de Edad o Personas Juridicas" VIEW-AS ALERT-BOX.
        END.
        WHEN 4 THEN DO:
            IF Clientes.Estado:SCREEN-VALUE IN FRAME F_Clientes EQ "2" THEN DO:
               MESSAGE "No se puede actualizar la información de Relaciones" SKIP
                       "ya que el cliente se encuentra retirado" VIEW-AS ALERT-BOX INFORMATION.
               DISABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
               APPLY "entry" TO Btn_Salir IN FRAME F_Clientes.
               RETURN NO-APPLY.
            END.
            FOR EACH T_Relaciones: DELETE T_Relaciones. END.
            OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
        
            HIDE FRAME F_segmentacion FRAME F_Ubicacion FRAME F_Otros FRAME F_Economica
                 FRAME F_Autorizaciones FRAME F_Documentacion FRAME F_Juridicas.
            /*Cmb_Relaciones:SCREEN-VALUE = Cmb_Relaciones:ENTRY(2).*/
            VIEW FRAME F_Relaciones.
                /*IF  Clientes.Tipo_Identificacion:SCREEN-VALUE <> "NIT" THEN
                Cmb_Relaciones:SCREEN-VALUE =  "99999 - Todas las Relaciones".
                 ELSE   
                   Cmb_Relaciones:SCREEN-VALUE = Cmb_Relaciones:ENTRY(7).*/
            APPLY "value-changed" TO  cmb_relaciones IN FRAME f_relaciones.
        END.
        WHEN 5 THEN DO:
            HIDE FRAME F_segmentacion FRAME F_Ubicacion FRAME F_Economica
                 FRAME F_Relaciones FRAME F_Otros FRAME F_Juridicas.
            VIEW FRAME F_Autorizaciones.
        END.
        WHEN 6 THEN DO:
            HIDE FRAME F_segmentacion FRAME F_Ubicacion FRAME F_Documentacion
                 FRAME F_Autorizaciones FRAME F_Relaciones FRAME F_Economica FRAME F_Juridicas.
            VIEW FRAME F_Otros.
        END.
        WHEN 7 THEN DO:
            HIDE FRAME F_segmentacion FRAME F_Ubicacion FRAME F_Autorizaciones
                 FRAME F_Relaciones FRAME F_Economica FRAME F_Otros FRAME F_Juridicas.
            VIEW FRAME F_Documentacion.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Conyuge
&Scoped-define SELF-NAME R_Btn_Direccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Btn_Direccion wWin
ON CHOOSE OF R_Btn_Direccion IN FRAME F_Conyuge /* R_Btn_Direccion */
DO:
    hDrccion = R_Direccion:HANDLE.
    WWin:SENSITIVE = FALSE.
    RUN viewObject IN h_wgnrdordrccion.
    RUN CmbiarDrccion IN h_wgnrdordrccion(R_Direccion:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME R_Btn_Direccion-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Btn_Direccion-3 wWin
ON CHOOSE OF R_Btn_Direccion-3 IN FRAME FRelNva /* R_Btn_Direccion */
DO:
    hDrccion = R_Direccion:HANDLE.
    WWin:SENSITIVE = FALSE.
    RUN viewObject IN h_wgnrdordrccion.
    RUN CmbiarDrccion IN h_wgnrdordrccion(R_Direccion:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Conyuge
&Scoped-define SELF-NAME R_Btn_Empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Btn_Empresa wWin
ON CHOOSE OF R_Btn_Empresa IN FRAME F_Conyuge /* Btn_Empresa */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Ubicacion.r (OUTPUT P_Ubi, OUTPUT P_NUbi).
  VIEW FRAME f_conyuge.
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  ASSIGN WR_CiuEmpresa:SCREEN-VALUE = LC(P_NUbi).
         R_Dir_Empresa:SCREEN-VALUE = P_Ubi. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Btn_Nacimiento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Btn_Nacimiento wWin
ON CHOOSE OF R_Btn_Nacimiento IN FRAME F_Conyuge /* R_Btn_nacimiento */
DO:
/*   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes. */
    ENABLE Btn_Ocultar_conyuge Btn_Grabar_conyuge  WITH FRAME F_Conyuge.
    
    ASSIGN WWin:SENSITIVE = FALSE.
    RUN C-Ubicacion.w(OUTPUT P_Ubi, OUTPUT P_NUbi).
    VIEW FRAME f_conyuge.
    ASSIGN WWin:SENSITIVE = TRUE.
    WWin:MOVE-TO-TOP().
    
    ASSIGN WR_CiuNacimiento:SCREEN-VALUE = LC(P_NUbi).
    R_Lugar_Nacimiento:SCREEN-VALUE = P_Ubi.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Btn_Profesion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Btn_Profesion wWin
ON CHOOSE OF R_Btn_Profesion IN FRAME F_Conyuge /* R_Btn_profesion */
DO:
/*   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes. */
  
  ASSIGN WWin:SENSITIVE = FALSE.
  RUN C-Varios.r (INPUT 1, OUTPUT V_Cod, OUTPUT V_Nom).
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().

  IF V_Cod EQ 0 THEN
     MESSAGE "No se ha elegido ninguna PROFESIÓN" VIEW-AS ALERT-BOX.
  ELSE
     ASSIGN WR_NomProfesion:SCREEN-VALUE = V_Nom.
            R_Cod_Profesion:SCREEN-VALUE IN FRAME F_conyuge = STRING(V_Cod). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Especifique
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Especifique wWin
ON VALUE-CHANGED OF R_Especifique IN FRAME F_Conyuge
DO:
  DO WITH FRAME F_Conyuge:
  
  IF R_Ocupacion:SCREEN-VALUE = "Otro" THEN DO:
     ENABLE R_Especifique. 
     R_Especifique:BGCOLOR = 15.
     R_Especifique:FGCOLOR = 0.
  END.
  ELSE 
     ASSIGN R_Especifique:SCREEN-VALUE = "".
     DISABLE R_Especifique.
     R_Especifique:BGCOLOR = 18.
     R_Especifique:FGCOLOR = 15.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Fec_Nacimiento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Fec_Nacimiento wWin
ON LEAVE OF R_Fec_Nacimiento IN FRAME F_Conyuge
DO:
  IF YEAR(DATE(R_Fec_Nacimiento:SCREEN-VALUE)) LT 1900 OR
     YEAR(DATE(R_Fec_Nacimiento:SCREEN-VALUE)) GT YEAR(TODAY) THEN DO:
     MESSAGE "Año fuera de Rango" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  IF DATE(SELF:SCREEN-VALUE) GT W_fecha THEN DO:
     MESSAGE "La Fecha de Nacimiento o de Constitución" SKIP
             "no puede ser mayor a la fecha actual" SKIP
             "Rectifique de nuevo la fecha" VIEW-AS ALERT-BOX ERROR.
     SELF:SCREEN-VALUE = STRING(W_Fecha).
     APPLY "ENTRY" TO R_Fec_Nacimiento IN FRAME F_conyuge.
     RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Fec_Nacimiento wWin
ON VALUE-CHANGED OF R_Fec_Nacimiento IN FRAME F_Conyuge
DO:
/*    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Niv_Educativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Niv_Educativo wWin
ON VALUE-CHANGED OF R_Niv_Educativo IN FRAME F_Conyuge
DO:
/*    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Segmentacion. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Ocupacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Ocupacion wWin
ON VALUE-CHANGED OF R_Ocupacion IN FRAME F_Conyuge
DO:
  DO WITH FRAME F_Conyuge:
      IF R_Ocupacion:SCREEN-VALUE = "Otro" THEN DO:
         ENABLE R_Especifique. 
         R_Especifique:BGCOLOR = 15.
         R_Especifique:FGCOLOR = 0.
      END.
      ELSE DO:
         ASSIGN R_Especifique:SCREEN-VALUE = "".
         DISABLE R_Especifique.
         R_Especifique:BGCOLOR = 18.
         R_Especifique:FGCOLOR = 15.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRelNva
&Scoped-define SELF-NAME R_Sucursal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Sucursal wWin
ON LEAVE OF R_Sucursal IN FRAME FRelNva /* Sucursal */
DO:
  R_Sucursal:BGCOLOR = 18.
  R_Sucursal:FGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Conyuge
&Scoped-define SELF-NAME R_Tercero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Tercero wWin
ON LEAVE OF R_Tercero IN FRAME F_Conyuge /* No. Identificación */
DO:
    DO WITH FRAME F_Conyuge:
        IF length(trim(SELF:SCREEN-VALUE)) < 5 THEN DO:
           MESSAGE "Número De Identificación Incorrecto" VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN NO-APPLY.
        END. /* IF length(trim(SELF:SCREEN-VALUE)) < 5  */
        
        IF CAN-FIND(FIRST Clientes WHERE Clientes.nit = SELF:SCREEN-VALUE) THEN DO:    
           MESSAGE "INFORMACION: Este Nit Corresponde A Un Afiliado." SKIP
                   "           : A Continuación Se Presenta La Información Existente."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            FIND FIRST Clientes        NO-LOCK WHERE Clientes.nit = SELF:SCREEN-VALUE NO-ERROR.    
            FIND FIRST Anexos_Clientes NO-LOCK WHERE Anexos_Clientes.nit = Clientes.nit NO-ERROR.
            ASSIGN  R_Tipo_Identificacion:SCREEN-VALUE                  = Clientes.Tipo_Identificacion
                    R_Sexo:SCREEN-VALUE                                 = string(Clientes.sexo)
                    R_Apellido1:SCREEN-VALUE                            = Clientes.Apellido1
                    R_Apellido2:SCREEN-VALUE                            = Clientes.Apellido2
                    R_Nombre:SCREEN-VALUE                               = Clientes.Nombre
                    WR_CiuNacimiento:SCREEN-VALUE                       = fDptoCiudad(Clientes.Lugar_Nacimiento)
                    R_Lugar_Nacimiento:SCREEN-VALUE                     = Clientes.Lugar_Nacimiento
                    R_Fec_Nacimiento:SCREEN-VALUE                       = string(Clientes.Fec_Nacimiento)
                    R_Nacionalidad:SCREEN-VALUE                         = IF AVAILABLE Anexos_Clientes THEN Anexos_Clientes.Nacionalidad ELSE R_Nacionalidad:SCREEN-VALUE
                    R_Niv_Educativo:SCREEN-VALUE                        = Clientes.Niv_Educativo
                    WR_NomProfesion:SCREEN-VALUE                        = fPrfsion(Clientes.Cod_Profesion)
                    R_Cod_Profesion:SCREEN-VALUE                        = string(Clientes.Cod_Profesion)
                    R_Ocupacion:SCREEN-VALUE                            = Clientes.Tipo_Actividad
                    R_Especifique:SCREEN-VALUE                          = IF AVAILABLE Anexos_Clientes THEN Anexos_Clientes.Especifique_Ocupacion ELSE R_Especifique:SCREEN-VALUE
                    /* R_Cargo_Actividad:SCREEN-VALUE                      = Relaciones.Rel_Cargo_Acti */
                    R_Empresa:SCREEN-VALUE                              = Clientes.Lugar_comercial
                    R_Direccion:SCREEN-VALUE                            = Clientes.Dir_comercial
                    WR_CiuEmpresa:SCREEN-VALUE                          = fDptoCiudad(Clientes.Lugar_comercial)
                    R_Dir_Empresa:SCREEN-VALUE                          = Clientes.Lugar_comercial
                    R_Tel_Empresa:SCREEN-VALUE                          = Clientes.Tel_Comercial
                    /* R_TIngreso:SCREEN-VALUE                             = Relaciones.Tot_Ing_Conyuge */
                    /* R_TEgresos:SCREEN-VALUE                             = Relaciones.Tot_Egr_Conyuge */
                    R_Tel_Empresa-Indctvo:SCREEN-VALUE                  = IF AVAILABLE Anexos_Clientes THEN string(Anexos_Clientes.Ind_cli) ELSE R_Tel_Empresa-Indctvo:SCREEN-VALUE
                    CnygeExtncion:SCREEN-VALUE                          = string(Anexos_Clientes.extencion_comercial).

        END. /* IF CAN-FIND(FIRST Clientes WHERE Clientes.nit = SELF:SCREEN-VALUE) */
    END. /* DO WITH FRAME F_Conyuge: */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Microempresas
&Scoped-define SELF-NAME Salir_MicroEmpresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Salir_MicroEmpresa wWin
ON CHOOSE OF Salir_MicroEmpresa IN FRAME F_Microempresas /* Salir */
DO:
  HIDE  FRAME F_Microempresas.
  CLEAR FRAME F_Microempresas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Otros
&Scoped-define SELF-NAME Clientes.Sancionado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Sancionado wWin
ON VALUE-CHANGED OF Clientes.Sancionado IN FRAME F_Otros /* Sancionado */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Sexo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Sexo wWin
ON VALUE-CHANGED OF Clientes.Sexo IN FRAME F_Segmentacion /* Sexo */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  IF SELF:SCREEN-VALUE = "1" THEN do:
     r_sexo:SCREEN-VALUE IN FRAME f_conyuge = "2". 
  END.
  ELSE do:
     r_sexo:SCREEN-VALUE IN FRAME f_conyuge = "1".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Anexos_Clientes.Tel_Ant_Residencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Tel_Ant_Residencia wWin
ON LEAVE OF Anexos_Clientes.Tel_Ant_Residencia IN FRAME F_Segmentacion /* Teléfono Anterior de la Residencia */
DO:
  Anexos_Clientes.Tel_Ant_Residencia:BGCOLOR = 18. 
  Anexos_Clientes.Tel_Ant_Residencia:FGCOLOR = 15. 
  
  IF Anexos_Clientes.Tel_Ant_Residencia:SCREEN-VALUE GT " "  THEN DO:
     ASSIGN Tel_Numerico = DEC(Anexos_Clientes.Tel_Ant_Residencia:SCREEN-VALUE) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Solo Numèrico...Corrija por favor." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO SELF.
        RETURN NO-APPLY.
     END.
  END.

  IF LENGTH(Anexos_Clientes.Tel_Ant_Residencia:SCREEN-VALUE) < 7 THEN DO:
     MESSAGE "Tel_Ant_Residencia Debe contener minimo 7 caracteres" VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "ENTRY" TO SELF.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Tel_Arrendatario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tel_Arrendatario wWin
ON LEAVE OF Clientes.Tel_Arrendatario IN FRAME F_Segmentacion /* Telefono Arrendatario */
DO:
    Clientes.Tel_Arrendatario:BGCOLOR = 18.
    Clientes.Tel_Arrendatario:FGCOLOR = 15.
    Anexos_Clientes.ind_arr:BGCOLOR = 18.
    Anexos_Clientes.ind_arr:FGCOLOR = 15.
    
    IF Clientes.Tel_Arrendatario:SCREEN-VALUE GT " "  
    THEN DO:
        ASSIGN Tel_Numerico = DEC(Clientes.Tel_Arrendatario:SCREEN-VALUE) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
           MESSAGE "Solo Numérico...Corrija por favor." VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "ENTRY" TO SELF.
           RETURN NO-APPLY.
        END.
    END.
    
    IF LENGTH(Clientes.tel_arrendatario:SCREEN-VALUE) < 7 THEN DO:
       MESSAGE "Clientes.tel_arrendatario Debe contener mínimo 7 caracteres" VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "ENTRY" TO SELF.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tel_Arrendatario wWin
ON VALUE-CHANGED OF Clientes.Tel_Arrendatario IN FRAME F_Segmentacion /* Telefono Arrendatario */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Clientes.Tel_comercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tel_comercial wWin
ON LEAVE OF Clientes.Tel_comercial IN FRAME F_Ubicacion /* Tel Comercial */
DO:
  DEFI VAR K AS INTEG FORM 99 NO-UNDO .

  ASSIGN Tel_Numerico = DEC(Clientes.Tel_Comercial:SCREEN-VALUE) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "Solo Numèrico ...Corrija por favor." VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "ENTRY" TO SELF.
     RETURN NO-APPLY.
  END.

  IF LENGTH(Clientes.Tel_Comercial:SCREEN-VALUE) < 7 THEN DO:
     MESSAGE "Clientes.Tel_Comercial Debe contener minimo 7 caracteres" VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "ENTRY" TO SELF.
     RETURN NO-APPLY.
  END.
  
  Clientes.Tel_comercial:BGCOLOR = 18.
  Clientes.Tel_comercial:FGCOLOR = 15.
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tel_comercial wWin
ON VALUE-CHANGED OF Clientes.Tel_comercial IN FRAME F_Ubicacion /* Tel Comercial */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Anexos_Clientes.Tel_Fax_Comercial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Tel_Fax_Comercial wWin
ON LEAVE OF Anexos_Clientes.Tel_Fax_Comercial IN FRAME F_Ubicacion /* Fax */
DO:
  IF Anexos_Clientes.Tel_Fax_Comercial:SCREEN-VALUE GT " "  THEN DO:
     ASSIGN Tel_Numerico = DEC(Anexos_Clientes.Tel_Fax_Comercial:SCREEN-VALUE) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Solo Numèrico...Corrija por favor." VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO SELF.
        RETURN NO-APPLY.
     END.
  END.

/*IF LENGTH(Anexos_Clientes.Tel_Fax_Comercial:SCREEN-VALUE) < 7 THEN DO:
     MESSAGE "Anexos_Clientes.Tel_Fax_Comercial Debe contener minimo 7 caracteres" VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "ENTRY" TO SELF.
     RETURN NO-APPLY.
  END.*/
  Anexos_Clientes.Tel_Fax_Comercial:BGCOLOR = 18.
  Anexos_Clientes.Tel_Fax_Comercial:FGCOLOR = 15.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Tel_Residencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tel_Residencia wWin
ON VALUE-CHANGED OF Clientes.Tel_Residencia IN FRAME F_Segmentacion /* Teléfono de la Residencia */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Anexos_Clientes.Tiempo_Actividad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Tiempo_Actividad wWin
ON VALUE-CHANGED OF Anexos_Clientes.Tiempo_Actividad IN FRAME F_Ubicacion /* Tiempo Actividad */
DO:
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Clientes
&Scoped-define SELF-NAME Clientes.Tipo_Cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Cliente wWin
ON VALUE-CHANGED OF Clientes.Tipo_Cliente IN FRAME F_Clientes /* Tipo de Cliente */
DO:
  RUN TipoCliente.
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Tipo_Identificacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Identificacion wWin
ON LEAVE OF Clientes.Tipo_Identificacion IN FRAME F_Clientes /* Tipo DocId */
DO:
  DO WITH FRAME F_Clientes:
      CASE Clientes.Tipo_Identificacion:SCREEN-VALUE:
         WHEN "NIT" THEN DO:
             Clientes.Tipo_Cliente:SCREEN-VALUE = "2".
             W_Ok = Clientes.Tipo_Cliente:DISABLE("Natural").
             W_Ok = Clientes.Tipo_Cliente:ENABLE("Jurídica").

/*              ENABLE Clientes.Id_Retencio Clientes.Gran_Contribuyente WITH FRAME F_Ubicacion. */
         END.
         OTHERWISE DO:
             Clientes.Tipo_Cliente:SCREEN-VALUE = "1".
             W_Ok = Clientes.Tipo_Cliente:ENABLE("Natural").
             W_Ok = Clientes.Tipo_Cliente:DISABLE("Juridica").
/*              DISABLE Clientes.Id_Retencio Clientes.Gran_Contribuyente WITH FRAME F_Ubicacion. */
         END.
      END CASE.
      RUN TipoCliente.
  END.
  
/*APPLY "ENTRY" TO Clientes.Nit.
  RETURN NO-APPLY.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Identificacion wWin
ON RETURN OF Clientes.Tipo_Identificacion IN FRAME F_Clientes /* Tipo DocId */
DO:
  APPLY "LEAVE" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Identificacion wWin
ON TAB OF Clientes.Tipo_Identificacion IN FRAME F_Clientes /* Tipo DocId */
DO:
  APPLY "LEAVE" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Identificacion wWin
ON VALUE-CHANGED OF Clientes.Tipo_Identificacion IN FRAME F_Clientes /* Tipo DocId */
DO:
    DEFINE VAR digitoVerificacion AS INTEGER.

    Anexos_Clientes.Dir_Correspondencia:SCREEN-VALUE IN FRAME F_segmentacion = "1".

    IF clientes.tipo_identificacion:SCREEN-VALUE = "NIT" /*AND clientes.dv:SCREEN-VALUE = ""*/ THEN DO:
        RUN digitoVerificacion.r(INPUT clientes.nit:SCREEN-VALUE,
                                 OUTPUT digitoVerificacion).

        clientes.dv:SCREEN-VALUE = STRING(digitoVerificacion).
    END.
    ELSE
        clientes.dv:SCREEN-VALUE = "".


    DO WITH FRAME F_Clientes:
        CASE Clientes.Tipo_Identificacion:SCREEN-VALUE:
            WHEN "NIT" THEN DO:
                Clientes.Tipo_Cliente:SCREEN-VALUE = "2".
                W_Ok = Clientes.Tipo_Cliente:DISABLE("Natural").
                W_Ok = Clientes.Tipo_Cliente:ENABLE("Jurídica").
                Anexos_Clientes.Dir_Correspondencia:SCREEN-VALUE IN FRAME F_segmentacion = "2".
                W_Ok = Anexos_Clientes.Dir_Correspondencia:ENABLE("Oficina").
                W_Ok = Anexos_Clientes.Dir_Correspondencia:DISABLE ("A.A.").

                clientes.nombre:LABEL = "Razón Social".
                
                HIDE FRAME F_Segmentacion. VIEW FRAME F_Juridicas.
            END.

            OTHERWISE DO:
                Clientes.Tipo_Cliente:SCREEN-VALUE = "1".
                W_Ok = Clientes.Tipo_Cliente:ENABLE("Natural").
                W_Ok = Clientes.Tipo_Cliente:DISABLE("Jurídica").
                W_Ok = Anexos_Clientes.Dir_Correspondencia:ENABLE("Oficina").
                W_Ok = Anexos_Clientes.Dir_Correspondencia:ENABLE("A.A.").

                clientes.nombre:LABEL = "Nombre".

                HIDE FRAME F_Juridicas.
                VIEW FRAME F_Segmentacion.
            END.
        END CASE.

        RUN TipoCliente.
    END.

    IF Clientes.Tipo_Identificacion:SCREEN-VALUE = "NIT" THEN DO:
        clientes.apellido1:VISIBLE = FALSE.
        clientes.apellido2:VISIBLE = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Anexos_Clientes.Tipo_Moneda_Divisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Tipo_Moneda_Divisa wWin
ON LEAVE OF Anexos_Clientes.Tipo_Moneda_Divisa IN FRAME F_Economica /* Tipo Moneda */
DO:
  Anexos_Clientes.Tipo_Moneda_Divisa:BGCOLOR = 18.
  Anexos_Clientes.Tipo_Moneda_Divisa:FGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Clientes.Tipo_Vinculo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Vinculo wWin
ON VALUE-CHANGED OF Clientes.Tipo_Vinculo IN FRAME F_Segmentacion /* codigo Vinculo */
DO:
  ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Tipo_Vivienda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tipo_Vivienda wWin
ON VALUE-CHANGED OF Clientes.Tipo_Vivienda IN FRAME F_Segmentacion /* Tipo de Vivienda */
DO:
    DO WITH FRAME F_Segmentacion:
        ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
        IF DECIMAL(SELF:SCREEN-VALUE) = 2 THEN DO:
            ENABLE  Clientes.Nom_Arrendatario 
            Clientes.Tel_Arrendatario
            Anexos_Clientes.Val_arriendo
            Anexos_Clientes.per_anos
            Anexos_Clientes.per_meses
            Anexos_Clientes.ind_arr
            Anexos_Clientes.Tel_Ant_Residencia
            Anexos_Clientes.per_ant_anos
            Anexos_Clientes.per_ant_meses
            /*Anexos_Clientes.Dir_Ant_Residencia */
            Anexos_Clientes.DIR_arrendatario
            Btn_Direccion_Arendatario
            btn_direccion_ante.
            
            Clientes.Nom_Arrendatario:BGCOLOR = 15.
            Clientes.Tel_Arrendatario:BGCOLOR = 15.
            Anexos_Clientes.Val_arriendo:BGCOLOR = 15.
            Anexos_Clientes.Dir_Arrendatario:BGCOLOR = 15.
            Anexos_Clientes.per_anos:BGCOLOR = 15.
            Anexos_Clientes.per_meses:BGCOLOR = 15.
            Anexos_Clientes.ind_arr:BGCOLOR = 15.
            Anexos_Clientes.Tel_Ant_Residencia:BGCOLOR = 15.
            Anexos_Clientes.per_ant_anos:BGCOLOR = 15.
            Anexos_Clientes.per_ant_meses:BGCOLOR = 15.
            Btn_Direccion_Arendatario:BGCOLOR = 15.
            btn_direccion_ante:BGCOLOR = 15.
            Anexos_Clientes.Dir_Ant_Residencia:BGCOLOR = 15.
            APPLY "entry" TO per_anos.
        END.
        ELSE DO:
            ASSIGN  Clientes.Nom_Arrendatario:SCREEN-VALUE = "".
            Clientes.Tel_Arrendatario:SCREEN-VALUE = "".
            Anexos_Clientes.Val_arriendo:SCREEN-VALUE = "".
            Anexos_Clientes.per_anos:SCREEN-VALUE = "".
            Anexos_Clientes.per_meses:SCREEN-VALUE = "".
            Anexos_Clientes.ind_arr:SCREEN-VALUE = "".
            Anexos_Clientes.Tel_Ant_Residencia:SCREEN-VALUE = "".
            Anexos_Clientes.per_ant_anos:SCREEN-VALUE = "".
            Anexos_Clientes.per_ant_meses:SCREEN-VALUE = "".
            Anexos_Clientes.Dir_Ant_Residencia:SCREEN-VALUE = "".
            DISABLE Clientes.Nom_Arrendatario 
            Clientes.Tel_Arrendatario
            Anexos_Clientes.Val_arriendo
            Anexos_Clientes.per_anos
            Anexos_Clientes.per_meses
            Anexos_Clientes.ind_arr
            Anexos_Clientes.Tel_Ant_Residencia
            Anexos_Clientes.per_ant_anos
            Anexos_Clientes.per_ant_meses
            Btn_Direccion_Arendatario
            btn_direccion_ante
            Anexos_Clientes.Dir_Ant_Residencia
            Anexos_Clientes.DIR_arrendatario.
            
            Clientes.Nom_Arrendatario:BGCOLOR = 18.
            Clientes.Tel_Arrendatario:BGCOLOR = 18.
            Anexos_Clientes.Val_arriendo:BGCOLOR = 18.
            Anexos_Clientes.Dir_Arrendatario:BGCOLOR = 18.
            Anexos_Clientes.per_anos:BGCOLOR = 18.
            Anexos_Clientes.per_meses:BGCOLOR = 18.
            Anexos_Clientes.ind_arr:BGCOLOR = 18.
            Anexos_Clientes.Tel_Ant_Residencia:BGCOLOR = 18.
            Anexos_Clientes.per_ant_anos:BGCOLOR = 18.
            Anexos_Clientes.per_ant_meses:BGCOLOR = 18.
            Btn_Direccion_Arendatario:BGCOLOR = 18.
            btn_direccion_ante:BGCOLOR = 18.
            Anexos_Clientes.Dir_Ant_Residencia:bgcolor = 18.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ubicacion
&Scoped-define SELF-NAME Clientes.Tip_Contrato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Tip_Contrato wWin
ON VALUE-CHANGED OF Clientes.Tip_Contrato IN FRAME F_Ubicacion /* Tipo Contrato */
DO:
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economica
&Scoped-define SELF-NAME Anexos_Clientes.Transac_Mod_Ext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Transac_Mod_Ext wWin
ON VALUE-CHANGED OF Anexos_Clientes.Transac_Mod_Ext IN FRAME F_Economica /* Transacciones Moneda Extranjera */
DO:
    IF can-do(SELF:SCREEN-VALUE,"otros")
    THEN Anexos_Clientes.Otra_Transac_extr:SENSITIVE = TRUE.
    ELSE Anexos_Clientes.Otra_Transac_extr:SENSITIVE = FALSE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Segmentacion
&Scoped-define SELF-NAME Anexos_Clientes.Val_arriendo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Anexos_Clientes.Val_arriendo wWin
ON LEAVE OF Anexos_Clientes.Val_arriendo IN FRAME F_Segmentacion /* Valor arriendo */
DO:
    Anexos_Clientes.Val_arriendo:BGCOLOR = 18.
    Anexos_Clientes.Val_arriendo:FGCOLOR = 15.
/*     IF length(string(decimal(SELF:SCREEN-VALUE))) < 4 THEN DO:                                        */
/*        MESSAGE "El Valor Del Arriendo No Puede Se Inferior A 1000" VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*        RETURN NO-APPLY.                                                                               */
/*     END.                                                                                              */
    Clientes.gto_arriendo:SCREEN-VALUE IN FRAME f_economica = Anexos_Clientes.val_arriendo:SCREEN-VALUE in FRAME f_segmentacion.
    RUN Totales_Economica.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Aportes
&Scoped-define BROWSE-NAME Br_Relaciones
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i} 

/* SOLO ALFABETICO + NUMERICO */
ON 'any-printable':U OF R_Nombre IN FRAME frelnva
DO:
    IF NOT (fchar(KEYLABEL(LASTKEY)) OR fdgto(KEYLABEL(LASTKEY))) 
    THEN  RETURN NO-APPLY.
   ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
       
    RETURN.    
END.
/* SOLO ALFABETICO */
ON 'any-printable':U OF nacionalidad,Clientes.nom_arrendatario,Clientes.nombre,Clientes.apellido1,Clientes.apellido2,R_Empresa IN FRAME f_conyuge,
                        R_Cargo_Actividad IN FRAME f_conyuge,Anexos_Clientes.tipo_moneda_divisa IN FRAME f_economica,Anexos_Clientes.nom_banco_extranjero
                        IN FRAME f_economica,Anexos_Clientes.ciudad_pais_bco_extranjero IN FRAME f_economica,R_Apellido1  IN FRAME frelnva
DO:
    IF NOT fchar(KEYLABEL(LASTKEY))
    THEN RETURN NO-APPLY.
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
    RETURN.
    
END.
/* SOLO NUMERICO */
ON 'any-printable':U OF Clientes.tel_residencia,Anexos_Clientes.AA_Cliente,R_Tel_Empresa IN FRAME f_conyuge,Clientes.tel_comercial IN FRAME F_Ubicacion,
                        Anexos_Clientes.nit_independiente IN FRAME f_ubicacion,Anexos_Clientes.num_cta_extranjera IN FRAME f_economica,Rela_Nit
                        IN FRAME FRelNva,R_Tel_Residencia IN FRAME FRelNva,R_Tel_Comercial IN FRAME frelnva,Clientes.celular IN FRAME f_segmentacion
DO:
    IF NOT fdgto(KEYLABEL(LASTKEY))
    THEN RETURN NO-APPLY.
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
    RETURN.    
END.
/* CONVIERTE A MAYUSCULAS */
ON 'leave':U OF R_Apellido2 in FRAME f_conyuge,R_Apellido2 IN FRAME frelnva,R_Nacionalidad IN FRAME f_conyuge,Anexos_Clientes.nacionalidad
                IN FRAME f_segmentacion,R_Cargo_Actividad
DO:
    SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
    RETURN.    
END.
/*  Rela_Nitzz IN FRAME frelnva,R_Nombre  IN FRAME frelnva,R_Apellido1  IN FRAME frelnva,R_Relacion IN FRAME frelnva*/
ON 'leave':U OF nacionalidad,Clientes.nom_arrendatario,R_Apellido2 in FRAME f_conyuge,R_Empresa in FRAME
                f_conyuge,R_Nombre IN FRAME frelnva,R_Apellido1 IN FRAME frelnva,R_Relacion IN FRAME frelnva
DO:
/*     IF LENGTH(trim(SELF:SCREEN-VALUE)) = 0 THEN DO:            */
/*        MESSAGE "Incorrecto" VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*        RETURN NO-APPLY.                                        */
/*     END.                                                       */
    SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
    RETURN.
END.

/* INDICATIVOS TELEFONICOS */
ON 'leave':U OF Anexos_Clientes.ind_cli,
                Anexos_Clientes.ind_arr,
                R_Tel_Empresa-Indctvo IN FRAME f_conyuge,
                Anexos_Clientes.ind_comercial IN FRAME f_ubicacion,
                R_Tel_ResidenciaIndctvo IN FRAME FRelNva,
                R_Tel_ComercialIndctvo IN FRAME frelnva
DO:
    IF NOT INTEGER(SELF:SCREEN-VALUE) = 0 
    THEN
    IF length(LEFT-TRIM(SELF:SCREEN-VALUE,"0")) < 2 THEN DO:
       MESSAGE "Indicativo Incorrecto" VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN NO-APPLY.
    END.

    RETURN.
END.
/* TELEFONOS */
ON 'leave':U OF Clientes.tel_residencia,Clientes.tel_arrendatario,R_Tel_Empresa IN FRAME f_conyuge,Clientes.tel_comercial IN FRAME F_Ubicacion
DO:
    IF LENGTH(LEFT-TRIM(SELF:SCREEN-VALUE)) < 6 THEN DO:
       MESSAGE "Número Telefónico Incorrecto" VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN NO-APPLY.
    END.
/*
  DEFI VAR K AS INTEG FORM "99".

  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "Solo Numèrico ...Corrija por favor."
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "ENTRY" TO SELF.
     RETURN NO-APPLY.
  END.
     
  IF LENGTH(Clientes.Tel_Resid:SCREEN-VALUE) < 7 THEN DO:
    MESSAGE "Debe contener minimo 7 caracteres.... Corriha por Favor."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "ENTRY" TO SELF.
    RETURN NO-APPLY.
  END.
*/  
END.
ON 'leave':U OF Clientes.nombre IN FRAME F_Clientes,Clientes.apellido1,
    R_Nombre IN FRAME f_conyuge,R_Apellido1 IN FRAME f_conyuge
DO:
    SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
/*     IF LENGTH(trim(SELF:SCREEN-VALUE)) = 0 THEN DO:                             */
/*        MESSAGE fTxtoCmpo(SELF) " Incorrecto" VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*        RETURN NO-APPLY.                                                         */
/*     END.                                                                        */
    ASSIGN Clientes.Nombre:SCREEN-VALUE IN FRAME F_Clientes = CAPS(Clientes.Nombre:SCREEN-VALUE IN FRAME F_Clientes).
           
    /*IF Clientes.Tipo_Identificacion:SCREEN-VALUE = "Nit" THEN DO:
     FIND FIRST ListaNegra WHERE ListaNegra.Nombre EQ NomLN NO-LOCK NO-ERROR.
     IF AVAILABLE ListaNegra THEN DO:
        MESSAGE "Esta empresa se encuentra en las listas negras" SKIP
                "de la cooperativa. y se identifica con el nit: " ListaNegra.Nit SKIP
                "La operacion se cancelara!" SKIP(1)
                "Reporte este incidente al departamento de CI" VIEW-AS ALERT-BOX WARNING TITLE "Lista Negra".
        APPLY "choose" TO btn_cancelar.
     END.
    END.*/
END.

ON 'leave':U OF Clientes.salario IN FRAME f_economica,Clientes.ing_honorarios IN FRAME f_economica,Clientes.ing_financieros IN FRAME f_economica,
                Clientes.ing_arriendos IN FRAME f_economica,Clientes.ing_otros IN FRAME f_economica,Clientes.gto_obligacion IN FRAME f_economica,
                Clientes.gto_arriendo IN FRAME f_economica,Clientes.sdo_obligaciones IN FRAME f_economica,Anexos_Clientes.gto_targetacredito
                IN FRAME f_economica,Clientes.gto_familiar IN FRAME f_economica,Anexos_Clientes.gto_otros IN FRAME f_economica
DO:
    IF decimal(SELF:SCREEN-VALUE) > 0 THEN
       IF decimal(SELF:SCREEN-VALUE) < 1000 THEN DO:
          MESSAGE "Valor Incorrecto" VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
       END.
    RUN Totales_Economica.
    DO WITH FRAME F_Economica:
        IF INT(Clientes.ing_otros:SCREEN-VALUE) <> 0 
        THEN DO:
            ENABLE  Anexos_Clientes.Especifique_OtrosIng.
                    Anexos_Clientes.Especifique_OtrosIng:BGCOLOR = 15.
                    Anexos_Clientes.Especifique_OtrosIng:FGCOLOR = 0.
            END.
            ELSE DO:
                ASSIGN  Anexos_Clientes.Especifique_OtrosIng:SCREEN-VALUE = '0'.
                DISABLE Anexos_Clientes.Especifique_OtrosIng.
                        Anexos_Clientes.Especifique_OtrosIng:BGCOLOR = 18.
                        Anexos_Clientes.Especifique_OtrosIng:FGCOLOR = 15.
        END.
    END.
END.
ON 'value-changed':U OF 
    Rela_Nit IN FRAME frelnva,
    R_Nombre IN FRAME frelnva,R_Apellido1 IN FRAME frelnva,
    R_Apellido2 IN FRAME frelnva,R_Relacion IN FRAME frelnva,R_Direccion IN FRAME frelnva,
    R_Ciu_Dpt IN FRAME frelnva,R_Tel_Residencia IN FRAME frelnva,R_Tel_Comercial IN FRAME frelnva,
    R_Tel_ResidenciaIndctvo IN FRAME FRelNva,Clientes.nombre IN FRAME F_Clientes,
    Cmb_Linea IN FRAME F_Documentacion,Cmb_Contrato IN FRAME f_documentacion,
    docum_requer[1]  IN FRAME f_documentacion,
    docum_requer[2]  IN FRAME f_documentacion,
    docum_requer[3]  IN FRAME f_documentacion,
    docum_requer[4]  IN FRAME f_documentacion,
    docum_requer[5]  IN FRAME f_documentacion,
    docum_requer[6]  IN FRAME f_documentacion,
    docum_requer[7]  IN FRAME f_documentacion,
    docum_requer[8]  IN FRAME f_documentacion,
    docum_requer[9]  IN FRAME f_documentacion,
    docum_requer[10]  IN FRAME f_documentacion,
    docum_requer[11]  IN FRAME f_documentacion,
    docum_requer[12]  IN FRAME f_documentacion,
    docum_requer[13]  IN FRAME f_documentacion,
    docum_requer[14]  IN FRAME f_documentacion,
    docum_requer[15]  IN FRAME f_documentacion,
    docum_requer[16]  IN FRAME f_documentacion,
    docum_requer[17]  IN FRAME f_documentacion,
    docum_requer[18]  IN FRAME f_documentacion,
    docum_requer[19]  IN FRAME f_documentacion,
    autorizaciones[1] IN FRAME f_autorizaciones,
    autorizaciones[2] IN FRAME f_autorizaciones,
    autorizaciones[3] IN FRAME f_autorizaciones,
    autorizaciones[4] IN FRAME f_autorizaciones,
    autorizaciones[5] IN FRAME f_autorizaciones,
    autorizaciones[6] IN FRAME f_autorizaciones,
    autorizaciones[7] IN FRAME f_autorizaciones,
    autorizaciones[8] IN FRAME f_autorizaciones,
    autorizaciones[9] IN FRAME f_autorizaciones,
    autorizaciones[10] IN FRAME f_autorizaciones,
    autorizaciones[11] IN FRAME f_autorizaciones,
    autorizaciones[12] IN FRAME f_autorizaciones,
    represetacion  IN FRAME f_autorizaciones,
    proviene  IN FRAME f_autorizaciones,
    declaracion_conocimiento IN FRAME f_autorizaciones,
    declaracion_aceptacion IN FRAME f_autorizaciones,
    Anexos_Clientes.per_anos IN FRAME f_segmentacion,
    Anexos_Clientes.per_meses IN FRAME f_segmentacion,
    Clientes.salario IN FRAME F_Economica,
    Clientes.ing_honorarios IN FRAME F_Economica,
    Clientes.ing_financieros  IN FRAME F_Economica,
    Clientes.ing_arriendos  IN FRAME F_Economica,
    Clientes.ing_otros  IN FRAME F_Economica,
    Clientes.gto_obligacion  IN FRAME F_Economica,
    Clientes.gto_arriendo  IN FRAME F_Economica,
    Clientes.sdo_obligaciones  IN FRAME F_Economica,
    Anexos_Clientes.gto_targetacredito IN FRAME f_economica,
    Clientes.gto_familiar  IN FRAME f_economica,
    Anexos_Clientes.transac_mod_ext IN FRAME f_economica,
    cam_log1 IN FRAME f_autorizaciones,
    Anexos_Clientes.fec_diligenciamiento IN FRAME F_Autorizaciones    
DO:
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
    RETURN.
END.

ON 'leave','value-changed','any-printable' OF per_anos
DO:
    DEF VAR iColor AS INTEGER NO-UNDO.
    icolor = IF INTEGER(SELF:SCREEN-VALUE) >= 2 THEN 18 ELSE 15.
    DO WITH FRAME F_Segmentacion:
       Anexos_Clientes.Dir_Ant_Residencia:SENSITIVE = FALSE.
       Btn_Direccion_Ante:SENSITIVE = NOT INTEGER(SELF:SCREEN-VALUE) >= 2.
       Anexos_Clientes.Tel_Ant_Residencia:SENSITIVE = NOT INTEGER(SELF:SCREEN-VALUE) >= 2.
       Anexos_Clientes.per_ant_anos:SENSITIVE  = NOT INTEGER(SELF:SCREEN-VALUE) >= 2.
       Anexos_Clientes.per_ant_meses:SENSITIVE = NOT INTEGER(SELF:SCREEN-VALUE) >= 2.
       Anexos_Clientes.Dir_Ant_Residencia:BGCOLOR = icolor.
       Anexos_Clientes.Tel_Ant_Residencia:BGCOLOR = icolor.
       Anexos_Clientes.per_ant_anos:BGCOLOR  = icolor.
       Anexos_Clientes.per_ant_meses:BGCOLOR = icolor.
       IF INTEGER(SELF:SCREEN-VALUE) >= 2 THEN DO:
          Anexos_Clientes.Dir_Ant_Residencia:SCREEN-VALUE = "".
          Anexos_Clientes.Tel_Ant_Residencia:SCREEN-VALUE = "".
          Anexos_Clientes.per_ant_anos:SCREEN-VALUE = "".
          Anexos_Clientes.per_ant_meses:SCREEN-VALUE = "".
       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Abrir_Excel wWin 
PROCEDURE Abrir_Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE "Excel.Application" chExcelApp.
    SwExiste = SEARCH(InputFile).
    IF SwExiste EQ ? THEN DO:
       MESSAGE InputFile "no Encontrado" VIEW-AS ALERT-BOX.
       RETURN.
    END.
    hWorkBooks = chExcelApp:WorkBooks:OPEN(SwExiste,,TRUE,).
    chExcelApp:Visible = TRUE.
    chWorkSheet = chExcelApp:Sheets:Item(1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'wgnrdordrccion.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_wgnrdordrccion ).
       /* Position in AB:  ( 6.88 , 132.86 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'wsolprofina.r':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_wsolprofina ).
       /* Position in AB:  ( 22.62 , 134.86 ) */
       /* Size in AB:  ( 1.50 , 7.72 ) */

       /* Links to SmartWindow h_wsolprofina. */
       RUN addLink ( h_wgnrdordrccion , 'Data':U , h_wsolprofina ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cerrar_Excel wWin 
PROCEDURE Cerrar_Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SYSTEM-DIALOG PRINTER-SETUP. 
    PrinterName = SESSION:PRINTER-NAME.
    hWorkBooks:PrintOut(1,2,1,FALSE,PrinterName,).
    chExcelApp:displayalerts = FALSE.
    chExcelApp:Application:WorkBooks:CLOSE() NO-ERROR.
    chExcelApp:Application:QUIT NO-ERROR.
    RELEASE OBJECT hWorkBooks.
    RELEASE OBJECT chExcelApp.      
    RELEASE OBJECT chWorksheet.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CierraClientesPasivosBienes wWin 
PROCEDURE CierraClientesPasivosBienes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

      ASSIGN WWin:SENSITIVE = TRUE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CodigoCiiu wWin 
PROCEDURE CodigoCiiu :
DEF VAR i AS INT NO-UNDO.
  DO WITH FRAME F_ubicacion:
     ASSIGN W_NomCiiu = "" W_NomCiiu-2 = "".
     FIND FIRST Ciiu WHERE Ciiu.Tipo EQ 1 AND Ciiu.Grupo EQ Clientes.Grupo NO-LOCK NO-ERROR.
     IF AVAIL Ciiu THEN ASSIGN W_NomCiiu = W_NomCiiu + LOWER(TRIM(STRING(Ciiu.Descripcion,"X(15)"))).
                   ELSE ASSIGN W_NomCiiu = W_NomCiiu + "Sin Grupo".
     FIND FIRST Ciiu WHERE Ciiu.Tipo EQ 2 AND Ciiu.Grupo EQ Clientes.Grupo AND Ciiu.SubGrupo EQ Clientes.SubGrupo NO-LOCK NO-ERROR.
     IF AVAIL Ciiu THEN ASSIGN W_NomCiiu = W_NomCiiu + " - " + LOWER(TRIM(STRING(Ciiu.Descripcion,"X(15)"))).
                   ELSE ASSIGN W_NomCiiu = W_NomCiiu + " - Sin SubGrupo".
     FIND FIRST Ciiu WHERE Ciiu.Tipo EQ 3 AND Ciiu.Grupo EQ Clientes.Grupo AND Ciiu.Subgrupo
          EQ Clientes.SubGrupo AND Ciiu.Codigo_Ciiu EQ Clientes.Codigo_Ciiu NO-LOCK NO-ERROR.
     IF AVAIL Ciiu THEN ASSIGN W_NomCiiu = W_NomCiiu + " - " + LOWER(TRIM(STRING(Ciiu.Descripcion,"X(30)"))).
                   ELSE ASSIGN W_NomCiiu = W_NomCiiu + " - Sin Codigo".
     DISP W_NomCiiu.
     cCdgoCiiu = Anexos_Clientes.Acti_Economica_Emp + FILL("-",2).
     DO i = 1 TO 3:
        FIND FIRST Ciiu NO-LOCK WHERE Ciiu.Tipo = i
             AND (IF i = 1 THEN Ciiu.Grupo = INTEGER(ENTRY(1,cCdgoCiiu,"-")) ELSE TRUE)
             AND (IF i = 2 THEN Ciiu.Grupo = INTEGER(ENTRY(1,cCdgoCiiu,"-")) AND Ciiu.subgrupo = INTEGER(ENTRY(2,cCdgoCiiu,"-")) ELSE TRUE)
             AND (IF i = 3 THEN Ciiu.Grupo = INTEGER(ENTRY(1,cCdgoCiiu,"-")) AND Ciiu.subgrupo = INTEGER(ENTRY(2,cCdgoCiiu,"-")) AND
                  Ciiu.Codigo_Ciiu = INTEGER(ENTRY(3,cCdgoCiiu,"-")) ELSE TRUE) NO-ERROR.
        IF AVAIL Ciiu THEN W_NomCiiu-2 = W_NomCiiu-2 + Ciiu.Descripcion + " - ".
     END.
     W_NomCiiu-2 = TRIM(W_NomCiiu-2,"-").
     DISP W_NomCiiu-2.
  END.
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

OCXFile = SEARCH( "W-ProClientesnew.wrx":U ).
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
ELSE MESSAGE "W-ProClientesnew.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crear_Microempresario wWin 
PROCEDURE Crear_Microempresario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME F_Microempresas:
      
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea_Faltante wWin 
PROCEDURE Crea_Faltante :
DEFINE INPUT PARAMETER BCampo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER BDonde AS CHARACTER NO-UNDO.
    CREATE TFalta.
    ASSIGN  TFalta.TCampo = BCampo
            TFalta.TDonde = BDonde.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DrccionCmbiada wWin 
PROCEDURE DrccionCmbiada :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    cDrccion = c.
    WWin:SENSITIVE = TRUE.
    
    RUN hideObject IN h_wgnrdordrccion.
    hDrccion:SCREEN-VALUE = c.
    ENABLE Btn_Salvar Btn_Deshacer WITH FRAME F_Clientes.
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
  DISPLAY Cmb_Agencia RSeleccion 
      WITH FRAME F_Clientes IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Nombre Clientes.Estado Clientes.Nit 
          Clientes.Tipo_Identificacion Clientes.Apellido1 Clientes.Apellido2 
          Clientes.Tipo_Cliente Clientes.dv 
      WITH FRAME F_Clientes IN WINDOW wWin.
  ENABLE Clientes.Nombre Btn_Consulta Btn_Ingresar Btn_Salir BUTTON-11 BUTTON-1 
         BUTTON-2 Clientes.Tipo_Identificacion Clientes.Apellido1 
         Clientes.Apellido2 Clientes.Tipo_Cliente RSeleccion BtonInfoCnyge Foto 
         RECT-2 RECT-216 RECT-317 RECT-318 RECT-218 
      WITH FRAME F_Clientes IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Clientes}
  DISPLAY R_Relacion Rela_Nit R_Nombre R_Nombre2 R_Apellido1 R_Apellido2 
          fechaNacimientoRelacion R_Direccion R_Sucursal R_Ciu_Dpt 
          R_Tel_ResidenciaIndctvo R_Tel_Residencia R_Producto 
          R_Tel_ComercialIndctvo R_Tel_Comercial R_Numero 
      WITH FRAME FRelNva IN WINDOW wWin.
  ENABLE RECT-303 RECT-331 RECT-333 Btn_SC R_Relacion Rela_Nit BUTTON-205 
         R_Nombre R_Nombre2 BUTTON-206 R_Apellido1 R_Apellido2 
         fechaNacimientoRelacion R_Btn_Direccion-3 R_Sucursal 
         Btn_Lugar_Comercial-2 R_Tel_ResidenciaIndctvo R_Tel_Residencia 
         R_Producto R_Tel_ComercialIndctvo R_Tel_Comercial R_Numero 
      WITH FRAME FRelNva IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRelNva}
  DISPLAY Edit_msaje 
      WITH FRAME F_Aportes IN WINDOW wWin.
  ENABLE Bt_FinAporte 
      WITH FRAME F_Aportes IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Aportes}
  IF AVAILABLE Anexos_Clientes THEN 
    DISPLAY Anexos_Clientes.Represetacion Anexos_Clientes.Proviene 
          Anexos_Clientes.Declaracion_Conocimiento 
          Anexos_Clientes.Autorizaciones[1] 
          Anexos_Clientes.Declaracion_aceptacion 
          Anexos_Clientes.Autorizaciones[2] Anexos_Clientes.Autorizaciones[3] 
          Anexos_Clientes.Autorizaciones[4] Anexos_Clientes.Autorizaciones[5] 
          Anexos_Clientes.Fec_Diligenciamiento Anexos_Clientes.Autorizaciones[6] 
          Anexos_Clientes.Autorizaciones[7] Anexos_Clientes.Autorizaciones[8] 
          Anexos_Clientes.Autorizaciones[9] Anexos_Clientes.Autorizaciones[10] 
          Anexos_Clientes.Autorizaciones[11] Anexos_Clientes.Cam_log1 
          Anexos_Clientes.Autorizaciones[12] 
      WITH FRAME F_Autorizaciones IN WINDOW wWin.
  ENABLE RECT-304 RECT-305 Anexos_Clientes.Represetacion 
         Anexos_Clientes.Proviene Anexos_Clientes.Declaracion_Conocimiento 
         Anexos_Clientes.Autorizaciones[1] 
         Anexos_Clientes.Declaracion_aceptacion 
         Anexos_Clientes.Autorizaciones[2] Anexos_Clientes.Autorizaciones[3] 
         Anexos_Clientes.Autorizaciones[4] Anexos_Clientes.Autorizaciones[5] 
         Anexos_Clientes.Fec_Diligenciamiento Anexos_Clientes.Autorizaciones[6] 
         Anexos_Clientes.Autorizaciones[7] Anexos_Clientes.Autorizaciones[8] 
         Anexos_Clientes.Autorizaciones[9] Anexos_Clientes.Autorizaciones[10] 
         Anexos_Clientes.Autorizaciones[11] Anexos_Clientes.Cam_log1 
         Anexos_Clientes.Autorizaciones[12] 
      WITH FRAME F_Autorizaciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Autorizaciones}

  {&OPEN-QUERY-F_Conyuge}
  GET FIRST F_Conyuge.
  DISPLAY CnygeExtncion R_Tipo_Identificacion R_Tercero R_Sexo R_Apellido1 
          R_Apellido2 R_Nombre WR_CiuNacimiento R_Lugar_Nacimiento 
          R_Fec_Nacimiento R_Nacionalidad R_Niv_Educativo WR_NomProfesion 
          R_Cod_Profesion R_Ocupacion R_Especifique R_Cargo_Actividad R_Empresa 
          R_Direccion WR_CiuEmpresa R_Dir_Empresa R_Tel_Empresa R_TIngreso 
          R_TEgresos R_Tel_Empresa-Indctvo 
      WITH FRAME F_Conyuge IN WINDOW wWin.
  ENABLE CnygeExtncion R_Tipo_Identificacion R_Tercero R_Apellido1 R_Apellido2 
         R_Nombre R_Btn_Nacimiento R_Fec_Nacimiento R_Nacionalidad 
         R_Niv_Educativo R_Btn_Profesion R_Ocupacion R_Especifique 
         R_Cargo_Actividad R_Empresa R_Btn_Empresa R_Btn_Direccion 
         R_Tel_Empresa R_TIngreso R_TEgresos Btn_Grabar_conyuge 
         Btn_Ocultar_conyuge R_Tel_Empresa-Indctvo RECT-231 RECT-5 RECT-232 
         RECT-233 RECT-234 
      WITH FRAME F_Conyuge IN WINDOW wWin.
  VIEW FRAME F_Conyuge IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Conyuge}
  DISPLAY Cmb_Linea Cmb_Contrato 
      WITH FRAME F_Documentacion IN WINDOW wWin.
  IF AVAILABLE Anexos_Clientes THEN 
    DISPLAY Anexos_Clientes.Docum_Requer[1] Anexos_Clientes.Docum_Requer[2] 
          Anexos_Clientes.Docum_Requer[3] Anexos_Clientes.Docum_Requer[4] 
          Anexos_Clientes.Docum_Requer[5] Anexos_Clientes.Docum_Requer[6] 
          Anexos_Clientes.Docum_Requer[7] Anexos_Clientes.Docum_Requer[8] 
          Anexos_Clientes.Docum_Requer[9] Anexos_Clientes.Docum_Requer[10] 
          Anexos_Clientes.Docum_Requer[11] Anexos_Clientes.Docum_Requer[12] 
          Anexos_Clientes.Docum_Requer[13] Anexos_Clientes.Docum_Requer[14] 
          Anexos_Clientes.Docum_Requer[15] Anexos_Clientes.Docum_Requer[16] 
          Anexos_Clientes.Docum_Requer[17] Anexos_Clientes.Docum_Requer[18] 
          Anexos_Clientes.Docum_Requer[19] 
      WITH FRAME F_Documentacion IN WINDOW wWin.
  ENABLE Cmb_Linea Cmb_Contrato Anexos_Clientes.Docum_Requer[1] 
         Anexos_Clientes.Docum_Requer[2] Anexos_Clientes.Docum_Requer[3] 
         Anexos_Clientes.Docum_Requer[4] Anexos_Clientes.Docum_Requer[5] 
         Anexos_Clientes.Docum_Requer[6] Anexos_Clientes.Docum_Requer[7] 
         Anexos_Clientes.Docum_Requer[8] Anexos_Clientes.Docum_Requer[9] 
         Anexos_Clientes.Docum_Requer[10] Anexos_Clientes.Docum_Requer[11] 
         Anexos_Clientes.Docum_Requer[12] Anexos_Clientes.Docum_Requer[13] 
         Anexos_Clientes.Docum_Requer[14] Anexos_Clientes.Docum_Requer[15] 
         Anexos_Clientes.Docum_Requer[16] Anexos_Clientes.Docum_Requer[17] 
         Anexos_Clientes.Docum_Requer[18] Anexos_Clientes.Docum_Requer[19] 
      WITH FRAME F_Documentacion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Documentacion}
  DISPLAY Tot_Ingresos Tot_Egresos Tot_Activos 
      WITH FRAME F_Economica IN WINDOW wWin.
  IF AVAILABLE Anexos_Clientes THEN 
    DISPLAY Anexos_Clientes.Transac_Mod_Ext Anexos_Clientes.Gto_TargetaCredito 
          Anexos_Clientes.Especifique_OtrosIng Anexos_Clientes.Gto_Otros 
          Anexos_Clientes.Val_Pasivos Anexos_Clientes.Otra_Transac_extr 
          Anexos_Clientes.Maneja_Cta_Extanjera 
          Anexos_Clientes.Tipo_Moneda_Divisa 
          Anexos_Clientes.Nom_Banco_extranjero 
          Anexos_Clientes.Num_cta_extranjera 
          Anexos_Clientes.Ciudad_Pais_Bco_Extranjero 
      WITH FRAME F_Economica IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Salario Clientes.Ing_Honorarios Clientes.Ing_financieros 
          Clientes.Ing_arriendos Clientes.Ing_Otros Clientes.Gto_obligacion 
          Clientes.Gto_Arriendo Clientes.Sdo_Obligaciones Clientes.Gto_Familiar 
          Clientes.Act_vehiculo Clientes.Act_casa Clientes.Act_inversion 
          Clientes.Id_Mon_Ext 
      WITH FRAME F_Economica IN WINDOW wWin.
  ENABLE Anexos_Clientes.Transac_Mod_Ext Anexos_Clientes.Gto_TargetaCredito 
         Clientes.Salario Clientes.Ing_Honorarios Clientes.Ing_financieros 
         Clientes.Ing_arriendos Clientes.Ing_Otros 
         Anexos_Clientes.Especifique_OtrosIng Clientes.Gto_obligacion 
         Clientes.Gto_Arriendo Clientes.Sdo_Obligaciones Clientes.Gto_Familiar 
         Anexos_Clientes.Gto_Otros Btn_Grabar_Act_Pas 
         Anexos_Clientes.Maneja_Cta_Extanjera 
         Anexos_Clientes.Tipo_Moneda_Divisa 
         Anexos_Clientes.Nom_Banco_extranjero 
         Anexos_Clientes.Num_cta_extranjera 
         Anexos_Clientes.Ciudad_Pais_Bco_Extranjero Clientes.Id_Mon_Ext 
         RECT-213 RECT-214 RECT-219 RECT-237 
      WITH FRAME F_Economica IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Economica}
  DISPLAY RActivas Cmb_Relaciones W_MenRel 
      WITH FRAME F_Relaciones IN WINDOW wWin.
  ENABLE RActivas Btn_CreRel Btn_Activas Br_Relaciones BUTTON-116 
      WITH FRAME F_Relaciones IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Relaciones}
  DISPLAY W_CiuExpedicion Nom_Colegio W_NomProfesion W_UbicacionResidencia 
      WITH FRAME F_Segmentacion IN WINDOW wWin.
  IF AVAILABLE Anexos_Clientes THEN 
    DISPLAY Anexos_Clientes.Num_Hijos_11 Anexos_Clientes.Num_Hijos_18 
          Anexos_Clientes.Nacionalidad Anexos_Clientes.Ind_cli 
          Anexos_Clientes.Val_arriendo Anexos_Clientes.Dir_Correspondencia 
          Anexos_Clientes.Per_Anos Anexos_Clientes.Per_Meses 
          Anexos_Clientes.Dir_Arrendatario Anexos_Clientes.Ind_Arr 
          Anexos_Clientes.AA_Cliente Anexos_Clientes.Per_Ant_Anos 
          Anexos_Clientes.Per_Ant_Meses Anexos_Clientes.Dir_Ant_Residencia 
          Anexos_Clientes.Tel_Ant_Residencia 
      WITH FRAME F_Segmentacion IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Est_Civil Clientes.Sexo Clientes.Tipo_Vinculo 
          Clientes.Fec_expedicion Clientes.Fec_Nacimiento Clientes.Num_Hijos 
          Clientes.Per_Acargo Clientes.Niv_Educativo Clientes.Niv_EduCop 
          Clientes.Num_ActaCop Clientes.Tipo_Vivienda Clientes.Jornada 
          Clientes.Grado Clientes.Lugar_Nacimiento Clientes.Lugar_expedicion 
          Clientes.Colegio Clientes.Cod_Profesion Clientes.Lugar_Residencia 
          Clientes.Dir_Residencia Clientes.Estrato Clientes.Tel_Residencia 
          Clientes.Celular Clientes.Email Clientes.Nom_Arrendatario 
          Clientes.Tel_Arrendatario 
      WITH FRAME F_Segmentacion IN WINDOW wWin.
  ENABLE RECT-1 RECT-228 RECT-306 RECT-309 RECT-334 Clientes.Est_Civil 
         Clientes.Sexo Clientes.Tipo_Vinculo Btn_Documento 
         Clientes.Fec_expedicion Clientes.Fec_Nacimiento Clientes.Num_Hijos 
         Anexos_Clientes.Num_Hijos_11 Anexos_Clientes.Num_Hijos_18 
         Clientes.Per_Acargo Btn_Nacionalidad Clientes.Niv_Educativo 
         Clientes.Niv_EduCop Clientes.Num_ActaCop Clientes.Tipo_Vivienda 
         Clientes.Jornada Clientes.Grado Btn_Colegio Btn_Profesion 
         Clientes.Estrato Btn_Direccion Anexos_Clientes.Ind_cli 
         Clientes.Tel_Residencia Btn_Residencia Clientes.Celular Clientes.Email 
         Anexos_Clientes.Val_arriendo Clientes.Nom_Arrendatario 
         Anexos_Clientes.Dir_Correspondencia Anexos_Clientes.Per_Anos 
         Anexos_Clientes.Per_Meses Anexos_Clientes.Ind_Arr 
         Clientes.Tel_Arrendatario Btn_Direccion_Arendatario 
         Anexos_Clientes.AA_Cliente Anexos_Clientes.Per_Ant_Anos 
         Anexos_Clientes.Per_Ant_Meses Anexos_Clientes.Tel_Ant_Residencia 
         Btn_Direccion_Ante 
      WITH FRAME F_Segmentacion IN WINDOW wWin.
  VIEW FRAME F_Segmentacion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Segmentacion}
  DISPLAY Cmb_TipAct W_NomEmpresa W_UbicacionComercial W_NomCargo Cmb_Est_Cargo 
          W_NomCiiu W_NomCiiu-2 W_NomCiiu-3 Cmb_Situacion_Actual 
          FacultadDepartamento 
      WITH FRAME F_Ubicacion IN WINDOW wWin.
  IF AVAILABLE Anexos_Clientes THEN 
    DISPLAY Anexos_Clientes.Cam_int1 Anexos_Clientes.Tel_Fax_Comercial 
          Anexos_Clientes.Exento_retencion Anexos_Clientes.Exento_GMF 
          Anexos_Clientes.Declara_Renta Anexos_Clientes.Adm_Recursos_Publ 
          Anexos_Clientes.Especifique_Ocupacion 
          Anexos_Clientes.Extencion_Comercial Anexos_Clientes.Nit_Independiente 
          Anexos_Clientes.Tiempo_Actividad Anexos_Clientes.Fec_Pensionado 
          Anexos_Clientes.Ind_Comercial 
      WITH FRAME F_Ubicacion IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Dir_comercial Clientes.Tel_comercial Clientes.Fec_IngEmpresa 
          Clientes.Tip_Contrato Clientes.Id_Micro Clientes.Fuerza_Mayor 
      WITH FRAME F_Ubicacion IN WINDOW wWin.
  ENABLE Anexos_Clientes.Cam_int1 Anexos_Clientes.Tel_Fax_Comercial 
         Anexos_Clientes.Exento_retencion Anexos_Clientes.Exento_GMF 
         Anexos_Clientes.Declara_Renta Anexos_Clientes.Adm_Recursos_Publ 
         Cmb_TipAct Anexos_Clientes.Especifique_Ocupacion Btn_Empresa 
         Btn_Lugar_Comercial Clientes.Tel_comercial 
         Anexos_Clientes.Extencion_Comercial Btn_Cargo Clientes.Fec_IngEmpresa 
         Cmb_Est_Cargo Clientes.Tip_Contrato Anexos_Clientes.Nit_Independiente 
         Anexos_Clientes.Tiempo_Actividad Btn_Ciiu 
         Anexos_Clientes.Fec_Pensionado Btn_DireccionComercial 
         Anexos_Clientes.Ind_Comercial Btn_Ciiu-3 Clientes.Id_Micro 
         Cmb_Situacion_Actual Clientes.Fuerza_Mayor btnFacultadDpto RECT-235 
         RECT-217 RECT-236 RECT-319 
      WITH FRAME F_Ubicacion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Ubicacion}
  DISPLAY W_NomSegmento W_NomRetiro W_NomUsuario 
      WITH FRAME F_Otros IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Fec_UltActualiza Clientes.Cod_Retiro Clientes.Reestructurado 
          Clientes.Fec_Ingreso Clientes.Cod_Ingreso Clientes.Fec_Retiro 
          Clientes.Dias_Sancion Clientes.Sancionado Clientes.Fec_Calificacion 
          Clientes.Id_PuedeCodeudar Clientes.Fec_IniSancion 
          Clientes.Reportado_Super Clientes.Id_Privilegiado 
          Clientes.Fec_fallecido Clientes.Aut_CentralRiesgo 
          Clientes.Id_Preexistentes Clientes.fecPagare 
          Clientes.Reportado_fiscalia Clientes.Reportado_Procredito 
          Clientes.id_AsistioAsamblea Clientes.Fec_Ult_Act[1] 
          Clientes.Fec_Ult_Act[2] Clientes.Calificacion Clientes.Fec_Ult_Act[3] 
          Clientes.Con_Sospechosas Clientes.Fec_Ult_Act[4] 
          Clientes.Fec_Ult_Act[5] Clientes.Fec_Ult_Act[6] 
          Clientes.Fec_Ult_Act[7] 
      WITH FRAME F_Otros IN WINDOW wWin.
  ENABLE RECT-221 RECT-3 RECT-320 RECT-321 RECT-323 RECT-324 RECT-325 RECT-326 
         Clientes.Reestructurado Clientes.Dias_Sancion Clientes.Sancionado 
         Clientes.Id_PuedeCodeudar Clientes.Reportado_Super 
         Clientes.Id_Privilegiado BUTTON-180 Clientes.Aut_CentralRiesgo 
         Clientes.Id_Preexistentes Clientes.fecPagare 
         Clientes.Reportado_fiscalia Clientes.Reportado_Procredito 
         Clientes.id_AsistioAsamblea Clientes.Calificacion Btn_Codseg 
      WITH FRAME F_Otros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Otros}
  ENABLE B_Falta BUTTON-179 
      WITH FRAME F_Falta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Falta}

  {&OPEN-QUERY-F_Juridicas}
  GET FIRST F_Juridicas.
  DISPLAY W_Ubi_Empresa W_Nom_Act_Ppal W_Nom_Ciu_Emp 
      WITH FRAME F_Juridicas IN WINDOW wWin.
  IF AVAILABLE Anexos_Clientes THEN 
    DISPLAY Anexos_Clientes.AA_Cliente Anexos_Clientes.Especifique_OtrosIng 
          Anexos_Clientes.Val_Pasivos Anexos_Clientes.Otra_Transac_extr 
          Anexos_Clientes.Maneja_Cta_Extanjera 
          Anexos_Clientes.Tipo_Moneda_Divisa Anexos_Clientes.Num_cta_extranjera 
          Anexos_Clientes.Nom_Banco_extranjero 
          Anexos_Clientes.Ciudad_Pais_Bco_Extranjero 
      WITH FRAME F_Juridicas IN WINDOW wWin.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Celular Clientes.Fec_expedicion Clientes.Dir_Residencia 
          Clientes.Tel_Residencia Clientes.Tel_Arrendatario Clientes.Email 
          Clientes.Tipo_Empresa Clientes.Salario Clientes.Ing_Otros 
          Clientes.Sdo_Obligaciones Clientes.Act_vehiculo Clientes.Id_Mon_Ext 
      WITH FRAME F_Juridicas IN WINDOW wWin.
  ENABLE Clientes.Celular Clientes.Fec_expedicion Btn_DirEmpresa Btn_UbiEmpresa 
         Clientes.Tel_Residencia Clientes.Tel_Arrendatario 
         Anexos_Clientes.AA_Cliente Clientes.Email Clientes.Tipo_Empresa 
         Btn_Act_Ppal Clientes.Salario Clientes.Ing_Otros 
         Anexos_Clientes.Especifique_OtrosIng Clientes.Sdo_Obligaciones 
         Clientes.Act_vehiculo Anexos_Clientes.Val_Pasivos Clientes.Id_Mon_Ext 
         Anexos_Clientes.Otra_Transac_extr Anexos_Clientes.Maneja_Cta_Extanjera 
         Anexos_Clientes.Tipo_Moneda_Divisa Anexos_Clientes.Num_cta_extranjera 
         Anexos_Clientes.Nom_Banco_extranjero 
         Anexos_Clientes.Ciudad_Pais_Bco_Extranjero RECT-327 RECT-328 RECT-329 
         RECT-330 
      WITH FRAME F_Juridicas IN WINDOW wWin.
  VIEW FRAME F_Juridicas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Juridicas}

  {&OPEN-QUERY-F_Microempresas}
  GET FIRST F_Microempresas.
  DISPLAY Nom_Ubicacion Nom_Actividad Nom_Ciiu 
      WITH FRAME F_Microempresas IN WINDOW wWin.
  IF AVAILABLE MicroEmpresas THEN 
    DISPLAY MicroEmpresas.NOMBRE MicroEmpresas.DIRECCION MicroEmpresas.TELEFONO 
          MicroEmpresas.ESTRATO MicroEmpresas.cod_actividad 
          MicroEmpresas.Codigo_CIIU MicroEmpresas.UBICACION 
          MicroEmpresas.HOMBRES MicroEmpresas.MUJERES MicroEmpresas.NINOS 
      WITH FRAME F_Microempresas IN WINDOW wWin.
  ENABLE RECT-311 RECT-312 RECT-313 MicroEmpresas.NOMBRE Btn_Direccion-2 
         Btn_Ubi_Micro MicroEmpresas.TELEFONO Btn_Actividad_Micro Nom_Ciiu 
         MicroEmpresas.ESTRATO Grabar_MicroEmpresa MicroEmpresas.HOMBRES 
         MicroEmpresas.MUJERES Salir_MicroEmpresa MicroEmpresas.NINOS 
      WITH FRAME F_Microempresas IN WINDOW wWin.
  VIEW FRAME F_Microempresas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Microempresas}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar wWin 
PROCEDURE Grabar :
DO WITH FRAME F_Clientes:
    ASSIGN FRAME F_Clientes
        Clientes.Nit
        clientes.dv
        Clientes.Tipo_Identificacion = Clientes.Tipo_Identificacion:SCREEN-VALUE
        Clientes.Tipo_Cliente
        Clientes.Nombre
        Clientes.Apellido1
        Clientes.Apellido2
        Anexos_Clientes.nit = Clientes.Nit.
END.

DO WITH FRAME F_Segmentacion:
    IF Clientes.Tipo_Identificacion <> "NIT" THEN
        ASSIGN FRAME F_Segmentacion
            Clientes.Celular
            Clientes.Fec_expedicion
            Clientes.Lugar_Residencia
            Clientes.Dir_Residencia
            Clientes.Tel_Residencia
            Clientes.Email
            Clientes.Tel_Arrendatario
            Anexos_Clientes.AA_Cliente.

    ASSIGN FRAME F_Segmentacion
        Clientes.Niv_Educativo
        Clientes.Niv_EduCop
        Clientes.Num_ActaCop
        Clientes.Cod_Profesion
        Clientes.Est_Civil
        Clientes.Grado
        Clientes.Jornada
        Clientes.Colegio
        Clientes.Num_Hijos
        Clientes.Per_ACargo
        Clientes.Estrato
        Clientes.Lugar_Expedicion
        Clientes.Lugar_Nacimiento
        Clientes.Tipo_Vivienda
        Clientes.Nom_Arrendatario
        Clientes.Sexo
        Clientes.Fec_Nacimiento
        Anexos_Clientes.Num_Hijos_11
        Anexos_Clientes.Num_Hijos_18
        Anexos_Clientes.Nacionalidad
        Anexos_Clientes.Per_Anos
        Anexos_Clientes.Per_Meses
        Anexos_Clientes.Val_arriendo
        Anexos_Clientes.Dir_Arrendatario
        Anexos_Clientes.Dir_Ant_Residencia
        Anexos_Clientes.Tel_Ant_Residencia
        Anexos_Clientes.Per_Ant_Anos
        Anexos_Clientes.Per_Ant_Meses
        Anexos_Clientes.Dir_Correspondencia
        Anexos_Clientes.ind_arr.
END.

DO WITH FRAME F_Ubicacion:
    IF Clientes.Tipo_Identificacion <> "NIT" THEN
        ASSIGN FRAME F_Ubicacion
            Clientes.Cod_Actividad
            Anexos_Clientes.Acti_Economica_Emp.

    ASSIGN FRAME F_Ubicacion
        Clientes.Cod_Empresa
        clientes.facultad
        clientes.departamento
        Clientes.Id_Micro
        Clientes.Cod_Cargo
        Clientes.Tip_Contrato
        Clientes.Lugar_Comercial
        Clientes.Dir_Comercial
        Clientes.Tel_Comercial
        Clientes.Tipo_Actividad = Cmb_TipAct:SCREEN-VALUE
        Clientes.Codigo_Ciiu
        Clientes.Grupo
        Clientes.Subgrupo
        Clientes.Situacion_Actual = INT(SUBSTR(Cmb_Situacion_Actual:SCREEN-VALUE,1,1))
        Clientes.Fuerza_Mayor
        Anexos_Clientes.Exento_retencion
        Anexos_Clientes.Exento_GMF
        Anexos_Clientes.Declara_Renta
        Anexos_Clientes.Adm_Recursos_Publ
        Anexos_Clientes.Especifique_Ocupacion
        Anexos_Clientes.Tel_Fax_Comercial
        Anexos_Clientes.Extencion_Comercial
        Anexos_Clientes.Estado_Cargo = Cmb_Est_Cargo:SCREEN-VALUE
        Anexos_Clientes.Nit_Independiente
        Anexos_Clientes.Tiempo_Actividad
        Anexos_Clientes.cam_int1
        Anexos_Clientes.Fec_Pensionado.

    FIND FIRST MicroEmpresa WHERE MicroEmpresa.Nit = Clientes.Nit EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE MicroEmpresa AND NOT Clientes.Id_Micro THEN
        DELETE MicroEmpresa.
END.

DO WITH FRAME F_Economica:
    IF Clientes.Tipo_Identificacion <> "NIT" THEN
        ASSIGN FRAME F_Economica
            Clientes.Salario
            Clientes.Id_Mon_Ext
            Clientes.Ing_otros
            Clientes.Sdo_Obligaciones
            Anexos_Clientes.Especifique_OtrosIng
            Anexos_Clientes.Val_Pasivos
            Anexos_Clientes.Maneja_Cta_Extanjera
            Anexos_Clientes.Tipo_Moneda_Divisa
            Anexos_Clientes.Nom_Banco_Extranjero
            Anexos_Clientes.Num_cta_Extranjera
            Clientes.Act_vehiculo
            Anexos_Clientes.Otra_Transac_Extr
            Anexos_Clientes.Ciudad_Pais_Bco_Extranjero.

    ASSIGN FRAME F_Economica
        Clientes.Ing_Honorarios
        Clientes.Ing_financieros
        Clientes.Ing_arriendos
        Clientes.Gto_obligacion
        Clientes.Gto_arriendo
        Clientes.Gto_Familiar
        Clientes.Act_casa
        Clientes.Act_inversion
        Anexos_Clientes.Gto_TargetaCredito
        Anexos_Clientes.Gto_Otros
        Anexos_Clientes.Transac_Mod_Ext.
END.

DO WITH FRAME F_Autorizaciones:
    ASSIGN FRAME F_Autorizaciones
        Anexos_Clientes.Declaracion_Conocimiento
        Anexos_Clientes.Proviene
        Anexos_Clientes.Represetacion
        Anexos_Clientes.Autorizaciones[1]
        Anexos_Clientes.Autorizaciones[2]
        Anexos_Clientes.Autorizaciones[3]
        Anexos_Clientes.Autorizaciones[4]
        Anexos_Clientes.Autorizaciones[5]
        Anexos_Clientes.Autorizaciones[6]
        Anexos_Clientes.Autorizaciones[7]
        Anexos_Clientes.Autorizaciones[8]
        Anexos_Clientes.Autorizaciones[9]
        Anexos_Clientes.Autorizaciones[10]
        Anexos_Clientes.Autorizaciones[11]
        Anexos_Clientes.Autorizaciones[12]
        Anexos_Clientes.Declaracion_aceptacion
        Anexos_Clientes.Fec_Diligenciamiento
        Anexos_Clientes.cam_log1.
END.

DO WITH FRAME F_Documentacion:
    ASSIGN FRAME F_Documentacion
        Anexos_Clientes.Docum_Requer[1]
        Anexos_Clientes.Docum_Requer[2]
        Anexos_Clientes.Docum_Requer[3]
        Anexos_Clientes.Docum_Requer[4]
        Anexos_Clientes.Docum_Requer[5]
        Anexos_Clientes.Docum_Requer[6]
        Anexos_Clientes.Docum_Requer[7]
        Anexos_Clientes.Docum_Requer[8]
        Anexos_Clientes.Docum_Requer[9]
        Anexos_Clientes.Docum_Requer[10]
        Anexos_Clientes.Docum_Requer[11]
        Anexos_Clientes.Docum_Requer[12]
        Anexos_Clientes.Docum_Requer[13]
        Anexos_Clientes.Docum_Requer[14]
        Anexos_Clientes.Docum_Requer[15]
        Anexos_Clientes.Docum_Requer[16]
        Anexos_Clientes.Docum_Requer[17]
        Anexos_Clientes.Docum_Requer[18]
        Anexos_Clientes.Docum_Requer[19].

    ASSIGN Anexos_Clientes.Documentos_linea = Cmb_Linea:SCREEN-VALUE
           Anexos_Clientes.Documentos_cont = Cmb_Contrato:SCREEN-VALUE.
END.

DO WITH FRAME F_Otros:
    ASSIGN FRAME F_Otros
        Clientes.Calificacion
        Clientes.Aut_CentralRiesgo
        Clientes.Reportado_Super
        Clientes.Reportado_Fiscalia
        Clientes.Reportado_Procredito
        clientes.id_asistioAsamblea
        Clientes.Sancionado = LOGICAL(Clientes.Sancionado:SCREEN-VALUE)
        Clientes.Cod_Segmento
        Clientes.Dias_Sancion
        Clientes.Id_Preexistentes
        Clientes.Id_Privilegiado
        Clientes.Id_PuedeCodeudar
        Clientes.Reestructurado
        Clientes.Usuario.

    ASSIGN Clientes.Usuario = Clientes.Usuario:SCREEN-VALUE
           Clientes.Cod_Ingreso.

    Clientes.Fec_UltActualiza:SCREEN-VALUE = STRING(w_fecha).

    ASSIGN FRAME F_Otros
        Clientes.Fec_Calificacion
        Clientes.Fec_IngEmpresa
        Clientes.Fec_Ingreso
        Clientes.Fec_IniSancion
        Clientes.Fec_fallecido
        Clientes.Fec_Retiro
        clientes.fecPagare.
END.

IF Clientes.Tipo_Identificacion = "NIT" THEN DO WITH FRAME F_Juridicas:
    ASSIGN FRAME F_Juridicas
        Clientes.Celular
        Clientes.Fec_Expedicion
        Clientes.DIR_Residencia
        Clientes.Tel_Residencia
        Clientes.Tel_Arrendatario
        Anexos_Clientes.AA_Cliente
        Clientes.Email
        Clientes.Tipo_Empresa
        Clientes.Salario
        Clientes.Ing_Otros
        Anexos_Clientes.Especifique_Otros
        Clientes.Sdo_Obligaciones
        Clientes.Act_Vehiculo
        Anexos_Clientes.Val_Pasivos
        Clientes.Id_Mon_Ext
        Anexos_Clientes.Otra_Transac_Extr
        Anexos_Clientes.Maneja_Cta_Extanjera
        Anexos_Clientes.Tipo_Moneda_Divisa
        Anexos_Clientes.Num_Cta_Extranjera
        Anexos_Clientes.Nom_Banco_Extranjero
        Anexos_Clientes.Ciudad_Pais_Bco_Extranjero
        Clientes.Lugar_Residencia
        Clientes.Cod_Actividad
        Anexos_Clientes.Acti_Economica_Emp.
END.

FIND FIRST TClientes NO-ERROR.
IF AVAIL TClientes THEN DO:
    FIND FIRST TAnexos_Clientes NO-ERROR.
    
    IF Clientes.Est_Civil <> TClientes.Est_Civil THEN
        Clientes.Fec_Ult_Act[1] = w_fecha.

    IF  Clientes.Niv_Educativo <> TClientes.Niv_Educativo THEN
        Clientes.Fec_Ult_Act[2] = w_fecha.

    IF (Clientes.Salario + Clientes.Ing_Honorarios + Clientes.Ing_Financieros + Clientes.Ing_Arriendos + Clientes.Ing_Otros) <> (TClientes.Salario + TClientes.Ing_Honorarios + TClientes.Ing_Financieros + TClientes.Ing_Arriendos + TClientes.Ing_Otros) THEN
        Clientes.Fec_Ult_Act[3] = w_fecha.

    IF Clientes.Cod_Empresa <> TClientes.Cod_Empresa THEN
        Clientes.Fec_Ult_Act[4] = w_fecha.

    IF Clientes.Tip_Contrato <> TClientes.Tip_Contrato THEN
        Clientes.Fec_Ult_Act[5] = w_fecha.

    IF Anexos_Clientes.Transac_Mod_Ext <> TAnexos_Clientes.Transac_Mod_Ext THEN
        Clientes.Fec_Ult_Act[6] = w_fecha.

    IF Clientes.Fuerza_Mayor <> TClientes.Fuerza_Mayor THEN
        Clientes.Fec_Ult_Act[7] = w_fecha.
END.

clientes.fec_ultActualiza = w_fecha.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Aportes wWin 
PROCEDURE Grabar_Aportes :
DEFINE VAR num_cuenta AS CHARACTER.
    
/* Aporte Social Obligatorio */
FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro EQ 4
                         AND Pro_Ahorros.Cod_ahorro EQ 2 NO-LOCK NO-ERROR.
IF AVAIL Pro_Ahorros THEN DO:
    IF Pro_Ahorros.Id_Consecutivo THEN DO TRANSACTION:
        FIND FIRST Ahorros WHERE Ahorros.Tip_Ahorro EQ 4
                             AND Ahorros.Cod_Ahorro EQ 2
                             AND Ahorros.Nit EQ Clientes.Nit NO-ERROR.
        IF NOT AVAIL Ahorros THEN DO:
            FIND CURRENT Pro_Ahorros NO-ERROR.

            CREATE Ahorros.
            ASSIGN Pro_Ahorros.Num_Consecutivo = Pro_Ahorros.Num_Consecutivo + 1
                   Ahorros.Cue_Ahorros = STRING(Pro_Ahorros.Num_Consecutivo)
                   Ahorros.Tip_Ahorro = 4
                   Ahorros.Cod_ahorro = 2
                   Ahorros.Nit = Clientes.Nit.
        END.
        
        ahorros.estado = 1.
        ahorros.fec_cancelacion = ?.
        ahorros.tasa = 0.
        ahorros.detalle_estado = 1.
        ahorros.agencia = w_agencia.
        Ahorros.FOR_Pago = 2.
        Ahorros.Monto_Apertura = Pro_Ahorros.Val_MonAper.
        Ahorros.Per_Liquidacion = 5.
        Ahorros.For_Liquidacion = Pro_Ahorros.FOR_Liquidacion.
        Ahorros.Usu_Creacion = W_Usuario.
        Ahorros.IdNombre = Clientes.Nombre.
        Ahorros.IdApellido1 = Clientes.Apellido1.
        Ahorros.Cuota = 0.
        Ahorros.Plazo = 9999.
        Ahorros.Per_Deduccion = 4.
        Ahorros.Fec_Apertura = TODAY.

        FIND CURRENT Pro_Ahorros NO-LOCK NO-ERROR.
        FIND CURRENT Ahorros NO-LOCK NO-ERROR.
    END.
    ELSE
        RETURN ERROR.
END.
    
/* Ahorro a la Vista */
FIND FIRST pro_ahorros WHERE pro_ahorros.tip_ahorro EQ 1
                         AND pro_ahorros.cod_ahorro EQ 4 NO-LOCK NO-ERROR.
IF AVAILABLE pro_ahorros THEN DO:
    FIND FIRST indicadores WHERE indicadores.indicador EQ pro_ahorros.indicador NO-LOCK NO-ERROR.
    IF Pro_Ahorros.Id_Consecutivo THEN DO TRANSACTION:
        FIND FIRST ahorros WHERE ahorros.tip_ahorro EQ pro_ahorros.tip_ahorro
                             AND ahorros.cod_ahorro EQ pro_ahorros.cod_ahorro
                             AND ahorros.nit EQ clientes.nit NO-ERROR.
        IF NOT AVAILABLE ahorros THEN DO:
            FIND CURRENT pro_ahorros EXCLUSIVE-LOCK NO-ERROR.

            CREATE ahorros.
            UPDATE pro_ahorros.num_consecutivo = pro_ahorros.num_consecutivo + 1
                   Ahorros.Cue_Ahorros = STRING(Pro_Ahorros.Num_Consecutivo)
                   Ahorros.Tip_Ahorro = pro_ahorros.tip_ahorro
                   Ahorros.Cod_ahorro = pro_ahorros.cod_ahorro
                   Ahorros.Nit = Clientes.Nit.
        END.

        Ahorros.Detalle_Estado = 1.
        Ahorros.Agencia = W_Agencia.
        Ahorros.FOR_Pago = 1.
        Ahorros.Monto_Apertura = 0.
        Ahorros.For_Liquidacion = Pro_Ahorros.FOR_Liquidacion.
        Ahorros.Estado = 1.
        Ahorros.Tasa = Indicadores.Tasa.
        Ahorros.Usu_Creacion = W_Usuario.
        Ahorros.IdNombre = Clientes.Nombre.
        Ahorros.IdApellido1 = Clientes.Apellido1.
        Ahorros.Cuota = 0.
        Ahorros.Plazo = 9999.
        Ahorros.Per_Deduccion = 1.
        Ahorros.per_Liquidacion = 6.
        Ahorros.Fec_Apertura = TODAY.
        ahorros.fec_cancelacion = ?.

        num_cuenta = ahorros.cue_ahorros.

        FIND CURRENT Ahorros NO-LOCK NO-ERROR.
        FIND CURRENT Pro_Ahorros NO-LOCK NO-ERROR.
    END.
END.
   

/* Ahorro permanente */
FIND FIRST pro_ahorros WHERE pro_ahorros.tip_ahorro EQ 2
                         AND pro_ahorros.cod_ahorro EQ 3 NO-LOCK NO-ERROR.
IF AVAILABLE pro_ahorros THEN DO:
    FIND FIRST indicadores WHERE indicadores.indicador EQ pro_ahorros.indicador NO-LOCK NO-ERROR.

    IF Pro_Ahorros.Id_Consecutivo THEN DO TRANSACTION:
        FIND FIRST ahorros WHERE ahorros.tip_ahorro EQ pro_ahorros.tip_ahorro
                             AND ahorros.cod_ahorro EQ pro_ahorros.cod_ahorro
                             AND ahorros.nit EQ clientes.nit NO-ERROR.
        IF NOT AVAILABLE ahorros THEN DO:
            FIND CURRENT pro_ahorros EXCLUSIVE-LOCK NO-ERROR.

            CREATE ahorros.
            UPDATE pro_ahorros.num_consecutivo = pro_ahorros.num_consecutivo + 1
                   Ahorros.Cue_Ahorros = STRING(Pro_Ahorros.Num_Consecutivo)
                   Ahorros.Tip_Ahorro = pro_ahorros.tip_ahorro
                   Ahorros.Cod_ahorro = pro_ahorros.cod_ahorro
                   Ahorros.Nit = Clientes.Nit.
        END.

        Ahorros.Detalle_Estado = 1.
        Ahorros.Agencia = W_Agencia.
        Ahorros.FOR_Pago = 2.
        Ahorros.Monto_Apertura = Pro_Ahorros.Val_MonAper.
        Ahorros.Per_Liquidacion = 6.
        Ahorros.For_Liquidacion = Pro_Ahorros.FOR_Liquidacion.
        Ahorros.Estado = 1.
        Ahorros.Tasa = Indicadores.Tasa.
        Ahorros.Usu_Creacion = W_Usuario.
        Ahorros.IdNombre = Clientes.Nombre.
        Ahorros.IdApellido1 = Clientes.Apellido1.
        Ahorros.Cuota = 0.
        Ahorros.Plazo = 9999.
        Ahorros.Per_Deduccion = 1.
        Ahorros.Fec_Apertura = TODAY.
        ahorros.des_interes = 1.
            
        ahorros.Agencia_Destino = w_agencia.
        ahorros.pro_Destino = 4.
        ahorros.cue_destino = num_cuenta.

        FIND CURRENT Pro_Ahorros NO-LOCK NO-ERROR.
        FIND CURRENT Ahorros NO-LOCK NO-ERROR.
    END.
END.

/*Ahorro Devoluciones*/
FIND FIRST pro_ahorros WHERE pro_ahorros.tip_ahorro EQ 1
                         AND pro_ahorros.cod_ahorro EQ 8 NO-LOCK NO-ERROR.
IF AVAILABLE pro_ahorros THEN DO:
    FIND FIRST indicadores WHERE indicadores.indicador EQ pro_ahorros.indicador NO-LOCK NO-ERROR.

    IF Pro_Ahorros.Id_Consecutivo THEN DO TRANSACTION:
        FIND FIRST ahorros WHERE ahorros.tip_ahorro EQ pro_ahorros.tip_ahorro
                             AND ahorros.cod_ahorro EQ pro_ahorros.cod_ahorro
                             AND ahorros.nit EQ clientes.nit NO-ERROR.
        IF NOT AVAILABLE ahorros THEN DO:
            FIND CURRENT pro_ahorros EXCLUSIVE-LOCK NO-ERROR.
            
            CREATE ahorros.
            UPDATE pro_ahorros.num_consecutivo = pro_ahorros.num_consecutivo + 1
                   Ahorros.Cue_Ahorros = STRING(Pro_Ahorros.Num_Consecutivo)
                   Ahorros.Tip_Ahorro = pro_ahorros.tip_ahorro
                   Ahorros.Cod_ahorro = pro_ahorros.cod_ahorro
                   Ahorros.Nit = Clientes.Nit.
        END.
        
        Ahorros.Detalle_Estado = 1.
        Ahorros.Agencia = W_Agencia.
        Ahorros.FOR_Pago = 1.
        Ahorros.Monto_Apertura = 0.
        Ahorros.For_Liquidacion = Pro_Ahorros.FOR_Liquidacion.
        Ahorros.Estado = 1.
        Ahorros.Tasa = Indicadores.Tasa.
        Ahorros.Usu_Creacion = W_Usuario.
        Ahorros.IdNombre = Clientes.Nombre.
        Ahorros.IdApellido1 = Clientes.Apellido1.
        Ahorros.Cuota = 0.
        Ahorros.Plazo = 9999.
        Ahorros.Per_Deduccion = 1.
        Ahorros.per_Liquidacion = 6.
        Ahorros.Fec_Apertura = TODAY.
        ahorros.fec_cancelacion = ?.

        FIND CURRENT Pro_Ahorros NO-LOCK NO-ERROR.
        FIND CURRENT Ahorros NO-LOCK NO-ERROR.
    END.
END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_PerJuridica wWin 
PROCEDURE Imprime_PerJuridica :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN ValCol = "A5" Dato = Clientes.Nombre.
   RUN Llenar_Celda.
   ASSIGN ValCol = "Q5" Dato = STRING(Clientes.Fec_Expedicion,"99/99/9999").
   RUN Llenar_Celda.
  /* ASSIGN ValCol = "Y5" Dato = Clientes.Celular.
   RUN Llenar_Celda. */
   ASSIGN ValCol = "A7" Dato = Clientes.Dir_Residencia.
   RUN Llenar_Celda.
   ASSIGN ValCol = "J7" Dato = "".
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = Clientes.Lugar_Residencia NO-LOCK NO-ERROR.
   IF   AVAIL Ubicacion THEN  Dato   =  Ubicacion.Nombre  +
        IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
   RUN Llenar_Celda.
   ASSIGN ValCol = "R7" Dato = "".
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
   IF   AVAIL Ubicacion THEN  Dato =  Ubicacion.Nombre.
   RUN Llenar_Celda.
   ASSIGN ValCol = "Y7" Dato = "".  
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "D" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Residencia,1,2) NO-LOCK NO-ERROR.
   IF   AVAIL Ubicacion THEN  Dato =  Ubicacion.Nombre.
   RUN Llenar_Celda.
   ASSIGN ValCol = "A9" Dato = Clientes.Tel_Residencia.
   RUN Llenar_Celda.
   ASSIGN ValCol = "G9" Dato = Clientes.Tel_Arrendatario.
   RUN Llenar_Celda.
   FIND FIRST Varios WHERE Varios.Tipo EQ 35 AND Varios.Codigo = INT(Clientes.Cod_Actividad) NO-LOCK NO-ERROR.
   IF AVAIL Varios THEN DO:
      ASSIGN ValCol = "I15" Dato = Varios.Descripcion.
      RUN Llenar_Celda.
   END.
   FIND FIRST Ciiu WHERE Ciiu.Codigo_Ciiu = INT(Anexos_Clientes.Acti_Economica_Emp) NO-LOCK NO-ERROR.
   IF AVAIL Ciiu THEN DO:
      ASSIGN ValCol = "T15" Dato = Ciiu.Descripcion.
      RUN Llenar_Celda.
   END.
   ASSIGN ValCol = "A17" Dato = STRING(Clientes.Salario,"->>>,>>>,>>>.99").
   RUN Llenar_Celda.
   ASSIGN ValCol = "L17" Dato = STRING(Clientes.Ing_Otros,"->>>,>>>,>>>.99").
   RUN Llenar_Celda.
   ASSIGN ValCol = "T17" Dato = Anexos_Clientes.Especifique_OtrosIng.
   RUN Llenar_Celda.
   ASSIGN ValCol = "A19" Dato = STRING(Clientes.Sdo_Obligaciones,"->>>,>>>,>>>.99").
   RUN Llenar_Celda.
   ASSIGN ValCol = "K19" Dato = STRING(Clientes.Act_Vehiculo,"->>>,>>>,>>>.99").
   RUN Llenar_Celda.
   ASSIGN ValCol = "U19" Dato = STRING(Anexos_Clientes.Val_Pasivos,"->>>,>>>,>>>.99").
   RUN Llenar_Celda.
   ASSIGN ValCol = "A21" Dato = STRING(Clientes.Id_Mon_Ext,"Si/No").
   RUN Llenar_Celda.
   ASSIGN ValCol = "J21" Dato = Anexos_Clientes.Otra_Transac_Extr.
   RUN Llenar_Celda.
   ASSIGN ValCol = "Q21" Dato = STRING(Anexos_Clientes.Maneja_Cta_Ext,"Si/No").
   RUN Llenar_Celda.
   ASSIGN ValCol = "Z21" Dato = Anexos_Clientes.Tipo_Moneda_Divisa.
   RUN Llenar_Celda.
   ASSIGN ValCol = "A23" Dato = Anexos_Clientes.Num_Cta_Extranjera.
   RUN Llenar_Celda.
   ASSIGN ValCol = "H23" Dato = Anexos_Clientes.Nom_Banco_Extranjero.
   RUN Llenar_Celda.
   ASSIGN ValCol = "Q23" Dato = Anexos_Clientes.Ciudad_Pais_Bco_Extranjero.
   RUN Llenar_Celda.
   FIND FIRST Relaciones WHERE Relaciones.Cod_Relacion = 5
                           AND Relaciones.Nit          = Clientes.Nit
                           AND Relaciones.Estado       = 1 NO-LOCK NO-ERROR.
   IF AVAIL Relaciones THEN RUN Imprime_Relacion.
   ASSIGN ValCol = "P30" Dato = Clientes.Tipo_Empresa.
   RUN Llenar_Celda.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_PerNatural wWin 
PROCEDURE Imprime_PerNatural :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN ValCol = "A5" Dato = Clientes.Apellido1 + " " + Clientes.Apellido2 + " " + Clientes.nombre.
   RUN Llenar_Celda.
   ASSIGN ValCol = "Q5" Dato = STRING(Clientes.Fec_Nacimiento,"99/99/9999").
   RUN Llenar_Celda.
   ASSIGN ValCol = "Y5" Dato = "".
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Nacimiento,1,5) NO-LOCK NO-ERROR.
   IF   AVAIL Ubicacion THEN  Dato   = Ubicacion.Nombre.
   RUN Llenar_Celda.    
   ASSIGN ValCol = "A7" Dato = Clientes.Dir_Residencia.
   RUN Llenar_Celda.
   ASSIGN ValCol = "J7" Dato = "".
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = Clientes.Lugar_Residencia NO-LOCK NO-ERROR.
   IF   AVAIL Ubicacion THEN  Dato   =  Ubicacion.Nombre  +
        IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
   RUN Llenar_Celda.
   ASSIGN ValCol = "R7" Dato = "".
   
    FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
   IF   AVAIL Ubicacion THEN  Dato =  Ubicacion.Nombre.    
   RUN Llenar_Celda.
    
   ASSIGN ValCol = "Y7" Dato = "".  
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "D" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Residencia,1,2) NO-LOCK NO-ERROR.
   IF   AVAIL Ubicacion THEN  Dato =  Ubicacion.Nombre.
   RUN Llenar_Celda.
   ASSIGN ValCol = "A9" Dato = Clientes.Tel_Residencia.
   RUN Llenar_Celda.
   ASSIGN ValCol = "M9" Dato = Clientes.Celular.
   RUN Llenar_Celda.
   ASSIGN ValCol = "T9" Dato = Clientes.Tipo_Actividad.
   RUN Llenar_Celda.
   FIND Empresas WHERE Empresas.Cod_Empresa = Clientes.Cod_Empresa NO-LOCK NO-ERROR.
   IF AVAIL Empresas THEN DO:
      ASSIGN ValCol = "A11" Dato = Empresas.Alias_Empresa.
      RUN Llenar_Celda.
   END.
   FIND FIRST Varios WHERE Varios.Tipo EQ 2 AND Varios.Codigo = Clientes.Cod_Cargo NO-LOCK NO-ERROR.
   IF AVAIL Varios THEN DO:
      ASSIGN ValCol = "T11" Dato = Varios.Descripcion.
      RUN Llenar_Celda.
   END.
   ASSIGN ValCol = "A13" Dato = Clientes.Dir_Comercial.
   RUN Llenar_Celda.
   ASSIGN ValCol = "K13" Dato = "".
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Comercial,1,5) NO-LOCK NO-ERROR.
   IF   AVAIL Ubicacion THEN  Dato =  Ubicacion.Nombre.
   RUN Llenar_Celda.
   ASSIGN ValCol = "S13" Dato = "".  
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "D" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Comercial,1,2) NO-LOCK NO-ERROR.
   IF   AVAIL Ubicacion THEN  Dato =  Ubicacion.Nombre.
   RUN Llenar_Celda.
   ASSIGN ValCol = "Z13" Dato = Clientes.Tel_Comercial.
   RUN Llenar_Celda.
   FIND FIRST Varios WHERE Varios.Tipo EQ 35 AND Varios.Codigo = INT(Clientes.Cod_Actividad) NO-LOCK NO-ERROR.
   IF AVAIL Varios THEN DO:
      ASSIGN ValCol = "I15" Dato = Varios.Descripcion.
      RUN Llenar_Celda.
   END.
   FIND FIRST Ciiu WHERE Ciiu.Codigo_Ciiu = INT(Anexos_Clientes.Acti_Economica_Emp) NO-LOCK NO-ERROR.
   IF AVAIL Ciiu THEN DO:
      ASSIGN ValCol = "T15" Dato = Ciiu.Descripcion.
      RUN Llenar_Celda.
   END.
   ASSIGN Tot_Ingresos = Clientes.Salario + Clientes.Ing_Arriendos + Clientes.Ing_Financieros + Clientes.Ing_Honorarios
          Tot_Egresos  = Clientes.Gto_Arriendo + Clientes.Gto_Familiar + Clientes.Sdo_Obligacion
                       + Clientes.Gto_Obligacion + Anexos_Clientes.Gto_TargetaCredito + Anexos_Clientes.Gto_Otros
          Tot_Activos  = Clientes.Act_Casa + Clientes.Act_Vehiculo + Clientes.Act_Inversion.
   ASSIGN ValCol = "A17" Dato = STRING(Tot_Ingresos,"->>>,>>>,>>>.99").
   RUN Llenar_Celda.
   ASSIGN ValCol = "L17" Dato = STRING(Clientes.Ing_Otros,"->>>,>>>,>>>.99").
   RUN Llenar_Celda.
   ASSIGN ValCol = "T17" Dato = Anexos_Clientes.Especifique_OtrosIng.
   RUN Llenar_Celda.
   ASSIGN ValCol = "A19" Dato = STRING(Tot_Egresos,"->>>,>>>,>>>.99").
   RUN Llenar_Celda.
   ASSIGN ValCol = "K19" Dato = STRING(Tot_Activos,"->>>,>>>,>>>.99").
   RUN Llenar_Celda.
   ASSIGN ValCol = "U19" Dato = STRING(Anexos_Clientes.Val_Pasivos,"->>>,>>>,>>>.99").
   RUN Llenar_Celda.
   ASSIGN ValCol = "A21" Dato = STRING(Clientes.Id_Mon_Ext,"Si/No").
   RUN Llenar_Celda.
   ASSIGN ValCol = "J21" Dato = Anexos_Clientes.Transac_Mod_Ext.
   RUN Llenar_Celda.
   ASSIGN ValCol = "Q21" Dato = STRING(Anexos_Clientes.Maneja_Cta_Ext,"Si/No").
   RUN Llenar_Celda.
   ASSIGN ValCol = "Z21" Dato = Anexos_Clientes.Tipo_Moneda_Divisa.
   RUN Llenar_Celda.
   ASSIGN ValCol = "A23" Dato = Anexos_Clientes.Num_Cta_Extranjera.
   RUN Llenar_Celda.
   ASSIGN ValCol = "H23" Dato = Anexos_Clientes.Nom_Banco_Extranjero.
   RUN Llenar_Celda.
   ASSIGN ValCol = "Q23" Dato = Anexos_Clientes.Ciudad_Pais_Bco_Extranjero.
   RUN Llenar_Celda.
   ASSIGN ValCol = "A15" Dato = STRING(anexos_clientes.Adm_recursos_publ,"Si/No").
    RUN Llenar_Celda.



      FIND FIRST Relaciones WHERE Relaciones.Nit             = Clientes.Nit 
                              AND Relaciones.Cod_Relacion    = 4
                              AND Relaciones.Estado          = 1  NO-LOCK NO-ERROR.
      IF AVAIL Relaciones THEN DO:  
         FIND  BClientes WHERE BClientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
         ASSIGN ValCol = "A32" Dato = BClientes.Apellido1 + " " + BClientes.Apellido2 + " " + BClientes.nombre.
         RUN Llenar_Celda.
         ASSIGN ValCol = "P32" Dato = BClientes.Nit.
         RUN Llenar_Celda.
         ASSIGN ValCol = "X32" Dato = Relaciones.Descripcion.
         RUN Llenar_Celda.
         ASSIGN ValCol = "AC32" Dato = STRING(Relaciones.Val_Autorizado,"Z9").
         RUN Llenar_Celda.
         FIND NEXT Relaciones WHERE Relaciones.Nit             = Clientes.Nit
                                AND Relaciones.Cod_Relacion    = 4                                
                                AND Relaciones.Estado          = 1 NO-LOCK NO-ERROR.
         IF AVAIL Relaciones THEN DO:
            FIND  BClientes WHERE BClientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
            ASSIGN ValCol = "A33" Dato = BClientes.Apellido1 + " " + BClientes.Apellido2 + " " + BClientes.nombre.
            RUN Llenar_Celda.
            ASSIGN ValCol = "P33" Dato = BClientes.Nit.
            RUN Llenar_Celda.
            ASSIGN ValCol = "X33" Dato = Relaciones.Descripcion.
            RUN Llenar_Celda.
            ASSIGN ValCol = "AC33" Dato = STRING(Relaciones.Val_Autorizado,"Z9").
            RUN Llenar_Celda.
         END.
         FIND NEXT Relaciones WHERE Relaciones.Nit             = Clientes.Nit
                                AND Relaciones.Cod_Relacion    = 4                                
                                AND Relaciones.Estado          = 1 NO-LOCK NO-ERROR.
         IF AVAIL Relaciones THEN DO:
            FIND  BClientes WHERE BClientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
            ASSIGN ValCol = "A34" Dato = BClientes.Apellido1 + " " + BClientes.Apellido2 + " " + BClientes.nombre.
            RUN Llenar_Celda.
            ASSIGN ValCol = "P34" Dato = BClientes.Nit.
            RUN Llenar_Celda.
            ASSIGN ValCol = "X34" Dato = Relaciones.Descripcion.
            RUN Llenar_Celda.
            ASSIGN ValCol = "AC34" Dato = STRING(Relaciones.Val_Autorizado,"Z9").
            RUN Llenar_Celda.
         END.
      END.
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_Relacion wWin 
PROCEDURE Imprime_Relacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND BClientes WHERE BClientes.Nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
    ASSIGN ValCol = "A26"  Dato = BClientes.Apellido1 + " " + BClientes.Apellido2 + " " + BClientes.nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "S26"  Dato = BClientes.Nit.
    RUN Llenar_Celda.
    ASSIGN ValCol = "AC26" Dato = BClientes.Tipo_Identificacion.
    RUN Llenar_Celda.
    ASSIGN ValCol = "A28"  Dato = BClientes.Dir_Residencia.
    RUN Llenar_Celda.
    ASSIGN ValCol = "J28" Dato = "".
    FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = BClientes.Lugar_Residencia NO-LOCK NO-ERROR.
    IF   AVAIL Ubicacion THEN  Dato   =  Ubicacion.Nombre  +
         IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
    RUN Llenar_Celda.
    ASSIGN ValCol = "R28" Dato = "".
    FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion BEGINS SUBSTR(BClientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
    IF   AVAIL Ubicacion THEN  Dato =  Ubicacion.Nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "Y28" Dato = "".  
    FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "D" AND Ubicacion.Ubicacion BEGINS SUBSTR(BClientes.Lugar_Residencia,1,2) NO-LOCK NO-ERROR.
    IF   AVAIL Ubicacion THEN  Dato =  Ubicacion.Nombre.
    RUN Llenar_Celda.
    ASSIGN ValCol = "A30" Dato = BClientes.Tel_Residencia.
    RUN Llenar_Celda.
    ASSIGN ValCol = "G30" Dato = BClientes.Celular.
    RUN Llenar_Celda.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime_Vinculacion wWin 
PROCEDURE Imprime_Vinculacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND Agencias NO-LOCK  WHERE Agencias.Agencia = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Clientes,1,3)) NO-ERROR.
    ASSIGN ValCol = "I3"  Dato = Agencias.Nombre.                                
    RUN Llenar_Celda.
    ASSIGN ValCol = "P3"  Dato = STRING(Clientes.Fec_Ingreso,"99/99/9999").
    RUN Llenar_Celda.
    ASSIGN ValCol = "X2"  Dato = Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes.
    RUN Llenar_Celda.
    ASSIGN ValCol = "U3" Dato = Clientes.Tipo_Identificacion.
    RUN Llenar_Celda.
    IF Clientes.Tipo_Identificacion <> "NIT" THEN DO:
       wEdad = TODAY - Clientes.Fec_Nacimiento.
       RUN Imprime_PerNatural.
       IF wEdad < 6570 THEN DO:
          FIND FIRST Relaciones WHERE Relaciones.Nit          = Clientes.Nit
                                  AND Relaciones.Cod_Relacion = 3
                                  AND Relaciones.Estado       = 1 NO-LOCK NO-ERROR.
          IF AVAIL Relaciones THEN RUN Imprime_Relacion.
       END.
    END.
    ELSE
    RUN Imprime_PerJuridica.
    FIND Usuarios WHERE Usuarios.Usuario = W_Usuario NO-LOCK NO-ERROR.
    ASSIGN ValCol = "AB41" Dato = Usuario.Nombre.
    RUN Llenar_Celda.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Cliente wWin 
PROCEDURE Imprimir_Cliente :
{Incluido\RepEncabezado.i}

DEFINE VAR W_NomTipoCliente AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VAR W_NomEstado AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VAR W_NomTipoContrat AS CHARACTER FORMAT "X(14)" NO-UNDO.
DEFINE VAR W_NomVinculo AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VAR W_NomSexo AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VAR W_NomRet AS CHARACTER FORMAT "X(2)" NO-UNDO.
DEFINE VAR W_NomGran AS CHARACTER FORMAT "X(2)" NO-UNDO.

W_Reporte = "REPORTE   : INFORMACION DEL CLIENTE - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
W_EncColumna = "CLIENTE: " + Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes + " - " + Clientes.Nombre:SCREEN-VALUE IN FRAME F_Clientes + " " +
               Clientes.Apellido1:SCREEN-VALUE IN FRAME F_Clientes + " " + Clientes.Apellido2:SCREEN-VALUE IN FRAME F_Clientes.

VIEW FRAME F-Encabezado.
VIEW FRAME F-Ftr.

DO WITH FRAME F_Clientes:
    CASE Clientes.Tipo_Cliente:SCREEN-VALUE:
        WHEN "1" THEN W_NomTipoCliente = "Natural".
        WHEN "2" THEN W_NomTipoCliente = "Jurídica".
    END CASE.

    IF Clientes.Estado:SCREEN-VALUE EQ "1" THEN
        W_NomEstado = "Activo".
    ELSE
        W_NomEstado = "Inactivo".

    DISPLAY "INFORMACION GENERAL----------------------------------------------------------------------------------------" AT 1 SKIP(1)
            "Agencia             : "    AT 1    Cmb_Agencia:SCREEN-VALUE FORMAT "X(30)"                 AT 25
            "Tipo Identificacion : "    AT 1    Clientes.Tipo_Identificacion:SCREEN-VALUE FORMAT "X(3)" AT 25   ": "    AT 29  Clientes.Nit:SCREEN-VALUE FORMAT "X(14)" AT 33
            "Tipo Cliente        : "    AT 1    W_NomTipoCliente                                        AT 25
            "Estado Cliente      : "    AT 1    W_NomEstado                                             AT 25 SKIP(2)
        WITH FRAME a WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
    END.

    FIND FIRST Anexos_Clientes WHERE Anexos_Clientes.nit = Clientes.Nit:SCREEN-VALUE IN FRAME f_Clientes NO-LOCK NO-ERROR.

    DO WITH FRAME F_Segmentacion:
        IF Clientes.Sexo:SCREEN-VALUE EQ "1" THEN
            W_NomSexo = "Hombre".
        ELSE
            W_NomSexo = "Mujer".

        DISPLAY "INFORMACION PERSONAL-----------------------------------------------------------------------------------------" AT 1 SKIP(1)
                "Expedición Documento: "    AT 1    W_CiuExpedicion:SCREEN-VALUE FORMAT "X(30)"         AT 25           "Expedición Documento: "    AT 60   Clientes.Fec_expedicion:SCREEN-VALUE                AT 85
                                                                                                                        "Fecha Nacimiento    : "    AT 60   Clientes.Fec_Nacimiento:SCREEN-VALUE                AT 85 SKIP
                "Nacionalidad        : "    AT 1    Anexos_Clientes.Nacionalidad:SCREEN-VALUE           AT 25           "Genero              : "    AT 60   W_NomSexo                                           AT 85
                "Estado Civil        : "    AT 1    Clientes.Est_Civil:SCREEN-VALUE FORMAT "X(30)"      AT 25           "Personas a Cargo    : "    AT 60   Clientes.Per_Acargo:SCREEN-VALUE                    AT 85
                "Número de Hijos     : "    AT 1    Clientes.Num_Hijos:SCREEN-VALUE                     AT 25 SKIP
                "Ubicación Residencia: "    AT 1    W_UbicacionResidencia:SCREEN-VALUE FORMAT "X(30)"   AT 25           "Dirección Residencia: "    AT 60   Clientes.DIR_Residencia:SCREEN-VALUE FORMAT "X(30)" AT 85
                "Teléfono Residencia : "    AT 1    Clientes.Tel_Residencia:SCREEN-VALUE                AT 25           "Teléfono Celular    : "    AT 60   Clientes.Celular:SCREEN-VALUE                       AT 85
                "Correo electrónico  : "    AT 1    Clientes.Email:SCREEN-VALUE FORMAT "X(30)"          AT 25 SKIP(2)   "Estrato Vivienda    : "    AT 60   Clientes.Estrato:SCREEN-VALUE                       AT 85
            WITH FRAME b WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
    END.

    DEFINE VAR W_Correspondencia AS CHARACTER FORMAT "X(30)" NO-UNDO.

    DO WITH FRAME F_Ubicacion:
        DISPLAY "INFORMACION ACTIVIDAD ECONÓMICA------------------------------------------------------------------------------" AT 1 SKIP(1)
                "Ubicación Comercial : "    AT 1    W_UbicacionComercial:SCREEN-VALUE FORMAT "X(30)"    AT 25           "Dirección Comercial : "    AT 60   Clientes.DIR_Comercial:SCREEN-VALUE FORMAT "X(30)"  AT 85
                "Teléfono Oficina    : "    AT 1    Clientes.Tel_Comercial:SCREEN-VALUE                 AT 25 SKIP(2)   "Tipo Contrato       : "    AT 60   W_NomTipoContrat FORMAT "X(30)"                     AT 85 SKIP(2)
                "Tipo de Vinculo     : "    AT 1    W_NomVinculo                                        AT 25
                "Ingreso a la Empresa: "    AT 1    Clientes.Fec_IngEmpresa:SCREEN-VALUE                AT 25           "Enviar Correo a     : "    AT 60   W_Correspondencia                                   AT 85 SKIP
                "Tipo de Vivienda    : "    AT 1    Clientes.Tipo_Vivienda:SCREEN-VALUE                 AT 25
            WITH FRAME c WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
    END.

    DO WITH FRAME F_Economica:
        DISPLAY "FINANCIERA---------------------------------------------------------------------------------------------------" AT 1 SKIP(1)
                "INGRESOS"                  AT 1    "EGRESOS"   AT 65 SKIP
                "Arriendos           : "    AT 1    Clientes.Ing_arriendos:SCREEN-VALUE FORMAT "X(30)"      AT 25   "Arriendos           : "    AT 60   Clientes.Gto_arriendo:SCREEN-VALUE FORMAT "X(30)"   AT 85
                "Financieros         : "    AT 1    Clientes.Ing_financieros:SCREEN-VALUE FORMAT "X(30)"    AT 25   "Gastos Familiares   : "    AT 60   Clientes.Gto_Familiar:SCREEN-VALUE FORMAT "X(30)"   AT 85
                "Salarios            : "    AT 1    Clientes.Salario:SCREEN-VALUE FORMAT "X(30)"            AT 25   "Obligaciones        : "    AT 60   Clientes.Gto_obligacion:SCREEN-VALUE FORMAT "X(30)" AT 85
                "Otros/Honorarios,etc: "    AT 1    Clientes.Ing_Honorarios:SCREEN-VALUE FORMAT "X(30)"     AT 25 SKIP
                                                    Clientes.Ing_Otros:SCREEN-VALUE FORMAT "X(30)"          AT 25 SKIP(2)
                "     ACTIVOS" AT 1 SKIP
                "Valor Propiedad     : "    AT 1    Clientes.Act_casa:SCREEN-VALUE FORMAT "X(30)"           AT 25
                "Inversiones         : "    AT 1    Clientes.Act_inversion:SCREEN-VALUE  FORMAT "X(30)"     AT 25
                "Vehiculo            : "    AT 1    Clientes.Act_vehiculo:SCREEN-VALUE  FORMAT "X(30)"      AT 25
            WITH FRAME e WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
    END.

    DO WITH FRAME F_Otros:
        IF Tip_Contrato:SCREEN-VALUE EQ "1" THEN
            W_NomtipoContrat = "Indefinido".
        ELSE
            W_NomtipoContrat = "A Termino Fijo".

        CASE Clientes.Tipo_Vinculo:SCREEN-VALUE:
            WHEN "1" THEN W_NomVinculo = "Asociado".
            WHEN "2" THEN W_NomVinculo = "Cliente no Asociado".
            WHEN "3" THEN W_NomVinculo = "Tercero".
            WHEN "4" THEN W_NomVinculo = "Proveedor".
            WHEN "5" THEN W_NomVinculo = "Empleado".
        END CASE.

        DISPLAY "OTRA INFORMACION---------------------------------------------------------------------------------------------" AT 1 SKIP(1)
                "Calificación        : "    AT 1    Clientes.Calificacion:SCREEN-VALUE                  AT 25   "Aut.Central Riesgo  : "    AT 60   Clientes.Aut_CentralRiesgo:SCREEN-VALUE     AT 85
                "Reestructurado      : "    AT 1    Clientes.Reestructurado:SCREEN-VALUE                AT 25   "Reportado Super     : "    AT 60   Clientes.Reportado_Super:SCREEN-VALUE       AT 85
                "Reportado Fiscalía  : "    AT 1    Clientes.Reportado_Fiscalia:SCREEN-VALUE            AT 25   "Reportado Procredito: "    AT 60   Clientes.Reportado_Procredito:SCREEN-VALUE  AT 85
                "Sancionado          : "    AT 1    Clientes.Sancionado:SCREEN-VALUE                    AT 25   "Dias de Sanción     : "    AT 60   Clientes.Dias_Sancion:SCREEN-VALUE          AT 85
                "Retiene en la Fuente: "    AT 1    W_NomRet                                            AT 25   "Gran Contribuyente  : "    AT 60   W_NomGran                                   AT 85
                "Código Ciiu         : "    AT 1    W_NomCiiu:SCREEN-VALUE FORMAT "X(30)"               AT 25 SKIP
                "Causa de Ingreso    : "    AT 1    W_NomRetiro:SCREEN-VALUE FORMAT "X(30)"             AT 25 SKIP
                "Asistió Asamblea?   : "    AT 1    Clientes.id_AsistioAsamblea:SCREEN-VALUE            AT 25 SKIP
                "Usuario Afilio      : "    AT 1    W_NomUsuario:SCREEN-VALUE FORMAT "X(30)"            AT 25 SKIP
                                                    W_NomEmpresa:SCREEN-VALUE FORMAT "X(30)"            AT 25   "Cargo               : "    AT 60   W_NomCargo:SCREEN-VALUE FORMAT "X(30)"      AT 85
                                                    FacultadDepartamento:SCREEN-VALUE FORMAT "X(30)"    AT 25
            WITH FRAME f WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.

        DISPLAY "FECHAS ------------------------------------------------------------------------------------------------------" AT 1 SKIP(1)
                "Fecha Calificación  : "    AT 1    Clientes.Fec_Calificacion:SCREEN-VALUE  AT 25   "Ingreso Fondo       : "    AT 60   Clientes.Fec_Ingreso:SCREEN-VALUE       AT 85
                "Inicio de Sanción   : "    AT 1    Clientes.Fec_IniSancion:SCREEN-VALUE    AT 25
                "Fecha Retiro        : "    AT 1    Clientes.Fec_Retiro:SCREEN-VALUE        AT 25   "Ultima Actualización: "    AT 60   Clientes.Fec_UltActualiza:SCREEN-VALUE  AT 85
                "Fecha fallecido     : "    AT 1    Clientes.Fec_fallecido:SCREEN-VALUE     AT 25
            WITH FRAME d WIDTH 132 NO-BOX USE-TEXT NO-LABELS.
    END.

    PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
MESSAGE "La opción de impresión hacia una hoja de calculo" SKIP
        "no esta habilitada para este programa" VIEW-AS ALERT-BOX INFORMATION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Relaciones wWin 
PROCEDURE Imprimir_Relaciones :
{Incluido\RepEncabezado.i}

  W_Reporte   = "REPORTE   : RELACIONES :" + STRING(SUBSTRING(Cmb_Relaciones:SCREEN-VALUE IN FRAME F_Relaciones,8,20)) + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "CLIENTE: " + Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes + " - " + 
                  Clientes.Nombre:SCREEN-VALUE IN FRAME F_Clientes + " " +
                  Clientes.Apellido1:SCREEN-VALUE IN FRAME F_Clientes + " " +
                  Clientes.Apellido2:SCREEN-VALUE IN FRAME F_Clientes.
     
    W_Linea = FILL(W_Raya,132).
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Ftr.
    DISPLAY "NOMBRE RELACION        AGE NIT            NOMBRE                             Descripcion     TELÉFONOS" SKIP(1) WITH WIDTH 132.
    FOR EACH T_Relaciones NO-LOCK:
      DISPLAY T_Relaciones.R_Relacion  FORMAT "X(22)"
              T_Relaciones.R_AgeObjeto FORMAT "999"  
              T_Relaciones.R_NitObjeto FORMAT "X(14)"
              T_Relaciones.R_NomObjeto FORMAT "X(35)"
              T_Relaciones.R_NomDescri FORMAT "X(15)"
              T_Relaciones.R_TelObjeto FORMAT "X(30)"
    WITH WIDTH 132 FRAME F-Relaciones NO-BOX USE-TEXT STREAM-IO NO-LABELS.
    END.  
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Campos wWin 
PROCEDURE Inicializar_Campos :
fIncialza(FRAME F_Clientes:FIRST-CHILD).

DO WITH FRAME F_Clientes:
    FIND FIRST Agencias WHERE Agencias.Agencia EQ W_Agencia NO-LOCK NO-ERROR.

    W_Ok = Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME F_Clientes.

    ASSIGN Clientes.Nit:SCREEN-VALUE = ""
           Clientes.Tipo_Identificacion:SCREEN-VALUE = "C.C"
           Clientes.Tipo_Cliente:SCREEN-VALUE = "1"
           Clientes.Nombre:SCREEN-VALUE = ""
           Clientes.Apellido1:SCREEN-VALUE = ""
           Clientes.Apellido2:SCREEN-VALUE = ""
           Clientes.Estado:SCREEN-VALUE = "1"
           clientes.dv:SCREEN-VALUE = "".

    clientes.dv:SENSITIVE = FALSE.
END.

DO WITH FRAME F_Segmentacion:
    ASSIGN Est_Civil:SCREEN-VALUE = "Soltero"
           Num_Hijos:SCREEN-VALUE = "0"
           Per_Acargo:SCREEN-VALUE = "0"
           Sexo:SCREEN-VALUE = "1"
           Tipo_Vinculo:SCREEN-VALUE = "2"
           W_NomProfesion:SCREEN-VALUE = " "
           Cod_Profesion:SCREEN-VALUE = "0"
           Clientes.Niv_Educativo:SCREEN-VALUE = "Ninguno"
           Lugar_Residencia:SCREEN-VALUE = "0"
           Clientes.DIR_Residencia:SCREEN-VALUE = ""
           Tel_Residencia:SCREEN-VALUE = ""
           Clientes.Email:SCREEN-VALUE = ""
           Estrato:SCREEN-VALUE = "1"
           Lugar_Expedicion:SCREEN-VALUE = "0"
           Lugar_Nacimiento:SCREEN-VALUE = "0"
           Clientes.Celular:SCREEN-VALUE = ""
           W_UbicacionResidencia:SCREEN-VALUE = "No se ha Escogido"
           W_CiuExpedicion:SCREEN-VALUE = "Ciudad No asignada"
           Clientes.Tipo_Vivienda:SCREEN-VALUE = "1"
           Clientes.Nom_Arrendatario:SCREEN-VALUE = ""
           Clientes.Tel_Arrendatario:SCREEN-VALUE = ""
           Anexos_Clientes.Nacionalidad:SCREEN-VALUE = "COLOMBIANA"
           Clientes.Fec_expedicion:SCREEN-VALUE = "?"
           Clientes.Fec_Nacimiento:SCREEN-VALUE = "?".

    DISABLE Clientes.DIR_Residencia
            Clientes.Nom_Arrendatario
            Clientes.Tel_Arrendatario.

    DISABLE Anexos_Clientes.Dir_Ant_Residencia
            Anexos_Clientes.Dir_Arrendatario.

    APPLY "value-changed" TO Clientes.sexo.
END.

DO WITH FRAME F_Conyuge:
    ASSIGN WR_NomProfesion:SCREEN-VALUE = ""
           R_Cod_Profesion:SCREEN-VALUE = "0"
           R_Niv_Educativo:SCREEN-VALUE = "Ninguno".
END.

DO WITH FRAME F_Ubicacion:
    ASSIGN Lugar_Comercial:SCREEN-VALUE = "0"
           DIR_Comercial:SCREEN-VALUE = ""
           Tel_Comercial:SCREEN-VALUE = ""
           W_UbicacionComercial:SCREEN-VALUE = "No se ha Escogido"
           Carnet:SCREEN-VALUE = ""
           Codigo_Ciiu:SCREEN-VALUE = "0"
           Clientes.Codigo_Ciiu:SCREEN-VALUE = "0"
           Clientes.Grupo:SCREEN-VALUE = "0"
           Clientes.Subgrupo:SCREEN-VALUE     = "0"
           W_NomCiiu:SCREEN-VALUE = ""
           W_NomCiiu-2:SCREEN-VALUE = ""
           W_NomCiiu-3:SCREEN-VALUE = " "
           Cod_Cargo:SCREEN-VALUE = "0" 
           Cod_Empresa:SCREEN-VALUE = "0"
           departamento:SCREEN-VALUE = "0"
           facultad:SCREEN-VALUE = "0"
           W_NomCargo:SCREEN-VALUE = ""
           W_NomEmpresa:SCREEN-VALUE = ""
           facultadDepartamento:SCREEN-VALUE = ""
           Cmb_TipAct:SCREEN-VALUE = ""
           Tip_Contrato:SCREEN-VALUE = "1"
           Anexos_Clientes.Fec_Pensionado:SCREEN-VALUE = ?.
END.

DO WITH FRAME F_Economica:
    ASSIGN Clientes.Salario:SCREEN-VALUE = "0"
           Clientes.Ing_Honorarios:SCREEN-VALUE = "0"
           Clientes.Ing_financieros:SCREEN-VALUE = "0"
           Clientes.Ing_arriendos:SCREEN-VALUE = "0"
           Clientes.Ing_Otros:SCREEN-VALUE = "0"
           Clientes.Gto_obligacion:SCREEN-VALUE = "0"
           Clientes.Gto_arriendo:SCREEN-VALUE = "0"
           Clientes.Sdo_Obligaciones:SCREEN-VALUE = "0"
           Clientes.Gto_Familiar:SCREEN-VALUE = "0"
           Clientes.Act_casa:SCREEN-VALUE = "0"
           Clientes.Act_inversion:SCREEN-VALUE = "0"
           Clientes.Act_vehiculo:SCREEN-VALUE = "0"
           Anexos_Clientes.Especifique_OtrosIng:SCREEN-VALUE = "0"
           Anexos_Clientes.Gto_TargetaCredito:SCREEN-VALUE = "0"
           Anexos_Clientes.Gto_Otros:SCREEN-VALUE = "0"
           Anexos_Clientes.Val_Pasivos:SCREEN-VALUE = "0"
           Anexos_Clientes.Maneja_Cta_Extanjera:SCREEN-VALUE = "NO"
           Anexos_Clientes.Tipo_Moneda_Divisa:SCREEN-VALUE = " "
           Anexos_Clientes.Nom_Banco_extranjero:SCREEN-VALUE = " "
           Anexos_Clientes.Num_cta_extranjera:SCREEN-VALUE = " "
           Anexos_Clientes.Ciudad_Pais_Bco_Extranjero:SCREEN-VALUE = " "
           Tot_Ingresos:SCREEN-VALUE = "0"
           Tot_Egresos:SCREEN-VALUE = "0"
           Tot_Activos:SCREEN-VALUE = "0".

    ASSIGN Clientes.Salario:LABEL = "Salario".

    IF AVAILABLE Clientes THEN
        IF clientes.tipo_cliente:SCREEN-VALUE IN FRAME f_clientes NE "1" THEN
            ASSIGN Clientes.Gto_Familiar:LABEL = "Gastos Empresariales"
                   Clientes.Salario:LABEL = "Ingresos Empresa".
        ELSE
            ASSIGN Clientes.Gto_Familiar:LABEL = "Gastos Familiares"
                   Clientes.Salario:LABEL = "Ingresos Familiares".
END.

DO WITH FRAME F_Autorizaciones:
    ASSIGN Anexos_Clientes.Proviene:SCREEN-VALUE = ""
           Anexos_Clientes.Represetacion:SCREEN-VALUE = ""
           Anexos_Clientes.Declaracion_Conocimiento:SCREEN-VALUE = "YES"
           Anexos_Clientes.Autorizaciones[1]:SCREEN-VALUE = "YES"
           Anexos_Clientes.Autorizaciones[2]:SCREEN-VALUE = "YES"
           Anexos_Clientes.Autorizaciones[3]:SCREEN-VALUE = "YES"
           Anexos_Clientes.Autorizaciones[4]:SCREEN-VALUE = "YES"
           Anexos_Clientes.Autorizaciones[5]:SCREEN-VALUE = "YES"
           Anexos_Clientes.Autorizaciones[6]:SCREEN-VALUE = "YES"
           Anexos_Clientes.Autorizaciones[7]:SCREEN-VALUE = "YES"
           Anexos_Clientes.Autorizaciones[8]:SCREEN-VALUE = "YES"
           Anexos_Clientes.Autorizaciones[9]:SCREEN-VALUE = "NO"
           Anexos_Clientes.Autorizaciones[10]:SCREEN-VALUE = "YES"
           Anexos_Clientes.Autorizaciones[11]:SCREEN-VALUE = "YES"
           Anexos_Clientes.Autorizaciones[12]:SCREEN-VALUE = "YES"
           Anexos_Clientes.Declaracion_aceptacion:SCREEN-VALUE = "YES"
           Anexos_Clientes.cam_log1:SCREEN-VALUE = "FIRMA SOLICITANTE Y HUELLA INDICE DERECHO".
END.

DO WITH FRAME F_Economica:
    ASSIGN Clientes.Act_casa:SCREEN-VALUE = "0"
           Clientes.Act_inversion:SCREEN-VALUE = "0"
           Clientes.Act_vehiculo:SCREEN-VALUE = "0"
           Clientes.Gto_arriendo:SCREEN-VALUE = "0"
           Clientes.Gto_Familiar:SCREEN-VALUE = "0"
           Clientes.Gto_obligacion:SCREEN-VALUE = "0"
           Clientes.Ing_arriendos:SCREEN-VALUE = "0"
           Clientes.Ing_financieros:SCREEN-VALUE = "0"
           Clientes.Ing_Honorarios:SCREEN-VALUE = "0"
           Clientes.Ing_otros:SCREEN-VALUE = "0"
           Clientes.Salario:SCREEN-VALUE = "0"
           Clientes.Sdo_Obligaciones:SCREEN-VALUE = "0"
           Tot_Ingresos:SCREEN-VALUE = "0"
           Tot_Egresos:SCREEN-VALUE = "0"
           Tot_Activos:SCREEN-VALUE = "0".
END.

DO WITH FRAME F_Otros:
    ASSIGN Clientes.Calificacion:SCREEN-VALUE = ""
           Clientes.Aut_CentralRiesgo:SCREEN-VALUE = "NO"
           Clientes.Reportado_Super:SCREEN-VALUE = "NO"
           Clientes.Reportado_Fiscalia:SCREEN-VALUE = "NO"
           Cod_Segmento:SCREEN-VALUE = "0"
           Clientes.Reportado_Procredito:SCREEN-VALUE = "NO"
           clientes.id_AsistioAsamblea:SCREEN-VALUE = "NO"
           Clientes.Sancionado:SCREEN-VALUE = "NO"
           Clientes.Id_Preexistentes:SCREEN-VALUE = "NO"
           Clientes.Id_Privilegiado:SCREEN-VALUE = "0"
           Clientes.Id_PuedeCodeudar:SCREEN-VALUE = "Yes"
           Clientes.Usuario:SCREEN-VALUE = W_Usuario
           W_NomRetiro:SCREEN-VALUE = "Persona aun Activa"
           Cod_Ingreso:SCREEN-VALUE = "0"
           Cod_Retiro:SCREEN-VALUE = "0".

    ASSIGN Clientes.Fec_Calificacion:SCREEN-VALUE = "?"
           Clientes.Fec_IngEmpresa:SCREEN-VALUE = "?"
           Clientes.Fec_Ingreso:SCREEN-VALUE = STRING(TODAY)
           Clientes.Fec_IniSancion:SCREEN-VALUE = "?"
           Clientes.Fec_fallecido:SCREEN-VALUE = "?"
           Clientes.Fec_Retiro:SCREEN-VALUE = "?"
           Clientes.Fec_UltActualiza:SCREEN-VALUE = STRING(TODAY)
           clientes.fecPagare:SCREEN-VALUE = "?".

    FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
    IF AVAILABLE(Usuarios) THEN DO:
        W_NomUsuario:SCREEN-VALUE = W_Usuario + " : " + Usuarios.Nombre + " - Nit: " + Usuarios.Nit.

        IF Usuarios.Id_OpeOfi THEN
            ENABLE Cmb_Agencia WITH FRAME F_Clientes.
        ELSE
            DISABLE Cmb_Agencia WITH FRAME F_Clientes.
    END.
    ELSE
        W_NomUsuario:SCREEN-VALUE = "Falta Usuario".
END.

HIDE FRAME F_Ubicacion
     FRAME F_Autorizaciones
     FRAME F_Economica
     FRAME F_Otros
     FRAME F_Relaciones
     FRAME F_Documentacion
     FRAME F_Act_Bienes
     FRAME F_Act_Otros
     FRAME F_Act_Veh.

VIEW FRAME F_Segmentacion.

ASSIGN RSeleccion:SCREEN-VALUE = "1".

HIDE FRAME F_Aportes.

DO with FRAME F_Segmentacion:
    Clientes.num_hijos:SCREEN-VALUE = "0".
    Anexos_Clientes.num_hijos_11:SCREEN-VALUE = "0".
    Anexos_Clientes.num_hijos_18:SCREEN-VALUE = "0".
    Clientes.per_acargo:SCREEN-VALUE = "0".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.

SUBSCRIBE TO "CierraClientesPasivosBienes" ANYWHERE.

W_Ok = Cmb_Relaciones:ADD-LAST("00000 - Ninguna Relacion") IN FRAME F_Relaciones.
W_Ok = Cmb_Relaciones:ADD-LAST("99999 - Todas las Relaciones") IN FRAME F_Relaciones.

HIDE FRAME F_MicroEmpresas.

FOR EACH Varios WHERE Varios.Tipo = 3
                  AND Varios.Codigo <> 11
                 AND varios.codigo = 1 NO-LOCK:
    W_Ok = Cmb_Relaciones:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME F_Relaciones.
    Cmb_Relaciones:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion.
END.

FIND FIRST Clientes WHERE Clientes.Agencia EQ W_Agencia NO-ERROR.
IF AVAIL(Clientes) THEN
    RUN Mostrar_Cliente.

FOR EACH Agencias NO-LOCK:
    W_Ok = Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME F_Clientes.

    IF AVAIL(Clientes) AND Agencias.Agencia EQ Clientes.Agencia THEN
        Cmb_Agencia:SCREEN-VALUE IN FRAME F_Clientes = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
END.

FIND FIRST CClientes WHERE CClientes.nit = Cliente.nit:SCREEN-VALUE IN FRAME F_Clientes NO-LOCK NO-ERROR.
IF AVAIL CClientes THEN DO WITH FRAME F_Economica:
    /* ENABLE Btn_Grabar_Act_Pas.*/
END.
ELSE DO:
    /*DISABLE Btn_Grabar_Act_Pas.*/
END.

DISABLE bdcentral.Clientes.Dir_Residencia.

HIDE FRAME F_Falta.

ENABLE Btn_Ingresar Btn_Borrar Btn_Salir WITH FRAME F_Clientes.

DISABLE Btn_Salvar Btn_Cancelar Btn_Deshacer Btn_Borrar WITH FRAME F_Clientes.

HIDE FRAME F_Aportes FRAME F_Act_Bienes FRAME F_Act_Otros FRAME F_Act_Veh.

IF AVAIL Clientes AND Clientes.Tipo_Identificacion = "NIT" THEN DO WITH FRAME F_Juridicas:
    /* */
END.
ELSE DO WITH FRAME f_segmentacion:
    /* */
END.

RUN Totales_Economica.
RUN pPrntHndle IN h_wsolprofina(THIS-PROCEDURE).
RUN hideObject IN h_wgnrdordrccion.
RUN hideObject IN h_wsolprofina.
SUBSCRIBE "DrccionCmbiada" IN h_wgnrdordrccion.
HIDE FRAM f_conyuge.

clientes.nombre:LABEL = "Nombre".
clientes.apellido1:LABEL = "Apellido 1".
clientes.apellido2:LABEL = "Apellido 2".

APPLY "value-changed" TO Clientes.tipo_vivienda.
APPLY "value-changed" TO  cmb_relaciones IN FRAME f_relaciones.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_Celda wWin 
PROCEDURE Llenar_Celda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    chWorkSheet:Range(ValCol):Value = Dato.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Cliente wWin 
PROCEDURE Mostrar_Cliente :
DO WITH FRAME F_Clientes:
    IF NOT CAN-FIND(FIRST Anexos_Clientes WHERE Anexos_Clientes.nit = Clientes.nit) THEN DO:
        CREATE Anexos_Clientes.
        Anexos_Clientes.nit = Clientes.nit.
    END.

    FIND FIRST Anexos_Clientes NO-LOCK WHERE Anexos_Clientes.nit = Clientes.nit NO-ERROR.

    EMPTY TEMP-TABLE TClientes.
    EMPTY TEMP-TABLE TAnexos_Clientes.
    
    CREATE TClientes.
    BUFFER-COPY Clientes TO TClientes.
    
    CREATE TAnexos_Clientes.
    BUFFER-COPY Anexos_Clientes TO TAnexos_Clientes.

    ASSIGN Clientes.Nit:SCREEN-VALUE IN FRAME F_Clientes = Clientes.Nit
           clientes.dv:SCREEN-VALUE = STRING(clientes.dv)
           Clientes.Tipo_Identificacion:SCREEN-VALUE = STRING(Clientes.Tipo_Identificacion)
           Clientes.Tipo_Cliente:SCREEN-VALUE = STRING(Clientes.Tipo_Cliente)
           Clientes.Estado:SCREEN-VALUE = STRING(Clientes.Estado)
           Anexos_Clientes.Dir_Correspondencia:SCREEN-VALUE IN FRAME F_Segmentacion = string(Anexos_Clientes.Dir_Correspondencia)
           Clientes.Nombre:SCREEN-VALUE = Clientes.Nombre.

    W_Ok = Foto:LOAD-IMAGE("imagenes\fotos\0.jpg") NO-ERROR.

    IF Clientes.Fotografia THEN DO:
        gTexto = SEARCH("imagenes\fotos\" + TRIM(Clientes.Nit:SCREEN-VALUE) + ".jpg").

        IF gTexto EQ ? THEN DO:
            MESSAGE "No ha sido capturada la fotografia del cliente."
                VIEW-AS ALERT-BOX INFORMATION.

            W_Ok = Foto:LOAD-IMAGE("imagenes\fotos\0.jpg") NO-ERROR.
        END.
        ELSE
            W_Ok = Foto:LOAD-IMAGE(gTexto) NO-ERROR.
    END.

    RUN TipoCliente.

    IF Clientes.Tipo_Identificacion = "NIT" THEN DO:
        W_Ok = Clientes.Tipo_Cliente:DISABLE("Natural").
        W_Ok = Clientes.Tipo_Cliente:ENABLE("Jurídica").
    END.
    ELSE DO:
        W_Ok = Clientes.Tipo_Cliente:ENABLE("Natural").
        W_Ok = Clientes.Tipo_Cliente:DISABLE("Jurídica").
        
    END.
END.

DO WITH FRAME F_Segmentacion:
    RUN Ubicacion.

    IF Clientes.Tipo_Identificacion <> "NIT" THEN
        DISP Clientes.Fec_expedicion
             Clientes.DIR_Residencia
             Clientes.Lugar_Residencia
             Anexos_Clientes.AA_cliente
             Clientes.Tel_Residencia
             Clientes.Celular
             Clientes.Email
             Clientes.Tel_Arrendatario.

    DISP Clientes.Tipo_Vinculo
         Clientes.Sexo
         Clientes.Grado
         Clientes.Jornada
         Clientes.Est_Civil
         Clientes.Num_Hijos
         Anexos_Clientes.Num_Hijos_11
         Anexos_Clientes.Num_Hijos_18
         Clientes.Per_ACargo
         Clientes.Lugar_Expedicion
         Clientes.Lugar_Nacimiento
         Clientes.Fec_Nacimiento
         Anexos_Clientes.Nacionalidad
         Clientes.Niv_Educativo
         Clientes.Niv_EduCop
         Clientes.Num_ActaCop
         Clientes.Tipo_Vivienda
         Clientes.Nom_Arrendatario
         Anexos_Clientes.Val_arriendo
         Anexos_Clientes.Dir_Arrendatario
         Clientes.Estrato
         Anexos_Clientes.per_anos
         Anexos_Clientes.per_meses
         Anexos_Clientes.Dir_Ant_Residencia
         Anexos_Clientes.Tel_Ant_Residencia
         Anexos_Clientes.per_ant_anos
         Anexos_Clientes.per_ant_meses
         Anexos_Clientes.ind_arr
        WITH FRAME F_Segmentacion.

    IF can-do("casado,Unión Libre",Clientes.Est_Civil:SCREEN-VALUE) THEN
        BtonInfoCnyge:SENSITIVE IN FRAME f_Clientes = TRUE.
    ELSE
        BtonInfoCnyge:SENSITIVE IN FRAME f_Clientes = FALSE.

    /*IF Clientes.Tipo_Vinculo GT 2 THEN
        Clientes.Tipo_Vinculo:SCREEN-VALUE IN FRAME F_Segmentacion = "2".*/

    FIND FIRST Varios WHERE Varios.Tipo EQ 1 AND Varios.Codigo EQ Clientes.Cod_Profesion NO-LOCK NO-ERROR.
    IF AVAIL Varios THEN
        W_NomProfesion:SCREEN-VALUE = Varios.Descripcion.
    ELSE
        W_NomProfesion:SCREEN-VALUE = "Profesión aun no escogida".

    FIND FIRST Varios WHERE Varios.Tipo EQ 37 AND Varios.Codigo EQ Clientes.Colegio NO-LOCK NO-ERROR.
    IF AVAIL Varios THEN
        Nom_Colegio:SCREEN-VALUE = Varios.Descripcion.

    IF Clientes.Tipo_Vivienda:SENSITIVE EQ YES THEN DO:
        IF Clientes.Tipo_Vivienda EQ 2 THEN
            ENABLE Clientes.Nom_Arrendatario
                   Anexos_Clientes.Val_arriendo
                   Anexos_Clientes.Dir_Arrendatario
                   Clientes.Tel_Arrendatario.
        ELSE
            DISABLE Clientes.Nom_Arrendatario
                    Anexos_Clientes.Val_arriendo
                    Anexos_Clientes.Dir_Arrendatario
                    Clientes.Tel_Arrendatario.
    END.
    ELSE
        DISABLE Clientes.Nom_Arrendatario
                Anexos_Clientes.Val_arriendo
                Anexos_Clientes.Dir_Arrendatario
                Clientes.Tel_Arrendatario.
END.

RUN Mostrar_Conyuge.

FIND FIRST Varios WHERE Varios.tipo = 2 AND Varios.Codigo = Clientes.cod_cargo NO-LOCK NO-ERROR.

W_NomCargo = IF AVAIL Varios THEN Varios.Descripcion ELSE "".

ASSIGN Cmb_Est_Cargo = Anexos_Clientes.Estado_Cargo
       w_nomCiiu-3 = "".

FIND FIRST Varios WHERE Varios.tipo = 35 AND Varios.Codigo = int(Clientes.Cod_Actividad) NO-LOCK NO-ERROR.
IF AVAIL Varios THEN
    w_nomCiiu-3 = Varios.Descripcion.

DO WITH FRAME F_Ubicacion:
    IF Clientes.Tipo_Identificacion <> "NIT" THEN
        DISP Clientes.Cod_Actividad
             Anexos_Clientes.Acti_Economica_Emp.

    DISP Clientes.Lugar_Comercial
         Clientes.DIR_Comercial
         Clientes.Tel_Comercial
         Clientes.Id_Micro /* Clientes.Carnet */
         Clientes.Codigo_Ciiu
         Clientes.Cod_Cargo
         Clientes.Cod_Empresa
         clientes.facultad
         clientes.departamento
         Clientes.Grupo
         Clientes.SubGrupo
         Clientes.Tip_Contrato
         Clientes.Fec_IngEmpresa
         Anexos_Clientes.Exento_retencion
         Anexos_Clientes.Exento_GMF
         Anexos_Clientes.Declara_Renta
         Anexos_Clientes.Adm_Recursos_Publ
         Anexos_Clientes.Especifique_Ocupacion
         Anexos_Clientes.Extencion_Comercial
         Anexos_Clientes.Tel_Fax_Comercial
         Anexos_Clientes.Nit_Independiente
         Anexos_Clientes.Tiempo_Actividad
         Anexos_Clientes.cam_int1
         Anexos_Clientes.Fec_Pensionado
         W_NomCargo
         w_nomCiiu-3
         Cmb_Est_Cargo
         Clientes.Fuerza_Mayor
        WITH FRAME F_Ubicacion.

    Cmb_Situacion_Actual:SCREEN-VALUE = Cmb_Situacion_Actual:ENTRY(INTEGER(Clientes.Situacion_Actual + 1)).

    RUN Ubicacion.

    IF AVAIL(Varios) THEN
        W_NomCargo:SCREEN-VALUE = Varios.Descripcion.
    ELSE
        W_NomCargo:SCREEN-VALUE = "Cargo aun no escogido".

    FIND FIRST Ciiu WHERE Ciiu.Codigo_Ciiu = INT(Anexos_Clientes.Acti_Economica_Emp) NO-LOCK NO-ERROR.
    IF AVAIL Ciiu THEN
        W_NomCiiu-2:SCREEN-VALUE = Ciiu.Descripcion.

    /* RUN CodigoCiiu.*/

    FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
    IF AVAIL(Empresas) THEN
        W_NomEmpresa:SCREEN-VALUE = Empresas.Alias_Empresa.

    FIND FIRST facultades WHERE facultades.agencia = clientes.agencia
                            AND facultades.tipo = "F"
                            AND facultades.codigo = STRING(clientes.facultad,"99") NO-LOCK NO-ERROR.
    IF AVAILABLE facultades THEN DO:
        facultadDepartamento:SCREEN-VALUE = facultades.nombre.

        FIND FIRST facultades WHERE facultades.agencia = clientes.agencia
                                AND facultades.tipo = "D"
                                AND facultades.codigo = STRING(clientes.facultad,"99") + STRING(clientes.departamento,"999") NO-LOCK NO-ERROR.
        IF AVAILABLE facultades THEN
            facultadDepartamento:SCREEN-VALUE = facultadDepartamento:SCREEN-VALUE + " - " + facultades.nombre.
    END.
    ELSE DO:
        facultadDepartamento:SCREEN-VALUE = "".
    END.

    Cmb_TipAct:SCREEN-VALUE = Clientes.Tipo_Actividad.

    FIND FIRST Varios WHERE Varios.Tipo EQ 2 AND Varios.Codigo EQ Clientes.Cod_Cargo NO-LOCK NO-ERROR.
END.

DO WITH FRAME F_Economica:
    IF Clientes.Tipo_Identificacion <> "NIT" THEN
        DISP Clientes.Salario
             Clientes.Id_Mon_Ext
             Clientes.Ing_Otros
             Clientes.Sdo_Obligaciones
             Clientes.Act_vehiculo
             Anexos_Clientes.Especifique_OtrosIng Anexos_Clientes.Val_Pasivos Anexos_Clientes.Maneja_Cta_Extanjera
           Anexos_Clientes.Tipo_Moneda_Divisa Anexos_Clientes.Nom_Banco_Extranjero Anexos_Clientes.Num_cta_Extranjera
           Anexos_Clientes.Otra_Transac_Extr Anexos_Clientes.Ciudad_Pais_Bco_Extranjero.
   DISP Clientes.Ing_Honorarios Clientes.Ing_financieros Clientes.Ing_arriendos Clientes.Gto_obligacion Clientes.Gto_arriendo
        Clientes.Gto_Familiar Clientes.Act_casa Clientes.Act_inversion Anexos_Clientes.Gto_TargetaCredito Anexos_Clientes.Gto_Otros
     /* Anexos_Clientes.Transac_Mod_Ext = Cmb_Ext_Transacciones:SCREEN-VALUE*/.
   RUN Totales_Economica.
   DISP Tot_Ingresos Tot_Egresos Tot_Activos WITH FRAME F_Economica.
/* ASSIGN Tot_Ingresos:SCREEN-VALUE = 
   STRING(Clientes.Salario + Clientes.Ing_Arriendos + Clientes.Ing_Financieros + Clientes.Ing_Honorarios + Clientes.Ing_Otros)
   Tot_Egresos:SCREEN-VALUE = 
   STRING(Clientes.Gto_Arriendo + Clientes.Gto_Familiar + Clientes.Gto_Obligacion)
   Tot_Activos:SCREEN-VALUE = 
   STRING(Clientes.Act_Casa + Clientes.Act_Vehiculo + Clientes.Act_Inversion). */
END.
DO WITH FRAME F_Autorizaciones:
   DISP Anexos_Clientes.Represetacion Anexos_Clientes.Proviene Anexos_Clientes.Declaracion_aceptacion
        Anexos_Clientes.Autorizaciones[ 1] Anexos_Clientes.Autorizaciones[ 2] Anexos_Clientes.Autorizaciones[3] 
        Anexos_Clientes.Autorizaciones[ 4] Anexos_Clientes.Autorizaciones[ 5] Anexos_Clientes.Autorizaciones[6] 
        Anexos_Clientes.Autorizaciones[ 7] Anexos_Clientes.Autorizaciones[ 8] Anexos_Clientes.Autorizaciones[10] 
        Anexos_Clientes.Autorizaciones[11] Anexos_Clientes.Autorizaciones[12]Anexos_Clientes.Declaracion_aceptacion
        Anexos_Clientes.Fec_Diligenciamiento Anexos_Clientes.cam_log1.
END.
DO WITH FRAME F_Otros:
   DISP Clientes.Calificacion Clientes.Aut_CentralRiesgo Clientes.Reestructurado Clientes.Reportado_Super
        Clientes.Reportado_Fiscalia Clientes.Reportado_Procredito clientes.id_AsistioAsamblea Clientes.Sancionado Clientes.Dias_Sancion
        Clientes.Id_Preexistentes Clientes.Id_Privilegiado Clientes.Id_PuedeCodeudar Clientes.Usuario 
        Clientes.Cod_Ingreso Clientes.Cod_Retiro /*Clientes.Med_Publicitario*/  Clientes.Fec_Ult_Act[1]
        Clientes.Fec_Ult_Act[2] Clientes.Fec_Ult_Act[3] Clientes.Fec_Ult_Act[4] Clientes.Fec_Ult_Act[5]
        Clientes.Fec_Ult_Act[6] Clientes.Fec_Ult_Act[7] WITH FRAME F_Otros.
   DISP Clientes.Fec_Calificacion Clientes.Fec_IniSancion Clientes.Fec_fallecido
        Clientes.Fec_Ingreso Clientes.Fec_Retiro Clientes.Fec_UltActualiza clientes.fecPagare WITH FRAME F_Otros.
   FIND FIRST Varios WHERE Varios.Tipo EQ 6 AND Varios.Codigo EQ Clientes.Cod_Segmento NO-LOCK NO-ERROR.
   IF AVAIL(Varios) THEN W_NomSegmento:SCREEN-VALUE = Varios.Descripcion.
                    ELSE W_NomSegmento:SCREEN-VALUE = "00000 - Segmento aun no escogida".
   FIND FIRST Varios WHERE Varios.Tipo EQ 4 AND Varios.Codigo EQ Clientes.Cod_Ingreso NO-LOCK NO-ERROR.
/* IF AVAIL(Varios) THEN W_NomIngreso:SCREEN-VALUE = Varios.Descripcion.       */
/*                  ELSE W_NomIngreso:SCREEN-VALUE = "Causal aun no escogida". */
   FIND FIRST Varios WHERE Varios.Tipo EQ 5 AND Varios.Codigo EQ Clientes.Cod_Retiro NO-LOCK NO-ERROR.
   IF AVAIL(Varios) THEN W_NomRetiro:SCREEN-VALUE = Varios.Descripcion.
                    ELSE W_NomRetiro:SCREEN-VALUE = "Persona aun Activa".
   FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Clientes.Usuario NO-LOCK NO-ERROR.
   IF AVAIL(Usuarios) THEN W_NomUsuario:SCREEN-VALUE = Clientes.Usuario + " : " + Usuarios.Nombre + " - Nit: " + Usuarios.Nit.
   ELSE DO:
      FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
      IF AVAIL Usuarios THEN ASSIGN  W_NomUsuario:SCREEN-VALUE = W_Usuario + " : " +
                                     Usuarios.Nombre + " - Nit: " + Usuarios.Nit
                                     Clientes.Usuario:SCREEN-VALUE = W_Usuario.
                        ELSE W_NomUsuario:SCREEN-VALUE = "Falta Usuario".
   END.
/*         IF Clientes.Med_Publicitario LE " "                */
/*         THEN Clientes.Med_Publicitario:SCREEN-VALUE = "".  */
END.
IF Clientes.Tipo_Identificacion = "NIT" THEN DO:
   HIDE FRAME F_segmentacion. VIEW FRAME F_Juridicas.
   ASSIGN W_Nom_Act_Ppal = "" W_Nom_Ciu_Emp = "" W_NUbicacion = "".

   FIND Varios WHERE Varios.tipo = 35 AND Varios.Codigo = int(Clientes.Cod_Actividad) NO-LOCK NO-ERROR.                                                                                            
   IF AVAIL Varios THEN W_Nom_Act_Ppal = Varios.Descripcion.
   FIND FIRST Ciiu WHERE Ciiu.Codigo_Ciiu = INT(Anexos_Clientes.Acti_Economica_Emp) NO-LOCK NO-ERROR.
   IF AVAIL Ciiu THEN W_Nom_Ciu_Emp = Ciiu.Descripcion.

/* cCdgoCiiu = Anexos_Clientes.Acti_Economica_Emp + FILL("-",2).                                                                          */
/* DO i = 1 TO 3:                                                                                                                         */
/*    FIND FIRST Ciiu NO-LOCK WHERE Ciiu.Tipo = i                                                                                         */
/*         AND (IF i = 1 THEN Ciiu.Grupo = INTEGER(ENTRY(1,cCdgoCiiu,"-")) ELSE TRUE)                                                     */
/*         AND (IF i = 2 THEN Ciiu.Grupo = INTEGER(ENTRY(1,cCdgoCiiu,"-")) AND Ciiu.subGrupo = INTEGER(ENTRY(2,cCdgoCiiu,"-")) ELSE TRUE) */
/*         AND (IF i = 3 THEN Ciiu.Grupo = INTEGER(ENTRY(1,cCdgoCiiu,"-")) AND Ciiu.subGrupo = INTEGER(ENTRY(2,cCdgoCiiu,"-")) AND        */
/*              Ciiu.Codigo_Ciiu = INTEGER(ENTRY(3,cCdgoCiiu,"-")) ELSE TRUE) NO-ERROR.                                                   */
/*    IF AVAIL Ciiu THEN W_Nom_Ciu_Emp = W_Nom_Ciu_Emp + Ciiu.Descripcion + " - ".                                                        */
/* END.                                                                                                                                   */
/* W_Nom_Ciu_Emp = TRIM(W_Nom_Ciu_Emp,"-").                                                                                               */

   FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = Clientes.Lugar_Residencia NO-LOCK NO-ERROR.
   IF   AVAIL Ubicacion THEN  W_NUbicacion   =  Ubicacion.Nombre +
        IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
   IF   AVAIL Ubicacion THEN  W_NUbicacion   =  W_NUbicacion + " - " + Ubicacion.Nombre.
   FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "D" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Residencia,1,2) NO-LOCK NO-ERROR.
   IF   AVAIL Ubicacion THEN  W_NUbicacion   =  W_NUbicacion + " - " + Ubicacion.Nombre.
   W_Ubi_Empresa = LC(W_NUbicacion).
   DISP Clientes.Celular Clientes.Fec_Expedicion Clientes.DIR_Residencia Clientes.Tel_Residencia Clientes.Tel_Arrendatario
        Anexos_Clientes.AA_Cliente Clientes.Email Clientes.Tipo_Empresa Clientes.Salario Clientes.Ing_Otros
        Anexos_Clientes.Especifique_Otros Clientes.Sdo_Obligaciones Clientes.Act_Vehiculo Anexos_Clientes.Val_Pasivos
        Clientes.Id_Mon_Ext Anexos_Clientes.Otra_Transac_Extr Anexos_Clientes.Maneja_Cta_Extanjera
        Anexos_Clientes.Tipo_Moneda_Divisa Anexos_Clientes.Num_Cta_Extranjera Anexos_Clientes.Nom_Banco_Extranjero
        Anexos_Clientes.Ciudad_Pais_Bco_Extranjero W_Nom_Act_Ppal W_Nom_Ciu_Emp W_Ubi_Empresa
  /* */ Clientes.Lugar_Residencia Clientes.Cod_Actividad Anexos_Clientes.Acti_Economica_Emp /* */ WITH FRAME F_Juridicas.
   DISABLE Anexos_Clientes.Otra_Transac_Ext Anexos_Clientes.Tipo_Moneda_Divisa Anexos_Clientes.Num_Cta_Extranjera
           Anexos_Clientes.Nom_Banco_Extranjero Anexos_Clientes.Ciudad_Pais_Bco_Extranjero WITH FRAME F_Juridicas.
   IF Clientes.Id_Mon_Ext THEN ENABLE Anexos_Clientes.Otra_Transac_Ext WITH FRAME F_Juridicas.
   IF Anexos_Clientes.Maneja_Cta_Extanjera THEN
      ENABLE Anexos_Clientes.Tipo_Moneda_Divisa Anexos_Clientes.Num_Cta_Extranjera
             Anexos_Clientes.Nom_Banco_Extranjero Anexos_Clientes.Ciudad_Pais_Bco_Extranjero WITH FRAME F_Juridicas.
END.
ELSE DO:
   HIDE FRAME F_Juridicas. VIEW FRAME F_Segmentacion.
END.
APPLY "value-changed" TO Clientes.tipo_vivienda.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Conyuge wWin 
PROCEDURE Mostrar_Conyuge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME F_Conyuge:  
        fIncialza(FRAME F_Conyuge:FIRST-CHILD).
        FIND LAST Relaciones 
            WHERE 
                Relaciones.Nit          = Clientes.nit:SCREEN-VALUE IN FRAME F_Clientes
            AND Relaciones.Cod_Relacion = 00001
            AND Relaciones.Descripcion  = "Conyuge"
            NO-LOCK NO-ERROR.
        IF AVAILABLE Relaciones 
        THEN DO:
            FIND FIRST BClientes WHERE BClientes.nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
            IF AVAILABLE bClientes
            THEN DO:
                /*FIND FIRST BAnexos_Clientes WHERE BAnexos_Clientes.nit = Relaciones.Nit_Relacion NO-LOCK NO-ERROR.*/
            
                ASSIGN  R_Tipo_Identificacion = BClientes.Tipo_identificacion
                        R_Sexo                = BClientes.sexo
                        R_Ocupacion           = BClientes.Tipo_Actividad
                        R_Niv_Educativo       = BClientes.Niv_Educativo
                        R_Dir_Empresa         = BClientes.Lugar_comercial
                        R_Lugar_Nacimiento    = BClientes.Lugar_Nacimiento
                        R_Tel_Empresa-Indctvo = /*IF AVAILABLE BAnexos_Clientes THEN BAnexos_Clientes.Ind_cli ELSE*/ 0
                        /*cnygeextncion         = bAnexos_Clientes.extencion_comercial*/ .
            
                DISPLAY Relaciones.Nit_Relacion        @ R_Tercero           
                        R_Tipo_Identificacion 
                        Clientes.Nombre                 @ R_Nombre
                       BClientes.Apellido1            @ R_Apellido1
                        BClientes.Apellido2            @ R_Apellido2
                        R_Sexo
                        R_Lugar_Nacimiento
                        BClientes.Fec_Nacimiento       @ R_Fec_Nacimiento
                        /*BAnexos_Clientes.Nacionalidad  WHEN AVAILABLE BAnexos_Clientes @ R_Nacionalidad */
                        R_Niv_Educativo
                        BClientes.Cod_Profesion        @ R_Cod_Profesion
                        R_Ocupacion
                        BClientes.Dir_comercial        @ R_Direccion
                        R_Dir_Empresa
                        BClientes.Tel_comercial        @ R_Tel_Empresa
                        Relaciones.Rel_Cargo_Acti      @ R_Cargo_Actividad
                        Relaciones.Rel_Nom_Empresa     @ R_Empresa        
                        Relaciones.Tot_Ing_Conyuge     @ R_TIngreso       
                        Relaciones.Tot_Egr_Conyuge     @ R_TEgresos
                        R_Tel_Empresa-Indctvo
                        cnygeextncion.      
        
                RUN Ubicacion_conyuge. 
        
                FIND FIRST Varios WHERE Varios.Tipo EQ 1 AND Varios.Codigo EQ BClientes.Cod_Profesion NO-LOCK NO-ERROR.
                IF AVAILABLE(Varios) 
                THEN WR_NomProfesion:SCREEN-VALUE = Varios.Descripcion.
                ELSE WR_NomProfesion:SCREEN-VALUE = "Profesión aun no escogida".
            end. /* IF AVAILABLE bClientes */
        END. /* IF AVAILABLE Relaciones  */
    END. /* DO WITH FRAME F_Conyuge:   */
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE o_Validacion_Informacion wWin 
PROCEDURE o_Validacion_Informacion :
/* Tipos de Clientes Asociado = 1, Cliente no Asociado 2, Tercero 3, Proveedor 4*/
fVldaDtos(FRAME F_Clientes:FIRST-CHILD).

DO WITH FRAME F_Clientes:
    IF trim(Clientes.Nombre:SCREEN-VALUE) EQ "" THEN
        RUN Crea_Faltante(INPUT "Nombre", INPUT "Principal").

    IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) = 1 THEN DO:
        IF Clientes.Apellido1:SCREEN-VALUE EQ "" THEN
            RUN Crea_Faltante(INPUT "Primer Apellido", INPUT "Principal").
    END.
END.
    
    DO WITH FRAME F_Segmentacion:
        IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) = 1 
        THEN DO:
            IF Clientes.Est_Civil:SCREEN-VALUE EQ "No Aplica" 
            THEN RUN Crea_Faltante(INPUT "Estado Civil", INPUT "Segmentacion").
        END.
    
        IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) = 1 
        THEN DO:
            IF W_UbicacionResidencia:SCREEN-VALUE = "" 
            THEN RUN Crea_Faltante(INPUT "Depto y Municipio. Residencia", INPUT "Segmentacion").
            IF Clientes.DIR_Residencia:SCREEN-VALUE EQ "" 
            THEN RUN Crea_Faltante(INPUT "Dirección Residencia", INPUT "Segmentacion").
            IF Clientes.Tel_Residencia:SCREEN-VALUE EQ "" 
            THEN RUN Crea_Faltante(INPUT "Teléfono Residencia", INPUT "Segmentacion").
        END.
    
        IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) = 1
        THEN DO:
            IF Clientes.Fec_Nacimiento:SCREEN-VALUE EQ "" 
            THEN RUN Crea_Faltante(INPUT "Fecha Nacimiento", INPUT "Segmentacion").
    
            Wk_Edad = YEAR(W_FEcha) - YEAR(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)).
    
            IF MONTH(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)) LT MONTH(W_Fecha) 
            THEN.
            ELSE DO:
                IF MONTH(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)) EQ MONTH(W_fecha) 
                AND DAY(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)) GT DAY(W_fecha) 
                THEN  Wk_edad = Wk_edad - 1.
                ELSE  
                IF MONTH(DATE(Clientes.Fec_Nacimiento:SCREEN-VALUE)) GT MONTH(W_fecha) 
                THEN    Wk_edad = Wk_edad - 1.
            END.
        
            IF Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes EQ "1" AND  Wk_Edad LT 18 THEN DO:
               MESSAGE "El Cliente esta identificado como mayor de edad" SKIP
                       "sin embargo la fecha de nacimiento no coincide" SKIP
                       "con esta identificación, según la fecha de" SKIP
                       "nacimiento el cliente tendría: " Wk_Edad " años" VIEW-AS ALERT-BOX ERROR.
               RUN Crea_Faltante(INPUT "Fecha nacimiento", INPUT "Segmentacion (Fecha inconsistente)").
            END.
            IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) = 1 THEN DO:
               IF Clientes.Tipo_Vivienda:SCREEN-VALUE EQ "2" 
               AND (Clientes.Nom_Arrendatario:SCREEN-VALUE EQ "" 
               OR Anexos_Clientes.Val_arriendo:SCREEN-VALUE EQ "" 
               OR Anexos_Clientes.Dir_Arrendatario:SCREEN-VALUE EQ "" 
               OR Clientes.Tel_Arrendatario:SCREEN-VALUE EQ "") 
               THEN RUN Crea_Faltante(INPUT "Información Arrendatario", INPUT "Segmentacion").
            END.
        END.
    /*comunes a todos*/
    END.
    DO WITH FRAME F_Ubicacion:
    
        IF Clientes.DIR_Comercial:SCREEN-VALUE EQ "" 
        THEN RUN Crea_Faltante(INPUT "Dirección Comercial", INPUT "Ubicación").
        IF Clientes.Tel_Comercial:SCREEN-VALUE EQ "" 
        THEN RUN Crea_Faltante(INPUT "Teléfono Comercial", INPUT "Ubicación").
        IF W_UbicacionComercial:SCREEN-VALUE EQ "" 
        THEN RUN Crea_Faltante(INPUT "Depto y Municipio Comercial", INPUT "Ubicación").
    
    
        /*    IF Clientes.DIR_Correspondencia:SCREEN-VALUE EQ "yes" AND                                  */
        /*       Clientes.Dir_Comercial:SCREEN-VALUE EQ "" THEN DO:                                      */
        /*       RUN Crea_Faltante(INPUT "Dir.Comercial x Envio de Correspondencia", INPUT "Ubicación"). */
        /*    END.                                                                                       */
    END.
    
DO WITH FRAME F_Economica:
    IF INTEGER(Clientes.Tip_Contrato:SCREEN-VALUE IN FRAME F_Ubicacion) LE 2 AND INTEGER(Clientes.Tip_Contrato:SCREEN-VALUE) GT 0 THEN DO:
        IF DECIMAL(Clientes.Salario:SCREEN-VALUE IN FRAME F_Economica) LE 0 THEN
            RUN Crea_Faltante(INPUT "Salario",
                              INPUT "Económica").

        IF W_NomEmpresa:SCREEN-VALUE IN FRAME F_Ubicacion EQ "" OR W_NomEmpresa:SCREEN-VALUE IN FRAME F_Ubicacion EQ "Empresa aun no escogida" THEN
            RUN Crea_Faltante(INPUT "Empresa",
                              INPUT "Ubicacion").

        IF facultadDepartamento:SCREEN-VALUE IN FRAME F_Ubicacion = "" THEN
            RUN Crea_faltante(INPUT "Facultad/Departamento",
                              INPUT "Ubicacion").

        IF Clientes.Fec_IngEmpresa:SCREEN-VALUE IN FRAME F_Ubicacion EQ "" THEN
            RUN Crea_Faltante(INPUT "Fecha Ingreso a Empresa", INPUT "Ubicacion").
    END.

    IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) = 1 THEN DO:
        IF Clientes.Tipo_Vivienda:SCREEN-VALUE IN FRAME F_Segmentacion EQ "1" 
            AND DECIMAL(Clientes.Act_Casa:SCREEN-VALUE IN FRAME F_Economica) LE 0 
            THEN RUN Crea_Faltante(INPUT "Valor Propiedad", INPUT "Económica").
        END.
    END.
END PROCEDURE.

/* DO WITH FRAME F_Otros: */
/* IF W_NomCiiu:SCREEN-VALUE EQ "" OR                           */
/*       W_NomCiiu:SCREEN-VALUE BEGINS "Sin grupo" THEN         */
/*       RUN Crea_Faltante(INPUT "Codigo Ciiu", INPUT "Otros"). */
/*    IF W_NomIngreso:SCREEN-VALUE EQ "Causal aun no escogida" OR     */
/*       W_NomIngreso:SCREEN-VALUE EQ "" THEN                         */
/*       RUN Crea_Faltante(INPUT "Causal de Ingreso", INPUT "Otros"). */
/* END.            */
/* END PROCEDURE.  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pActvaVntna wWin 
PROCEDURE pActvaVntna :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    wwin:SENSITIVE = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParentHandle wWin 
PROCEDURE pParentHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    pParentHandle   
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER hParent AS HANDLE NO-UNDO.
    GHPARENT = hParent.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
IF W_Inf EQ "Relaciones" THEN RUN Imprimir_Relaciones.
IF W_Inf EQ "Cliente"    THEN RUN Imprimir_Cliente.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProSolNit wWin 
PROCEDURE ProSolNit :
DEF INPUT PARAMETER c AS CHAR NO-unDO.

FIND FIRST Clientes NO-LOCK WHERE Clientes.nit = c NO-ERROR.
IF AVAILABLE(Clientes) THEN
    RUN Mostrar_Cliente.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TipoCliente wWin 
PROCEDURE TipoCliente :
DO WITH FRAME F_Clientes:
    DISABLE Btn_Salvar
            Btn_Cancelar
            Btn_Deshacer
        WITH FRAME F_Clientes.

    CASE INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE):
        WHEN 1 THEN DO:
            IF Clientes.Tipo_Cliente:SCREEN-VALUE EQ "1" THEN DO:
                IF Clientes.Tipo_identificacion:SCREEN-VALUE NE "C.C" THEN DO:
                    IF Clientes.Tipo_Identificacion:SCREEN-VALUE NE "C.E" AND Clientes.Tipo_Identificacion:SCREEN-VALUE NE "PPTE" THEN DO:
                        MESSAGE "Un Cliente mayor de edad debe identificarse con" SKIP
                                "Cédula de Ciudadanía o Cédula de Extranjería." SKIP(1)
                                "El sistema pondrá automáticamente C.C como" SKIP
                                "Configuración tentativa, la cual usted como" SKIP
                                "usuario podrá variar según el documento real"
                            VIEW-AS ALERT-BOX WARNING.

                        Clientes.Tipo_Identificacion:SCREEN-VALUE = "C.C".
                        
                        IF AVAIL Clientes THEN
                            Clientes.Tipo_Identificacion = "C.C".
                    END.
                END.

                ENABLE Btn_Salvar WITH FRAME F_Clientes.
            END.

            ENABLE Clientes.Apellido1
                   Clientes.Apellido2
                WITH FRAME F_Clientes.

            /*campos habilitados*/
            ENABLE Clientes.Est_Civil
                   Clientes.Per_Acargo
                   Clientes.Num_Hijos
                   Btn_Profesion
                   Clientes.Sexo
                   Btn_Residencia
                   Clientes.Tel_Residencia
                   Clientes.Celular
                   Clientes.Tipo_Vivienda
                   Clientes.Estrato
                   Clientes.Nom_Arrendatario
                   Clientes.Tel_Arrendatario
                   Btn_Documento
                   Clientes.Fec_Expedicion
                WITH FRAME F_Segmentacion.

            ENABLE WITH FRAME F_Autorizaciones.

            ENABLE Clientes.Fec_IngEmpresa
                   Btn_Empresa
                   Btn_Cargo
                   Clientes.Tip_Contrato
                WITH FRAME F_Ubicacion.

            ASSIGN Clientes.Salario:LABEL IN FRAME F_Economica = "Salario"
                   Clientes.Gto_Familiar:LABEL IN FRAME F_Economica = "Gastos Familiares".

            ENABLE Clientes.Id_Preexistentes WITH FRAME F_Otros.

            IF AVAIL(Clientes) THEN
                ASSIGN Clientes.Nombre:SCREEN-VALUE = Clientes.Nombre
                       Clientes.Apellido1:SCREEN-VALUE = Clientes.Apellido1
                       Clientes.Apellido2:SCREEN-VALUE = Clientes.Apellido2.
        END.

        OTHERWISE DO:
            ASSIGN Clientes.Apellido1:SCREEN-VALUE = ""
                   Clientes.Apellido2:SCREEN-VALUE = "".
             
            IF DECIMAL(Clientes.Tipo_Cliente:SCREEN-VALUE) = 2 AND Clientes.Tipo_identificacion:SCREEN-VALUE NE "NIT" THEN DO:
                MESSAGE "Un Cliente Jurídico debe identificarse con NIT" SKIP(1)
                        "El sistema pondrá automáticamente NIT como" SKIP
                        "Configuración definitiva"
                    VIEW-AS ALERT-BOX WARNING.

                Clientes.Tipo_Identificacion:SCREEN-VALUE = "NIT".

                IF AVAIL Clientes THEN
                    Clientes.Tipo_Identificacion = "NIT".
            END.

            DISABLE Clientes.Apellido1
                    Clientes.Apellido2
                WITH FRAME F_Clientes.

            /*campos deshabilitados*/
            DISABLE Clientes.Est_Civil
                    Clientes.Per_Acargo
                    Clientes.Num_Hijos
                    Btn_Profesion
                    Clientes.Sexo
                    Btn_Documento
                    Clientes.Tipo_Vivienda
                    Clientes.Estrato
                    Btn_Residencia
                    Clientes.Tel_Residencia
                    Clientes.Celular
                    Clientes.Nom_Arrendatario
                    Clientes.Tel_Arrendatario
                    Clientes.Fec_Expedicion
                WITH FRAME F_Segmentacion.

            Clientes.Niv_Educativo:SCREEN-VALUE IN FRAME F_Segmentacion = "Ninguno".
            R_Niv_Educativo:SCREEN-VALUE IN FRAME F_conyuge = "Ninguno".

            DISABLE Clientes.Fec_IngEmpresa
                    Btn_Empresa
                    Btn_Cargo
                    Clientes.Tip_Contrato
                WITH FRAME F_Ubicacion.

            DISABLE Clientes.Id_Preexistentes
                WITH FRAME F_Otros.

            IF AVAIL(Clientes) THEN
                Clientes.Nombre:SCREEN-VALUE = Clientes.Nombre.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Totales_Economica wWin 
PROCEDURE Totales_Economica :
DO WITH FRAME F_Economica:
        ASSIGN  Tot_Ingresos = 0
                Tot_Ingresos:SCREEN-VALUE = STRING(Tot_Ingresos)
                Tot_Egresos = 0
                Tot_Egresos:SCREEN-VALUE = STRING(Tot_Egresos)
                Tot_Activos = 0
                Tot_Activos:SCREEN-VALUE = STRING(Tot_Activos).
        
        
        
        ASSIGN Tot_Ingresos:SCREEN-VALUE = 
               STRING(DECIMAL(Clientes.Salario:SCREEN-VALUE)
                    + DECIMAL(Clientes.Ing_Arriendos:SCREEN-VALUE)
                    + DECIMAL(Clientes.Ing_Financieros:SCREEN-VALUE)
                    + DECIMAL(Clientes.Ing_Honorarios:SCREEN-VALUE)
                    + DECIMAL(Clientes.Ing_Otros:SCREEN-VALUE)).
        ASSIGN Tot_Egresos:SCREEN-VALUE =
               STRING(DECIMAL(Clientes.Gto_Arriendo:SCREEN-VALUE)
                    + DECIMAL(Clientes.Gto_Familiar:SCREEN-VALUE)
                    + DECIMAL(Clientes.Sdo_Obligacion:SCREEN-VALUE)
                    + DECIMAL(Clientes.Gto_Obligacion:SCREEN-VALUE)
                    + DECIMAL(Anexos_Clientes.Gto_TargetaCredito:SCREEN-VALUE)
                    + DECIMAL(Anexos_Clientes.Gto_Otros:SCREEN-VALUE)).
        
        
        ASSIGN  Tot_Activos:SCREEN-VALUE = STRING(DECIMAL(Clientes.Act_Casa:SCREEN-VALUE) + 
                DECIMAL(Clientes.Act_Vehiculo:SCREEN-VALUE) + 
                DECIMAL(Clientes.Act_Inversion:SCREEN-VALUE)).
        IF NOT decimal(tot_ingresos:SCREEN-VALUE) >= decimal(tot_egresos:SCREEN-VALUE)
        THEN DO:
        
            MESSAGE "ERROR: El Valor De Los Egresos Superan Los Ingresos" tot_ingresos tot_egresos
                VIEW-AS ALERT-BOX ERROR TITLE "ERROR:".
            RETURN "ERROR".
           
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ubicacion wWin 
PROCEDURE Ubicacion :
/*residencia*/
    ASSIGN W_NUbicacion = "".
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = Clientes.Lugar_Residencia NO-LOCK NO-ERROR.
     IF   AVAIL Ubicacion THEN  W_NUbicacion   =  Ubicacion.Nombre +
          IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
     IF   AVAIL Ubicacion THEN  W_NUbicacion   =  W_NUbicacion + " - " + Ubicacion.Nombre.
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "D" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Residencia,1,2) NO-LOCK NO-ERROR.
     IF   AVAIL Ubicacion THEN  W_NUbicacion   =  W_NUbicacion + " - " + Ubicacion.Nombre.
     W_UbicacionResidencia = LC(W_NUbicacion).
     DISP W_UbicacionResidencia WITH FRAME F_segmentacion.
   /*comercial*/
     W_NUbicacion = "".
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = Clientes.Lugar_Comercial NO-LOCK NO-ERROR.
     IF   AVAIL Ubicacion THEN  W_NUbicacion   =  Ubicacion.Nombre +
          IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Comercial,1,5) NO-LOCK NO-ERROR.
     IF   AVAIL Ubicacion THEN  W_NUbicacion   =  W_NUbicacion + " - " + Ubicacion.Nombre.
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "D" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Comercial,1,2) NO-LOCK NO-ERROR.
     IF   AVAIL Ubicacion THEN  W_NUbicacion   =  W_NUbicacion + " - " + Ubicacion.Nombre.
     W_UbicacionComercial:SCREEN-VALUE IN FRAME F_Ubicacion = LC(W_NUbicacion).
   /*nacimiento*/
     W_NUbicacion = "".
     ASSIGN W_NUbicacion = "".
     IF Clientes.Lugar_Nacimiento NE "" THEN DO:
         FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = Clientes.Lugar_Nacimiento NO-LOCK NO-ERROR.
         IF   AVAIL Ubicacion THEN  W_NUbicacion   =  Ubicacion.Nombre +
              IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
         FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Nacimiento,1,5) NO-LOCK NO-ERROR.
         IF   AVAIL Ubicacion THEN  W_NUbicacion   =  W_NUbicacion + " - " + Ubicacion.Nombre.
         FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "D" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Nacimiento,1,2) NO-LOCK NO-ERROR.
         IF   AVAIL Ubicacion THEN  W_NUbicacion   =  W_NUbicacion + " - " + Ubicacion.Nombre.
     END.
     
     /*expedicion*/
     ASSIGN W_NUbicacion = "" W_CiuExpedicion:SCREEN-VALUE = "".
     ASSIGN W_NUbicacion = "".
     IF Clientes.Lugar_Expedicion NE "" THEN DO:
         FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "B" AND Ubicacion.Ubicacion = Clientes.Lugar_Expedicion NO-LOCK NO-ERROR.
         IF   AVAIL Ubicacion THEN  W_NUbicacion   =  Ubicacion.Nombre +
              IF Ubicacion.Comuna < 1 THEN "" ELSE " - Comuna " + STRING(Ubicacion.Comuna).
         FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "C" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Expedicion,1,5) NO-LOCK NO-ERROR.
         IF   AVAIL Ubicacion THEN  W_NUbicacion   =  W_NUbicacion + " - " + Ubicacion.Nombre.
         FIND FIRST Ubicacion WHERE Ubicacion.Tipo = "D" AND Ubicacion.Ubicacion BEGINS SUBSTR(Clientes.Lugar_Expedicion,1,2) NO-LOCK NO-ERROR.
         IF   AVAIL Ubicacion THEN  W_NUbicacion   =  W_NUbicacion + " - " + Ubicacion.Nombre.
     END.
     W_CiuExpedicion:SCREEN-VALUE = LC(W_NUbicacion).
/*zonas*/
/*   FIND Zonas WHERE Zonas.Cod_Zona EQ Clientes.Cod_Zona NO-LOCK NO-ERROR.                                                                */
/*   IF AVAILABLE(Zonas) THEN ASSIGN Cmb_Zonas:SCREEN-VALUE IN FRAME F_Ubicacion = (STRING(Zonas.Cod_Zona,"9999") + " - " + Zonas.Nombre). */
/*   ELSE ASSIGN Cmb_Zonas:SCREEN-VALUE IN FRAME F_Ubicacion = "0000 - Zona no Escogida".                                                  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ubicacion_Conyuge wWin 
PROCEDURE Ubicacion_Conyuge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*comercial*/
     WR_CiuEmpresa = "".
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ BClientes.Lugar_Comercial NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN WR_CiuEmpresa  = Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(BClientes.Lugar_Comercial,1,5) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN WR_CiuEmpresa  = WR_CiuEmpresa + " - " + Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(BClientes.Lugar_Comercial,1,2) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN WR_CiuEmpresa  = WR_CiuEmpresa + " - " + Ubicacion.Nombre.
     
     WR_CiuEmpresa:SCREEN-VALUE IN FRAME F_Conyuge = LC(WR_CiuEmpresa).

     /*nacimiento*/
     ASSIGN WR_CiuEmpresa = ""
            WR_CiuNacimiento:SCREEN-VALUE IN FRAME F_Conyuge = "".
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ BClientes.Lugar_Nacimiento NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN
        ASSIGN WR_CiuEmpresa  = Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(BClientes.Lugar_Nacimiento,1,5) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN 
        ASSIGN WR_CiuEmpresa  = WR_CiuEmpresa + " - " + Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(BClientes.Lugar_Nacimiento,1,2) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN 
        ASSIGN WR_CiuEmpresa  = WR_CiuEmpresa + " - " + Ubicacion.Nombre.
     WR_CiuNacimiento:SCREEN-VALUE IN FRAME F_Conyuge = LC(WR_CiuEmpresa).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validacion_Informacion wWin 
PROCEDURE Validacion_Informacion :
/* nombre: Validacion_Informacion*/
    fVldaDtos(FRAME F_Clientes:FIRST-CHILD).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Tamano wWin 
PROCEDURE Validar_Tamano :
DEF VAR i AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DEF VAR XPos AS INT NO-UNDO FORMAT "99".
DEF VAR XTemp AS DEC NO-UNDO FORMAT "999999999999999".

DO WITH FRAME F_Clientes:
    CASE Clientes.Tipo_Identificacion:SCREEN-VALUE:
        WHEN "C.C" OR WHEN "C.E" OR WHEN "R.C" THEN DO:
            ASSIGN XTemp = DECIMAL(Clientes.Nit:SCREEN-VALUE) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "El documento de identidad no puede contener ningún caracter" SKIP
                        "diferente a los números del 0 al 9. Rectifique..."
                    VIEW-AS ALERT-BOX INFORMATION.

                RETURN ERROR.
            END.
        END.

        WHEN "NIT" THEN DO:
            ASSIGN XTemp = DECIMAL(Clientes.Nit:SCREEN-VALUE) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "El NIT no puede contener ningún caracter diferente" SKIP
                        "a los números del 0 al 9. Rectifique..."
                    VIEW-AS ALERT-BOX INFORMATION.

                RETURN ERROR.
            END.

            IF LENGTH(STRING(DECIMAL(Clientes.Nit:SCREEN-VALUE))) <> 9 THEN DO:
                MESSAGE "Para este tipo de documento sólo se admiten números de identificación" SKIP
                        "de nueve (9) dígitos. Verifique por favor la información..."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN ERROR.
            END.
        END.

        WHEN "T.I" THEN DO:
            ASSIGN XTemp = DECIMAL(Clientes.Nit:SCREEN-VALUE) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "El documento de identidad no puede contener ningún caracter" SKIP
                        "diferente a los números del 0 al 9. Rectifique"
                    VIEW-AS ALERT-BOX INFORMATION.

                RETURN ERROR.
            END.
        END.
    END CASE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_Tam_coy wWin 
PROCEDURE Val_Tam_coy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR XTemp AS DECIMAL FORMAT "999999999999999" NO-UNDO.
  DEFINE VAR XPos  AS INTEGER FORMAT "99" NO-UNDO.

  DO WITH FRAME F_Conyuge:
  CASE R_Tipo_Identificacion:SCREEN-VALUE:                                                  
    WHEN "C.C" OR WHEN "C.E" OR WHEN "R.C" THEN DO:
      IF LENGTH(R_Tipo_Identificacion:SCREEN-VALUE) LT 5 OR 
         LENGTH(R_Tipo_Identificacion:SCREEN-VALUE) GT 11 THEN DO:
         MESSAGE "El documento de identidad debe contener entre 5 y 11 caracteres" SKIP
                 "digite de nuevo el número de documento." VIEW-AS ALERT-BOX INFORMATION.
         RETURN ERROR.
      END.
      ASSIGN XTemp = DECIMAL(R_Tipo_Identificacion:SCREEN-VALUE) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "El documento de identidad no puede contener ningún caracter diferente" SKIP
                 "a los números del 0 al 9. Rectifique" VIEW-AS ALERT-BOX INFORMATION.
         APPLY "entry" TO R_Tipo_Identificacion.
         RETURN ERROR.
      END.
    END.
    WHEN "NIT" THEN DO:
      IF LENGTH(R_Tipo_Identificacion:SCREEN-VALUE) LT 7 OR 
         LENGTH(R_Tipo_Identificacion:SCREEN-VALUE) GT 12 THEN DO:
         MESSAGE "El Numero de documento debe contener entre 7 y 12 caracteres"
                 VIEW-AS ALERT-BOX INFORMATION.
         RETURN ERROR.
      END.
      XPos = (LENGTH(R_Tipo_Identificacion:SCREEN-VALUE)) - 1.
      IF SUBSTRING(R_Tipo_Identificacion:SCREEN-VALUE,XPos,1) NE "-" THEN DO:
         MESSAGE "El documento de identidad debe tener como penultimo digito un guion" SKIP
                 "digite de nuevo el número de documento." VIEW-AS ALERT-BOX INFORMATION.
         RETURN ERROR.
      END.
    END.
    WHEN "T.I" THEN DO:
      IF LENGTH(R_Tipo_Identificacion:SCREEN-VALUE) LT 10 THEN DO:
         MESSAGE "El documento de identidad debe contener entre más de 9 caracteres" SKIP
                 "digite de nuevo el número de documento." VIEW-AS ALERT-BOX INFORMATION.
         RETURN ERROR.
      END.
      ASSIGN XTemp = DECIMAL(R_Tipo_Identificacion:SCREEN-VALUE) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "El documento de identidad no puede contener ningún caracter diferente" SKIP
                 "a los números del 0 al 9. Rectifique" VIEW-AS ALERT-BOX INFORMATION.
         APPLY "entry" TO R_Tipo_Identificacion.
         RETURN ERROR.
      END.
    END.
  END CASE.
END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verifica_RepLegal wWin 
PROCEDURE Verifica_RepLegal :
IF INTEGER(Clientes.Tipo_Cliente:SCREEN-VALUE IN FRAME F_Clientes) = 2 THEN DO:
     FIND FIRST Relaciones WHERE Relaciones.Cod_Relacion EQ 5            AND 
                                 Relaciones.Nit          EQ Clientes.Nit AND
                                 Relaciones.Estado       EQ 1       NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Relaciones) THEN DO:
        MESSAGE "La EMPRESA debe tener un REPRESENTANTE LEGAL" SKIP(1)
                "Utilice la ventana de Relaciones para establecer este vinvulo" SKIP
                "con la persona que cumple con esta condicion!" VIEW-AS ALERT-BOX WARNING.
         ASSIGN gerror = TRUE.   
        FOR EACH T_Relaciones: DELETE T_Relaciones. 
        END.
        OPEN QUERY BR_Relaciones FOR EACH T_Relaciones NO-LOCK INDEXED-REPOSITION.
        HIDE FRAME F_segmentacion FRAME F_Ubicacion FRAME F_Economica FRAME F_Autorizacion 
             FRAME F_Documentacion FRAME F_Otros.
        RSeleccion:SCREEN-VALUE IN FRAME F_Clientes = "4".
        VIEW FRAME F_Relaciones.
            
            Cmb_Relaciones:SCREEN-VALUE IN FRAME F_Relaciones = Cmb_Relaciones:ENTRY(7).  
            APPLY "value-changed" TO  cmb_relaciones IN FRAME f_relaciones.
        /*APPLY 'choose' TO Btn_CreRel IN FRAME F_Relaciones.
        Cmb_Relaciones:SCREEN-VALUE IN FRAME F_Relaciones = "00005 - Representante Legal".   */         
            RETURN  NO-APPLY.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verifica_Tutor wWin 
PROCEDURE Verifica_Tutor :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewObject wWin 
PROCEDURE viewObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
    FRAME F_Aportes:HIDDEN = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fchar wWin 
FUNCTION fchar RETURNS LOGICAL
  (c AS CHAR  /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  fchar
    Notes:  
------------------------------------------------------------------------------*/
    RETURN CAN-DO(cAlfbto,LC(c)).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDgto wWin 
FUNCTION fDgto RETURNS LOGICAL
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: fDgto  
------------------------------------------------------------------------------*/

    RETURN CAN-DO(cdgtos,c).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDptoCiudad wWin 
FUNCTION fDptoCiudad RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR cRtrno AS CHAR NO-UNDO.
    FOR EACH ubicacion NO-LOCK
        WHERE
            Ubicacion.Ubicacion = SUBSTRING(c,1,2) + "000000"
        OR  Ubicacion.Ubicacion = SUBSTRING(c,1,5) + "000"
        OR  Ubicacion.Ubicacion = c
        BY (IF SUBSTRING(Ubicacion.Ubicacion,3,6) = "000000" THEN 1
            ELSE
            IF SUBSTRING(Ubicacion.Ubicacion,6,3) = "000" THEN 2
            ELSE 3)    :
        cRtrno = cRtrno + Ubicacion.Nombre + CHR(10).
    END.
    RETURN cRtrno.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fImprmeDtos wWin 
FUNCTION fImprmeDtos RETURNS LOGICAL
  (h AS HANDLE /* el frame principal */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Ing. Edilberto Mariño Moya
------------------------------------------------------------------------------*/
    DEF VAR c AS CHAR NO-UNDO.
    DEF VAR cTpo AS CHAR NO-UNDO.
    DEF VAR h1 AS HANDLE NO-UNDO.
    DEF VAR cT AS CHAR NO-UNDO.
    DEF VAR cLbel AS CHAR NO-UNDO.
    DEF VAR cHja AS CHAR NO-UNDO.
    DEF VAR cLnea AS CHAR NO-UNDO.
    DEF VAR cCmpos AS CHAR NO-UNDO.
    DEF VAR iClmna AS INTEGER NO-UNDO.
    DEF VAR iFla AS INTEGER NO-UNDO.
    
    cHja = FILL(CHR(10),100).
    h1 = h:PARENT.
    ct = IF h1:PRIVATE-DATA = ? THEN (IF h1:TITLE = ? THEN "" ELSE h1:TITLE) ELSE h1:PRIVATE-DATA.
    h = h:FIRST-CHILD.
    DO WHILE VALID-HANDLE(h):
        CASE h:TYPE:
            WHEN "FRAME"
            THEN DO.
                corden = SUBSTRING(h:PRIVATE-DATA,1,2).
                CREATE t.
                ASSIGN  t.tTtlo     = (IF h:PRIVATE-DATA = ? THEN h:TITLE ELSE h:PRIVATE-DATA)
                        t.tfla      = 0
                        t.tclmna    = 0
                        t.th        = h
                        t.tlbel     = t.tTtlo
                        t.tcmpo     = ""
                        t.tfrme     = h
                        t.torden    = cOrden.
                fImprmeDtos(h:FIRST-CHILD).
                ASSIGN h = h:NEXT-SIBLING.
            END.
            OTHERWISE DO:
                IF NOT can-do("image,RECTANGLE,Button,literal,browse,control-frame",h:TYPE)
                THEN DO:
                    IF CAN-DO("*",h:NAME)
                    THEN DO:
                        /* c = (IF h:TYPE = "literal" THEN h:TEXT ELSE trim(h:SCREEN-VALUE)) NO-ERROR.*/
                        c = trim(h:SCREEN-VALUE) NO-ERROR.
                        IF h:LABELS 
                        THEN cLbel = (IF NOT h:LABEL = ? THEN h:LABEL ELSE h:NAME) + " : ".
                        ELSE cLbel = "".
                        iClmna = round(h:FRAME-COL + 0.9,0).
                        iFla = round(h:FRAME-ROW + 0.9,0).
                        CREATE t.
                        ASSIGN  t.tTtlo         = cLbel
                                t.tfla          = iFla
                                t.tclmna        = iClmna
                                t.th            = h
                                t.tlbel         = t.tTtlo
                                t.tCmpo         = h:NAME
                                t.tfrmeHndle    = h:FRAME
                                t.tvlor         = IF NOT c = ? THEN c ELSE ""
                                t.tOrden        = cOrden.
                    END.
                END.
                ASSIGN h = h:NEXT-SIBLING.
            END.
        END CASE.
        
    END.
    RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fIncialza wWin 
FUNCTION fIncialza RETURNS LOGICAL
  (h AS HANDLE /* el frame principal */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Ing. Edilberto Mariño Moya
------------------------------------------------------------------------------*/
    DEF VAR c AS CHAR NO-UNDO.
    DEF VAR cTpo AS CHAR NO-UNDO.
    DEF VAR h1 AS HANDLE NO-UNDO.
    DEF VAR cT AS CHAR NO-UNDO.
    h1 = h:PARENT.
    ct = IF h1:PRIVATE-DATA = ? THEN (IF h1:TITLE = ? THEN "" ELSE h1:TITLE) ELSE h1:PRIVATE-DATA.
    h = h:FIRST-CHILD.
    DO WHILE VALID-HANDLE(h):
        CASE h:TYPE:
            WHEN "FRAME"
            THEN DO.
                fIncialza(h:FIRST-CHILD).
                ASSIGN h = h:NEXT-SIBLING.
            END.
            OTHERWISE DO:
                c = ?.
                c = h:DATA-TYPE NO-ERROR.
                IF NOT can-do("COMBO-BOX",h:TYPE)
                THEN DO:
                    IF NOT c = ?
                    THEN h:SCREEN-VALUE = "" NO-ERROR.
                END.
                ELSE DO:
                    IF can-do("COMBO-BOX",h:TYPE) AND CAN-DO("r_tipo_identificacion,r_niv_educativo,r_ocupacion",h:NAME) /* lista de combos que se deben inicializar */
                    THEN DO:
                        h:SCREEN-VALUE = ?.
                    END.
                END.
                ASSIGN h = h:NEXT-SIBLING.
            END.
        END CASE.
        
    END.
    RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fPrfsion wWin 
FUNCTION fPrfsion RETURNS CHARACTER
  (iv AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR cRtrno AS CHAR NO-UNDO.
    cRtrno = "Profesión No Existe".
    FOR FIRST varios NO-LOCK
        WHERE
            Varios.Tipo = 1
        AND varios.Codigo = iv:
        cRtrno = varios.Descripcion.
    END.
    RETURN crtrno.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fTxtoCmpo wWin 
FUNCTION fTxtoCmpo RETURNS CHARACTER
  (h AS HANDLE /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  fTxtoCmpo
    Notes:  
------------------------------------------------------------------------------*/
    RETURN
    "'" +
    (IF NOT h:LABEL = ?
    THEN h:LABEL
    ELSE
    IF NOT h:HELP = ?
    THEN h:HELP
    ELSE h:NAME) +
    "'".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fVldaDtos wWin 
FUNCTION fVldaDtos RETURNS LOGICAL
  (h AS HANDLE /* el frame principal */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Ing. Edilberto Mariño Moya
------------------------------------------------------------------------------*/
    DEF VAR c AS CHAR NO-UNDO.
    DEF VAR cTpo AS CHAR NO-UNDO.
    DEF VAR h1 AS HANDLE NO-UNDO.
    DEF VAR cT AS CHAR NO-UNDO.
    h1 = h:PARENT.
    ct = IF h1:PRIVATE-DATA = ? THEN (IF h1:TITLE = ? THEN "" ELSE h1:TITLE) ELSE h1:PRIVATE-DATA.
    h = h:FIRST-CHILD.
    DO WHILE VALID-HANDLE(h):
        CASE h:TYPE:
            WHEN "FRAME"
            THEN DO.
                fVldaDtos(h:FIRST-CHILD).
                ASSIGN h = h:NEXT-SIBLING.
            END.
            OTHERWISE DO:
                IF INDEX(h:PRIVATE-DATA,"obligatorio") <> 0
                THEN DO:
                    c = trim(h:SCREEN-VALUE) NO-ERROR.
                    cTpo = h:DATA-TYPE NO-ERROR.
                    CASE cTpo:
                        WHEN "character" OR WHEN "logical"
                        THEN DO:
                            IF c = "" OR c = ? THEN RUN Crea_Faltante(IF h:LABEL = ? THEN h:NAME ELSE h:LABEL, ct).
                        END.
                        WHEN "INTEGER"
                        THEN DO:
                            IF INTEGER(c) = 0 OR c = ? THEN RUN Crea_Faltante(IF h:LABEL = ? THEN h:NAME ELSE h:LABEL, ct).
                        END.
                        WHEN "decimal"
                        THEN DO:
                            IF DECIMAL(c) = 0 OR c = ? THEN RUN Crea_Faltante(IF h:LABEL = ? THEN h:NAME ELSE h:LABEL, ct).
                        END.
                        WHEN "date"
                        THEN DO:
                            IF DATE(c) = ? OR c = ? THEN RUN Crea_Faltante(IF h:LABEL = ? THEN h:NAME ELSE h:LABEL, ct).
                        END.
                    END CASE.
                END.
                ASSIGN h = h:NEXT-SIBLING.
            END.
        END CASE.
        
    END.
    RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION vinculantes wWin 
FUNCTION vinculantes RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

lg = FALSE.
FOR FIRST vinculantes WHERE vinculantes.texto CONTAINS Clientes.nit :
     message "Vinculantes.. " vinculantes.nro.
     ASSIGN lg = TRUE.
END.
FOR FIRST vinculantes WHERE vinculantes.texto CONTAINS Clientes.nombre :
     message "Vinculantes.. " vinculantes.nro.
     ASSIGN lg = TRUE.
END.
RETURN lg.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

