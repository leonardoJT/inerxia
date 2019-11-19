   DEFINE VAR W_NomUsu                LIKE Usuarios.Nombre FORMAT "X(30)" INITIAL "".
   DEFINE VAR W_NomUsuEnc             LIKE Usuarios.Nombre FORMAT "X(30)" INITIAL "".
   DEFINE VAR W_NomEstEnc             LIKE Estacion.Descripcion FORMAT "X(30)" INITIAL "".
   DEFINE VAR W_CodUsu                LIKE Usuarios.Usuario.
   DEFINE VAR W_FechaHora AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_Reporte   AS CHARACTER FORMAT "X(120)".
   DEFINE VAR W_IdReporta AS CHARACTER FORMAT "X(120)".
   DEFINE VAR W_IdEstacion AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_Ubicacion AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_PiePagina AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_UsuEncabe AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_Nom_Entidad AS CHARACTER FORMAT "X(50)" INITIAL "FONDO EMPLEADOS DOCENTES UNIVERSIDAD NACIONAL".


  DEFINE VAR W_Linea     AS CHARACTER FORMAT "X(130)" INITIAL "".
  DEFINE VAR W_Linea2    AS CHARACTER FORMAT "X(130)" INITIAL "".

  DEFINE VAR W_Raya      AS CHARACTER INITIAL "_".
  DEFINE VAR W_Raya2     AS CHARACTER INITIAL "-".
  DEFINE VAR W_UsuTra    AS CHAR FORMAT "X(4)" INITIAL "".
  DEFINE VAR HoraEnvio   AS CHARACTER FORMAT "X(8)".
  DEFINE VAR HoraLee     AS CHARACTER FORMAT "X(8)".
  DEFINE VAR FechaLee    AS CHARACTER FORMAT "X(10)".
  DEFINE VAR W_EncColumna  AS CHARACTER FORMAT "X(200)".
  DEFINE FRAME F-Encabezado
      HEADER
        W_Nom_Entidad AT 2
        "PAGINA:"     AT 90 PAGE-NUMBER FORMAT ">>>9"
        W_Ubicacion   AT 2
        W_IdEstacion  AT 2
        W_IdReporta   AT 2
        W_Reporte     AT 2
/*         W_Linea       AT 1 FORMAT "X(120)" */
/*         W_EncColumna                       */
        W_Linea2      AT  1  FORMAT "X(120)"
    WITH DOWN WIDTH 220 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.

    DEFINE FRAME f-ftr
      HEADER 
        "________________________________________" AT 2
        W_PiePagina AT 2
      WITH DOWN WIDTH 132 FRAME f-ftr PAGE-BOTTOM USE-TEXT STREAM-IO. 

    FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
    IF AVAILABLE(Usuarios) THEN W_NomUsuEnc = Usuarios.Nombre.
    ELSE W_NomUsuEnc = "Usuario No Encontrado".
    FIND Estacion WHERE Estacion.Estacion EQ W_Estacion NO-LOCK NO-ERROR.
    IF AVAILABLE(Estacion) THEN W_NomEstEnc = Estacion.Descripcion.
    ELSE W_NomEstEnc = "Estacion No Encontrada".

    ASSIGN W_Linea     = FILL(W_Raya,130)
           W_Linea2    = FILL(W_Raya2,130)
           W_Ubicacion = "UBICACION : " + TRIM(W_Nom_Agencia)
           W_IdReporta = "USUARIO   : " + W_Usuario + " - " + TRIM(W_NomUsuEnc)
           W_IdEstacion = "ESTACION  : " + STRING(INTEGER(W_Estacion)) + " - " + W_NomEstEnc
           W_PiePagina = TRIM(W_Nom_Agencia) + " / " + STRING(TODAY) + " / " + STRING(TIME,"hh:mm am").

