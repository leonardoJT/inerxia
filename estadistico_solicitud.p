DEFINE VAR TipIns LIKE Cfg_Instancias.Tipo_Instancia.
DEFINE VAR Encabezado AS CHARACTER FORMAT "X(132)".
DEFINE VAR Linea AS CHARACTER FORMAT "X(232)".

DEFINE VAR W_NomUsu AS CHARACTER FORMAT "X(30)".
DEFINE VAR W_NomIns AS CHARACTER FORMAT "X(30)".
DEFINE VAR Xnom     AS CHARACTER FORMAT "X(30)".
DEFINE VARIABLE xtiempo AS INTEGER INITIAL 0.
DEFINE VARIABLE w_fecha AS DATE INITIAL TODAY.

DEFINE VAR i AS INTEGER.
TipIns = 1.
OUTPUT TO C:\info_juriscoop\SOLICITUD_ESTADISTICA.TXT.
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
MESSAGE "Informe Generado en : C:\info_juriscoop\SOLICITUD_ESTADISTICA.TXT." VIEW-AS ALERT-BOX.
