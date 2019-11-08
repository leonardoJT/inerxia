DEFINE {1} VAR W_Estacion    LIKE Estaciones.Estacion.
DEFINE {1} VAR W_Usuario     LIKE Usuarios.Usuario.
DEFINE {1} VAR W_Clave       LIKE Usuarios.Clave FORMAT "X(16)".
DEFINE {1} VAR W_Prioridad   LIKE Usuarios.Prioridad INITIAL "".
DEFINE {1} VAR W_Agencia     LIKE Usuarios.Agencia INITIAL 0.
DEFINE {1} VAR W_Ciudad      LIKE Agencia.Ciudad INITIAL 0.
DEFINE {1} VAR W_Nom_Agencia   AS CHARACTER FORMAT "X(60)".
DEFINE {1} VAR W_UbiDatos      AS CHAR INITIAL "D".
/*DEFINE {1} VAR W_Ninv        LIKE Inversion.Nro_inversion.*/
DEFINE {1} VAR W_Nom_Entidad   AS CHARACTER FORMAT "X(60)".
DEFINE {1} VAR W_Entidad     LIKE Entidad.Entidad.
DEFINE {1} VAR W_NitGlobal   LIKE Clientes.Nit INITIAL "".
DEFINE {1} VAR W_SMLV        LIKE Indicadores.Valor INITIAL 0.


DEFINE {1} VAR W_Manija        AS HANDLE.
DEFINE {1} VAR W_ManFin        AS HANDLE.
DEFINE {1} VAR W_ManTaq        AS HANDLE.
DEFINE {1} VAR W_Nivel       LIKE Cuentas.Nivel.
DEFINE {1} VAR W_CtaMay      LIKE Cuentas.Cuenta.
DEFINE {1} VAR W_Fecha         AS DATE FORMAT "99/99/9999" INITIAL TODAY.
DEFINE {1} VAR W_ficina        AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 4 SIZE 40 BY 1.
DEFINE {1} VAR W_Path        LIKE Entidad.Dir_Programas.
DEFINE {1} VAR W_Pathspl     LIKE Entidad.Dir_Spl.
DEFINE {1} VAR W_Eleccion      AS LOGICAL.       
DEFINE {1} VAR W_CedGral     LIKE Clientes.Nit.
DEFINE {1} VAR W_CenCosGral  LIKE Cen_Costos.Cen_Costos.
DEFINE {1} VAR W_Cadena        AS CHARACTER FORMAT "X(9)" INITIAL "SIFINCOOP".
/*DEFINE     VAR Agencia_Cnt     AS INTEGER FORMAT "999".*/
DEFINE {1} VAR P-Valida        AS LOGICAL.
DEFINE {1} VAR W_VCodPcto    LIKE Ahorros.Cod_Ahorro.
DEFINE {1} VAR W_VCueAhorro  LIKE Ahorros.Cue_Ahorros.

DEFINE {1} VAR W_Solicitud   LIKE Solicitud.Num_Solicitud.
DEFINE {1} VAR W_PagareS     LIKE Creditos.Pagare.

DEFINE {1} VAR P_SdoTot      LIKE Creditos.Sdo_Capital.
DEFINE {1} VAR W_OfiCierre   LIKE Agencias.Agencia.
