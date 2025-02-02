DEFINE {1} VAR W_Estacion AS CHARACTER.
DEFINE {1} VAR W_Usuario AS CHARACTER.
DEFINE {1} VAR W_Clave AS CHARACTER FORMAT "X(16)".
DEFINE {1} VAR W_Prioridad AS INTEGER.
DEFINE {1} VAR W_Agencia AS INTEGER.
DEFINE {1} VAR W_Ciudad AS CHARACTER.
DEFINE {1} VAR W_Nom_Agencia AS CHARACTER FORMAT "X(60)".
DEFINE {1} VAR W_UbiDatos AS CHARACTER INITIAL "D".
DEFINE {1} VAR W_Nom_Entidad AS CHARACTER FORMAT "X(60)".
DEFINE {1} VAR W_Entidad AS INTEGER.
DEFINE {1} VAR W_NitGlobal AS CHARACTER.
DEFINE {1} VAR W_SMLV AS DECIMAL.

DEFINE {1} VAR W_Manija AS HANDLE.
DEFINE {1} VAR W_ManFin AS HANDLE.
DEFINE {1} VAR W_ManTaq AS HANDLE.

DEFINE {1} VAR W_Nivel AS INTEGER.
DEFINE {1} VAR W_CtaMay AS CHARACTER.
DEFINE {1} VAR W_Fecha AS DATE FORMAT "99/99/9999" INITIAL TODAY.
DEFINE {1} VAR W_ficina AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 4 SIZE 40 BY 1.
DEFINE {1} VAR W_Path AS CHARACTER.
DEFINE {1} VAR W_Pathspl AS CHARACTER.
DEFINE {1} VAR W_Eleccion AS LOGICAL.       
DEFINE {1} VAR W_CedGral AS CHARACTER.
DEFINE {1} VAR W_CenCosGral AS INTEGER.
DEFINE {1} VAR W_Cadena AS CHARACTER FORMAT "X(9)".
DEFINE {1} VAR P-Valida AS LOGICAL.
DEFINE {1} VAR W_VCodPcto AS INTEGER.
DEFINE {1} VAR W_VCueAhorro AS CHARACTER.
DEFINE {1} VAR W_Solicitud AS INTEGER.
DEFINE {1} VAR W_PagareS AS CHARACTER.
DEFINE {1} VAR P_SdoTot AS DECIMAL.
DEFINE {1} VAR W_OfiCierre AS INTEGER.
