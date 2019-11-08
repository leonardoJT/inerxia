 DEFINE VARIABLE j AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>>".
 DEFINE TEMP-TABLE devolver
  FIELD tced LIKE ahorros.nit
  FIELD tval LIKE ahorros.sdo_disponible.

 INPUT FROM c:\devolver.csv.
 REPEAT:
     CREATE devolver.
     IMPORT DELIMITER ";" devolver.
 END.

 FOR EACH devolver:
   
      FIND FIRST Ahorros WHERE
           Ahorros.Cod_Ahorro EQ 3 AND
           Ahorros.Nit        EQ devolver.tced NO-ERROR.
      IF AVAILABLE(Ahorros) THEN DO:
          ASSIGN j = j + tval.
          
         ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - devolver.tval
                Ahorros.Fec_UltTransaccion = today.
         
         CREATE Mov_Ahorros.
         ASSIGN Mov_Ahorros.Agencia         = Ahorros.agencia 
                Mov_Ahorros.Age_Destino     = Ahorros.agencia 
                Mov_Ahorros.Age_Fuente      = Ahorros.agencia 
                Mov_Ahorros.Cod_Ahorro      = Ahorros.Cod_Ahorro
                Mov_Ahorros.Cod_Operacion   = 010102001
                Mov_Ahorros.Cpte            = 4
                Mov_Ahorros.Cue_Ahorros     = ahorros.cue_ahorros
                Mov_Ahorros.Descrip         = "Corr. Dble Importacion de Productos nota 5229"
                Mov_Ahorros.Fecha           = TODAY
                Mov_Ahorros.Hora            = TIME
                Mov_Ahorros.Nit             = Ahorros.nit
                Mov_Ahorros.Num_Documento   = STRING(5229)
                Mov_Ahorros.Sdo_Disponible  = Ahorros.Sdo_disponible
                Mov_Ahorros.Usuario         = "14"
                Mov_Ahorros.Val_Efectivo    = devolver.tval
                mov_ahorros.Cedula_Trans    = Ahorros.nit. 
          
      END.
 END.
 DISP j.
