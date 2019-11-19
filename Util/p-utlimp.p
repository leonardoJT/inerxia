INPUT CLOSE.

DEFINE VARIABLE CueAho_Ant AS INTEGER FORMAT "9999999999".
INPUT FROM Cupos.d.
DO TRANSACTION:
   FOR EACH Cupos:
       DELETE Cupos.
   END.    
   REPEAT:
      CREATE Cupos.
      IMPORT Cupos.Clave Cupos.Corte CueAho_Ant Cupos.Cupo Cupos.Envio Cupos.Estado Cupos.Fec_Actualizacion Cupos.Fec_Creacion Cupos.Nit Cupos.Num_Tarjeta Cupos.Oficina Cupos.Plazo Cupos.Tip_Cupo Cupos.Usu_Creo Cupos.Usu_Vendio.
      ASSIGN Cupos.Cue_Ahorros = STRING(CueAho_Ant).
   END.
END.
INPUT CLOSE.

DEFINE VARIABLE TipPro_Ant AS CHARACTER FORMAT "XXX".
INPUT FROM Libreta_Che.d.
DO TRANSACTION:
   FOR EACH Libreta_Chequera:
       DELETE Libreta_Chequera.
   END.    
   REPEAT:
      CREATE Libreta_Chequera.
      IMPORT Libreta_Chequera.Cod_Producto Libreta_Chequera.Cue_Ahorros Libreta_Chequera.Estado Libreta_Chequera.Fec_Entrega Libreta_Chequera.Num_Final Libreta_Chequera.Num_Inicial Libreta_Chequera.Oficina Libreta_Chequera.Pagada Libreta_Chequera.Reg_Chebloqueados[1] Libreta_Chequera.Reg_Chebloqueados[2] Libreta_Chequera.Reg_Chebloqueados[3] Libreta_Chequera.Reg_Chebloqueados[4] Libreta_Chequera.Reg_Chebloqueados[5] TipPro_Ant Libreta_Chequera.Tip_Talonario.
      ASSIGN Libreta_Chequera.Tip_Producto = INTEGER(TipPro_Ant).
   END.
END.

INPUT CLOSE.
