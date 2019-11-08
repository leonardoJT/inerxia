DEFINE TEMP-TABLE Cruce_cartera
    FIELD tag  LIKE creditos.agencia
    FIELD tnit LIKE creditos.nit
    FIELD tnc  LIKE creditos.num_credito
    FIELD tct  LIKE creditos.categoria
    FIELD tctm LIKE creditos.categoria
    FIELD tsal LIKE creditos.sdo_capital.


INPUT FROM c:\tmpcartera.csv.
REPEAT:
    CREATE cruce_cartera.
    IMPORT DELIMITER ";" cruce_cartera.
END.

FOR EACH cruce_cartera:
  FIND FIRST  creditos WHERE creditos.nit = cruce_cartera.tnit AND
                             creditos.num_credito = cruce_cartera.tnc NO-ERROR.
  IF AVAILABLE(creditos) THEN DO:
      DISPL creditos.categoria creditos.categoriames cruce_cartera.tct cruce_cartera.tctm.
  END.
END.
