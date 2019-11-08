DEFINE SHARED BUFFER Clientes FOR bdcentral.Clientes.
DEFINE SHARED QUERY qbf-query-1
  FOR bdcentral.Clientes
  SCROLLING.

OPEN QUERY qbf-query-1
  FOR EACH bdcentral.Clientes NO-LOCK
    WHERE (bdcentral.Clientes.agencia = 1) INDEXED-REPOSITION.
