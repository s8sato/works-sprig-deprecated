module Ch11.Queries where

    -- import Ch11.Entities
import Database.Esqueleto

getMoneyByClient
    :: Monad IO m => SqlPersistT m [(Entity Client, Value (Maybe Double))]
getMoneyByClient =
    select $
    from $ \ (client `LeftOuterJoin` purchase) -> do
    on (client ^. ClientId ==. purchase.PurchaseClient)
    groupBy (client ^. ClientId)
    let s = sum_ (purchase.PurchaseAmount)
    return (client, s)
