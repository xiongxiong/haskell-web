module Adapter.RabbitMQ.Common where

import ClassyPrelude
import Network.AMQP

data State = State
    {
          statePublisherChan :: Channel
        , stateConsumerChan :: Channel
    }

withState :: String -> Integer -> (State -> IO a) -> IO a
withState connUri prefetchCount action = 
    bracket initState destroyState action'
    where
        initState = do
            publisher <- openConnAndChan
            consumer <-  openConnAndChan
            return (publisher, consumer)
        openConnAndChan = do
            conn <- openConnection'' . fromURI $ connUri
            chan <- openChannel conn
            confirmSelect chan False
            qos chan 0 (fromInteger prefetchCount) True
            return (conn, chan)
        destroyState ((conn1, _), (conn2, _)) = do
            closeConnection conn1
            closeConnection conn2
        action' ((_, pubChan), (_, conChan)) = action (State pubChan conChan)
 
initExchange :: State -> Text -> IO ()
initExchange (State pubChan _) exchangeName = do
    let exchange = newExchange {exchangeName = exchangeName, exchangeType = "topic"}
    declareExchange pubChan exchange

initQueue :: State -> Text -> Text -> Text -> IO ()
initQueue state@(State pubChan _) queueName exchangeName routingKey = do
    initExchange state exchangeName
    void $ declareQueue pubChan (newQueue {queueName = queueName})
    bindQueue pubChan queueName exchangeName routingKey

initConsumer :: State -> Text -> (Message -> IO Bool) -> IO ()
initConsumer (State _ conChan)