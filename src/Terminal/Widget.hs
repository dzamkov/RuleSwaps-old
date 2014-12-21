module Terminal.Widget (

) where

import Terminal.Figure

data Context i o m = {

    watch :: forall a. i a -> (a -> m ()) -> m (m ())

    signal :: forall a. o a -> a -> m ()

data Widget i o 
