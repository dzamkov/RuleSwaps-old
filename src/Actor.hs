module Actor (
    Channel,
    Source,
    noSource,
    source,
    (<>),
    mapSource,
    ActorT,
    Actor,
    MonadActor (..),
    stop,
    liftActor,
    stepper,
    module Actor.IO
) where

import Data.Monoid
import Actor.Internal
import Actor.IO
