{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
module Reactive where

import Data.Monoid
import Control.Monad.Fix
import Control.Applicative

-- | @e@ is a type constructor for an event in a reactive system. An event
-- can be thought of as a stream of (a countable number) of occurences.
-- There is a systemwide partial ordering of event occurences, which we use
-- to define /before/ and /after/. The 'Functor' instance of @e@ maps the
-- values of occurences of an event while preserving the order of occurences
-- with respect to all others.
class Functor e => Event e where

    -- | An event that never occurs.
    never :: e a
    default never :: Monoid (e a) => e a
    never = mempty

    -- | An event that occurs when either of the given events occur,
    -- preserving the ordering of occurences.
    union :: e a -> e a -> e a
    default union :: Monoid (e a) => e a -> e a -> e a
    union = mappend

    -- | Filters the occurences of an event, preserving the order of
    -- occurences.
    filterJust :: e (Maybe a) -> e a

-- | @e@ is a type constructor for an event that implements 'subdivide'. This
-- requires that there be a dense ordering of times at which events can occur
-- (which is not the case if there are discrete time steps).
class Event e => EventSubdivide e where

    -- | Constructs two events that both occur when the given event occurs,
    -- preserving the ordering of occurences with respect to all
    -- existing events. Additionally, all occurences of the second event
    -- will occur after the corresponding occurence of the first event.
    subdivide :: e a -> (e a, e a)

-- | A reactive system consisting of events of type @e@ and behaviors of type
-- @f@.
class (Event e, Applicative f) => Reactive e f | f -> e where

    -- | Tags an event with the value of a behavior at the time it occurs.
    infixl 4 <@>
    (<@>) :: f (a -> b) -> e a -> e b

-- | Tags an event with the value of a behavior at the time it occurs.
infixl 4 <@
(<@) :: (Reactive e f) => f a -> e b -> e a
(<@) x y = const <$> x <@> y

-- TODO: remove once AMP goes through
type MonadFix' m = (Applicative m, MonadFix m)

-- | A reactive system where behaviors (@f@) can depend on events.
-- This allows the system to hold time-varying state. The @m@ monad carries
-- a time-dependent value.
class (MonadFix' m, Reactive e f) => ReactiveState m e f | e -> m where

    -- | Constructs a behavior with the given initial value that changes in
    -- response to an event, starting at a particular moment.
    accumB :: a -> e (a -> a) -> m (f a)

    -- | Obtains the value of a behavior at a particular moment.
    sample :: f a -> m a

    -- | Instantiates time-dependent values with the time they occur, producing
    -- an analogous stream of results.
    execute :: e (m a) -> e a

-- | Constructs a behavior with the given initial value that changes in
-- response to an event.
stepper :: (ReactiveState m e f) => a -> e a -> m (f a)
stepper init = accumB init . (const <$>)

-- | A reactive system where all behaviors (@f@) change at discrete moments
-- (thus having a countable number of changes).
class Reactive e f => ReactiveDiscrete e f where

    -- | Constructs an event which occurs when the given behavior changes
    -- (and sometimes when it doesn't). Each occurence is guranteed to
    -- occur after the corresponding change, but before the next change.
    -- Occurences are tagged with the old and new values for the behavior.
    changes :: f a -> e (a, a)

-- | A reactive system where signals of type @g@ can be dynamically switched
-- using behaviors.
class (Reactive e f, Functor g) => ReactiveSwitch e f g where

    -- | Constructs a signal which defers to the signal specified by the given
    -- behavior.
    switch :: f (g a) -> g a

-- | Constructs an event that occurs when the event specified by the
-- given behavior occurs.
switchE :: (ReactiveSwitch e f e) => f (e a) -> e a
switchE = switch

-- | Constructs a behavior that takes the value of the behavior specified by
-- the given behavior.
switchB :: (ReactiveSwitch e f f) => f (f a) -> f a
switchB = switch
