{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Reactive where

import Data.Monoid
import Control.Applicative

-- | @e@ is a type constructor for an event in a reactive system. Events can
-- be mapped and combined.
class Functor e => Event e where

    -- | An event that never occurs.
    never :: e a
    default never :: Monoid (e a) => e a
    never = mempty

    -- | An event that occurs when either of the given events occur.
    union :: e a -> e a -> e a
    default union :: Monoid (e a) => e a -> e a -> e a
    union = mappend

    -- | Filters the occurences of an event.
    filterJust :: e (Maybe a) -> e a

-- | A reactive system consisting of events of type @e@ and behaviors of type
-- @f@. All events created using event function may lag behind the original
-- events by an unspecified amount. This prevents issues associated with
-- simultaneous event occurence at the cost of undefined semantics for some
-- programs.
class (Event e, Applicative f) => Reactive e f where

    -- | Tags an event with the value of a behavior at the time it occurs.
    infixl 4 <@>
    (<@>) :: f (a -> b) -> e a -> e b

-- | Tags an event with the value of a behavior at the time it occurs.
infixl 4 <@
(<@) :: (Reactive e f) => f a -> e b -> e a
(<@) x y = const <$> x <@> y

-- | A reactive system where behaviors (@f@) can depend on events.
-- This allows the system to hold time-varying state.
class Reactive e f => ReactiveState e f | e -> f where

    -- | Constructs a behavior with the given initial value that changes in
    -- response to an event.
    accumB :: a -> e (a -> a) -> f a

-- | Constructs a behavior with the given initial value that changes in
-- response to an event.
stepper :: (ReactiveState e f) => a -> e a -> f a
stepper init = accumB init . (const <$>)

-- | A reactive system where behaviors (@f@) change only at discrete moments.
class ReactiveState e f => ReactiveDiscrete e f | f -> e where

    -- | Constructs an event which occurs when the given behavior changes
    -- some time after the change.
    changes :: f a -> e a

-- | A reactive system where signals of type @g@ can be dynamically switched
-- using behaviors.
class (Reactive e f, Functor g) => ReactiveSwitch e f g | f -> e where

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
