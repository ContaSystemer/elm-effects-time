module Effects.Time exposing
    ( Effects, every
    , map, subscription
    )

{-| Time system is used to subscribe for time events.
It uses effects for communication with other systems.


# Create

@docs Effects, every


# Transform

@docs map, subscription

-}

import Effects
import Time


{-| Time system effects which are used for communication with other systems.
-}
type alias Effects msg =
    Effects.Effects (Effect msg)


{-| Maps time system effects.
-}
map : (msgA -> msgB) -> Effects msgA -> Effects msgB
map tagger effects =
    Effects.map (mapEffect tagger) effects


{-| Time system effect. Describes what other systems can ask for from time system.
-}
type Effect msg
    = Every Float (Time.Posix -> msg)


{-| Maps time system effect.
-}
mapEffect : (msgA -> msgB) -> Effect msgA -> Effect msgB
mapEffect tagger (Every time toMsg) =
    Every time (\posix -> tagger (toMsg posix))


{-| Adds time system effect subscription to the list of time system effect subscriptions.
-}
effectSubscription : Effect msg -> Sub msg -> Sub msg
effectSubscription (Every time toMsg) sub =
    Sub.batch
        [ Time.every time toMsg
        , sub
        ]


{-| Creates time system effect to subscribe for time event.
-}
every : Float -> (Time.Posix -> msg) -> Effects msg
every time toMsg =
    Effects.from (Every time toMsg)


{-| Time system subscription for all effects.
-}
subscription : Effects msg -> Sub msg
subscription =
    Effects.apply effectSubscription Sub.none
