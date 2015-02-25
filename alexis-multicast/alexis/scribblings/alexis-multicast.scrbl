#lang scribble/manual

@(require (for-label racket/base
                     racket/contract
                     racket/async-channel
                     alexis/multicast))

@title{Multicast Asynchronous Channels}

@defmodule[alexis/multicast]

This provides an implementation of @deftech{multicast channels}, an extension of
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{asynchronous channels} that may have
multiple recievers. Unlike asynchronous channels, multicast channels have distinct “server” and
“receiver” objects, since multicast channels create a one-to-many relationship.

@defproc[(make-multicast-channel) multicast-channel?]{
Creates and returns a new @tech{multicast channel} server.}

@defproc[(multicast-channel? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a @tech{multicast channel}, @racket[#f] otherwise.}

@defproc[(make-multicast-receiver [mc multicast-channel?]) async-channel?]{
Creates a receiver asynchronous channel that receives messages from @racket[mc]. The returned value is
a standard @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{asynchronous channel} from
@racketmodname[racket/async-channel], but it does @italic{not} support @racket[async-channel-put].
Attempting to write to a receiver channel will raise an exception.}

@defproc[(multicast-channel-put [mc multicast-channel?] [v any/c]) void?]{
Broadcasts @racket[v] to all receivers listening to @racket[mc].}
