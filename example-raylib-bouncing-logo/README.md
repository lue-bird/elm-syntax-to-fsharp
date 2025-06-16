One nice thing this example makes use of:
Declaring [`Codec`](https://dark.elm.dmy.fr/packages/miniBill/elm-codec/latest/)s for what's passed over the wire.
This way, both sides will warn you about most inconsistencies between them.
It's also more convenient because you only need to declare all the types in one place.

Note that this example only declares a codec for the port from elm but if messages to elm become more complex, you can make use of codecs there, too.
