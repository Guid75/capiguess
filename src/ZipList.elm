module ZipList
    exposing
        ( ZipList
        , init
        , current
        , next
        , previous
        , toList
        , length
        )


type ZipList a
    = Empty
    | NonEmpty (Something a)


type alias Something a =
    { before : List a
    , current : a
    , after : List a
    }


init : List a -> ZipList a
init list =
    case list of
        [] ->
            Empty

        head :: queue ->
            NonEmpty
                { before = []
                , current = head
                , after = queue
                }


current : ZipList a -> Maybe a
current zipList =
    case zipList of
        Empty ->
            Nothing

        NonEmpty something ->
            Just something.current


performIfNonEmpty : (Something a -> Something a) -> ZipList a -> ZipList a
performIfNonEmpty f zipList =
    case zipList of
        Empty ->
            zipList

        NonEmpty something ->
            NonEmpty <| f something


next : ZipList a -> ZipList a
next zipList =
    performIfNonEmpty
        (\something ->
            case something.after of
                [] ->
                    something

                head :: queue ->
                    { before = something.current :: something.before
                    , current = head
                    , after = queue
                    }
        )
        zipList


previous : ZipList a -> ZipList a
previous zipList =
    performIfNonEmpty
        (\something ->
            case something.before of
                [] ->
                    something

                head :: queue ->
                    { before = queue
                    , current = head
                    , after = something.current :: something.after
                    }
        )
        zipList


toList : ZipList a -> List a
toList zipList =
    case zipList of
        Empty ->
            []

        NonEmpty something ->
            List.concat
                [ List.reverse something.before
                , [ something.current ]
                , something.after
                ]


length : ZipList a -> Int
length zipList =
    case zipList of
        Empty ->
            0

        NonEmpty something ->
            1 + (List.length something.before) + (List.length something.after)
