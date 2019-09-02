module Main exposing (Id, Model, Msg(..), NodeData, addChild, deleteChild, goToNodeById, init, main, nodeOfTreeById, nodeTree, resetId, tree2Html, tree2Zipper, treeOfZipper, update, view)

import Browser
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import List exposing ((::))
import Monocle.Optional as Optional exposing (Optional)
import MultiwayTree exposing (Tree(..), children, datum)
import MultiwayTreeZipper as Zipper exposing (Zipper)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Id =
    List Int


type alias NodeData =
    { id : Id, value : String }


type alias Model =
    { node : Tree NodeData, length : Int }


nodeTree : Tree NodeData
nodeTree =
    Tree { id = [], value = "root" }
        [ Tree { id = [ 0 ], value = "aaa" } []
        , Tree { id = [ 1 ], value = "bbb" } []
        , Tree { id = [ 2 ], value = "ccc" }
            [ Tree { id = [ 2, 0 ], value = "ccc" } []
            ]
        ]


init : Model
init =
    Model nodeTree 1



-- UPDATE


type Msg
    = Change Id String
    | Add Id
    | Delete Id


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change id value ->
            { model | node = model.node |> Optional.modify (nodeOfTreeById id) (\nd -> { nd | value = value }) }

        Delete id ->
            case deleteChild id model.node of
                Just zip ->
                    { model | node = treeOfZipper zip |> resetId }

                Nothing ->
                    model

        Add id ->
            case addChild id model.node of
                Just zip ->
                    { model | node = treeOfZipper zip }

                Nothing ->
                    model


treeOfZipper : Zipper a -> Tree a
treeOfZipper ( tree, _ ) =
    tree


tree2Zipper : Tree a -> Zipper a
tree2Zipper tree =
    ( tree, [] )


goToNodeById : Id -> Maybe (Zipper a) -> Maybe (Zipper a)
goToNodeById id mZipper =
    List.foldl (\idx mz -> mz |> Maybe.andThen (Zipper.goToChild idx)) mZipper id


nodeOfTreeById : Id -> Optional (Tree a) a
nodeOfTreeById id =
    let
        -- treeをZipper化して、指定されたidまで潜る
        targetZipper tree =
            Just (tree2Zipper tree) |> goToNodeById id

        -- 指定されたidをdataで書き換え、Root Zipperに戻したもの
        replacedZipper tree data =
            targetZipper tree |> Maybe.andThen (Zipper.replaceDatum data) |> Maybe.andThen Zipper.goToRoot

        -- 指定されたidのnodeDataを得るgetter
        get tree =
            Maybe.map Zipper.datum (targetZipper tree)

        -- 指定されたidのnodeDateを書き換えるsetter, もし該当箇所が無ければ元のtreeを返す
        set data tree =
            Maybe.withDefault tree (Maybe.map treeOfZipper <| replacedZipper tree data)
    in
    Optional get set


resetId : Tree NodeData -> Tree NodeData
resetId tree =
    let
        newTree =
            Tree { id = [], value = (datum tree).value } (children tree)
    in
    resetChildrenId newTree


resetChildrenId : Tree NodeData -> Tree NodeData
resetChildrenId tree =
    let
        id =
            (datum tree).id

        newChildren =
            List.foldl
                (\child acc ->
                    let
                        newChild =
                            Tree { id = id ++ [ List.length acc ], value = (datum child).value } (children child)
                    in
                    acc ++ [ resetChildrenId newChild ]
                )
                []
                (children tree)
    in
    Tree (datum tree) newChildren


deleteChild : Id -> Tree NodeData -> Maybe (Zipper NodeData)
deleteChild id tree =
    List.reverse id
        |> List.head
        |> Maybe.andThen
            (\index ->
                let
                    removeElement i list =
                        List.take i list ++ List.drop (i + 1) list
                in
                Just (tree2Zipper tree)
                    |> goToNodeById (List.take (List.length id - 1) id)
                    |> Maybe.andThen
                        (\zip ->
                            Zipper.updateChildren (removeElement index (treeOfZipper zip |> children)) zip
                        )
                    |> Maybe.andThen Zipper.goToRoot
            )


addChild : Id -> Tree NodeData -> Maybe (Zipper NodeData)
addChild id tree =
    let
        mZip =
            Just (tree2Zipper tree) |> goToNodeById id
    in
    mZip
        |> Maybe.andThen
            (\zip ->
                let
                    len =
                        treeOfZipper zip |> children |> List.length

                    emptyTree =
                        Tree { id = id ++ [ len ], value = "" } []
                in
                zip |> Zipper.appendChild emptyTree |> Maybe.andThen Zipper.goToRoot
            )


tree2Html : Tree NodeData -> Html Msg
tree2Html node =
    let
        data =
            datum node

        forest =
            children node
    in
    ul []
        [ li []
            ((::)
                (div []
                    [ input [ value data.value, onInput (Change data.id) ] []
                    , button [ onClick (Add data.id) ] [ text "+" ]
                    , button [ onClick (Delete data.id) ] [ text "-" ]
                    ]
                )
                (List.map tree2Html forest)
            )
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "sitemap" ]
            [ tree2Html model.node
            ]
        ]
