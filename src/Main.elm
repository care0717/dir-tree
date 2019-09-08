module Main exposing (korenani, main, resetRootId)

import Browser
import Html exposing (Html, button, div, input, li, pre, text, ul)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import List exposing ((::))
import Monocle.Optional as Optional exposing (Optional)
import MultiwayTree exposing (Forest, Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Id =
    List Int


type alias NodeData =
    { id : Id, value : String }


type alias Model =
    { tree : Tree NodeData }


nodeTree : Tree NodeData
nodeTree =
    Tree { id = [], value = "root" }
        [ Tree { id = [ 0 ], value = "aaa" } []
        , Tree { id = [ 1 ], value = "bbb" } [ Tree { id = [ 1, 0 ], value = "ccc" } [] ]
        , Tree { id = [ 2 ], value = "ddd" }
            []
        ]


init : Model
init =
    Model nodeTree



-- UPDATE


type Msg
    = Change Id String
    | Add Id
    | Delete Id


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change id value ->
            { model | tree = model.tree |> Optional.modify (nodeOfTreeById id) (\nd -> { nd | value = value }) }

        Delete id ->
            case
                List.reverse id
                    |> List.head
            of
                Just index ->
                    { model
                        | tree =
                            model.tree
                                |> Optional.modify (parentId id |> childrenOfTreeById) (\children -> removeElement index children)
                                |> resetRootId
                    }

                Nothing ->
                    model

        Add id ->
            { model
                | tree =
                    model.tree
                        |> Optional.modify (childrenOfTreeById id) (\children -> children ++ [ Tree { id = [], value = "" } [] ])
                        |> resetRootId
            }


treeOfZipper : Zipper a -> Tree a
treeOfZipper ( tree, _ ) =
    tree


goToZipperById : Id -> Tree a -> Maybe (Zipper a)
goToZipperById id tree =
    let
        goToNode : Zipper a -> Maybe (Zipper a)
        goToNode zipper =
            List.foldl (\idx mz -> mz |> Maybe.andThen (Zipper.goToChild idx)) (Just zipper) id
    in
    ( tree, [] ) |> goToNode


getOptionalByGetterAndSetter : Id -> (Zipper a -> b) -> (b -> Zipper a -> Maybe (Zipper a)) -> Optional (Tree a) b
getOptionalByGetterAndSetter id getFunc setFunc =
    let
        -- 指定されたidのZipperをdataで書き換える関数を用いて書き換え、Root Zipperに戻したもの
        replacedZipper tree data =
            goToZipperById id tree |> Maybe.andThen (setFunc data) |> Maybe.andThen Zipper.goToRoot

        -- 指定されたidのZipperからdataを得るgetter
        get tree =
            goToZipperById id tree |> Maybe.map getFunc

        -- 指定されたidのZipperのdataを書き換えるsetter, もし該当箇所が無ければ元のtreeを返す
        set data tree =
            Maybe.withDefault tree (Maybe.map treeOfZipper <| replacedZipper tree data)
    in
    Optional get set


childrenOfTreeById : Id -> Optional (Tree a) (Forest a)
childrenOfTreeById id =
    getOptionalByGetterAndSetter id (treeOfZipper >> MultiwayTree.children) Zipper.updateChildren


nodeOfTreeById : Id -> Optional (Tree a) a
nodeOfTreeById id =
    getOptionalByGetterAndSetter id Zipper.datum Zipper.replaceDatum


parentId : Id -> Id
parentId id =
    List.take (List.length id - 1) id


removeElement : Int -> List a -> List a
removeElement i list =
    List.take i list ++ List.drop (i + 1) list


resetRootId : Tree NodeData -> Tree NodeData
resetRootId tree =
    let
        newTree =
            Tree { id = [], value = (MultiwayTree.datum tree).value } (MultiwayTree.children tree)
    in
    resetId newTree


resetId : Tree NodeData -> Tree NodeData
resetId tree =
    let
        id =
            (MultiwayTree.datum tree).id

        newChildren =
            List.foldl
                (\child acc ->
                    let
                        newChild =
                            Tree { id = id ++ [ List.length acc ], value = (MultiwayTree.datum child).value } (MultiwayTree.children child)
                    in
                    acc ++ [ resetId newChild ]
                )
                []
                (MultiwayTree.children tree)
    in
    Tree (MultiwayTree.datum tree) newChildren


tree2Html : Tree NodeData -> Html Msg
tree2Html tree =
    let
        data =
            MultiwayTree.datum tree

        children =
            MultiwayTree.children tree
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
                (List.map tree2Html children)
            )
        ]


tree2Plane : Tree NodeData -> String
tree2Plane tree =
    let
        addHeader : String -> String
        addHeader s =
            case String.split "\n" s of
                first :: list ->
                    "\n┣━ " ++ first ++ List.foldl (\x acc -> acc ++ "\n┃   " ++ x) "" list

                _ ->
                    ""

        addHeader4Last : String -> String
        addHeader4Last s =
            case String.split "\n" s of
                first :: list ->
                    "\n┗━ " ++ first ++ List.foldl (\x acc -> acc ++ "\n     " ++ x) "" list

                _ ->
                    ""
    in
    case MultiwayTree.children tree |> List.reverse of
        last :: rest ->
            (MultiwayTree.datum tree).value
                ++ List.foldl
                    (\x acc ->
                        acc ++ addHeader (tree2Plane x)
                    )
                    ""
                    (List.reverse rest)
                ++ addHeader4Last (tree2Plane last)

        _ ->
            (MultiwayTree.datum tree).value



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "sitemap" ]
            [ tree2Html model.tree
            ]
        , pre []
            [ text <| tree2Plane model.tree ]
        ]
