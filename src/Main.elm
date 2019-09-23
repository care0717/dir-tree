module Main exposing (main, resetRootId)

import Browser
import Html exposing (Html, button, div, hr, i, input, li, pre, section, text, ul)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import List exposing ((::))
import Monocle.Optional as Optional exposing (Optional)
import MultiwayTree exposing (Forest, Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)
import UndoList exposing (UndoList)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Id =
    List Int


type alias NodeData =
    { id : Id, value : String }


type alias Model =
    UndoList (Tree NodeData)


nodeTree : Tree NodeData
nodeTree =
    Tree { id = [], value = "" }
        []


init : Model
init =
    UndoList.fresh nodeTree



-- UPDATE


type Msg
    = Change Id String
    | Add Id
    | Delete Id
    | Reset
    | Undo
    | Redo
    | Save
    | RemoveNoChangeUndoItem


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change id value ->
            { model | present = model.present |> Optional.modify (setNodeById id) (\nd -> { nd | value = value }) }

        Delete id ->
            case
                List.reverse id
                    |> List.head
            of
                Just index ->
                    UndoList.new
                        (model.present
                            |> Optional.modify (parentId id |> setChildrenById) (\children -> removeElement index children)
                            |> resetRootId
                        )
                        model

                Nothing ->
                    model

        Add id ->
            UndoList.new
                (model.present
                    |> Optional.modify (addTreeById id) (\_ -> Tree { id = [], value = "" } [])
                    |> resetRootId
                )
                model

        Reset ->
            UndoList.new (Tree { id = [], value = "" } []) model |> update RemoveNoChangeUndoItem

        Undo ->
            UndoList.undo model

        Redo ->
            UndoList.redo model

        Save ->
            UndoList.new model.present model

        RemoveNoChangeUndoItem ->
            if model.present == (UndoList.undo model).present then
                if UndoList.lengthPast model == 1 then
                    UndoList.fresh model.present

                else
                    UndoList.new (UndoList.undo model).present (UndoList.undo model |> UndoList.undo)

            else
                model


treeOfZipper : Zipper a -> Tree a
treeOfZipper ( tree, _ ) =
    tree


getOptionalByGetterAndSetter : (Zipper a -> b) -> (b -> Zipper a -> Maybe (Zipper a)) -> Id -> Optional (Tree a) b
getOptionalByGetterAndSetter getFunc setFunc id =
    let
        goToZipper : Tree a -> Maybe (Zipper a)
        goToZipper tree =
            let
                goToNode zipper =
                    List.foldl (\idx mz -> mz |> Maybe.andThen (Zipper.goToChild idx)) (Just zipper) id
            in
            ( tree, [] ) |> goToNode

        -- 指定されたidのZipperをdataで書き換える関数を用いて書き換え、Root Zipperに戻したもの
        replacedZipper tree data =
            goToZipper tree |> Maybe.andThen (setFunc data) |> Maybe.andThen Zipper.goToRoot

        -- 指定されたidのZipperからdataを得るgetter
        get tree =
            goToZipper tree |> Maybe.map getFunc

        -- 指定されたidのZipperのdataを書き換えるsetter, もし該当箇所が無ければ元のtreeを返す
        set data tree =
            Maybe.withDefault tree (Maybe.map treeOfZipper <| replacedZipper tree data)
    in
    Optional get set


addTreeById : Id -> Optional (Tree a) (Tree a)
addTreeById =
    getOptionalByGetterAndSetter treeOfZipper Zipper.appendChild


setChildrenById : Id -> Optional (Tree a) (Forest a)
setChildrenById =
    getOptionalByGetterAndSetter (treeOfZipper >> MultiwayTree.children) Zipper.updateChildren


setNodeById : Id -> Optional (Tree a) a
setNodeById =
    getOptionalByGetterAndSetter Zipper.datum Zipper.replaceDatum


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
                    [ input [ type_ "text", value data.value, onInput (Change data.id), onFocus Save, onBlur RemoveNoChangeUndoItem ] []
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
                    "\n├── " ++ first ++ List.foldl (\x acc -> acc ++ "\n│   " ++ x) "" list

                _ ->
                    ""

        addHeader4Last : String -> String
        addHeader4Last s =
            case String.split "\n" s of
                first :: list ->
                    "\n└── " ++ first ++ List.foldl (\x acc -> acc ++ "\n    " ++ x) "" list

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
    let
        canNotUndo =
            UndoList.hasPast model |> not

        canNotRedo =
            UndoList.hasFuture model |> not
    in
    section [ class "section" ]
        [ div [ class "container" ]
            [ button [ class "button", type_ "reset", onClick Reset ] [ text "Reset" ]
            , button
                [ class "icon is-medium button", type_ "button", onClick Undo, disabled canNotUndo ]
                [ i [ class "fas fa-undo" ] [] ]
            , button
                [ class "icon is-medium button", type_ "button", onClick Redo, disabled canNotRedo ]
                [ i [ class "fas fa-redo" ] [] ]
            , hr [] []
            , div [ class "sitemap" ]
                [ tree2Html model.present
                ]
            , pre [ class "pre_text" ]
                [ text <| tree2Plane model.present ]
            ]
        ]
