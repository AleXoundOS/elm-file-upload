module Main exposing (init)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id, src, title, type_)
import Html.Events exposing (on)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Ports exposing (FilePortData, fileContentRead, fileSelected)


type Msg
    = ImageSelected
    | ImageRead FilePortData
    | Response (Result Http.Error String)


type alias File =
    { contents : String
    , filename : String
    }


type alias Model =
    { id : String
    , mFile : Maybe File
    , response : Maybe String
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { id = "ImageInputId"
      , mFile = Nothing
      , response = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageSelected ->
            ( model
            , fileSelected model.id
            )

        ImageRead data ->
            let
                newFile =
                    { contents = data.contents
                    , filename = data.filename
                    }
            in
            ( { model | mFile = Just newFile }
            , Http.send Response (postRequest model newFile)
            )

        Response r ->
            case r of
                Ok str ->
                    ( { model | response = Just str }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | response = Nothing }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    let
        imagePreview =
            case model.mFile of
                Just i ->
                    viewPreviewFileAsImage i

                Nothing ->
                    text ""

        responseText =
            case .response model of
                Just str ->
                    text str

                Nothing ->
                    text ""
    in
    div [ class "imageWrapper" ]
        [ input
            [ type_ "file"
            , id model.id
            , on "change"
                (JD.succeed ImageSelected)
            ]
            []
        , imagePreview
        , responseText
        ]


viewPreviewFileAsImage : File -> Html Msg
viewPreviewFileAsImage image =
    img
        [ src image.contents
        , title image.filename
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead ImageRead


postRequest : Model -> File -> Http.Request String
postRequest model file =
    let
        body =
            Http.jsonBody <|
                JE.object
                    [ ( "test_field", JE.string "test_data" )
                    , ( "file", JE.string <| .contents file )
                    ]
    in
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://httpbin.org/post"
        , body = body
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
