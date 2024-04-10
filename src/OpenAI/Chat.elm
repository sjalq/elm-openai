module OpenAI.Chat exposing
    ( create
    , Input, ChatMessage, ChatMessageRole(..), Output, Choice
    , PropertyType(..),  addParameter, makeFunction, requireParams
    )

{-| <https://platform.openai.com/docs/api-reference/chat/create>

> Given a chat conversation, the model will return a chat completion response.

@docs create

@docs Input, ChatMessage, ChatMessageRole, ModelID, Output, Choice

-}

import Dict exposing (Dict)
import Element.Region exposing (description)
import Ext.Http
import Html.Attributes exposing (property)
import Http
import Json.Decode
import Json.Encode
import OpenAI.Common
import OpenAI.Internal exposing (andMap)
import Set exposing (Set)
import Time


{-| -}
type ChatMessageRole
    = SystemRole
    | UserRole
    | AssistantRole


stringFromChatMessageRole : ChatMessageRole -> String
stringFromChatMessageRole role =
    case role of
        SystemRole ->
            "system"

        UserRole ->
            "user"

        AssistantRole ->
            "assistant"


messageRoleFromString : String -> Maybe ChatMessageRole
messageRoleFromString role =
    case role of
        "system" ->
            Just SystemRole

        "user" ->
            Just UserRole

        "assistant" ->
            Just AssistantRole

        _ ->
            Nothing


decodeChatMessageRole : Json.Decode.Decoder ChatMessageRole
decodeChatMessageRole =
    Json.Decode.string
        |> Json.Decode.andThen
            (\role ->
                case messageRoleFromString role of
                    Just r ->
                        Json.Decode.succeed r

                    Nothing ->
                        Json.Decode.fail ("Invalid ChatMessageRole: " ++ role)
            )


{-| -}
type alias ChatMessage =
    { role : ChatMessageRole
    , content : String
    }


encodeChatMessage : ChatMessage -> Json.Encode.Value
encodeChatMessage message =
    Json.Encode.object
        [ ( "role", Json.Encode.string (stringFromChatMessageRole message.role) )
        , ( "content", Json.Encode.string message.content )
        ]


decodeChatMessage : Json.Decode.Decoder ChatMessage
decodeChatMessage =
    Json.Decode.succeed ChatMessage
        |> andMap (Json.Decode.field "role" decodeChatMessageRole)
        |> andMap (Json.Decode.field "content" Json.Decode.string)


{-| See <https://platform.openai.com/docs/api-reference/chat/create>

If `stream` is set to `True`, response will stream back partial progress. If set, tokens will be sent as data-only server-sent events as they become available, with the stream terminated by a `data: [DONE]` message. This is unsupported in this library for now.

-}
type alias Input =
    { model : String
    , messages : List ChatMessage
    , temperature : Maybe Float
    , top_p : Maybe Float
    , n : Maybe Int
    , stream : Maybe Bool
    , stop : Maybe (List String)
    , max_tokens : Maybe Int
    , presence_penalty : Maybe Float
    , frequency_penalty : Maybe Float
    , logit_bias : Maybe (Dict String Int)
    , user : Maybe String
    , tools : List Tool
    }


type alias FunctionDef =
    { name : String
    , description : String
    , paramObject : ParameterObject
    }


type alias ParameterObject =
    { properties : List Property
    , required : Set String
    }


type alias Property =
    { name : String
    , details : PropertyDetails
    }


type alias PropertyDetails =
    { type_ : PropertyType
    , description : String
    }


type PropertyType
    = PString
    | PInt
    | PFloat
    | PBool
    | PEnum (List String)


type Tool
    = Function FunctionDef



--builder pattern to make a function


makeFunction : String -> String -> Tool
makeFunction name description =
    FunctionDef
        name
        description
        (ParameterObject
            []
            (Set.fromList [])
        )
        |> Function


addParameter : String -> PropertyType -> String -> Tool -> Tool
addParameter name type_ description (Function f) =
    let
        newProperty =
            Property
                name
                (PropertyDetails
                    type_
                    description
                )

        newProperties =
            newProperty :: f.paramObject.properties

        currentParamObject =
            f.paramObject

        newParamObject =
            { currentParamObject | properties = newProperties }
    in
    { f | paramObject = newParamObject } |> Function


requireParams : List String -> Tool -> Tool
requireParams required (Function f) =
    let
        currentParamObject =
            f.paramObject

        newParamObject =
            { currentParamObject | required = Set.fromList required }
    in
    { f | paramObject = newParamObject } |> Function


encodeInput : Input -> Json.Encode.Value
encodeInput input =
    Json.Encode.object
        (List.filterMap identity
            [ Just ( "model", Json.Encode.string input.model )
            , Just ( "messages", Json.Encode.list encodeChatMessage input.messages )
            , Maybe.map (\a -> ( "temperature", Json.Encode.float a )) input.temperature
            , Maybe.map (\a -> ( "top_p", Json.Encode.float a )) input.top_p
            , Maybe.map (\a -> ( "n", Json.Encode.int a )) input.n
            , Maybe.map (\a -> ( "stream", Json.Encode.bool a )) input.stream
            , Maybe.map (\a -> ( "stop", Json.Encode.list Json.Encode.string a )) input.stop
            , Maybe.map (\a -> ( "max_tokens", Json.Encode.int a )) input.max_tokens
            , Maybe.map (\a -> ( "presence_penalty", Json.Encode.float a )) input.presence_penalty
            , Maybe.map (\a -> ( "frequency_penalty", Json.Encode.float a )) input.frequency_penalty
            , Maybe.map (\a -> ( "logit_bias", Json.Encode.dict identity Json.Encode.int a )) input.logit_bias
            , Maybe.map (\a -> ( "user", Json.Encode.string a )) input.user
            , Just ( "tools", Json.Encode.list encodeTool input.tools )
            ]
        )


encodeTool : Tool -> Json.Encode.Value
encodeTool tool =
    case tool of
        Function f ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "function" )
                , ( "function", encodeFunctionDef f )
                ]


encodeFunctionDef : FunctionDef -> Json.Encode.Value
encodeFunctionDef f =
    Json.Encode.object
        [ ( "name", Json.Encode.string f.name )
        , ( "description", Json.Encode.string f.description )
        , ( "parameters", encodeParamObject f.paramObject )
        ]



-- add "type" : "object" and then
-- "properties" : with the properties encoded here


encodeParamObject : ParameterObject -> Json.Encode.Value
encodeParamObject po =
    Json.Encode.object
        [ ( "type", Json.Encode.string "object" )
        , ( "properties", encodeProperties po.properties )
        , ( "required", Json.Encode.list Json.Encode.string (Set.toList po.required) )
        ]


encodeProperties : List Property -> Json.Encode.Value
encodeProperties ps =
    Json.Encode.object
        (List.map (\p -> ( p.name, encodePropertyDetails p.details )) ps)



-- encodeProperty : Property -> Json.Encode.Value
-- encodeProperty p =
--     Json.Encode.object
--         [ ( p.name, encodePropertyDetails p.details )
--         ]


encodePropertyDetails : PropertyDetails -> Json.Encode.Value
encodePropertyDetails pd =
    Json.Encode.object
        ([ ( "type", encodePropertyType pd.type_ )
         , ( "description", Json.Encode.string pd.description )
         ]
            ++ (case pd.type_ of
                    PEnum values ->
                        [ ( "enum", Json.Encode.list Json.Encode.string values ) ]

                    _ ->
                        []
               )
        )


encodePropertyType : PropertyType -> Json.Encode.Value
encodePropertyType t =
    case t of
        PString ->
            Json.Encode.string "string"

        PInt ->
            Json.Encode.string "integer"

        PFloat ->
            Json.Encode.string "number"

        PBool ->
            Json.Encode.string "boolean"

        PEnum _ ->
            Json.Encode.string "string"


{-| -}
type alias Choice =
    { index : Int
    , message : ChatMessage
    , finish_reason : Maybe String
    }


decodeChoice : Json.Decode.Decoder Choice
decodeChoice =
    Json.Decode.succeed Choice
        |> andMap (Json.Decode.field "index" Json.Decode.int)
        |> andMap (Json.Decode.field "message" decodeChatMessage)
        |> andMap (Json.Decode.field "finish_reason" (Json.Decode.maybe Json.Decode.string))


{-| -}
type alias Output =
    { id : String
    , object : String
    , created : Time.Posix
    , choices : List Choice
    , usage : OpenAI.Common.Usage
    }


decodeOutput : Json.Decode.Decoder Output
decodeOutput =
    Json.Decode.succeed Output
        |> andMap (Json.Decode.field "id" Json.Decode.string)
        |> andMap (Json.Decode.field "object" Json.Decode.string)
        |> andMap (Json.Decode.field "created" OpenAI.Internal.decodeUnixTimeAsTimePosix)
        |> andMap (Json.Decode.field "choices" (Json.Decode.list decodeChoice))
        |> andMap (Json.Decode.field "usage" OpenAI.Internal.decodeUsage)


{-|

    OpenAI.Chat.create
        { model = GPT3_5_Turbo
        , messages =
            [ ChatMessage SystemRole
                "You are an AI assistant whose goal is to promote the Elm programming language."
            , ChatMessage UserRole
                "What is the best way to learn Elm?"
            ]
        , temperature = Nothing
        , top_p = Nothing
        , n = Nothing
        , stream = Nothing
        , stop = Nothing
        , max_tokens = Nothing
        , presence_penalty = Nothing
        , frequency_penalty = Nothing
        , logit_bias = Nothing
        , user = Nothing
        }
        |> OpenAI.withConfig cfg
        |> Http.task

-}
create : Input -> Ext.Http.TaskInput (Ext.Http.Error String) Output
create input =
    { method = "POST"
    , headers = []
    , url = "/chat/completions"
    , body = Http.jsonBody (encodeInput input)
    , resolver =
        Http.stringResolver
            (Ext.Http.jsonResolver decodeOutput >> Result.map .data)
    , timeout = Nothing
    }
