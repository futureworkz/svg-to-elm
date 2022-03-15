module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Media exposing (only, screen, withMedia)
import Html.Styled exposing (Attribute, Html, a, code, div, h2, img, input, p, span, text, textarea)
import Html.Styled.Attributes exposing (autocomplete, css, href, placeholder, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttr
import Tuple



---- MODEL ----


type alias Model =
    { showSetting : Bool
    , settings : Setting
    , htmlInput : String
    }


type alias Setting =
    { html : ( String, String )
    , htmlAttr : ( String, String )
    , svg : ( String, String )
    , svgAttr : ( String, String )
    }


init : ( Model, Cmd Msg )
init =
    ( Model False
        { html = ( "Html", ".." )
        , htmlAttr = ( "Attr", "css" )
        , svg = ( "Svg", "svg, path" )
        , svgAttr = ( "SvgAttr", "" )
        }
        "<a href=\" / \">Home Page</a>"
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = OnShowSetting
    | OnChangeHtmlAlias String
    | OnChangeHtmlAttrAlias String
    | OnChangeSvgAlias String
    | OnChangeSvgAttrAlias String
    | OnChangeHtmlExposing String
    | OnChangeHtmlAttrExposing String
    | OnChangeSvgExposing String
    | OnChangeSvgAttrExposing String
    | OnChangeHtmlInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnShowSetting ->
            ( { model | showSetting = not model.showSetting }, Cmd.none )

        OnChangeHtmlAlias val ->
            ( { model | settings = updateHtmlAlias model.settings val }, Cmd.none )

        OnChangeHtmlAttrAlias val ->
            ( { model | settings = updateHtmlAttrAlias model.settings val }, Cmd.none )

        OnChangeSvgAlias val ->
            ( { model | settings = updateSvgAlias model.settings val }, Cmd.none )

        OnChangeSvgAttrAlias val ->
            ( { model | settings = updateSvgAttrAlias model.settings val }, Cmd.none )

        OnChangeHtmlExposing val ->
            ( { model | settings = updateHtmlExposing model.settings val }, Cmd.none )

        OnChangeHtmlAttrExposing val ->
            ( { model | settings = updateHtmlAttrExposing model.settings val }, Cmd.none )

        OnChangeSvgExposing val ->
            ( { model | settings = updateSvgExposing model.settings val }, Cmd.none )

        OnChangeSvgAttrExposing val ->
            ( { model | settings = updateSvgAttrExposing model.settings val }, Cmd.none )

        OnChangeHtmlInput val ->
            ( { model | htmlInput = val }, Cmd.none )


updateHtmlAlias : Setting -> String -> Setting
updateHtmlAlias settings newAlias =
    { settings | html = settings.html |> updateAlias "Html" newAlias }


updateHtmlExposing : Setting -> String -> Setting
updateHtmlExposing settings newExposing =
    { settings | html = settings.html |> updateExposing "Css" newExposing }


updateHtmlAttrAlias : Setting -> String -> Setting
updateHtmlAttrAlias settings newAlias =
    { settings | htmlAttr = settings.htmlAttr |> updateAlias "" newAlias }


updateHtmlAttrExposing : Setting -> String -> Setting
updateHtmlAttrExposing settings newExposing =
    { settings | htmlAttr = settings.htmlAttr |> updateExposing "css" newExposing }


updateSvgAlias : Setting -> String -> Setting
updateSvgAlias settings newAlias =
    { settings | svg = settings.svg |> updateAlias "Svg" newAlias }


updateSvgExposing : Setting -> String -> Setting
updateSvgExposing settings newExposing =
    { settings | svg = settings.svg |> updateExposing "svg, path" newExposing }


updateSvgAttrAlias : Setting -> String -> Setting
updateSvgAttrAlias settings newAlias =
    { settings | svgAttr = settings.svgAttr |> updateAlias "Svg, path" newAlias }


updateSvgAttrExposing : Setting -> String -> Setting
updateSvgAttrExposing settings newExposing =
    { settings | svgAttr = settings.svgAttr |> updateExposing "" newExposing }


updateAlias : String -> String -> ( String, String ) -> ( String, String )
updateAlias defaultAlias newAlias ( importAlias, importExposing ) =
    ( if newAlias == "" then
        defaultAlias

      else
        newAlias
    , importExposing
    )


updateExposing : String -> String -> ( String, String ) -> ( String, String )
updateExposing defaultExposing newExposing ( importAlias, exportExposing ) =
    ( importAlias
    , if newExposing == "" then
        defaultExposing

      else
        newExposing
    )



---- VIEW ----


view : Model -> Html Msg
view model =
    div
        [ css style.container ]
        [ div [ css style.headerContainer ]
            [ div [ css style.contentContainter ]
                [ h2 [ css style.title ] [ text "html-to-elm.com" ]
                , div [ css style.buttonsContainer ]
                    [ div [ css style.settingContainer, onClick OnShowSetting ]
                        [ div [ css style.actionlabel ] [ text "Settings" ]
                        , settingsIcon
                        ]
                    , div [ css style.settingContainer ]
                        [ div [ css style.actionlabel ] [ text "Copy" ]
                        , copyIcon
                        ]
                    ]
                ]
            ]
        , div [ css style.bodyContainer ]
            [ div [ css style.inputContainer ]
                [ div
                    [ css
                        (if model.showSetting == True then
                            style.importSettings

                         else
                            style.hiddenImportSettings
                        )
                    ]
                    [ settingItem "import Html" (model.settings.html |> Tuple.first) (model.settings.html |> Tuple.second) OnChangeHtmlAlias OnChangeHtmlExposing
                    , settingItem "import Html.Attributes" (model.settings.htmlAttr |> Tuple.first) (model.settings.htmlAttr |> Tuple.second) OnChangeHtmlAttrAlias OnChangeHtmlAttrExposing
                    , settingItem "import Svg" (model.settings.svg |> Tuple.first) (model.settings.svg |> Tuple.second) OnChangeSvgAlias OnChangeSvgExposing
                    , settingItem "import Svg.Attributes" (model.settings.svgAttr |> Tuple.first) (model.settings.svgAttr |> Tuple.second) OnChangeSvgAttrAlias OnChangeSvgAttrExposing
                    ]
                , div [ css style.inputHTML ]
                    [ Html.Styled.label [ css style.inputHTMLLabel ] [ text "Input HTML" ]
                    , textarea [ value model.htmlInput, autocomplete False, css style.textAreaHTML, onInput OnChangeHtmlInput ] []
                    ]
                ]
            , div
                [ css style.outputContainer ]
                [ Html.Styled.label [ css style.inputHTMLLabel ] [ text "Output ELM" ]
                , Html.Styled.pre [ css style.preContainer ] [ code [] [ text model.htmlInput ] ]
                ]
            ]
        , div [ css style.footerContainer ]
            [ a [ href "https://www.futureworkz.com/", Html.Styled.Attributes.target "_blank" ]
                [ img [ src "futureworkz-logo.avif", css style.logo ] []
                ]
            ]
        ]


settingItem : String -> String -> String -> (String -> msg) -> (String -> msg) -> Html msg
settingItem titleValue importValue exposingValue onImport onExposing =
    div [ css style.importSetting ]
        [ div [ css style.importContainer ] [ p [ css style.labelImport ] [ text titleValue ] ]
        , div [ css style.inputsContainer ]
            [ div [ css style.asContainer ]
                [ span [ css style.subLabelImport ] [ text "as" ]
                , input [ value importValue, autocomplete False, css style.inputImport, onInput onImport ] []
                ]
            , div [ css style.asContainer ]
                [ span [ css style.subLabelImport ] [ text "exposing (" ]
                , input [ value exposingValue, autocomplete False, css style.inputExposing, onInput onExposing ] []
                , span [ css style.rightSubLabelImport ] [ text ")" ]
                ]
            ]
        ]


label : String -> Html msg
label value =
    span [ Html.Styled.Attributes.style "text-align" "left", Html.Styled.Attributes.style "margin-bottom" "10px" ] [ text value ]


inputContainer : List (Attribute msg)
inputContainer =
    [ Html.Styled.Attributes.style "margin-bottom" "20px" ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


style : { container : List Style, headerContainer : List Style, contentContainter : List Style, title : List Style, buttonsContainer : List Style, settingContainer : List Style, actionlabel : List Style, bodyContainer : List Style, inputContainer : List Style, importSettings : List Style, importSetting : List Style, importContainer : List Style, asContainer : List Style, labelImport : List Style, subLabelImport : List Style, inputImport : List Style, inputExposing : List Style, inputHTML : List Style, inputHTMLLabel : List Style, textAreaHTML : List Style, outputContainer : List Style, preContainer : List Style, inputsContainer : List Style, hiddenImportSettings : List Style, footerContainer : List Style, logo : List Style, rightSubLabelImport : List Style }
style =
    { container =
        [ height (vh 100)
        , displayFlex
        , flexDirection column
        , overflow hidden
        ]
    , headerContainer =
        [ width (pct 100)
        , height (px 50)
        , backgroundColor (hex "#1f2937")
        ]
    , contentContainter =
        [ displayFlex
        , paddingLeft (pct 20)
        , paddingRight (pct 20)
        , height (pct 100)
        , justifyContent spaceBetween
        , alignItems center
        ]
    , title =
        [ color (hex "#ffffff")
        , margin (px 0)
        ]
    , buttonsContainer =
        [ displayFlex
        , alignItems center
        ]
    , settingContainer =
        [ displayFlex
        , alignItems center
        , fontSize (px 16)
        , paddingLeft (px 10)
        , paddingRight (px 10)
        , cursor pointer
        , color (hex "#ffffff")
        ]
    , actionlabel =
        [ marginRight (px 10)
        ]
    , bodyContainer =
        [ displayFlex
        , flexGrow (num 1)
        , overflow hidden
        , withMedia [ only screen [ Css.Media.maxWidth (px 800) ] ]
            [ flexDirection column
            ]
        ]
    , inputContainer =
        [ width (pct 50)
        , displayFlex
        , flexDirection column
        , withMedia [ only screen [ Css.Media.maxWidth (px 800) ] ]
            [ width (pct 100)
            , flexGrow (num 1)
            ]
        ]
    , importSettings =
        [ position absolute
        , padding (px 16)
        , backgroundColor (hex "#ffffff")
        , boxShadow4 (px -5) (px -4) (px 18) (hex "#000000")
        , withMedia [ only screen [ Css.Media.maxWidth (px 800) ] ]
            [ overflow scroll
            , position relative
            ]
        ]
    , hiddenImportSettings =
        [ display none
        ]
    , importSetting =
        [ displayFlex
        , flexGrow (num 1)
        , marginBottom (px 32)
        , alignItems center
        , withMedia [ only screen [ Css.Media.maxWidth (px 800) ] ]
            [ flexDirection column
            ]
        ]
    , importContainer =
        [ width (pct 30)
        , marginRight (px 10)
        , withMedia [ only screen [ Css.Media.maxWidth (px 800) ] ]
            [ displayFlex
            , justifyContent center
            , alignItems center
            , marginBottom (px 10)
            ]
        ]
    , inputsContainer =
        [ displayFlex
        , width (pct 100)
        , withMedia [ only screen [ Css.Media.maxWidth (px 800) ] ]
            [ flexDirection column
            , alignItems center
            ]
        ]
    , labelImport =
        [ margin (px 0)
        , fontWeight (int 600)
        , textAlign center
        ]
    , asContainer =
        [ flex (int 1)
        , position relative
        , paddingLeft (px 5)
        , borderBottom3 (px 2) solid (hex "#000000")
        , withMedia [ only screen [ Css.Media.maxWidth (px 800) ] ]
            [ marginBottom (px 10)
            ]
        ]
    , subLabelImport =
        [ position absolute
        , top (px 0)
        , left (px 0)
        , displayFlex
        , alignItems center
        , fontSize (px 14)
        , color (hex "#767d8a")
        , marginRight (px 5)
        ]
    , rightSubLabelImport =
        [ position absolute
        , top (px 0)
        , right (px 0)
        , displayFlex
        , alignItems center
        , fontSize (px 14)
        , color (hex "#767d8a")
        , marginRight (px 5)
        ]
    , inputImport =
        [ width (pct 100)
        , borderWidth (px 0)
        , fontSize (px 16)
        , paddingLeft (px 32)
        , paddingRight (px 40)
        , focus
            [ outline zero
            ]
        ]
    , inputExposing =
        [ borderWidth (px 0)
        , fontSize (px 16)
        , paddingLeft (px 100)
        , paddingRight (px 16)
        , focus
            [ outline zero
            ]
        ]
    , inputHTML =
        [ displayFlex
        , flexDirection column
        , flexGrow (num 1)
        ]
    , inputHTMLLabel =
        [ fontWeight (int 700)
        , padding2 (px 10) (px 5)
        ]
    , textAreaHTML =
        [ flexGrow (num 1)
        , padding2 (px 10) (px 10)
        ]
    , outputContainer =
        [ width (pct 50)
        , displayFlex
        , flexDirection column
        , withMedia [ only screen [ Css.Media.maxWidth (px 800) ] ]
            [ width (pct 100)
            , flexGrow (num 1)
            ]
        ]
    , preContainer =
        [ displayFlex
        , flexGrow (num 1)
        , backgroundColor (hex "#000000")
        , margin (px 0)
        , color (hex "#ffffff")
        , padding2 (px 10) (px 10)
        ]
    , footerContainer =
        [ width (pct 100)
        , height (px 60)
        , displayFlex
        , alignItems center
        , justifyContent center
        ]
    , logo =
        [ width (px 200)
        , height (px 40)
        ]
    }


settingsIcon : Html msg
settingsIcon =
    Svg.svg
        [ SvgAttr.fill "none"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.css [ width (px 20) ]
        ]
        [ Svg.path
            [ SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.d "M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4"
            ]
            []
        ]


copyIcon : Html msg
copyIcon =
    Svg.svg
        [ SvgAttr.fill "none"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.css [ width (px 20) ]
        ]
        [ Svg.path
            [ SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.d "M8 5H6a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2v-1M8 5a2 2 0 002 2h2a2 2 0 002-2M8 5a2 2 0 012-2h2a2 2 0 012 2m0 0h2a2 2 0 012 2v3m2 4H10m0 0l3-3m-3 3l3 3"
            ]
            []
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> Html.Styled.toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
