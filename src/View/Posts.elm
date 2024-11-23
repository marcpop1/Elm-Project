module View.Posts exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (href)
import Html.Events
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable _ now posts =
    Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "Score" ]
                , Html.th [] [ Html.text "Title" ]
                , Html.th [] [ Html.text "Type" ]
                , Html.th [] [ Html.text "Posted Date" ]
                , Html.th [] [ Html.text "Link" ]
                ]
            ]
        , Html.tbody []
            (List.map (postRow now) posts)
        ]


postRow : Time.Posix -> Post -> Html Msg
postRow now post =
    Html.tr []
        [ Html.td [ Html.Attributes.class "post-score" ] [ Html.text (String.fromInt post.score) ]
        , Html.td [ Html.Attributes.class "post-title" ] [ Html.text post.title ]
        , Html.td [ Html.Attributes.class "post-type" ] [ Html.text post.type_ ]
        , Html.td [ Html.Attributes.class "post-time" ] [ Html.text (Util.Time.formatTime Time.utc post.time) ]
        , Html.td [ Html.Attributes.class "post-url" ]
            [ Html.a [ Html.Attributes.href (Maybe.withDefault "#" post.url) ] [ Html.text "Link" ] ]
        ]



{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    div []
        [ -- Dropdown to select the number of posts to show (10, 25, or 50)
          Html.label [] [ Html.text "Posts per page: " ]
        , Html.select
            [ Html.Attributes.id "select-posts-per-page"
            , Html.Events.onInput (\value -> ConfigChanged (SetPostsToShow (String.toInt value |> Maybe.withDefault config.postsToShow)))
            ]
            [ Html.option [ Html.Attributes.value "10", Html.Attributes.selected (config.postsToShow == 10) ] [ Html.text "10" ]
            , Html.option [ Html.Attributes.value "25", Html.Attributes.selected (config.postsToShow == 25) ] [ Html.text "25" ]
            , Html.option [ Html.Attributes.value "50", Html.Attributes.selected (config.postsToShow == 50) ] [ Html.text "50" ]
            ]

          -- Dropdown to select the field to sort by (score, title, date posted, unsorted)
        , Html.label [] [ Html.text "Sort by: " ]
        , Html.select
            [ Html.Attributes.id "select-sort-by"
            , Html.Events.onInput (\value ->
                case sortFromString value of
                    Just sortBy -> ConfigChanged (SetSortBy sortBy)
                    Nothing -> ConfigChanged (SetSortBy config.sortBy)
              )
            ]
            (List.map (sortOptionToHtml config.sortBy) sortOptions)

          -- Checkbox for showing job posts
        , Html.label []
            [ Html.input
                [ Html.Attributes.id "checkbox-show-job-posts"
                , Html.Attributes.type_ "checkbox"
                , Html.Attributes.checked config.showJobs
                , Html.Events.onCheck (\checked -> ConfigChanged (SetShowJobs checked))
                ]
                []
            , Html.text " Show job posts"
            ]

          -- Checkbox for showing text-only posts (no URL)
        , Html.label []
            [ Html.input
                [ Html.Attributes.id "checkbox-show-text-only-posts"
                , Html.Attributes.type_ "checkbox"
                , Html.Attributes.checked config.showTextOnly
                , Html.Events.onCheck (\checked -> ConfigChanged (SetShowTextOnly checked))
                ]
                []
            , Html.text " Show text-only posts"
            ]
        ]


sortOptionToHtml : SortBy -> SortBy -> Html Msg
sortOptionToHtml current sortOption =
    Html.option
        [ Html.Attributes.value (sortToString sortOption)
        , Html.Attributes.selected (current == sortOption)
        ]
        [ Html.text (sortToString sortOption) ]
