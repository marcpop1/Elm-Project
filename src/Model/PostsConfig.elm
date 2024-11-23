module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time


type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


{-|

    sortFromString "Score" --> Just Score

    sortFromString "Invalid" --> Nothing

    sortFromString "Title" --> Just Title

-}
sortFromString : String -> Maybe SortBy
sortFromString sort =
    case sort of
        "Score" ->
            Just Score

        "Title" ->
            Just Title

        "Posted" ->
            Just Posted

        "None" ->
            Just None

        _ ->
            Nothing


sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }


defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True


{-| A type that describes what option changed and how
-}
type Change
    = SetPostsToShow Int
    | SetSortBy SortBy
    | SetShowJobs Bool
    | SetShowTextOnly Bool


{-| Given a change and the current configuration, return a new configuration with the changes applied
-}
applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges change config =
    case change of
        SetPostsToShow postsToShow ->
            { config | postsToShow = postsToShow }

        SetSortBy sortBy ->
            { config | sortBy = sortBy }

        SetShowJobs showJobs ->
            { config | showJobs = showJobs }

        SetShowTextOnly showTextOnly ->
            { config | showTextOnly = showTextOnly }


{-| Given the configuration and a list of posts, return the relevant subset of posts according to the configuration

Relevant local functions:

  - sortToCompareFn

Relevant library functions:

  - List.sortWith

-}
filterPosts : PostsConfig -> List Post -> List Post
filterPosts config posts =
    let
        -- Step 1: Filter out posts based on showTextOnly (remove posts with no URL)
        postsWithUrl =
            if config.showTextOnly then
                posts
            else
                List.filter (\post -> case post.url of
                                        Just _ -> True
                                        Nothing -> False
                                      ) posts

        -- Step 2: Filter out job posts based on showJobs (remove posts with type "job")
        nonJobPosts =
            if config.showJobs then
                postsWithUrl
            else
                List.filter (\post -> post.type_ /= "job") postsWithUrl

        -- Step 3: Limit the posts to the number specified by postsToShow
        limitedPosts = List.take config.postsToShow nonJobPosts

        -- Step 4: Sort the posts based on the sortBy configuration
        sortedPosts = List.sortWith (sortToCompareFn config.sortBy) limitedPosts
    in
    sortedPosts