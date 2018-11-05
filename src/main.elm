module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, br, div, input, text, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Dice =
    Int

type alias Weapon =
    { dice : Dict Dice Int 
    , bonus : Int
    }

type alias Model =
    { enemyAC : Int
    , characterDC : Int
    , acInputString : String
    , dcInputString : String
    , strengthModifier : Int
    , weapon : Weapon
    , attacksPerTurn : Int
    }

type alias HitChance = 
    { normal : Float
    , advantage : Float
    , disadvantage : Float
    }

type alias Damage = 
    { normal : Float
    , advantage : Float
    , disadvantage : Float
    }

type alias CalculatedStats = 
    { hitChance : HitChance
    , averageDamage : Damage
    , averageDamagePerTurn : Damage
    }


init : Model
init =
    { enemyAC = 15
    , characterDC = 7
    , acInputString = ""
    , dcInputString = ""
    , strengthModifier = 5
    , weapon = { dice = Dict.singleton 6 2, bonus = 0 }
    , attacksPerTurn = 2
    }



-- UPDATE


type Msg
    = ChangeDCInput String
    | ChangeACInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeDCInput newInput ->
            { model | dcInputString = newInput, characterDC = newInput |> maybeStringToInt }

        ChangeACInput newInput ->
            { model | acInputString = newInput, enemyAC = newInput |> maybeStringToInt }



-- VIEW


renderInputs model =
    div []
        [ text "Character DC "
        , input [ placeholder (String.fromInt model.characterDC), value model.dcInputString, onInput ChangeDCInput ] []
        , text " | "
        , text "Enemy AC "
        , input [ placeholder (String.fromInt model.enemyAC), value model.acInputString, onInput ChangeACInput ] []
        ]


renderStatistics calculatedHitChance =
    div []
        [ text "Hit chance: "
        , span [style "color" "red"] [ text (calculatedHitChance.disadvantage |> decimalToPercentString)]
        , text " "
        , span [style "color" "black"] [ text (calculatedHitChance.normal |> decimalToPercentString)]
        , text " "
        , span [style "color" "green"] [ text (calculatedHitChance.advantage |> decimalToPercentString)]
        ]


renderDamage : Model -> HitChance -> Html Msg
renderDamage model calculatedHitChance =
    let
        dc =
            calculateCharacterDc model.characterDC model.weapon

        averageHitDamage =
            calculateAverageDamage model.weapon model.strengthModifier

        averageDamageWithAttack = 
            { normal = averageHitDamage * calculatedHitChance.normal
            , advantage = averageHitDamage * calculatedHitChance.advantage
            , disadvantage = averageHitDamage * calculatedHitChance.disadvantage
            }
    in
    div []
        [ Html.table []
            [ Html.tr []
                [ text ("Damage " ++ weaponDamageToString model.weapon ++ " + " ++ String.fromInt (model.strengthModifier + model.weapon.bonus))
                ]
            , Html.tr []
                [ text ("Average damage " ++ (averageHitDamage |> floatToString))
                ]
            , Html.tr []
                [ text "Average damage with attack: "
                , span [style "color" "red"] [ text (averageDamageWithAttack.disadvantage |> floatToString)]
                , text " : "
                , span [style "color" "black"] [ text (averageDamageWithAttack.normal |> floatToString)]
                , text " : "
                , span [style "color" "green"] [ text (averageDamageWithAttack.advantage |> floatToString)]
                ]
            , Html.tr []
                [ text "Average damage per turn: "
                , span [style "color" "red"] [ text (averageDamageWithAttack.disadvantage |> (*) (toFloat model.attacksPerTurn) |> floatToString)]
                , text " : "
                , span [style "color" "black"] [ text (averageDamageWithAttack.normal |> (*) (toFloat model.attacksPerTurn) |> floatToString)]
                , text " : "
                , span [style "color" "green"] [ text (averageDamageWithAttack.advantage |> (*) (toFloat model.attacksPerTurn) |> floatToString)]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        hitChance = 
            { normal = calculateHitChance model.enemyAC model.characterDC
            , advantage = calculateHitChanceWithAdvantage2 model.enemyAC model.characterDC
            , disadvantage = calculateHitChanceWithDisadvantage2 model.enemyAC model.characterDC
            }
    in

    div []
        [ renderInputs model
        , renderStatistics hitChance
        , renderDamage model hitChance
        ]



-- Logic
-- Source of calculations: https://rpg.stackexchange.com/a/112498


calculateHitChanceWithAdvantage : Int -> Int -> Int
calculateHitChanceWithAdvantage ac dc =
    (1 - (toFloat (ac - dc - 1) ^ 2 / 400)) * 100 |> round


calculateHitChanceWithDisadvantage : Int -> Int -> Int
calculateHitChanceWithDisadvantage ac dc =
    toFloat (21 + dc - ac) ^ 2 / 400 * 100 |> round


calculateHitChanceWithAdvantage2 ac dc =
    1 - ((1 - calculateHitChance ac dc) ^ 2)


calculateHitChanceWithDisadvantage2 ac dc =
    calculateHitChance ac dc * calculateHitChance ac dc


calculateHitChance : Int -> Int -> Float
calculateHitChance ac dc =
    toFloat (21 + dc - ac) / 20 |> clamp 0.05 0.95


calculateAverageRollForDice : Int -> Float
calculateAverageRollForDice dice =
    toFloat (dice + 1) / 2


calculateAverageDamageForWeapon : Weapon -> Float
calculateAverageDamageForWeapon weapon =
    weapon.dice |> Dict.foldl (\dice amount total -> calculateAverageRollForDice dice |> (*) (toFloat amount) |> (+) total) (toFloat weapon.bonus)


calculateAverageDamage : Weapon -> Int -> Float
calculateAverageDamage weapon modifier =
    calculateAverageDamageForWeapon weapon |> (+) (toFloat (modifier + weapon.bonus))


calculateAverageDamageWithHit : Float -> Float -> Float
calculateAverageDamageWithHit damage hitChance =
    damage * hitChance


calculateCharacterDc : Int -> Weapon -> Int
calculateCharacterDc base weapon =
    base + weapon.bonus


maybeStringToInt input =
    input |> String.toInt |> Maybe.withDefault 0


decimalToPercentString input =
    (input * 100 |> round |> String.fromInt) ++ "%"


floatToString : Float -> String
floatToString input =
    input * 10 |> round |> toFloat |> (\f -> f / 10) |> String.fromFloat


weaponDamageToString : Weapon -> String
weaponDamageToString weapon =
    weapon.dice |> Dict.foldr (\dice amount value -> value ++ String.fromInt amount ++ "d" ++ String.fromInt dice) ""
