module Store.Update exposing (..)

import Store.Commands
import Store.Msgs exposing (..)
import Store.Model exposing (Model)
import Materials.Model
import Units.Model


-- Here we handle the materials list, please check the units list above for comments


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadMaterials ->
            ( model, Store.Commands.loadMaterials () )

        MaterialAdded material ->
            let
                newMaterials =
                    case model.materials of
                        Nothing ->
                            Just [ (Materials.Model.parseMaterialFromDB material) ]

                        Just materials ->
                            Just (Materials.Model.parseMaterialFromDB material :: materials)
            in
                ( { model | materials = newMaterials }
                , Cmd.none
                )

        MaterialUpdated updatedMaterial ->
            let
                newMaterials =
                    case model.materials of
                        Nothing ->
                            Nothing

                        Just materials ->
                            Just (List.map (Materials.Model.updateMaterialAtId (Materials.Model.parseMaterialFromDB updatedMaterial) updatedMaterial.id) materials)
            in
                ( { model | materials = newMaterials }
                , Cmd.none
                )

        MaterialRemoved id ->
            let
                newMaterials =
                    case model.materials of
                        Nothing ->
                            Nothing

                        Just materials ->
                            Just (List.filter (\material -> material.id /= id) materials)
            in
                ( { model | materials = newMaterials }
                , Cmd.none
                )

        LoadUnits ->
            ( model, Store.Commands.loadUnits () )

        UnitAdded unit ->
            let
                newUnits =
                    case model.units of
                        Nothing ->
                            Just [ (Units.Model.parseUnitFromDB unit) ]

                        Just units ->
                            Just (Units.Model.parseUnitFromDB unit :: units)
            in
                ( { model | units = newUnits }
                , Cmd.none
                )

        UnitUpdated updatedUnit ->
            let
                newUnits =
                    case model.units of
                        Nothing ->
                            Nothing

                        Just units ->
                            Just (List.map (Units.Model.updateUnitAtId (Units.Model.parseUnitFromDB updatedUnit) updatedUnit.id) units)
            in
                ( { model | units = newUnits }
                , Cmd.none
                )

        UnitRemoved id ->
            let
                newUnits =
                    case model.units of
                        Nothing ->
                            Nothing

                        Just units ->
                            Just (List.filter (\unit -> unit.id /= id) units)
            in
                ( { model | units = newUnits }
                , Cmd.none
                )



-- otherwise we just pass the notification to unit module
