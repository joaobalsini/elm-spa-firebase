module Store.Update exposing (..)

import Store.Commands
import MessageModule exposing (Message, initMessage, errorMessage, successMessage)
import Store.Msgs exposing (..)
import Store.Model exposing (Model)
import Materials.Model
import Units.Model


-- Here we handle the materials list, please check the units list above for comments


update : Msg -> Model -> ( Model, Cmd Msg, Message )
update msg model =
    case msg of
        LoadMaterials ->
            ( model, Store.Commands.loadMaterials (), initMessage )

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
                , successMessage "Material succesfully added"
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
                , successMessage "Material succesfully updated"
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
                , successMessage "Material succesfully removed"
                )

        LoadUnits ->
            ( model, Store.Commands.loadUnits (), initMessage )

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
                , successMessage "Unit succesfully added"
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
                , successMessage "Unit succesfully updated"
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
                , successMessage "Unit succesfully removed"
                )



-- otherwise we just pass the message to unit module
