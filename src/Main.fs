module Index

open Elmish
open Types
open Feliz
open Fable.SimpleJson
open Fable.React
open DataLoading
open FileUpload

type Model = {
    FileUploadError: bool
    EditingName: bool
    EditingCode: bool
    NameInput: string
    JsonData: Json

}

type Msg =
    | UploadData of string
    | ChangeName of string
    | SetInput of string
    | ChangeNameEditMode of bool

let init () : Model * Cmd<Msg> =
    {
        FileUploadError = false
        EditingName = false
        NameInput = ""
        EditingCode = false
        JsonData = JNull},
    Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UploadData data ->
        let loadedDataOption = loadJson data

        match loadedDataOption with
        | Some(data) ->
            match data with
            | JObject obj ->
                {
                    model with
                        FileUploadError = false
                        JsonData = data
                },
                Cmd.none
            | _ -> { model with FileUploadError = true }, Cmd.none
        | None -> { model with FileUploadError = true }, Cmd.none
    | ChangeName newName ->
        {
            model with
                NameInput = ""
                EditingName = false
        },
        Cmd.none
    | SetInput input -> { model with NameInput = input }, Cmd.none
    | ChangeNameEditMode value ->
        {
            model with
                EditingName = value
                NameInput = ""
        },
        Cmd.none



let view (model: Model) (dispatch: Msg -> unit) =

    let upploadButtonView onLoad =
        Html.div [
            prop.className "mt-10 w-full h-full flex items-center justify-center"
            prop.children [
                Html.div [
                    prop.className "flex justify-center"
                    prop.children [
                        Html.label [
                            prop.children [
                                Html.input [
                                    prop.type' "file"
                                    prop.name "component-data"
                                    prop.onChange (handleFileEvent onLoad)
                                    prop.className "hidden"
                                ]
                                Html.span [
                                    prop.className
                                        "px-8 py-4 bg-blue-500 text-white rounded cursor-pointer hover:bg-blue-700 transition duration-200 ease-in-out text-xl"
                                    prop.children [ Html.text "Select a file" ]
                                ]
                            ]
                        ]
                    ]
                ]
                if model.FileUploadError then
                    Html.div [
                        prop.className "mt-4 p-4 bg-red-500 text-white rounded"
                        prop.children [ Html.text "The selected file could not be used for creation." ]
                    ]
                else
                    Html.text ""
            ]
        ]


    let uploadButton = upploadButtonView (UploadData >> dispatch)
    let mainView =
        Html.div [ prop.className "mt-16 flex"; prop.children [  ] ]

    mainView