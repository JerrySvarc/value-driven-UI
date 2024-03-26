module Index

open Elmish
open Types
open Feliz
open Fable.SimpleJson

open DataLoading
open FileUpload

type Card = { x: float; y: float; width: float; height: float; }
type ComponentItem = { card : Card; uiComponent : Component }

type Model = {
    FileUploadError: bool
    EditingName: bool
    EditingCode: bool
    NameInput: string
    JsonData: Json
    Components: ComponentItem list
    IsDragging: bool
    OffsetX: float
    OffsetY: float
    MenuVisible: bool
    MenuPosition: Card
    ItemIndex: int option
}

type Msg =
    | UploadData of string
    | ChangeName of string
    | SetInput of string
    | ChangeNameEditMode of bool
    | MouseDown of float * float
    | MouseMove of float * float
    | MouseUp
    | CanvasClick of float * float
    | CreateRectangle of ComponentItem

let init () : Model * Cmd<Msg> =
    {
        FileUploadError = false
        EditingName = false
        NameInput = ""
        EditingCode = false
        JsonData = JNull
        MenuVisible = false
        MenuPosition = { x = 0.0; y = 0.0; width = 0.0; height = 0.0; }
        Components = []
        IsDragging = false
        OffsetX = 0.0
        OffsetY = 0.0
        ItemIndex = None
    },
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
    | CreateRectangle rect ->
        { model with MenuVisible = false; Components = rect :: model.Components }, Cmd.none
    | CanvasClick (x, y) ->
        if not model.IsDragging then
            { model with MenuVisible = true; MenuPosition = { x = x; y = y; width = 100.0; height = 100.0 } }, Cmd.none
        else
            model, Cmd.none
    | MouseDown (x, y) ->
        let componentOption =
            model.Components
            |> List.indexed
            |> List.tryFind (fun (i, rect) -> x > rect.card.x && x < rect.card.x + rect.card.width && y > rect.card.y && y < rect.card.y + rect.card.height)
            |> Option.map fst

        match componentOption with
        | Some componentIndex ->
            let comp = model.Components.[componentIndex]
            { model with IsDragging = true; OffsetX = x - comp.card.x; OffsetY = y - comp.card.y; ItemIndex = Some componentIndex }, Cmd.none
        | None -> model, Cmd.none
    | MouseMove (x, y) ->
        if model.IsDragging then
            match model.ItemIndex with
            | Some componentIndex ->
                let updatedRectangles =
                    model.Components
                    |> List.indexed
                    |> List.map (fun (i, rect) ->
                        if i = componentIndex then
                            { rect with card.x = x - model.OffsetX; card.y = y - model.OffsetY }
                        else
                            rect
                    )

                { model with Components = updatedRectangles }, Cmd.none
            | None -> model, Cmd.none
        else
            model, Cmd.none
    | MouseUp ->
        { model with IsDragging = false; ItemIndex = None }, Cmd.none

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

        Html.div [
            prop.style [
                style.width (length.percent 100)
                style.height (length.percent 100)
                style.position.absolute
            ]
            prop.onContextMenu (fun e ->
                e.preventDefault()
                dispatch (CanvasClick (e.clientX, e.clientY))
            )
            prop.onMouseUp (fun _ -> dispatch MouseUp)
            prop.onMouseMove (fun e ->
                if model.IsDragging then
                    dispatch (MouseMove (e.clientX, e.clientY))
            )
            prop.children ([
                uploadButton
                Html.div [
                    prop.style [
                        style.width (length.px (int model.MenuPosition.width))
                        style.height (length.px (int model.MenuPosition.height))
                        style.position.absolute
                        style.left (length.px (int model.MenuPosition.x))
                        style.top (length.px (int model.MenuPosition.y))
                        style.backgroundColor "grey"
                        if model.MenuVisible then style.display.block else style.display.none
                    ]
                    prop.children [
                        Html.button [
                            prop.text "Create component"
                            prop.onMouseDown (fun e ->
                                e.stopPropagation()
                                e.preventDefault()

                            )
                        ]
                    ]
                ]
            ] @ (model.Components |> List.mapi (fun i rect ->
                Html.div [
                    prop.style [
                        style.width (length.px (int rect.card.width))
                        style.height (length.px (int rect.card.height))
                        style.position.absolute
                        style.left (length.px (int rect.card.x))
                        style.top (length.px (int rect.card.y))
                        style.backgroundColor "red"
                    ]
                    prop.onMouseDown (fun e ->
                        e.stopPropagation()
                        dispatch (MouseDown (e.clientX, e.clientY))
                    )
                ]
            )))
        ]
    mainView