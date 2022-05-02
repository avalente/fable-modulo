module App

open System
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.Modulo

type MyType =
    | First
    | Second
    | Third

type MyOtherType =
    {
        Key : MyType
        Description : string
    }

type AutoField<'t> = FormFieldModel<AutoForm, 't>
and AutoForm =
    {
        String : AutoField<string>
        FloatOption : AutoField<float option>
        Checkbox : AutoField<bool>
        Choice : AutoField<MyOtherType>
        ChoiceOptional : AutoField<{| Key : int; Value : string |} option>
    }

type Model =
    {
        AutoForm : AutoForm
        AutoFormValidationError : string option
    }

type Msg = 
    | UpdateAutoForm of AutoForm
    | ValidateAutoForm

let transformValidation x =
    x |> Option.map (fun e -> e |> List.map(fun (k, v) -> sprintf "%s: %s" k v) |> String.concat ", ")
    
let init _ =
    let autoForm = 
        let inline f (value : Result<'t, string>) label = Auto.field'<AutoForm, 't> value label

        {
            String = f (Ok "") "A field taking a string"
            FloatOption = 
                f (Error "please fill me") "A float optional value" 
                |> FormFieldModel.withValidator (fun _ v -> match v with | None -> Ok None | Some v when v < 100.0 -> Ok (Some v) | _ -> Error "The value should be < 100")
                |> FormFieldModel.withPlaceholder "please insert a value < 100"
                |> FormFieldModel.withTooltip "This is a float input"
            Checkbox = f (Ok false) "A checkbox"
            Choice = Auto.select'<AutoForm, _> (Error "please select an item") [{Key = First; Description = "first option"}; {Key = Second; Description = "second option"}; {Key = Third; Description = "third option"}] (fun x -> x.Description) "Choice"
            ChoiceOptional = Auto.select'<AutoForm, _> (Ok None) [None; Some {|Key = 1; Value = "option 1"|}; Some {|Key = 2; Value = "option 2"|}] (fun x -> x |> Option.map (fun x -> x.Value) |> Option.defaultValue "<no choice>") "Choice optional"
        }
        |> Auto.initForm

    let model =
        {
            AutoForm = autoForm
            AutoFormValidationError = None
        }
    model, Cmd.none

let update msg model =
    match msg with
    | UpdateAutoForm newForm ->
        {model with AutoForm = newForm}, Cmd.none
    | ValidateAutoForm ->
        {model with AutoFormValidationError = validate model.AutoForm |> transformValidation}, Cmd.none

let view model dispatch =
    section [ClassName "section"] [
        h1 [ClassName "title"] [str "Fable.Modulo demo"]
        
        div [ClassName "columns"] [
            div [ClassName "column"] [
                div [ClassName "box"] [
                    h2 [ClassName "title is-5"] [str "Automatic form with Bulma styling"]
                    Auto.View.Bulma.form model.AutoForm (UpdateAutoForm >> dispatch) None [
                        button [ClassName "button is-link"; OnClick (fun e -> dispatch ValidateAutoForm; e.preventDefault())] [str "Validate"]
                    ]
                ]
            ]

            div [ClassName "column"] [
                div [ClassName "box"] [
                    h2 [ClassName "title is-5"] [str "Automatic form data"]

                    match model.AutoFormValidationError with
                    | None -> div [ClassName "help is-success"] [str "The form is valid"]
                    | Some e -> div [ClassName "help is-danger"] [sprintf "Error: %s" e |> str]

                    div [ClassName "content"] [
                        dl [] [
                            for name, value in Auto.debugForm model.AutoForm do
                                dt [] [str name]
                                dd [] [str value]
                        ]
                    ]
                ]
            ]
        ]
    ]

open Elmish.Debug

Program.mkProgram init update view
|> Program.withConsoleTrace
|> Program.withDebugger
|> Program.withReactSynchronous "react-main"
|> Program.run
