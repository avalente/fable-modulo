module App

open System
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.Modulo
open Browser.Types
open Browser


type MyType =
    | First
    | Second
    | Third

type MyOtherType =
    {
        Key : MyType
        Description : string
    }

type MyThirdType =
    {
        Id : int
        Value : string
    }

type AutoField<'t> = FormFieldModel<AutoForm, 't>
and AutoForm =
    {
        Id : AutoField<int>
        String : AutoField<string>
        FloatOption : AutoField<float option>
        Checkbox : AutoField<bool>
        Choice : AutoField<MyOtherType>
        ChoiceOptional : AutoField<{| Key : int; Value : string |} option>
        Custom : AutoField<MyThirdType>
    }

type Model =
    {
        AutoForm : AutoForm
        AutoFormValidationError : string option
    }

type Msg = 
    | UpdateAutoForm of AutoForm
    | ValidateAutoForm
    | Reset

let transformValidation x =
    x |> Option.map (fun e -> e |> List.map(fun (k, v) -> sprintf "%s: %s" k v) |> String.concat ", ")

let createForm () = 
    Auto.initForm {
        Id = input {
            value' 1
            label' "Id"
            disabled true
            size 1
        }
        String = input {
            raw_value (Ok "")
            label' "A field taking a string"
            size 5
        }
        FloatOption = input {
            error "please fill me"
            label "A float optional value" 
            validator (fun _ v -> match v with | None -> Ok None | Some v when v < 100.0 -> Ok (Some v) | _ -> Error "The value should be < 100")
            placeholder "please insert a value < 100"
            tooltip
        }
        Checkbox = checkbox {
            value false
            label "A checkbox"
        }
        Choice = select {
            error "please select an item"
            values [
                {Key = First; Description = "first option"}
                {Key = Second; Description = "second option"}
                {Key = Third; Description = "third option"}
            ]
            value_label (fun x -> x.Description)
            label "Choice"
        }
        ChoiceOptional = select {
            value None
            values (Helpers.optionValues [{|Key = 1; Value = "option 1"|}; {|Key = 2; Value = "option 2"|}])
            value_label (fun x -> x |> Option.map (fun x -> x.Value) |> Option.defaultValue "<no choice>")
            label "Choice optional"
        }
        Custom = input {
            error "please fill me"
            label "Custom"
            parser (fun x -> 
                let re = Text.RegularExpressions.Regex("(.*)?\s+(\d+)")
                let m = re.Match x
                if m.Success then
                    Ok {Id = Int32.Parse m.Groups.[2].Value; Value = m.Groups.[1].Value}
                else
                    Error "invalid value. try a string followed by a space followed by a number, e.g. test 1"
            )
            formatter (fun x -> string x)
        } 
    }

let init _ =
    let autoForm = createForm()

    let model =
        {
            AutoForm = autoForm
            AutoFormValidationError = validate autoForm |> transformValidation
        }
    model, Cmd.none

let update msg model =
    match msg with
    | UpdateAutoForm newForm ->
        {model with AutoForm = newForm}, Cmd.none
    | ValidateAutoForm ->
        {model with AutoFormValidationError = validate model.AutoForm |> transformValidation}, Cmd.none
    | Reset ->
        let newForm = createForm()
        {model with AutoForm = newForm}, Cmd.none

let view model dispatch =
    section [ClassName "section"] [
        h1 [ClassName "title"] [str "Fable.Modulo demo"]
        
        div [ClassName "columns"] [
            div [ClassName "column"] [
                div [ClassName "box"] [
                    h2 [ClassName "title is-5"] [str "Automatic form with Bulma styling"]
                    let custom =
                        Map.empty
                        |> Map.add "Custom" (fun form (field : Auto.View.Field) ->
                            Standard.input [
                                ClassName "input"
                                Props.Value (View.defaultValue model.AutoForm.Custom)
                                Props.OnChange (View.onChange model.AutoForm model.AutoForm.Custom (UpdateAutoForm >> dispatch))
                            ]
                        )
                    Auto.View.Bulma.customForm model.AutoForm (UpdateAutoForm >> dispatch) (Some "is-small") custom [
                        button [ClassName "button is-link"; OnClick (fun e -> dispatch ValidateAutoForm; e.preventDefault())] [str "Validate"]
                        button [ClassName "button is-warning"; OnClick (fun e -> dispatch Reset; e.preventDefault())] [str "Reset"]
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
