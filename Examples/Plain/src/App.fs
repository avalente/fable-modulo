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

module MyType =
    let parse (x : string) =
        match x.ToLower() with
        | "first" -> Ok First
        | "second" -> Ok Second
        | "third" -> Ok Third
        | e -> Error (sprintf "Invalid value: '%s' (try 'first', 'second' or 'third')" e)

    let formatter (x : MyType) = string x

type Input<'t> = FormInputModel<Form, 't>
and Select<'t> = FormSelectModel<Form, 't>
and Checkbox = FormCheckboxModel<Form>
and Form =
    {
        String : Input<string>
        StringOption : Input<string option>
        StringWithValidator : Input<string>
        Int : Input<int>
        Float : Input<float>
        DateOption : Input<DateTime option>
        Custom : Input<MyType>
        NotAField : Option<unit>
        DateTime : Input<DateTimeOffset>
        Bool : Checkbox
        Choice : Select<MyType>
        ChoiceOption : Select<MyType option>
        ChoiceComplex : Select<MyOtherType>
    }

type Model =
    {
        Form : Form
        ValidationError : string option
    }

type Msg = 
    | UpdateForm of Form

let transformValidation x =
    x |> Option.map (fun e -> e |> List.map(fun (k, v) -> sprintf "%s: %s" k v) |> String.concat ", ")
    
let init _ =
    let form =
        {
            String = FormInputModel.stringField (Ok "") (fun item form -> {form with String = item})
            StringOption = FormInputModel.stringOptionField (Ok None) (fun item form -> {form with StringOption = item})
            StringWithValidator = 
                FormInputModel.stringField (Ok "") (fun item form -> {form with StringWithValidator = item})
                |> FormInputModel.withValidator (fun form value -> 
                    if value |> Seq.forall(Char.IsUpper) then
                        Ok value
                    else
                        Error "this field should be UPPERCASE"
                )
            Int = FormInputModel.intField (Ok 0) (fun item form -> {form with Int = item})
            Float = FormInputModel.floatField (Error "please fill me") (fun item form -> {form with Float = item})
            DateOption = FormInputModel.dateOptionField (Ok None) (fun item form -> {form with DateOption = item})
            Custom = FormInputModel.create' (Error "please fill me") MyType.parse MyType.formatter (fun item form -> {form with Custom = item})
            NotAField = None
            DateTime = FormInputModel.dateTimeField (Error "please fill me") (fun item form -> {form with DateTime = item})
            Bool = FormCheckboxModel.create (Ok false) (fun item form -> {form with Bool = item})
            Choice = FormSelectModel.createOfDU (Error "choose one item") typeof<MyType> (fun item form -> {form with Choice = item})
            ChoiceOption = 
                FormSelectModel.create 
                    (Ok None) 
                    [None; Some First; Some Second; Some Third] 
                    (function | None -> ""; | Some First -> "first item" | Some Second -> "second item" | Some Third -> "third item") 
                    (fun item form -> {form with ChoiceOption = item})
            ChoiceComplex = 
                FormSelectModel.create
                    (Error "select an option")
                    [{Key = First; Description = "first option"}; {Key = Second; Description = "second option"}; {Key = Third; Description = "third option"}]
                    (fun x -> x.Description)
                    (fun item form -> {form with ChoiceComplex = item})
        }

    let model =
        {
            Form = form
            ValidationError = validate form |> transformValidation
        }
    model, Cmd.none

let update msg model =
    match msg with
    | UpdateForm newForm ->
        {model with Form = newForm; ValidationError = validate newForm |> transformValidation}, Cmd.none

let formatField (item : IFormFieldModel<_, _>) =
    match item.FieldValue with
    | Error e -> 
        span [Style [Color "red"]] [str (sprintf "Error: %s" e)]
    | Ok x -> 
        span [] [str (sprintf "Ok: %A" x)]

// this is a custom component used to input a date time with time zone
module CustomComponent =
    type Props<'f> =
        {
            Item : FormInputModel<'f, DateTimeOffset>
            Form : 'f
            MessageDispatcher : 'f -> unit
        }

    type private SubForm =
        {
            Date : FormInputModel<SubForm, DateTime>
            Time : FormInputModel<SubForm, TimeSpan>
            Tz : FormInputModel<SubForm, float>
        }

    let private buildCustom<'f> = FunctionComponent.Of (fun (props : Props<'f>) ->
        let item = props.Item

        let miniForm = 
            Hooks.useState
                {
                    Date = FormInputModel.dateField (match item.Value with | Error e -> Error e | Ok x -> Ok x.Date) (fun item form -> {form with Date = item})
                    Time = FormInputModel.timeSpanField (match item.Value with | Error e -> Error e | Ok x -> Ok x.TimeOfDay) (fun item form -> {form with Time = item})
                    Tz = FormInputModel.floatField (match item.Value with | Error e -> Error e | Ok x -> Ok x.Offset.TotalHours) (fun item form -> {form with Tz = item})
                }

        let state = Hooks.useState item.Value

        let updateModel (form : SubForm) =
            miniForm.update form

            let newValue =
                match form with
                | { Date = FieldValue date; Time = FieldValue time; Tz = FieldValue tz } ->
                    DateTimeOffset(date.Year, date.Month, date.Day, time.Hours, time.Minutes, time.Seconds, time.Milliseconds, TimeSpan.FromHours tz) |> Ok
                | { Date = FieldError e } -> Error (sprintf "invalid date part")
                | { Time = FieldError e } -> Error (sprintf "invalid time part")
                | { Tz = FieldError e } -> Error (sprintf "invalid timezone part")
                | _ -> Error "invalid date/time"

            state.update newValue

            (item |> FormInputModel.updateValue newValue).UpdateForm props.Form
            |> props.MessageDispatcher

        div [Style [Display DisplayOptions.Flex; FlexDirection "column"]] [
            div [Style [Display DisplayOptions.Flex; FlexDirection "row"]] [
                match item.Layout.Label with 
                | None -> ()
                | Some l -> label [] [str l]

                View.basicField miniForm.current miniForm.current.Date updateModel
                View.basicField miniForm.current miniForm.current.Time updateModel
                View.basicField miniForm.current miniForm.current.Tz updateModel
            ]
            div [Style [Color "red"]] [
                match state.current with
                | Error e -> str e
                | Ok _ -> ()
            ]
        ]
    )

    let custom<'f> (form : 'f) (item : FormInputModel<'f, DateTimeOffset>) (messageDispatcher : 'f -> unit) = 
        buildCustom<'f> {Form = form; Item = item; MessageDispatcher = messageDispatcher}

let view model dispatch =
    section [] [
        h3 [] [str "Fable.Modulo demo"]
        
        div [Style [Display DisplayOptions.Grid; GridTemplateColumns "1fr 1fr"]] [
            div [] [
                div [] [
                    fieldset [] [
                        legend [] [str "String field"]

                        // this one doesn't use the helpers in View
                        input [
                            Value model.Form.String.Text
                            OnChange (View.onChange model.Form model.Form.String (UpdateForm >> dispatch))
                            AutoFocus true
                        ]
                    ]
                    fieldset [] [
                        legend [] [str "String option field"]

                        View.basicField model.Form model.Form.StringOption (UpdateForm >> dispatch)
                    ]
                    fieldset [] [
                        legend [] [str "String field with validator"]

                        View.basicField model.Form model.Form.StringWithValidator (UpdateForm >> dispatch)
                    ]
                    fieldset [] [
                        legend [] [str "Integer field"]

                        View.basicField model.Form model.Form.Int (UpdateForm >> dispatch)
                    ]
                    fieldset [] [
                        legend [] [str "Float field"]

                        View.basicField model.Form model.Form.Float (UpdateForm >> dispatch)
                    ]
                    fieldset [] [
                        legend [] [str "Date option field"]

                        View.basicField model.Form model.Form.DateOption (UpdateForm >> dispatch)
                    ]
                    fieldset [] [
                        legend [] [str "Custom field"]

                        View.basicField model.Form model.Form.Custom (UpdateForm >> dispatch)
                    ]
                    fieldset [] [
                        legend [] [str "DateTimeOffset field with custom component"]

                        CustomComponent.custom model.Form model.Form.DateTime (UpdateForm >> dispatch)
                    ]
                    fieldset [] [
                        legend [] [str "Bool field implemented by checkbox"]

                        View.basicCheckbox model.Form model.Form.Bool (UpdateForm >> dispatch)
                    ]

                    fieldset [] [
                        legend [] [str "Choice field implemented by select"]

                        View.basicSelect model.Form model.Form.Choice (UpdateForm >> dispatch)
                    ]

                    fieldset [] [
                        legend [] [str "Choice field optional"]

                        View.basicSelect model.Form model.Form.ChoiceOption (UpdateForm >> dispatch)
                    ]

                    fieldset [] [
                        legend [] [str "Choice field with complex data"]

                        View.basicSelect model.Form model.Form.ChoiceComplex (UpdateForm >> dispatch)
                    ]

                ]
            ]

            div [] [
                fieldset [] [
                    legend [] [str "Form data"]

                    match model.ValidationError with
                    | None -> span [Style [Color "green"]] [str "The form is valid!"]
                    | Some e -> span [Style [Color "red"]] [str e]

                    dl [] [
                        dt [] [str "String"]
                        dd [] [model.Form.String |> formatField]

                        dt [] [str "String option"]
                        dd [] [model.Form.StringOption |> formatField]

                        dt [] [str "String with validator"]
                        dd [] [model.Form.StringWithValidator |> formatField]

                        dt [] [str "Int"]
                        dd [] [model.Form.Int |> formatField]

                        dt [] [str "Float"]
                        dd [] [model.Form.Float |> formatField]

                        dt [] [str "Date option"]
                        dd [] [model.Form.DateOption |> formatField]

                        dt [] [str "Custom"]
                        dd [] [model.Form.Custom |> formatField]

                        dt [] [str "DateTime custom"]
                        dd [] [model.Form.DateTime |> formatField]

                        dt [] [str "Bool checkbox"]
                        dd [] [model.Form.Bool |> formatField]

                        dt [] [str "Choice select"]
                        dd [] [model.Form.Choice |> formatField]

                        dt [] [str "Choice option"]
                        dd [] [model.Form.ChoiceOption |> formatField]

                        dt [] [str "Choice with complex data"]
                        dd [] [model.Form.ChoiceComplex |> formatField]
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
