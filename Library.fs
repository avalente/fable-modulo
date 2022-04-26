module Fable.Modulo

open System
open Fable.React
open Fable.React.Props
open Browser.Types

/// Helpers to parse the string inserted by the user
module Parsers =
    /// Returns Ok None if the string is empty, else wraps the given parser's value in Option
    let optionParser parser x =
        if String.IsNullOrWhiteSpace x then Ok None
        else parser x |> Result.map Some

    /// Parse a string. Always successful.
    let string x = Ok x

    /// Parse an optional string. Always successful.
    let stringOption = optionParser string

    /// Try to parse a 32-bit signed integer
    let int (x : string) =
        match Int32.TryParse x with
        | false, _ -> Error "Invalid integer"
        | true, x -> Ok x


    let intOption = optionParser int

    /// Try to parse a double precision floating-point number
    let float (x : string) =
        match Double.TryParse x with
        | false, _ -> Error "Invalid decimal number"
        | true, x -> Ok x

    let floatOption = optionParser float

    /// Try to parse the date part of a datetime object. Depends on the current CultureInfo.
    let date (x : string) =
        match DateTime.TryParse x with
        | false, _ -> Error "Invalid date"
        | true, x -> Ok x.Date

    let dateOption = optionParser date

    /// Try to parse a timespan object object. Depends on the current CultureInfo.
    let timeSpan (x : string) =
        match TimeSpan.TryParse x with
        | false, _ -> Error "Invalid time"
        | true, x -> Ok x

    let timeSpanOption = optionParser timeSpan

    /// Try to parse a datetime with a timezone. Depends on the current CultureInfo.
    let dateTime (x : string) =
        match DateTimeOffset.TryParse x with
        | false, _ -> Error "Invalid datetime"
        | true, x -> Ok x

    let dateTimeOption = optionParser dateTime

    let bool (x : string) : Result<bool, string> =
        match x.ToLower() with
        | "on" | "true" -> Ok true
        | _ -> Ok false

    let boolOption = optionParser bool

/// Helpers to transform a given object to a string representation
module Formatters =
    let string (x : string) = x

    let int (x : int) = x.ToString()

    let float (x : double) = x.ToString()

    let date (x : DateTime) = x.ToShortDateString()

    let timeSpan (x : TimeSpan) = x.ToString()
    
    let dateTime (x : DateTimeOffset) = x.ToString()

    let bool (x : bool) =
        if x then "on" else "off"

    let option<'t> (f : 't -> string) (x : 't option) =
        match x with 
        | None -> ""
        | Some x -> f x

/// Interface for a form's field model
type IFormFieldModel<'f, 't> =
    abstract FieldValue : Result<'t, string>
    abstract UpdateForm : 'f -> 'f
    abstract Validate : 'f -> Result<'t, string> -> Result<'t, string>

/// The form field model, used for "input" elements
// in order to keep the implementation as simple as possible, the "fixed" and "runtime" parts
// of the model are kept in the same structure. In particular:
// - Text and Value change as response to the user's actions
// - the other fields are set at initialization time and should never change
type FormFieldModel<'f, 't> =
    {
        /// Input text
        Text : string
        /// Parsed and validated value
        Value : Result<'t, string>
        /// Parser for the given type
        Parser : string -> Result<'t, string>
        /// A function that updates the form with the current field. Usually just something such as (fun item form -> {form with MyItem = item})
        Updater : FormFieldModel<'f, 't> -> 'f -> 'f
        /// Gets a string representation of the underlying value
        Formatter : 't -> string
        /// Validate the field's value, possibly against the whole form
        Validator : ('f -> 't -> Result<'t, string>) option
        /// The field's label. Useful for auto-generation
        Label : string option
    }

    interface IFormFieldModel<'f, 't> with
        member self.FieldValue = self.Value

        member self.Validate form value = 
            match self.Validator with
            | None -> value
            | Some f -> value |> Result.bind (f form)

        member self.UpdateForm form =
            let item = {self with Value = self.Value |> (self :> IFormFieldModel<'f, 't>).Validate form}
            item.Updater item form

/// Model for a "checkbox" element.
type FormCheckboxModel<'f> =
    {
        Value : Result<bool, string>
        Updater : FormCheckboxModel<'f> -> 'f -> 'f
        Validator : ('f -> bool -> Result<bool, string>) option
        Label : string option
    }

    interface IFormFieldModel<'f, bool> with
        member self.FieldValue = self.Value

        member self.Validate form value = 
            match self.Validator with
            | None -> value
            | Some f -> value |> Result.bind (f form)

        member self.UpdateForm form =
            let item = {self with Value = self.Value |> (self :> IFormFieldModel<'f, bool>).Validate form}
            item.Updater item form

/// Model for a "select"-like element
type FormListModel<'f, 't> =
    {
        /// The available values
        Values : 't[]
        /// A function to get the user-facing string representation of a value
        ValueLabel : 't -> string
        /// The current item's value
        Value : Result<'t, string>
        /// A function used to serialize the value to a string that is stored in the underlying "select"/"option"
        Serializer : 't -> string
        /// A function used to deserialize the string stored in the underlying "select"/"option"
        Deserializer : string -> 't
        /// Gets a string representation of the underlying value
        Updater : FormListModel<'f, 't> -> 'f -> 'f
        /// Validate the field's value, possibly against the whole form
        Validator : ('f -> 't -> Result<'t, string>) option
        /// The field's label. Useful for auto-generation
        Label : string option
    }

    interface IFormFieldModel<'f, 't> with
        member self.FieldValue = self.Value

        member self.Validate form value = 
            match self.Validator with
            | None -> value
            | Some f -> value |> Result.bind (f form)

        member self.UpdateForm form =
            let item = {self with Value = self.Value |> (self :> IFormFieldModel<'f, 't>).Validate form}
            item.Updater item form

module FormFieldModel =
    let create<'f, 't> text (value : Result<'t, string>) parser formatter (updater : FormFieldModel<'f, 't> -> 'f -> 'f) =
        {
            Text = text
            Value = value
            Parser = parser
            Updater = updater
            Formatter = formatter
            Validator = None
            Label = None
        }

    let create'<'f, 't> (value : Result<'t, string>) parser formatter (updater : FormFieldModel<'f, 't> -> 'f -> 'f) =
        create (match value with | Error _ -> "" | Ok x -> formatter x) value parser formatter updater

    let withValidator validator (item : FormFieldModel<_, _>) = {item with Validator = Some validator}
    let withLabel label (item : FormFieldModel<_, _>) = {item with Label = Some label}

    // constructors

    let stringField initialValue updater =
        create' initialValue Parsers.string Formatters.string updater

    let stringOptionField initialValue updater =
        create' initialValue Parsers.stringOption (Formatters.option Formatters.string) updater

    let intField initialValue updater =
        create' initialValue Parsers.int Formatters.int updater

    let intOptionField initialValue updater =
        create' initialValue Parsers.intOption (Formatters.option Formatters.int) updater

    let floatField initialValue updater =
        create' initialValue Parsers.float Formatters.float updater

    let floatOptionField initialValue updater =
        create' initialValue Parsers.floatOption (Formatters.option Formatters.float) updater

    let dateField<'f> initialValue updater =
        create'<'f, _> initialValue Parsers.date Formatters.date updater

    let dateOptionField initialValue updater =
        create' initialValue Parsers.dateOption (Formatters.option Formatters.date) updater

    let timeSpanField initialValue updater =
        create' initialValue Parsers.timeSpan Formatters.timeSpan updater

    let timeSpanOptionField initialValue updater =
        create' initialValue Parsers.timeSpanOption (Formatters.option Formatters.timeSpan) updater

    let dateTimeField initialValue updater =
        create' initialValue Parsers.dateTime Formatters.dateTime updater

    let dateTimeOptionField initialValue updater =
        create' initialValue Parsers.dateTimeOption (Formatters.option Formatters.dateTime) updater

    let boolField initialValue updater =
        create' initialValue Parsers.bool Formatters.bool updater
    
    let boolOptionField initialValue updater =
        create' initialValue Parsers.boolOption (Formatters.option Formatters.bool) updater

    // helpers

    let inline updateForm<'f, 't> (form : 'f) (item : FormFieldModel<'f, 't>) text =
        let item' = {item with Text = text; Value = item.Parser text} 
        (item' :> IFormFieldModel<'f, 't>).UpdateForm form
    
module FormCheckboxModel =
    let create initialValue updater =
        {
            Value = initialValue
            Updater = updater
            Validator = None
            Label = None
        }

    let withValidator validator (item : FormCheckboxModel<_>) = {item with Validator = Some validator}
    let withLabel label (item : FormCheckboxModel<_>) = {item with Label = Some label}

module FormListModel =
    open Fable.Core.JS

    let inline create<'f, 't> initialValue (availableValues : seq<'t>) (labelForValue : 't -> string) (updater : FormListModel<'f, 't> -> 'f -> 'f) =
        {
            Values = availableValues |> Seq.toArray
            ValueLabel = labelForValue
            Value = initialValue
            Updater = updater
            Validator = None
            Label = None
            Serializer = JSON.stringify
            Deserializer = (fun x -> (if String.IsNullOrEmpty x then null else JSON.parse(x)) |> unbox<'t>)
        }

    let inline createOfDU<'f, 't> initialValue (duType : Type) updater =
        let values = 
            Reflection.FSharpType.GetUnionCases(duType)
            |> Seq.map (fun x ->
                if x.GetFields().Length > 0 then failwithf "Only plain discriminated unions are supported by now (e.g. cases without data)"
                Reflection.FSharpValue.MakeUnion(x, [||]) |> unbox<'t>
            )

        create<'f, 't> initialValue values (box >> string) updater

    let withValidator validator (item : FormListModel<_, _>) = {item with Validator = Some validator}
    let withLabel label (item : FormListModel<_, _>) = {item with Label = Some label}
    let withSerializer serializer item = {item with Serializer = serializer}
    let withDeserializer deserializer item = {item with Deserializer = deserializer}
    let withValueLebel f item = {item with ValueLabel = f}

// helpers

/// Extract the field's value in a pattern matching
/// Example:
/// match myForm with
/// | {MyField = FieldValue value} -> ...do something...
/// | _ -> ...do sometnihg else...
let (|FieldValue|_|) (x : IFormFieldModel<_, _>) =
    match x.FieldValue with
    | Ok x -> Some x
    | _ -> None

/// Extract the field's error in a pattern matching
/// Example:
/// match myForm with
/// | {MyField = FieldError value} -> ...do something...
/// | _ -> ...do sometnihg else...
let (|FieldError|_|) (x : IFormFieldModel<_, _>) =
    match x.FieldValue with
    | Error e -> Some e
    | _ -> None

/// Helpers for the view. Based on Fable.React: no css or other assumptions are made.
module View =
    let inline onChange<'f, 't> (form : 'f) (item : FormFieldModel<'f, 't>) (messageDispatcher : 'f -> unit) (ev : Event) = FormFieldModel.updateForm form item ev.Value |> messageDispatcher

    let fieldBase form item messageDispatcher (props : IHTMLProp list) =
        let props' = props @ [
            DefaultValue item.Text
            OnChange (onChange form item messageDispatcher)
        ] 
        input props'
        
    let basicField form item messageDispatcher =
        fieldBase form item messageDispatcher []

    let basicField' form item name messageDispatcher =
        fieldBase form item messageDispatcher [Name name] 

    let checkboxBase<'f> (form : 'f) (item : FormCheckboxModel<'f>) messageDispatcher (props : IHTMLProp list) =
        input [
            Type "checkbox"
            DefaultValue (match item.Value with | Ok x -> x | Error e -> false)
            OnChange (fun ev ->
                let item = {item with Value = Ok ev.Checked}
                (item :> IFormFieldModel<_, _>).UpdateForm form |> messageDispatcher
            )
        ]

    let selectBase<'f, 't> (form : 'f) (item : FormListModel<'f, 't>) messageDispatcher (selectProps : IHTMLProp list) (optionProps : IHTMLProp list) =
        let props' =  selectProps @ [
            DefaultValue (match item.Value with | Ok x -> item.Serializer x | Error e -> "")
            OnChange (fun ev -> 
                let value = item.Deserializer ev.Value
                let item = {item with Value = Ok value}
                (item :> IFormFieldModel<_, _>).UpdateForm form |> messageDispatcher
            )
        ]

        let optionProps' o = optionProps @ [
            Value (item.Serializer o)
        ]

        select props' [
            for o in item.Values do
                option (optionProps' o) [
                    str (item.ValueLabel o)
                ]
        ]

    let basicCheckbox<'f> (form : 'f) (item : FormCheckboxModel<'f>) messageDispatcher =
        checkboxBase form item messageDispatcher []
        
    let basicSelect<'f, 't> (form : 'f) (item : FormListModel<'f, 't>) messageDispatcher =
        selectBase form item messageDispatcher [] []
        
/// Validate the form. Return a list of couples (<field name>, <error message>) if there is at least one error, None if the form is valid
let inline validate<'f> (f : 'f) =
    let t = typeof<'f>
    if Reflection.FSharpType.IsRecord t |> not then
        failwithf "Only records are supported by now"

    let ft = typeof<FormFieldModel<_, _>>

    let fields =
        Reflection.FSharpType.GetRecordFields(t)
        |> Array.filter(fun x -> 
            let t = x.PropertyType
            // we know that out type is generic, so let's drop non generics
            if not t.IsGenericType then false
            else
                t.GetGenericTypeDefinition() = ft.GetGenericTypeDefinition()
        )

    let errors =
        fields
        |> Array.choose (fun pi ->
            let field = Reflection.FSharpValue.GetRecordField(f, pi) |> unbox<FormFieldModel<_, _>>
            match field.Value with
            | Ok _ -> None
            | Error x -> Some (pi.Name, x)
        )
        |> List.ofArray

    match errors with
    | [] -> None
    | e -> Some e

let inline isValid<'f> (f : 'f) =
    validate f |> Option.isNone

/// This module aims to reduce the form model's boilerplate by reflection. 
/// Currently only forms represented by plain records of "FormFieldModel" ("input" elements) are supported
module Auto =
    let inline field<'f, 't> (initialValue : Result<'t, string>) =
        if typeof<'t> = typeof<string> then
            FormFieldModel.stringField (initialValue |> box |> unbox<Result<string, string>>) (fun _ f -> f) |> box
        elif typeof<'t> = typeof<string option> then
            FormFieldModel.stringOptionField (initialValue |> box |> unbox<Result<string option, string>>) (fun _ f -> f) |> box
        elif typeof<'t> = typeof<int> then
            FormFieldModel.intField (initialValue |> box |> unbox<Result<int, string>>) (fun _ f -> f) |> box
        elif typeof<'t> = typeof<int option> then
            FormFieldModel.intOptionField (initialValue |> box |> unbox<Result<int option, string>>) (fun _ f -> f) |> box
        elif typeof<'t> = typeof<float> then
            FormFieldModel.floatField (initialValue |> box |> unbox<Result<float, string>>) (fun _ f -> f) |> box    
        elif typeof<'t> = typeof<option<float>> then
            FormFieldModel.floatOptionField (initialValue |> box |> unbox<Result<float option, string>>) (fun _ f -> f) |> box
        elif typeof<'t> = typeof<DateTime> then
            FormFieldModel.dateField (initialValue |> box |> unbox<Result<DateTime, string>>) (fun _ f -> f) |> box    
        elif typeof<'t> = typeof<option<DateTime>> then
            FormFieldModel.dateOptionField (initialValue |> box |> unbox<Result<DateTime option, string>>) (fun _ f -> f) |> box
        elif typeof<'t> = typeof<TimeSpan> then
            FormFieldModel.timeSpanField (initialValue |> box |> unbox<Result<TimeSpan, string>>) (fun _ f -> f) |> box    
        elif typeof<'t> = typeof<option<TimeSpan>> then
            FormFieldModel.timeSpanOptionField (initialValue |> box |> unbox<Result<TimeSpan option, string>>) (fun _ f -> f) |> box
        elif typeof<'t> = typeof<DateTimeOffset> then
            FormFieldModel.dateTimeField (initialValue |> box |> unbox<Result<DateTimeOffset, string>>) (fun _ f -> f) |> box    
        elif typeof<'t> = typeof<option<DateTimeOffset>> then
            FormFieldModel.dateTimeOptionField (initialValue |> box |> unbox<Result<DateTimeOffset option, string>>) (fun _ f -> f) |> box
        else
            failwithf "field type not supported yet: %s" (typeof<'t>.Name)
        |> unbox<FormFieldModel<'f, 't>>
        
    let inline field'<'f, 't> (initialValue : Result<'t, string>) label =
        field<'f, 't> initialValue |> FormFieldModel.withLabel label

    /// Automatically build an "updater" function
    let inline private autoUpdater<'f, 't> (pi : Reflection.PropertyInfo) (fields : Reflection.PropertyInfo[]) (field : FormFieldModel<'f, 't>) (form : 'f) =
        let values = 
            [|
                for f in fields do
                    if f.Name = pi.Name then
                        box field
                    else
                        Reflection.FSharpValue.GetRecordField(form, f)
            |]
        Reflection.FSharpValue.MakeRecord(typeof<'f>, values) |> unbox<'f>
    
    let inline private getFields<'f>(f : 'f) =
        let t = typeof<'f>
        let ft = typeof<FormFieldModel<_, _>>
    
        if Reflection.FSharpType.IsRecord t |> not then
            failwithf "Only records are supported by now"

        Reflection.FSharpType.GetRecordFields(t)
        |> Array.map(fun x -> 
            let t = x.PropertyType
            // we know that out type is generic, so let's drop non generics
            if not t.IsGenericType then failwithf "Unsupported field type"
            else
                if t.GetGenericTypeDefinition() <> ft.GetGenericTypeDefinition() then
                    failwithf "Unsupported field type"
                else
                    x
        )

    /// Initialize the form by attaching an "updater" to each field
    let inline initForm<'f>(f : 'f) =
        let fields = getFields<'f> f
    
        let fields' =
            fields
            |> Array.map (fun pi ->
                let field = Reflection.FSharpValue.GetRecordField(f, pi)
                let field = field |> unbox<FormFieldModel<_, _>>
                {field with Updater = (autoUpdater<'f, _> pi fields) |> box |> unbox} |> box
            )
        Reflection.FSharpValue.MakeRecord(typeof<'f>, fields') |> unbox<'f>

    module View =
        /// Return a list of (field name, (label element, input element)). The field name is the name of the form record field.
        let inline fields<'f> (f : 'f) messageDispatcher =
            [
                for pi in getFields<'f> f do
                    let field = Reflection.FSharpValue.GetRecordField(f, pi) |> unbox<FormFieldModel<_, _>>
                    let labelText = field.Label |> Option.defaultValue pi.Name
                    let label = label [HtmlFor pi.Name ] [str labelText]
                    let el = View.basicField' f field pi.Name messageDispatcher
                    pi.Name, (label, el)
            ]

        let inline form<'f> (f : 'f) messageDispatcher (extraElements : ReactElement seq) =
            form [] [
                for name, (label, element) in fields<'f> f messageDispatcher do
                    div [] [
                        label
                        element
                    ]
                for extra in extraElements do
                    extra
            ]

/// Builder for a FormFieldModel record
type FieldBuilder() =
    member self.Yield(_) =
        {
            Text = ""
            Value = Error "no value specified"
            Parser = fun _ -> Error "no parser specified"
            Updater = fun _ f -> f
            Formatter = string
            Validator = None
            Label = None
        }

    [<CustomOperation("text")>]
    member self.Text(s : FormFieldModel<_, _>, x) = {s with Text = x}
    
    [<CustomOperation("value")>]
    member self.Value(s : FormFieldModel<_, _>, x) = {s with Value = x}

    [<CustomOperation("parser")>]
    member self.Parser(s, x) = {s with Parser = x}

    [<CustomOperation("updater")>]
    member self.Updater(s : FormFieldModel<_, _>, x) = {s with Updater = x}

    [<CustomOperation("formatter")>]
    member self.Formatter(s, x) = {s with Formatter = x}

    [<CustomOperation("validator")>]
    member self.Validator(s : FormFieldModel<_, _>, x) = {s with Validator = Some x}

    [<CustomOperation("label")>]
    member self.Label(s : FormFieldModel<_, _>, x) = {s with Label = Some x}

    member self.Return(x) = x

let field = FieldBuilder()
