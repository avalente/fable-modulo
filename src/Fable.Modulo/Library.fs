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

    /// Parse a non-empty string
    let nonEmptyString x =
        if String.IsNullOrWhiteSpace x then Error "Empty string not allowed"
        else Ok x

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

/// Interface for the "runtime" part of a field (its value)
type IFormFieldModel<'f, 't> =
    abstract member FieldValue : Result<'t, string>
    abstract member FormattedValue : string

/// Layout-related data
type FormFieldLayout =
    {
        /// The field's label. Useful for auto-generation
        Label : string option
        /// The field's placeholder
        Placeholder : string option
        /// Tooltip text
        Tooltip : string option
        /// If true the field is required. Only used for UI purposes
        IsRequired : bool
    }

    static member Empty(required : bool) =
        {
            Label = None
            Placeholder = None
            Tooltip = None
            IsRequired = required
        }

module FormFieldLayout =
    let label l = l.Label
    let placeholder l = l.Placeholder
    let tooltip l = l.Tooltip
    
/// The form field model, used for "input" elements
// in order to keep the implementation as simple as possible, the "fixed" and "runtime" parts
// of the model are kept in the same structure. In particular:
// - Text and Value change as response to the user's actions
// - the other fields are set at initialization time and should never change
type FormInputModel<'f, 't> =
    {
        /// Input text
        Text : string
        /// Parsed and validated value
        Value : Result<'t, string>
        /// Parser for the given type
        Parser : string -> Result<'t, string>
        /// A function that updates the form with the current field. Usually just something such as (fun item form -> {form with MyItem = item})
        Updater : FormInputModel<'f, 't> -> 'f -> 'f
        /// Gets a string representation of the underlying value
        Formatter : 't -> string
        /// Validate the field's value, possibly against the whole form
        Validator : ('f -> 't -> Result<'t, string>) option
        /// Layout options
        Layout : FormFieldLayout
    }

    member self.Validate form value = 
        match self.Validator with
        | None -> value
        | Some f -> value |> Result.bind (f form)

    member self.UpdateForm form =
        let item = {self with Value = self.Value |> self.Validate form}
        item.Updater item form

    interface IFormFieldModel<'f, 't> with
        member self.FieldValue = self.Value
        member self.FormattedValue = 
            match self.Value with
            | Error e -> sprintf "Error: %s" e
            | Ok x -> self.Formatter x

/// Model for a "checkbox" element.
type FormCheckboxModel<'f> =
    {
        Value : Result<bool, string>
        Updater : FormCheckboxModel<'f> -> 'f -> 'f
        Validator : ('f -> bool -> Result<bool, string>) option
        Layout : FormFieldLayout
    }

    member self.Validate form value = 
        match self.Validator with
        | None -> value
        | Some f -> value |> Result.bind (f form)

    member self.UpdateForm form =
        let item = {self with Value = self.Value |> self.Validate form}
        item.Updater item form

    interface IFormFieldModel<'f, bool> with
        member self.FieldValue = self.Value
        member self.FormattedValue = 
            match self.Value with
            | Error e -> sprintf "Error: %s" e
            | Ok x -> string x

/// Model for a "select"-like element
type FormSelectModel<'f, 't> =
    {
        /// The available values
        Values : 't[]
        /// A function that generates an unique string for each value
        KeyFunction : 't -> string
        /// A function to get the user-facing string representation of a value
        ValueLabel : 't -> string
        /// The current item's value
        Value : Result<'t, string>
        /// A function used to serialize the value to a string that is stored in the underlying "select"/"option"
        Updater : FormSelectModel<'f, 't> -> 'f -> 'f
        /// Validate the field's value, possibly against the whole form
        Validator : ('f -> 't -> Result<'t, string>) option
        /// Adds an empty element if true
        AddEmptySelection : bool
        /// The error to display when no selection has been made
        EmptyErrorString : string
        /// Layout options
        Layout : FormFieldLayout
    }

    member self.Validate form value = 
        match self.Validator with
        | None -> value
        | Some f -> value |> Result.bind (f form)

    member self.UpdateForm form =
        let item = {self with Value = self.Value |> self.Validate form}
        item.Updater item form

    interface IFormFieldModel<'f, 't> with
        member self.FieldValue = self.Value
        member self.FormattedValue = 
            match self.Value with
            | Error e -> sprintf "Error: %s" e
            | Ok x -> self.ValueLabel x

/// Wrapper type needed to unify the field types
type FormFieldModel<'f, 't> = 
    | Input of FormInputModel<'f, 't>
    | Select of FormSelectModel<'f, 't>
    | Checkbox of FormCheckboxModel<'f>

    member self.Value = 
        match self with
        | Input x -> x.Value
        | Select x -> x.Value
        | Checkbox x -> x.Value |> box |> unbox<Result<'t, string>>

    member self.Error =
        match self.Value with
        | Error e -> Some e
        | Ok _ -> None

    member self.Validate form (value : Result<'t, string>) =
        match self with
        | Input x -> x.Validate form value
        | Select x -> x.Validate form value
        | Checkbox x -> x.Validate form (value |> box |> unbox<Result<bool, string>>) |> box |> unbox<Result<'t, string>>

    member self.UpdateForm form =
        match self with
        | Input x -> x.UpdateForm form
        | Select x -> x.UpdateForm form
        | Checkbox x -> x.UpdateForm form

    interface IFormFieldModel<'f, 't> with
        member self.FieldValue = self.Value
        member self.FormattedValue =
            match self with
            | Input x -> (x :> IFormFieldModel<_, _>).FormattedValue
            | Select x -> (x :> IFormFieldModel<_, _>).FormattedValue
            | Checkbox x -> (x :> IFormFieldModel<_, _>).FormattedValue

module FormInputModel =
    /// Create a FormInputModel object 
    let inline create<'f, 't> text (value : Result<'t, string>) parser formatter (updater : FormInputModel<'f, 't> -> 'f -> 'f) =
        
        let isNotOption = typedefof<'t> <> typedefof<Option<_>>

        {
            Text = text
            Value = value
            Parser = parser
            Updater = updater
            Formatter = formatter
            Validator = None
            Layout = FormFieldLayout.Empty(isNotOption)
        }

    /// Create a FormInputModel object with the initial text generated by the "formatter" function
    let inline create'<'f, 't> (value : Result<'t, string>) parser formatter (updater : FormInputModel<'f, 't> -> 'f -> 'f) =
        create (match value with | Error _ -> "" | Ok x -> formatter x) value parser formatter updater

    /// Set the validator
    let withValidator validator (item : FormInputModel<_, _>) = {item with Validator = Some validator}
    /// Set the label
    let withLabel label (item : FormInputModel<_, _>) = {item with Layout = {item.Layout with Label = Some label}}
    /// Set the placeholder
    let withPlaceholder placeholder (item : FormInputModel<_, _>) = {item with Layout = {item.Layout with Placeholder = Some placeholder}}
    /// Set the tooltip text
    let withTooltip text (item : FormInputModel<_, _>) = {item with Layout = {item.Layout with Tooltip = Some text}}
    /// Set the formatter
    let withFormatter formatter (item : FormInputModel<_, _>) = {item with Formatter = formatter}

    // constructors

    /// Construct a FormInputModel backed by a string
    let stringField initialValue updater =
        create' initialValue Parsers.string Formatters.string updater

    /// Construct a FormInputModel backed by an optional string
    let stringOptionField initialValue updater =
        create' initialValue Parsers.stringOption (Formatters.option Formatters.string) updater

    /// Construct a FormInputModel backed by a System.Int32 value
    let intField initialValue updater =
        create' initialValue Parsers.int Formatters.int updater

    /// Construct a FormInputModel backed by an optional System.Int32 value    
    let intOptionField initialValue updater =
        create' initialValue Parsers.intOption (Formatters.option Formatters.int) updater

    /// Construct a FormInputModel backed by a System.Double value
    let floatField initialValue updater =
        create' initialValue Parsers.float Formatters.float updater

    /// Construct a FormInputModel backed by an optional System.Double value
    let floatOptionField initialValue updater =
        create' initialValue Parsers.floatOption (Formatters.option Formatters.float) updater

    /// Construct a FormInputModel backed by the Date part of a System.DateTime value
    let dateField<'f> initialValue updater =
        create'<'f, _> initialValue Parsers.date Formatters.date updater

    /// Construct a FormInputModel backed by the Date part of a System.DateTime option value
    let dateOptionField initialValue updater =
        create' initialValue Parsers.dateOption (Formatters.option Formatters.date) updater

    /// Construct a FormInputModel backed by a System.TimeSpan value
    let timeSpanField initialValue updater =
        create' initialValue Parsers.timeSpan Formatters.timeSpan updater

    /// Construct a FormInputModel backed by a System.TimeSpan optional value
    let timeSpanOptionField initialValue updater =
        create' initialValue Parsers.timeSpanOption (Formatters.option Formatters.timeSpan) updater

    /// Construct a FormInputModel backed by a System.DateTimeOffset value
    let dateTimeField initialValue updater =
        create' initialValue Parsers.dateTime Formatters.dateTime updater

    /// Construct a FormInputModel backed by a System.DateTimeOffset optional value
    let dateTimeOptionField initialValue updater =
        create' initialValue Parsers.dateTimeOption (Formatters.option Formatters.dateTime) updater

    /// Construct a FormInputModel backed by a boolean value
    let boolField initialValue updater =
        create' initialValue Parsers.bool Formatters.bool updater
    
    /// Construct a FormInputModel backed by a boolean optional value
    let boolOptionField initialValue updater =
        create' initialValue Parsers.boolOption (Formatters.option Formatters.bool) updater

    // helpers

    /// Update the field's value with the given text. The validation function is not called.
    let inline updateText<'f, 't> text (item : FormInputModel<'f, 't>) =
        {item with Text = text; Value = item.Parser text}

    /// Update the field's value. The field's text is generated by the formatter. The validation function is not called.
    let inline updateValue<'f, 't> value (item : FormInputModel<'f, 't>) =
        match value with
        | Ok x -> {item with Value = value; Text = item.Formatter x}
        | Error _ -> {item with Value = value}

    /// Update the form by changing the text on the given field.
    let inline updateForm<'f, 't> (form : 'f) (item : FormInputModel<'f, 't>) text =
        let item' = updateText<'f, 't> text item
        item'.UpdateForm form
    
module FormCheckboxModel =
    /// Create a FormCheckboxModel
    let create initialValue updater =
        {
            Value = initialValue
            Updater = updater
            Validator = None
            Layout = FormFieldLayout.Empty(true)
        }

    /// Update the field's value
    let inline update<'f> value (item : FormCheckboxModel<'f>) =
        {item with Value = value}

    /// Set the validator
    let withValidator validator (item : FormCheckboxModel<_>) = {item with Validator = Some validator}
    /// Set the label
    let withLabel label (item : FormCheckboxModel<_>) = {item with Layout = {item.Layout with Label = Some label}}
    /// Set the placeholder
    let withPlaceholder placeholder (item : FormCheckboxModel<_>) = {item with Layout = {item.Layout with Placeholder = Some placeholder}}
    /// Set the tooltip text
    let withTooltip text (item : FormCheckboxModel<_>) = {item with Layout = {item.Layout with Tooltip = Some text}}

module FormSelectModel =
    open Fable.Core.JS

    /// Create a FormSelectModel object. If the backing type is not an option, an empty choice is
    /// inserted at the beginning of the list. The default KeyFunction is JSON.stringify.
    let inline create<'f, 't> initialValue (availableValues : seq<'t>) (labelForValue : 't -> string) (updater : FormSelectModel<'f, 't> -> 'f -> 'f) =
        let kf = JSON.stringify
        // check that the initial value is contained in the available values
        match initialValue with
        | Ok x ->
            let k = kf x
            match availableValues |> Seq.tryFind (fun x -> kf x = k) with
            | None -> failwithf "The given values do not contain the initial value"
            | _ -> ()
        | _ -> ()

        // by default adds an empty invalid selection if the data type is not Option<_>
        let isNotOption = typedefof<'t> <> typedefof<Option<_>>

        let emptyErrorString =
            match initialValue with
            | Error e -> e
            | _ -> "please choose a value"

        {
            Values = availableValues |> Seq.toArray
            ValueLabel = labelForValue
            Value = initialValue 
            KeyFunction = kf
            Updater = updater
            Validator = None
            AddEmptySelection = isNotOption
            EmptyErrorString = emptyErrorString
            Layout = FormFieldLayout.Empty(isNotOption)
        }

    /// Create a FormSelectModel object using the cases from the given discriminated union.
    /// Currently only simple discriminated unions are supported (e.g. plain cases without data)
    let inline createOfDU<'f, 't> initialValue (duType : Type) updater =
        let values = 
            Reflection.FSharpType.GetUnionCases(duType)
            |> Seq.map (fun x ->
                if x.GetFields().Length > 0 then failwithf "Only plain discriminated unions are supported by now (e.g. cases without data)"
                Reflection.FSharpValue.MakeUnion(x, [||]) |> unbox<'t>
            )

        create<'f, 't> initialValue values (box >> string) updater

    /// Update the field's value
    let inline update<'f, 't> value (item : FormSelectModel<'f, 't>) =
        {item with Value = value}
    
    /// Set the key generator function
    let withKeyFunction f (item : FormSelectModel<_, _>) = {item with KeyFunction = f}
    /// Set the validator
    let withValidator validator (item : FormSelectModel<_, _>) = {item with Validator = Some validator}
    /// Set the label
    let withLabel label (item : FormSelectModel<_, _>) = {item with Layout = {item.Layout with Label = Some label}}
    /// Set the placeholder
    let withPlaceholder placeholder (item : FormSelectModel<_, _>) = {item with Layout = {item.Layout with Placeholder = Some placeholder}}
    /// Set the tooltip text
    let withTooltip text (item : FormSelectModel<_, _>) = {item with Layout = {item.Layout with Tooltip = Some text}}
    /// Set the function to generate value labels
    let withValueLebel f item = {item with ValueLabel = f}
    /// If called with 'true' an empty, invalid choice is added at the start of the list
    let withEmptySelection add item = {item with AddEmptySelection = add}
    /// Set the error message in case of no choice
    let withEmptyErrorString error item = {item with EmptyErrorString = error}
    /// Replace the available values
    let withValues values item = 
        // check that the initial value is contained in the available values
        let newValue =
            match item.Value with
            | Ok x when values |> Seq.contains x |> not -> Error "choose an item"
            | e -> e
        {item with Values = values |> Seq.toArray; Value = newValue}

module FormFieldModel =
    /// Extract the layout options
    let layout<'f, 't> (x : FormFieldModel<'f, 't>) = 
        match x with
        | Input x -> x.Layout
        | Select x -> x.Layout
        | Checkbox x -> x.Layout

    /// Set the label
    let withLabel l x = 
        match x with
        | Input x -> FormInputModel.withLabel l x |> Input
        | Select x -> FormSelectModel.withLabel l x |> Select
        | Checkbox x -> FormCheckboxModel.withLabel l x |> Checkbox

    /// Set the placeholder
    let withPlaceholder t x = 
        match x with
        | Input x -> FormInputModel.withPlaceholder t x |> Input
        | Select x -> FormSelectModel.withPlaceholder t x |> Select
        | Checkbox x -> FormCheckboxModel.withPlaceholder t x |> Checkbox

    /// Set the tooltip text 
    let withTooltip t x = 
        match x with
        | Input x -> FormInputModel.withTooltip t x |> Input
        | Select x -> FormSelectModel.withTooltip t x |> Select
        | Checkbox x -> FormCheckboxModel.withTooltip t x |> Checkbox
    
    /// Extract the label
    let label<'f, 't> (item : FormFieldModel<'f, 't>) = item |> layout |> FormFieldLayout.label

    /// Extract the placeholder
    let placeholder<'f, 't> (item : FormFieldModel<'f, 't>) = item |>  layout |> FormFieldLayout.placeholder 

    /// Extract the tooltip text
    let tooltip<'f, 't> (item : FormFieldModel<'f, 't>) = item |> layout |> FormFieldLayout.tooltip
        
    /// Set the validator
    let withValidator f x =
        match x with
        | Input x -> {x with Validator = Some f} |> Input
        | Select x -> {x with Validator = Some f} |> Select
        | Checkbox x -> {x with Validator = Some (f |> box |> unbox)} |> Checkbox

    /// Update the underlying field's value
    let update (v : Result<'t, string>) field =
        match field with
        | Input field -> field |> FormInputModel.updateValue v |> Input
        | Select field -> field |> FormSelectModel.update v |> Select
        | Checkbox field -> field |> FormCheckboxModel.update (v |> box |> unbox) |> Checkbox

    /// Update the form with the new field
    let updateForm form (field : FormFieldModel<_, _>) = field.UpdateForm form

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
    /// OnChange handler for the input element
    let inline onChange<'f, 't> (form : 'f) (item : FormInputModel<'f, 't>) (messageDispatcher : 'f -> unit) (ev : Event) = FormInputModel.updateForm form item ev.Value |> messageDispatcher

    /// Build a ReactElement 'input' with the given props except for 'DefaultValue', 'OnChange' and 'Placeholder' 
    /// that are set automatically
    let fieldBase form item messageDispatcher (props : IHTMLProp list) =
        let props' = props @ [
            DefaultValue item.Text
            OnChange (onChange form item messageDispatcher)
            match item.Layout.Placeholder with | None -> () | Some t -> Placeholder t
        ] 
        input props'

    /// Build a ReactElement 'input' with the correct 'DefaultValue', 'OnChange' and 'Placeholder' properties
    let basicField form item messageDispatcher =
        fieldBase form item messageDispatcher []

    /// Build a ReactElement 'input' with the correct 'DefaultValue', 'OnChange', 'Name' and 'Placeholder' properties
    let basicField' form item name messageDispatcher =
        fieldBase form item messageDispatcher [Name name] 

    /// Build a ReactElement 'input' with type 'checkbox' and the given props except for
    /// 'DefaultValue', 'OnChange' and 'Placeholder' that are set automatically
    let checkboxBase<'f> (form : 'f) (item : FormCheckboxModel<'f>) messageDispatcher (props : IHTMLProp list) =
        input [
            Type "checkbox"
            DefaultValue (match item.Value with | Ok x -> x | Error e -> false)
            OnChange (fun ev ->
                let item = {item with Value = Ok ev.Checked}
                item.UpdateForm form |> messageDispatcher
            )
            match item.Layout.Placeholder with | None -> () | Some t -> Placeholder t
        ]

    /// Build a ReactElement 'select' with properties given in the 'selectProps' parameter.
    /// The props 'DefaultValue', 'OnChange' and 'Placeholder' are set automatically
    /// Each value from the input FormSelectModel is represented by a ReactElement 'option':
    /// you can pass its HTML properties but the 'Value' is set automatically.
    let selectBase<'f, 't> (form : 'f) (item : FormSelectModel<'f, 't>) messageDispatcher (selectProps : IHTMLProp list) (optionProps : IHTMLProp list) =
        let props' =  selectProps @ [
            DefaultValue (match item.Value with | Ok x -> item.KeyFunction x |> string | Error e -> "")
            OnChange (fun ev -> 
                let value = 
                    if String.IsNullOrWhiteSpace ev.Value && item.AddEmptySelection then
                        Error item.EmptyErrorString
                    else
                        item.Values |> Array.find (fun x -> item.KeyFunction x = ev.Value) |> Ok
                let item = {item with Value = value}
                item.UpdateForm form |> messageDispatcher
            )
            match item.Layout.Placeholder with | None -> () | Some t -> Placeholder t
        ]

        let optionProps' x = optionProps @ [
            HTMLAttr.Value x
        ]

        select props' [
            if item.AddEmptySelection then
                option (optionProps @ [
                    HTMLAttr.Value ""
                ]) [str ""]

            for i, o in Seq.indexed item.Values do
                option (optionProps' (item.KeyFunction o)) [
                    str (item.ValueLabel o)
                ]
        ]

    /// Build a ReactElement 'input' with type 'checkbox' and the properties
    /// 'DefaultValue', 'OnChange' and 'Placeholder' set automatically
    let basicCheckbox<'f> (form : 'f) (item : FormCheckboxModel<'f>) messageDispatcher =
        checkboxBase form item messageDispatcher []

    /// Build a ReactElement 'select' with the properties
    /// 'DefaultValue', 'OnChange' and 'Placeholder' set automatically,
    /// and a set of ReactElement 'option' with the 'Value' property set automatically
    let basicSelect<'f, 't when 't : equality> (form : 'f) (item : FormSelectModel<'f, 't>) messageDispatcher =
        selectBase form item messageDispatcher [] []
        
/// Update the form with the given field
let inline updateForm<'f, 't> form (field : FormFieldModel<'f, 't>) =
    field.UpdateForm form

/// Validate the form. Return a list of couples (<field name>, <error message>) if there is at least one error, None if the form is valid
let inline validate<'f> (f : 'f) =
    let t = typeof<'f>
    if Reflection.FSharpType.IsRecord t |> not then
        failwithf "Only records are supported by now"

    // unfortunately fable does not support reflection on interfaces yet
    // so we must check for each known type, including the wrapper
    let fti = typeof<FormInputModel<_, _>>
    let fts = typeof<FormSelectModel<_, _>>
    let ftc = typeof<FormCheckboxModel<_>>
    let ftw = typeof<FormFieldModel<_, _>>

    let fields =
        Reflection.FSharpType.GetRecordFields(t)
        |> Array.filter(fun x -> 
            let t = x.PropertyType
            // we know that out type is generic, so let's drop non generics
            if not t.IsGenericType then false
            else
                let td = t.GetGenericTypeDefinition()
                td = fti.GetGenericTypeDefinition() || 
                td = fts.GetGenericTypeDefinition() || 
                td = ftc.GetGenericTypeDefinition() ||
                td = ftw.GetGenericTypeDefinition()
        )

    let errors =
        fields
        |> Array.choose (fun pi ->
            let field = Reflection.FSharpValue.GetRecordField(f, pi) |> unbox<IFormFieldModel<_, _>>
            match field.FieldValue with
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
    let inline private u _ _ = failwithf "looks like you forgot to call 'initForm'"

    /// Build a FormFieldModel automatically given the initial value
    let inline field<'f, 't> (initialValue : Result<'t, string>) =
        let inline i x = x |> unbox<FormInputModel<'f, 't>> |> FormFieldModel.Input

        if typeof<'t> = typeof<string> then
            FormInputModel.stringField (initialValue |> box |> unbox<Result<string, string>>) u |> box |> i
        elif typeof<'t> = typeof<string option> then
            FormInputModel.stringOptionField (initialValue |> box |> unbox<Result<string option, string>>) u |> box |> i
        elif typeof<'t> = typeof<int> then
            FormInputModel.intField (initialValue |> box |> unbox<Result<int, string>>) u |> box |> i
        elif typeof<'t> = typeof<int option> then
            FormInputModel.intOptionField (initialValue |> box |> unbox<Result<int option, string>>) u |> box |> i
        elif typeof<'t> = typeof<float> then
            FormInputModel.floatField (initialValue |> box |> unbox<Result<float, string>>) u |> box |> i
        elif typeof<'t> = typeof<option<float>> then
            FormInputModel.floatOptionField (initialValue |> box |> unbox<Result<float option, string>>) u |> box |> i
        elif typeof<'t> = typeof<DateTime> then
            FormInputModel.dateField (initialValue |> box |> unbox<Result<DateTime, string>>) u |> box |> i
        elif typeof<'t> = typeof<option<DateTime>> then
            FormInputModel.dateOptionField (initialValue |> box |> unbox<Result<DateTime option, string>>) u |> box |> i
        elif typeof<'t> = typeof<TimeSpan> then
            FormInputModel.timeSpanField (initialValue |> box |> unbox<Result<TimeSpan, string>>) u |> box |> i
        elif typeof<'t> = typeof<option<TimeSpan>> then
            FormInputModel.timeSpanOptionField (initialValue |> box |> unbox<Result<TimeSpan option, string>>) u |> box |> i
        elif typeof<'t> = typeof<DateTimeOffset> then
            FormInputModel.dateTimeField (initialValue |> box |> unbox<Result<DateTimeOffset, string>>) u |> box |> i
        elif typeof<'t> = typeof<option<DateTimeOffset>> then
            FormInputModel.dateTimeOptionField (initialValue |> box |> unbox<Result<DateTimeOffset option, string>>) u |> box |> i
        elif typeof<'t> = typeof<bool> then
            FormCheckboxModel.create (initialValue |> box |> unbox<Result<bool, string>>) u |> box |> unbox<FormCheckboxModel<'f>> |> FormFieldModel.Checkbox
        else
            failwithf "field type not supported yet: %s" (typeof<'t>.Name)

    /// Build a FormFieldModel automatically given the initial value and the field's label
    let inline field'<'f, 't> (initialValue : Result<'t, string>) fieldLabel =
        field<'f, 't> initialValue |> FormFieldModel.withLabel fieldLabel

    /// Build a FormFieldModel from the given initial value, available choices and value label-generating function
    let inline select<'f, 't> (initialValue : Result<'t, string>) (values : 't seq) (labelFunction : 't -> string) =
        FormSelectModel.create<'f, 't> initialValue values labelFunction u |> FormFieldModel.Select

    /// Build a FormFieldModel from the given initial value, available choices, value label-generating function and the field's label
    let inline select'<'f, 't> (initialValue : Result<'t, string>) (values : 't seq) (labelFunction : 't -> string) fieldLabel =
        select<'f, 't> initialValue values labelFunction |> FormFieldModel.withLabel fieldLabel

    /// Automatically build an "updater" function
    let inline private autoUpdater<'f, 't, 'w> (pi : Reflection.PropertyInfo) (fields : Reflection.PropertyInfo[]) (wrapper : 'w -> FormFieldModel<_, _>) (field : 'w) (form : 'f) =
        let values = 
            [|
                for f in fields do
                    if f.Name = pi.Name then
                        box (wrapper field)
                    else
                        Reflection.FSharpValue.GetRecordField(form, f)
            |]
        Reflection.FSharpValue.MakeRecord(typeof<'f>, values) |> unbox<'f>
    
    // Return the list of fields (fields of the type FormFieldModel) by reflecting
    // the given record that should represent a form
    let inline private getFields<'f>(f : 'f) =
        let t = typeof<'f>
        let ft = typeof<FormFieldModel<_, _>>
    
        if Reflection.FSharpType.IsRecord t |> not then
            failwithf "Only records are supported by now"

        Reflection.FSharpType.GetRecordFields(t)
        |> Array.map(fun x -> 
            let t = x.PropertyType
            // we know that our type is generic, so let's drop non generics
            if not t.IsGenericType then failwithf "Unsupported field type"
            else
                if t.GetGenericTypeDefinition() = ft.GetGenericTypeDefinition() then
                    x
                else
                    failwithf "Unsupported field type: %A" g
        )

    let inline debugForm<'f>(f : 'f) =
        [
            for pi in getFields<'f> f do
                let field = Reflection.FSharpValue.GetRecordField(f, pi) |> unbox<IFormFieldModel<'f, _>>
                pi.Name, field.FormattedValue
        ]

    /// Initialize the form by attaching an "updater" to each field
    let inline initForm<'f>(f : 'f) =
        let fields =
            let fields = getFields<'f> f
            fields
            |> Array.map (fun pi ->
                let field = Reflection.FSharpValue.GetRecordField(f, pi) |> unbox<FormFieldModel<_, _>>
                
                match field with
                | Input x -> {x with Updater = (autoUpdater<'f, _, _> pi fields Input) |> box |> unbox} |> Input
                | Select x -> {x with Updater = (autoUpdater<'f, _, _> pi fields Select) |> box |> unbox} |> Select
                | Checkbox x -> {x with Updater = (autoUpdater<'f, _, _> pi fields Checkbox) |> box |> unbox} |> Checkbox
                |> box
    
            )
        Reflection.FSharpValue.MakeRecord(typeof<'f>, fields) |> unbox<'f>

    module View =
        [<RequireQualifiedAccess>]
        type Kind = | Input | Select | Checkbox

        /// Represents the field element
        type Field =
            {
                Name : string
                Label : ReactElement
                LabelText : string option
                Element : ReactElement
                Kind : Kind
                Layout : FormFieldLayout
                Error : string option
            }
         
        module Classes =
            let Form = "modulo-form"
            let Field = "modulo-field"
            let InputField = "modulo-field-input"
            let SelectField = "modulo-field-select"
            let CheckboxField = "modulo-field-checkbox"
            let Required = "modulo-field-is-required"
            let FieldName x = sprintf "modulo-field-name-%s" x
            let FieldKind = function | Kind.Input -> InputField | Kind.Select -> SelectField | Kind.Checkbox -> CheckboxField

        // field index -> field name -> field error -> 
        type PropsFun = int -> string -> string option -> list<IHTMLProp>
        let emptyProps x y z = []

        /// Return a list of Field records
        let inline fieldsBase<'f> (form : 'f) messageDispatcher (inputProps : PropsFun) (selectProps : PropsFun) optionProps (checkboxProps : PropsFun) labelProps =
            [
                for i, pi in getFields<'f> form |> Seq.indexed do
                    let field = Reflection.FSharpValue.GetRecordField(form, pi) |> unbox<FormFieldModel<'f, _>>
                    let labelText = field |> FormFieldModel.label |> Option.defaultValue pi.Name
                    let label = label (labelProps @ [HtmlFor pi.Name]) [str labelText]

                    let commonProps : List<IHTMLProp> = [Name pi.Name; AutoFocus (i = 0)]

                    let error = field.Error

                    let f = 
                        {
                            Name = pi.Name
                            Label = label
                            Element = str ""
                            LabelText = field |> FormFieldModel.label
                            Kind = Kind.Input
                            Layout = FormFieldModel.layout field
                            Error = error
                        }

                    match field with
                    | Input field -> {f with Element = View.fieldBase form field messageDispatcher (commonProps @ inputProps i pi.Name error); Kind = Kind.Input}
                    | Select field -> {f with Element = View.selectBase form field messageDispatcher (commonProps @ selectProps i pi.Name error) optionProps; Kind =  Kind.Select}
                    | Checkbox field -> {f with Element = View.checkboxBase form field messageDispatcher (commonProps @ checkboxProps i pi.Name error); Kind = Kind.Checkbox}
            ]

        /// Return a list of (field name, (label element, input element)). The field name is the name of the form record field.
        let inline fields<'f> (f : 'f) messageDispatcher =
            fieldsBase<'f> f messageDispatcher emptyProps emptyProps [] emptyProps []

        /// Return a basic form with the fields extracted from the given record
        let inline basicForm<'f> (f : 'f) messageDispatcher (extraElements : ReactElement seq) =
            let fields =
                fieldsBase<'f> f messageDispatcher (fun _ _ _ -> [TabIndex 0]) (fun _ _ _ -> [TabIndex 0]) [] (fun _ _ _ -> [TabIndex 0]) []

            form [ClassName Classes.Form] [
                for field in fields do
                    let className = 
                        [
                            Classes.Field
                            Classes.FieldKind field.Kind
                            Classes.FieldName field.Name
                            if field.Layout.IsRequired then Classes.Required
                        ] |> String.concat " "
                        
                    div [ClassName className; match field.Layout.Tooltip with | None -> () | Some t -> Title t] [
                        field.Label
                        field.Element
                    ]
                for extra in extraElements do
                    extra
            ]

        module Bulma =
            module Classes =
                let Field = "field"
                let Control = "control"
                let Help = "help"
                let Input = "input"
                let Select = "select"
                let Checkbox = "checkbox"

                let HasIconsRight = "has-icons-right"
                let Icon = "icon"
                let IsSmall = "is-small"
                let IsRight = "is-right"
                let IsSuccess = "is-success"
                let IsDanger = "is-danger"
                let Fa kind = sprintf "fas fa-%s" kind

                let concat (classes : string list) = ClassName (classes |> String.concat " ")

            let inline form<'f> (f : 'f) messageDispatcher (sizeClass : string option) (extraElements : ReactElement seq) =
                let inputProps idx name (error : string option) : List<IHTMLProp> = 
                    [
                        Classes.concat [
                            "input"
                            if error.IsSome then "is-danger" else ()
                            match sizeClass with | None -> () | Some x -> x
                        ]
                    ]
                let labelProps : List<IHTMLProp> = [Classes.concat ["label"; match sizeClass with | None -> () | Some x -> x]]

                form [Classes.concat [Classes.Form]] [
                    for field in fieldsBase<'f> f messageDispatcher inputProps emptyProps [] emptyProps labelProps do
                        let className = 
                            [
                                Classes.Field
                                Classes.FieldKind field.Kind
                                Classes.FieldName field.Name
                                if field.Layout.IsRequired then Classes.Required
                            ] |> String.concat " "
                            
                        div [ClassName className; match field.Layout.Tooltip with | None -> () | Some x -> Title x] [
                            // for input and select display the label
                            match field.Kind with
                            | Kind.Input | Kind.Select -> field.Label
                            | Kind.Checkbox -> ()

                            div [Classes.concat [Classes.Control; Classes.HasIconsRight]] [
                                match field.Kind with
                                | Kind.Input -> 
                                    field.Element
                                    span [Classes.concat [Classes.Icon; Classes.IsSmall; Classes.IsRight]] [
                                        match field.Error with
                                        | None ->
                                            i [Classes.concat [Classes.IsSuccess; Classes.Fa "check"]] []
                                        | Some e ->
                                            i [Classes.concat [Classes.IsDanger; Classes.Fa "exclamation-triangle"]] []
                                    ]
    
                                | Kind.Select -> 
                                    div [Classes.concat [Classes.Select; match field.Error with | None -> () | Some _ -> Classes.IsDanger; match sizeClass with | None -> () | Some x -> x]] [field.Element]
                                | Kind.Checkbox ->
                                    label [Classes.concat [Classes.Checkbox; match sizeClass with | None -> () | Some x -> x]] [
                                        field.Element
                                        match field.LabelText with 
                                        | None -> () 
                                        | Some x when x.StartsWith(" ") -> str x
                                        | Some x -> sprintf " %s" x |> str
                                    ]
                            ]
                            match field.Error with
                            | Some e ->
                                p [Classes.concat [Classes.Help; Classes.IsDanger]] [str e]
                            | None ->
                                p [Classes.concat [Classes.Help; Classes.IsSuccess]] [str "\u00a0"]
                        ]
                    for extra in extraElements do
                        extra
                ]
    