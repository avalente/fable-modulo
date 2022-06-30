Fable.Modulo
============

This is a Fable library designed to help with the HTML forms boilerplate code, inspired by [Fable.Form](https://mangelmaxime.github.io/Fable.Form/)

# Introduction

Handling an HTML form requires a lot of boilerplate code, even more when the form is part of an [Elmish](https://elmish.github.io/elmish/) application, since:

1. you need at least one attribute for each form's field in your application's model
1. you need one message for each field with related handling code
1. you have to handle each field's view individually

`Fable.Modulo` helps you by providing a set of helpers with a balanced trade-off between ease of develpment and ability of customization.

`Fable.Modulo` is based on [Fable.React](https://github.com/fable-compiler/fable-react) and it doesn't require or assume any other dependency: you are free to
use your favourite `CSS` framework or `react-based` library.

# Getting started

This example is a step-by-step guide to create a working `Elmish` application ran by `Fable`, however there is no need to use `Elmish` nor the 
`fable template`: the only required dependency (other than of course `Fable.Core`) is [Fable.React](https://github.com/fable-compiler/fable-react).

You will need `.NET 5` or above.

1. install the fable template:
    ```console
    dotnet new --install Fable.Template
    ````

1. create a new fable project:
    ```console
    dotnet new fable -o fable-modulo-example -n fable-modulo-example
    ```

1. install the nuget packages: 

    ```console
    dotnet add src/App.fsproj package Fable.React
    dotnet add src/App.fsproj package Fable.Elmish
    dotnet add src/App.fsproj package Fable.Elmish.React
    dotnet add src/App.fsproj package Fable.Modulo
    ```

1. add the `npm` dependencies:
    ```console
    npm add react react-dom
    ```

1. replace the content of the file `public/index.html` with the following code:
    ```html
    <!doctype html>
    <html>
    <head>
    <title>Fable.Modulo</title>
    <meta http-equiv='Content-Type' content='text/html; charset=utf-8'>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="shortcut icon" href="fable.ico" />
    </head>
    <body>
        <div id="react-main"></div>
        <script src="bundle.js"></script>
    </body>
    </html>
    ```

1. replace the content of the file `src/App.fs` with the following code:

    ```fsharp
    module App

    open System
    open Elmish
    open Elmish.React
    open Fable.React
    open Fable.React.Props
    open Fable.Modulo

    type Field<'t> = FormFieldModel<FormModel, 't>
    and FormModel =
        {
            String : Field<string>
            FloatOption : Field<float option>
            Checkbox : Field<bool>
            ChoiceOptional : Field<{| Key : int; Value : string |} option>
        }

    type Model =
        {
            Form : FormModel
            FormValidationError : string option
        }

    type Msg = 
        | UpdateForm of FormModel
        | ValidateForm

    let transformValidation x =
        x |> Option.map (fun e -> e |> List.map(fun (k, v) -> sprintf "%s: %s" k v) |> String.concat ", ")
        
    let init _ =
        let form = 
            Auto.initForm {
                String = input {
                    value ""
                    label "A field taking a string"
                }
                FloatOption = input {
                    error "please fill me"
                    label "A float optional value" 
                    validator (fun _ v -> match v with | None -> Ok None | Some v when v < 100.0 -> Ok (Some v) | _ -> Error "The value should be < 100")
                    placeholder "please insert a value < 100"
                    tooltip "This is a float input"
                }
                Checkbox = checkbox {
                    value false
                    label "A checkbox"
                }
                ChoiceOptional = select {
                    value None
                    values (Helpers.optionValues [{|Key = 1; Value = "option 1"|}; {|Key = 2; Value = "option 2"|}])
                    value_label (fun x -> x |> Option.map (fun x -> x.Value) |> Option.defaultValue "<no choice>")
                    label "Choice optional"
                }

            }

        let model =
            {
                Form = form
                FormValidationError = validate form |> transformValidation
            }
        model, Cmd.none

    let update msg model =
        match msg with
        | UpdateForm newForm -> {model with Form = newForm}, Cmd.none
        | ValidateForm ->
            {model with FormValidationError = validate model.Form |> transformValidation}, Cmd.none

    let view model dispatch =
        section [] [
            h3 [] [str "Fable.Modulo example"]
            
            div [Style [Display DisplayOptions.Grid; GridTemplateColumns "1fr 1fr"]] [
                div [] [
                    div [] [
                        fieldset [] [
                            legend [] [str "Form"]

                            Auto.View.basicForm model.Form (UpdateForm >> dispatch) [
                                button [OnClick (fun e -> dispatch ValidateForm; e.preventDefault())] [str "Validate"]
                            ]
                        ]
                    ]
                ]

                div [] [
                    div [] [
                        match model.FormValidationError with
                        | None -> str "The form is valid"
                        | Some e -> sprintf "Error: %s" e |> str
                    ]
                ]
            ]
        ]

    Program.mkProgram init update view
    |> Program.withConsoleTrace
    |> Program.withReactSynchronous "react-main"
    |> Program.run

    ```

Let's see the relevant code

#### Form Model

A `form` is represented by a plain record type in `Fable.Modulo`, each field of the record should be of type ``cref:T:Fable.Modulo.FormFieldModel`2``:

    type Field<'t> = FormFieldModel<FormModel, 't>
    and FormModel =
        {
            String : Field<string>
            FloatOption : Field<float option>
            Checkbox : Field<bool>
            ChoiceOptional : Field<{| Key : int; Value : string |} option>
        }

Here a type abbreviation is used to reduce the boilerplate. As you can see ``cref:T:Fable.Modulo.FormFieldModel`2`` is a generic type that accepts 
two arguments:

1. the type of the enclosing form (the record this field belongs to)
1. the type of the underlying value: there is no restriction on it, for instance I used an anonymous record wrapped in an `Option` in the example above.

Refer to the [modeling](#Modeling) section for further informations.

#### Message

A single message is used for the state change of every field in the form:

    type Msg = 
        | UpdateForm of FormModel

In the `update` function you should put all the form's logic: validation, computation of derived fields, update of other pieces of the model and so on.
Since you get the whole form in every dispatched message the internal consistency is automatically ensured; on the other hand it is not easy to identify
the field that caused the message, but since you have both the old and the new form models it should be easy to check what changed.

#### View

The example uses the `cref:M:Fable.Modulo.Auto.View.basicForm` function:

    Auto.View.basicForm model.Form (UpdateForm >> dispatch) [
        button [OnClick (fun e -> dispatch ValidateForm; e.preventDefault())] [str "Validate"]
    ]

It generates automatically an html `form` element by reflecting the `model.Form` object and dispatching an `UpdateForm` message for
each underlying field's state update. Refer to the [view](#View) section for informations about the customization of the html output.

# Modeling

The main building block of `Fable.Modulo` modeling is the type ``cref:T:Fable.Modulo.FormFieldModel`2``, which wraps three more types:

- ``cref:T:Fable.Modulo.FormInputModel`2``: used to represent the state underlying an html `input` element
- ``cref:T:Fable.Modulo.FormCheckboxModel`1``: used to represent the state underlying an html `checkbox` element
- ``cref:T:Fable.Modulo.FormSelectModel`2``: used to represent the state underlying an html `select` element

Every type implements the ``cref:T:Fable.Modulo.IFormFieldModel`2``; some informations are common to the three types, such as:
- underlying value
- layout data (label, tooltip etc.)
- validation function

Other informations are specific, for instance:
- ``cref:T:Fable.Modulo.FormInputModel`2`` needs to keep track of the text displayed in the input element and a parser function to transform the text to the underlying value
- ``cref:T:Fable.Modulo.FormSelectModel`2`` needs the list of allowed values, a function to transform an underlying value to a displayed text and so on

#### Underlying value

The underlying value is represented by the type:

    Result<'t, string>

The value can be `Error`, for instance if you have an integer field and the user typed some alphabetic character or if the field has a custom
validator attached.

The type of the value for a ``cref:T:Fable.Modulo.FormCheckboxModel`1`` is constrained to `bool`, but for the other kind of fields it can be every
f# type including records and discriminated unions.


#### Updater

A central concept in `Fable.Modulo` is the `updater`: it is a function that takes a field and a `form` and returns an updated `form`

    FormInputModel<'f, 't> -> 'f -> 'f

For instance a raw ``cref:T:Fable.Modulo.FormInputModel`2`` could be created explicitly by using ``cref:M:Fable.Modulo.FormInputModel.create`2``:

    FormInputModel.create "" (Ok "") Parsers.string Formatters.string (fun item form -> {form with MyField = item})

The line above creates a new field with:

- an initial underlying value `Ok ""` 
- an empty string as the text initially displayed (`""`)
- `Parsers.string` as the parser: when the html input element changes that funcion gets called with the new text to produce the new underlying value
- `Formatters.string` as the formatter, used to generate the display text from the underlying value
- an `updater function` that replace the field `MyField` in the given form with the new instance of the field

The updater function is just boilerplate and if you use the function ``cref:M:Fable.Modulo.Auto.initForm`1`` it gets created automatically for every field in the record.

Thw workflow when the use changes the content of an input element for a ``cref:T:Fable.Modulo.FormInputModel`2`` is the following:
1. the field's underlying value is updated by the result of the parser function called on the input element's text
1. if a validator function is available, the new value is validated and the field's underlying value is update accordingly
1. the updater function is called with the new field
1. the final callback (for instance in the example above `UpdateForm >> dispatch`) gets called with the updated form 

# View

Since `Fable.Modulo` doesn't have dependecies other than `Fable.React` you can provide your own implementation for the view and use only
the modeling part of the library or you can use one of the provided helpers.

### Manual implementation

The project placed in `Examples/Plain` shows how you can implement your own view:

1. completely on your own, for instance:

    ```fsharp
    Fable.React.Standard.input [
        Value model.Form.String.Text
        OnChange (View.FormInputModel.onChange model.Form model.Form.String (UpdateForm >> dispatch))
        AutoFocus true
    ]
    ```
    this creates a plain `Fable.React` element that displays the text stored in the field named `String` in the form model.
    The function `cref:M:Fable.Modulo.View.FormInputModel.onChange` provides the glue between the react element and the form model

1. with basic helpers, for instance:

    ```fsharp
    View.basicField model.Form model.Form.Custom (UpdateForm >> dispatch)
    View.basicSelect model.Form model.Form.Choice (UpdateForm >> dispatch)
    ```
    the first one creates a react element equivalent to the one created manually above; the second one create an html `<select>` and related `<option>`s. No layout is assumed.

### Automatic implementation

In order to further reduce the boilerplate code `Fable.Modulo` provides the `cref:T:Fable.Modulo.Auto.View` module that uses reflection to generate
react elements automatically from the model record. 

The module `Bulma` implements an automatic form and applies the structure and styling from the [bulma](https://bulma.io/) framework.



