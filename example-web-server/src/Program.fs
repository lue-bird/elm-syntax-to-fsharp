module Program

open Falco

// for the interesting part, skip to [<EntryPoint>]

[<Struct>]
type HtmlString_VirtualDomNodeToStringState<'event> =
    { Depth: int
      Result: List<string>
      Stack: List<(struct (string * List<Elm.VirtualDom_Node<'event>>))> }

let virtualDomPropertyKeyToString (prop: string) : string =
    match prop with
    | "className" -> "class"
    | "defaultValue" -> "value"
    | "htmlFor" -> "for"
    | propNameNotException -> propNameNotException

let lineIndent (perLevel: int) (level: int) (x: string) : string =
    String.replicate (perLevel * level) " " + x

let virtualDomHyphenate: string -> string =
    String.collect (fun (char_: char) ->
        if System.Char.IsAsciiLetterUpper char_ then
            "-" + string (System.Char.ToLowerInvariant char_)

        else
            string char_)

let virtualDomEscapeHtmlText (text: string) : string =
    text.Replace("&", "&amp;").Replace(">", "&gt;").Replace("<", "&lt;")

let virtualDomEscape (string_: string) : string = string_.Replace("\"", "\\\"")

let virtualDomClosingTag (tagName: string) : string = "</" + tagName + ">"

let virtualDomBuildProp (key: string) (value: string) : string =
    virtualDomHyphenate key + "=\"" + virtualDomEscape value + "\""

let virtualDomBuildPropNamespaced
    (maybeNamespace: ValueOption<string>)
    (key: string)
    (value: string)
    : string =
    match maybeNamespace with
    | ValueNone -> virtualDomBuildProp key value
    | ValueSome namespace_ ->
        if String.forall System.Char.IsAscii namespace_ then
            namespace_
            + ":"
            + virtualDomHyphenate key
            + "=\""
            + virtualDomEscape value
            + "\""
        else
            virtualDomBuildProp key value

let virtualDomModifierStringsAddClasses
    (classes: List<string>)
    (attrs: List<string>)
    : List<string> =
    match classes with
    | [] -> attrs
    | _ -> virtualDomBuildProp "class" (String.concat " " classes) :: attrs

let virtualDomModifierStringsAddStyles
    (styles: List<string>)
    (attrs: List<string>)
    : List<string> =
    match styles with
    | [] -> attrs
    | _ -> virtualDomBuildProp "style" (String.concat "; " styles) :: attrs

let virtualDomModifierStringsAddModifier
    (modifier: Elm.VirtualDom_Attribute<'event>)
    ((struct (classes, styles, attrs) as acc):
        (struct (List<string> * List<string> * List<string>)))
    : (struct (List<string> * List<string> * List<string>)) =
    match modifier with
    | Elm.VirtualDom_ModifierAttribute modifierAttribute ->
        struct (classes,
                styles,
                virtualDomBuildProp modifierAttribute.Key modifierAttribute.Value
                :: attrs)

    | Elm.VirtualDom_ModifierProperty property ->
        match
            Elm.JsonDecode_decodeValue Elm.JsonDecode_stringRaw property.Value
        with
        | Ok valueString ->
            match property.Key with
            | "className" -> struct (valueString :: classes, styles, attrs)

            | keyNotClassName ->
                struct (classes,
                        styles,
                        virtualDomBuildProp
                            (virtualDomPropertyKeyToString keyNotClassName)
                            valueString
                        :: attrs)

        | Error _ ->
            match Elm.JsonDecode_decodeValue Elm.JsonDecode_bool property.Value with
            | Ok enabled ->
                if enabled then
                    struct (classes,
                            styles,
                            virtualDomHyphenate (
                                virtualDomPropertyKeyToString property.Key
                            )
                            :: attrs)

                else
                    acc

            | Error _ ->
                struct (classes,
                        styles,
                        virtualDomBuildProp
                            (virtualDomPropertyKeyToString property.Key)
                            (Elm.StringRope.toString (
                                Elm.JsonEncode_encode 0L property.Value
                            ))
                        :: attrs)

    | Elm.VirtualDom_ModifierStyle style ->
        struct (classes,
                virtualDomEscape style.Key + ": " + virtualDomEscape style.Value
                :: styles,
                attrs)

    | Elm.VirtualDom_ModifierEventListener _ -> acc

let virtualDomModifiersToString
    (attrs: List<Elm.VirtualDom_Attribute<'event>>)
    : List<string> =
    let struct (classes, styles, regular)
        : (struct (List<string> * List<string> * List<string>)) =
        List.fold
            (fun state modifier ->
                virtualDomModifierStringsAddModifier modifier state)
            struct ([], [], [])
            attrs

    virtualDomModifierStringsAddStyles
        styles
        (virtualDomModifierStringsAddClasses classes regular)

let virtualDomTagNamespacedToString
    (maybeNamespace: ValueOption<string>)
    (tag: string)
    (modifiers: List<Elm.VirtualDom_Attribute<'event>>)
    : string =
    "<"
    + tag
    + (match maybeNamespace with
       | ValueNone -> ""
       | ValueSome namespace_ -> " xmlns=\"" + namespace_ + "\" ")
    + (match modifiers with
       | [] -> ""
       | modifier0 :: modifier1Up ->
           " "
           + String.concat
               " "
               (virtualDomModifiersToString (modifier0 :: modifier1Up)))
    + ">"

let rec virtualDomNodeToStringHelp
    (indenter: int -> string -> string)
    (nodes: List<Elm.VirtualDom_Node<'event>>)
    (soFar: HtmlString_VirtualDomNodeToStringState<'event>)
    : HtmlString_VirtualDomNodeToStringState<'event> =
    match nodes with
    | [] ->
        match soFar.Stack with
        | [] -> soFar

        | struct (tagName, cont) :: rest ->
            virtualDomNodeToStringHelp
                indenter
                cont
                { Depth = soFar.Depth - 1
                  Result =
                    indenter (soFar.Depth - 1) (virtualDomClosingTag tagName)
                    :: soFar.Result
                  Stack = rest }

    | nextNode :: rest ->
        match nextNode with
        | Elm.VirtualDom_Element element ->
            virtualDomNodeToStringHelp
                indenter
                element.Subs
                { Depth = soFar.Depth + 1
                  Result =
                    indenter
                        soFar.Depth
                        (virtualDomTagNamespacedToString
                            element.Namespace
                            element.Tag
                            element.Modifiers)
                    :: soFar.Result
                  Stack = struct (element.Tag, rest) :: soFar.Stack }

        | Elm.VirtualDom_ElementKeyed element ->
            virtualDomNodeToStringHelp
                indenter
                (List.map
                    (fun (keyedSub: Elm.VirtualDom_SubNodeKeyed<'event>) ->
                        keyedSub.Node)
                    element.Subs)
                { Depth = soFar.Depth + 1
                  Result =
                    indenter
                        soFar.Depth
                        (virtualDomTagNamespacedToString
                            element.Namespace
                            element.Tag
                            element.Modifiers)
                    :: soFar.Result
                  Stack = struct (element.Tag, rest) :: soFar.Stack }

        | Elm.VirtualDom_Text string_ ->
            virtualDomNodeToStringHelp
                indenter
                rest
                { Depth = soFar.Depth
                  Result =
                    indenter soFar.Depth (virtualDomEscapeHtmlText string_)
                    :: soFar.Result
                  Stack = soFar.Stack }

let virtualDomNodeToString
    (depth: int)
    (html: Elm.VirtualDom_Node<'event>)
    : string =
    String.concat
        (match depth with
         | 0 -> ""

         | _ -> "\n")
        (List.rev
            (virtualDomNodeToStringHelp
                (fun (indentLevel: int) (line: string) ->
                    match depth with
                    | 0 -> line

                    | _ -> lineIndent depth indentLevel line)
                [ html ]
                { Depth = 0; Result = []; Stack = [] })
                .Result)


[<EntryPoint>]
let main (_arguments: array<string>) : int =
    let app = Microsoft.AspNetCore.Builder.WebApplication.Create()

    let htmlToServe: string =
        "<!DOCTYPE html>\n\
            <html>\n\
            <body>\n"
        + virtualDomNodeToString 4 (Elm.Main_view(Elm.StringRopeOne "client"))
        + "\n</body>\n\
            </html>"

    app
        .UseRouting()
        .UseFalco([ Falco.Routing.get "/" (Response.ofHtmlString htmlToServe) ])
        .Run(Response.ofPlainText "Not found")

    0
