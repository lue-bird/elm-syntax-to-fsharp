module Main

type ElmVirtualDomModifierToBolero =
    | ElmVirtualDomModifierToBoleroStyle of Elm.VirtualDom_ModifierStyle
    | ElmVirtualDomModifierToBoleroAttr of Bolero.Attr

let elmVirtualDomModifierToBolero
    (dispatch: Elmish.Dispatch<'event>)
    (elmVirtualDomModifier: Elm.VirtualDom_Attribute<'event>)
    : ElmVirtualDomModifierToBolero =
    match elmVirtualDomModifier with
    | Elm.VirtualDom_ModifierStyle style -> ElmVirtualDomModifierToBoleroStyle style
    | Elm.VirtualDom_ModifierAttribute attribute ->
        ElmVirtualDomModifierToBoleroAttr(
            Bolero.Html.(=>) attribute.Key attribute.Value
        )
    | Elm.VirtualDom_ModifierProperty property ->
        ElmVirtualDomModifierToBoleroAttr(
            Bolero.Html.(=>) property.Key property.Value
        )
    | Elm.VirtualDom_ModifierEventListener eventListener ->
        ElmVirtualDomModifierToBoleroAttr(
            Bolero.Html.on.event eventListener.Name (fun event ->
                // needs to be annotated to please c# overloading
                let eventAsObj: obj = event

                let eventAsString: Elm.StringRope =
                    Elm.StringRopeOne(
                        System.Text.Json.JsonSerializer.Serialize(
                            eventAsObj,
                            event.GetType(),
                            System.Text.Json.JsonSerializerOptions(
                                System.Text.Json.JsonSerializerDefaults.Web
                            )
                        )
                    )
                // sadly does not respect stopping propagation
                // and preventing default because bolero does
                // not support running these after the event already happened:
                // https://github.com/fsbolero/Bolero/issues/97#issuecomment-555940005
                match eventListener.Handler with
                | Elm.VirtualDom_Normal decoder ->
                    match Elm.JsonDecode_decodeString decoder eventAsString with
                    | Error jsonDecodeError ->
                        System.Console.WriteLine(
                            "failed to decode DOM event "
                            + Elm.StringRope.toString (
                                Elm.JsonDecode_errorToString jsonDecodeError
                            )
                        )
                    | Ok elmEvent -> dispatch elmEvent
                | Elm.VirtualDom_MayStopPropagation decoder ->
                    match Elm.JsonDecode_decodeString decoder eventAsString with
                    | Error jsonDecodeError ->
                        System.Console.WriteLine(
                            "failed to decode DOM event "
                            + Elm.StringRope.toString (
                                Elm.JsonDecode_errorToString jsonDecodeError
                            )
                        )
                    | Ok struct (elmEvent, _) -> dispatch elmEvent
                | Elm.VirtualDom_MayPreventDefault decoder ->
                    match Elm.JsonDecode_decodeString decoder eventAsString with
                    | Error jsonDecodeError ->
                        System.Console.WriteLine(
                            "failed to decode DOM event "
                            + Elm.StringRope.toString (
                                Elm.JsonDecode_errorToString jsonDecodeError
                            )
                        )
                    | Ok struct (elmEvent, _) -> dispatch elmEvent
                | Elm.VirtualDom_Custom decoder ->
                    match Elm.JsonDecode_decodeString decoder eventAsString with
                    | Error jsonDecodeError ->
                        System.Console.WriteLine(
                            "failed to decode DOM event "
                            + Elm.StringRope.toString (
                                Elm.JsonDecode_errorToString jsonDecodeError
                            )
                        )
                    | Ok elmEvent -> dispatch elmEvent.Message)
        )

let elmVirtualDomModifiersToBolero
    (dispatch: Elmish.Dispatch<'event>)
    (elmVirtualDomModifiers: List<Elm.VirtualDom_Attribute<'event>>)
    : Bolero.Attr =
    let combined
        : {| Attr: Bolero.Attr
             Styles: List<Elm.VirtualDom_ModifierStyle> |} =
        List.fold
            (fun
                (soFar:
                    {| Attr: Bolero.Attr
                       Styles: List<Elm.VirtualDom_ModifierStyle> |})
                elmVirtualDomModifier ->
                match
                    elmVirtualDomModifierToBolero dispatch elmVirtualDomModifier
                with
                | ElmVirtualDomModifierToBoleroStyle additionalStyle ->
                    {| Attr = soFar.Attr
                       Styles = additionalStyle :: soFar.Styles |}
                | ElmVirtualDomModifierToBoleroAttr additionalAttr ->
                    {| Styles = soFar.Styles
                       Attr = Bolero.Html.attrs.Combine(soFar.Attr, additionalAttr) |})
            {| Attr = Bolero.Html.attr.empty ()
               Styles = [] |}
            elmVirtualDomModifiers

    match combined.Styles with
    | [] -> combined.Attr
    | style0 :: style1Up ->
        Bolero.Html.attrs.Combine(
            combined.Attr,
            Bolero.Html.attr.style (
                String.concat
                    " "
                    (List.map
                        (fun (style: Elm.VirtualDom_ModifierStyle) ->
                            style.Key + ": " + style.Value + ";")
                        (style0 :: style1Up))
            )
        )

let rec elmVirtualDomNodeToBolero
    (dispatch: Elmish.Dispatch<'event>)
    (maybeKey: ValueOption<string>)
    (elmVirtualDomNode: Elm.VirtualDom_Node<'event>)
    : Bolero.Node =
    match elmVirtualDomNode with
    | Elm.VirtualDom_Text text -> Bolero.Html.text text
    | Elm.VirtualDom_Element element ->
        Bolero.Html.elt element.Tag {
            match maybeKey with
            | ValueSome key -> Bolero.Html.attr.key key
            | ValueNone -> Bolero.Html.attr.empty ()

            elmVirtualDomModifiersToBolero dispatch element.Modifiers

            Bolero.Html.forEach
                element.Subs
                (elmVirtualDomNodeToBolero dispatch ValueNone)
        }
    | Elm.VirtualDom_ElementKeyed element ->
        Bolero.Html.elt element.Tag {
            match maybeKey with
            | ValueSome key -> Bolero.Html.attr.key key
            | ValueNone -> Bolero.Html.attr.empty ()

            elmVirtualDomModifiersToBolero dispatch element.Modifiers

            Bolero.Html.forEach element.Subs (fun subKeyed ->
                elmVirtualDomNodeToBolero
                    dispatch
                    (ValueSome subKeyed.Key)
                    subKeyed.Node)
        }
    | Elm.VirtualDom_NodeLazy nodeLazy ->
        Bolero.Html.lazyComp
            (fun _ ->
                elmVirtualDomNodeToBolero dispatch maybeKey (nodeLazy.Construct()))
            nodeLazy.Keys

type App() =
    inherit Bolero.ProgramComponent<Elm.Main_State, Elm.Main_Event>()

    override _.Program =
        Elmish.Program.mkSimple
            (fun _ -> Elm.Main_initialState)
            Elm.Main_update
            (fun state dispatch ->
                elmVirtualDomNodeToBolero dispatch ValueNone (Elm.Main_view state))
