module Renderer

open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Electron
open Electron.Helpers
open ModelType
open CommonTypes
open Fable.SimpleJson


let isMac = Node.Api.``process``.platform = Node.Base.Darwin



module TestJson =
    type PortType = Input | Output

    type Port = {
        Id : string
        // For example, an And would have input ports 0 and 1, and output port 0.
        // If the port is used in a Connection record as Source or Target, the Number is None. 
        PortNumber : int option
        PortType : PortType
        HostId : string
    }

    /// Name identified the LoadedComponent used.
    /// The labels define legends on symbol.
    /// Label strings are unique per CustomComponent.
    /// Multiple CustomComponent instances are differentiated by Component data.
    type CustomComponentType = {
        Name: string
        // Tuples with (label * connection width).
        InputLabels: (string * int) list
        OutputLabels: (string * int) list 
    }

    type Memory = {
        // How many bits the address should have.
        // The memory will have 2^AddressWidth memory locations.
        AddressWidth : int 
        // How wide each memory word should be, in bits.
        WordWidth : int
        // Data is a list of <2^AddressWidth> elements, where each element is a
        // 64 bit integer. This makes words longer than 64 bits not supported.
        // This can be changed by using strings instead of int64, but that is way
        // less memory efficient.
        Data : Map<int64,int64>
    }

    // Types instantiating objects in the Digital extension.
    type ComponentType =
        | Input of BusWidth: int | Output of BusWidth: int | IOLabel 
        | BusSelection of OutputWidth: int * OutputLSBit: int
        | Constant of Width: int * ConstValue: int
        | Not | And | Or | Xor | Nand | Nor | Xnor |Decode4
        | Mux2 | Demux2
        | NbitsAdder of BusWidth: int
        | Custom of CustomComponentType // schematic sheet used as component
        | MergeWires | SplitWire of BusWidth: int // int is bus width
        // DFFE is a DFF with an enable signal.
        // No initial state for DFF or Register? Default 0.
        | DFF | DFFE | Register of BusWidth: int | RegisterE of BusWidth: int 
        | AsyncROM of Memory | ROM of Memory | RAM of Memory // memory is contents

    /// JSComponent mapped to F# record.
    /// Id uniquely identifies the component within a sheet and is used by draw2d library.
    /// Label is optional descriptor displayed on schematic.
    type Component = {
        Id : string
        Type : ComponentType
        Label : string // All components have a label that may be empty.
        InputPorts : Port list
        OutputPorts : Port list
        X : int
        Y : int
        H : int
        W : int
    }


let checkJson () =


    let json3 = """
            {
             "H":65,"Id":"a46275de-c854-18f1-a9bd-929544c3336b",
             "InputPorts":[{"HostId":"a46275de-c854-18f1-a9bd-929544c3336b","Id":"8f654572-db8a-f3f4-5a01-71f470db0cc7","PortNumber":0,"PortType":"Input"}],
             "Label":"G1",
             "OutputPorts":[{"HostId":"a46275de-c854-18f1-a9bd-929544c3336b",
             "Id":"b911a531-46f2-5e15-8e6e-39a38c49af69",
             "PortNumber":0,
             "PortType":"Output"}],
             "Type":"Not",
             "W":42,
             "X":379,
             "Y":60
             }"""


    //Json.tryParseAs<Component1> json2 // error
    //|> printfn "\n%A\n"
    Json.tryParseAs<TestJson.Component> json3 // error
    |> printfn "TestJson: \n%A\n"
    Json.tryParseAs<ModelType.TestJson2.Component> json3 // error
    |> printfn "TestJson2: \n%A\n"
    Json.tryParseAs<CommonTypes.TestJson3.Component> json3
    |> printfn "testJson3: \n%A\n"

(****************************************************************************************************
*
*                                  MENU HELPER FUNCTIONS
*
****************************************************************************************************)



let exitApp() =
    electron.ipcRenderer.send("exit-the-app",[||])

let menuSeparator =
   let sep = createEmpty<MenuItemOptions>
   sep.``type`` <- MenuItemType.Separator
   sep

/// Make action menu item from name, opt key to trigger, and action.
let makeItem (label : string) (accelerator : string option) (iAction : KeyboardEvent -> unit) =
   let handlerCaster f = System.Action<MenuItem, BrowserWindow, KeyboardEvent> f 
   let item = createEmpty<MenuItemOptions>
   item.label <- label
   match accelerator with | Some a -> item.accelerator <- a | _ -> ()
   item.click <- handlerCaster (fun _ _ keyEvent-> iAction keyEvent)
   item

/// Make role menu from name, opt key to trigger, and action.
let makeRoleItem label accelerator role =
   let item = makeItem label accelerator (fun _ -> ())
   item.role <- role
   item

/// make conditional menu item from condition, name, opt key to trigger, and role
let makeCondRoleItem cond label accelerator role =
   let item = makeItem label accelerator (fun _ -> ())
   item.role <- role
   item.visible <- cond
   item

/// make conditional menu item from condition, name, opt key to trigger, and action
let makeCondItem cond label accelerator action =
   let item = makeItem label accelerator action
   item.visible <- cond
   item


let makeElmItem (label:string) (accelerator : string) (action : unit -> unit) =
    jsOptions<MenuItemOptions> <| fun item ->
        item.label <- label
        item.accelerator <- accelerator
        item.click <- fun _ _ _ -> action()


/// Make a new menu from a a list of menu items
let makeMenu (topLevel: bool) (name : string) (table : MenuItemOptions list) =
   let subMenu = createEmpty<MenuItemOptions>
   subMenu.``type`` <- if topLevel then MenuItemType.Normal else MenuItemType.SubMenu
   subMenu.label <- name
   subMenu.submenu <- U2.Case1 (table |> Array.ofList)
   subMenu    

let displayPerformance n m = Helpers.checkPerformance n m JSHelpers.startTimer JSHelpers.stopAndLogTimer




let fileMenu (dispatch) =
    makeMenu false "Sheet" [
        makeItem "New Sheet" (Some "CmdOrCtrl+N") (fun ev -> dispatch (MenuAction(MenuNewFile,dispatch)))
        makeItem "Save Sheet" (Some "CmdOrCtrl+S") (fun ev -> dispatch (MenuAction(MenuSaveFile,dispatch)))
        makeItem "Print Sheet" (Some "CmdOrCtrl+P") (fun ev -> dispatch (MenuAction(MenuPrint,dispatch)))
        makeItem "Exit Issie" None (fun ev -> exitApp())
        makeItem ("About Issie " + Version.VersionString) None (fun ev -> PopupView.viewInfoPopup dispatch)
        makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) "Restart app" None (fun _ -> 
            let webContents = electron.remote.getCurrentWebContents()
            webContents.reload())
        makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) "Trace all" None (fun _ -> 
            JSHelpers.debugTrace <- Set.ofList ["update";"view"])
        makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) "Trace off" None (fun _ -> 
            JSHelpers.debugTrace <- Set.ofList [])
        makeCondItem (JSHelpers.debugLevel <> 0 && not isMac) "Run performance check" None (fun _ -> 
            displayPerformance 100 1000000)
     ]

let viewMenu dispatch =
    let devToolsKey = if isMac then "Alt+Command+I" else "Ctrl+Shift+I"
    makeMenu false "View" [
        makeRoleItem "Toggle Fullscreen" (Some "F11") MenuItemRole.ToggleFullScreen
        menuSeparator
        makeRoleItem "Zoom  In" (Some "CmdOrCtrl+Plus") MenuItemRole.ZoomIn
        makeRoleItem "Zoom  Out" (Some "CmdOrCtrl+-") MenuItemRole.ZoomOut
        makeRoleItem "Reset Zoom" (Some "CmdOrCtrl+0") MenuItemRole.ResetZoom
        menuSeparator
        makeItem "Diagram Zoom In" (Some "CmdOrCtrl+z") (fun ev -> dispatch <| MenuAction(MenuZoom 1.25, dispatch))
        makeItem "Diagram Zoom Out" (Some "CmdOrCtrl+y") (fun ev -> dispatch <| MenuAction(MenuZoom (1. / 1.25), dispatch))
        menuSeparator
        makeCondItem (JSHelpers.debugLevel <> 0) "Toggle Dev Tools" (Some devToolsKey) (fun _ -> 
            let webContents = electron.remote.getCurrentWebContents()
            webContents.toggleDevTools())
    ]


// Editor Keybindings (also items on Edit menu)
// Use Elmish subscriptions to attach external source of events such as keyboard
// shortcuts. According to electron documentation, the way to configure keyboard
// shortcuts is by creating a menu.
let editMenu dispatch =
    let dispatch = ModelType.KeyboardShortcutMsg >> dispatch

    jsOptions<MenuItemOptions> <| fun invisibleMenu ->
        invisibleMenu.``type`` <- MenuItemType.SubMenu
        invisibleMenu.label <- "Edit"
        invisibleMenu.visible <- true
        invisibleMenu.submenu <-
            [| makeElmItem "Save Sheet" "CmdOrCtrl+S" (fun () -> dispatch ModelType.CtrlS)
               makeElmItem "Copy" "Alt+C" (fun () -> dispatch ModelType.AltC)
               makeElmItem "Paste" "Alt+V" (fun () -> dispatch ModelType.AltV)
               makeElmItem "Delete"  (if isMac then "Backspace" else "delete") (fun () -> dispatch ModelType.DEL)
               makeElmItem "Undo" "Alt+Z" (fun () -> dispatch ModelType.AltZ)
               makeElmItem "Redo" "Alt+Shift+Z" (fun () -> dispatch ModelType.AltShiftZ) |]
            |> U2.Case1

let attachMenusAndKeyShortcuts dispatch =
    let sub dispatch =
        let menu = 
            [|

                fileMenu dispatch

                editMenu dispatch 

                viewMenu dispatch
            |]          
            |> Array.map U2.Case1
            |> electron.remote.Menu.buildFromTemplate   
        menu.items.[0].visible <- Some true
        electron.remote.app.applicationMenu <- Some menu

    Cmd.ofSub sub    

// This setup is useful to add other pages, in case they are needed.

type Model = ModelType.Model

type Messages = ModelType.Msg

// -- Init Model

let init() = 
    checkJson()
    JSHelpers.setDebugLevel()
    DiagramMainView.init(), Cmd.none


// -- Create View

let view model dispatch = DiagramMainView.displayView model dispatch

// -- Update Model

let update msg model = Update.update msg model

printfn "Starting renderer..."

Program.mkProgram init update view
|> Program.withReactBatched "app"
|> Program.withSubscription attachMenusAndKeyShortcuts
|> Program.run
