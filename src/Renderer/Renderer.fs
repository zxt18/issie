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
open CommonTypes
open Fable.SimpleJson



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
    Json.tryParseAs<CommonTypes.TestJson3.Component> json3
    |> printfn "testJson3: \n%A\n"




printfn "Starting renderer..."

checkJson()

printfn "done"

