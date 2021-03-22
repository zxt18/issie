module DrawLibHLP9

open CommonTypes
open JSTypes
open JSHelpers
//open DiagramStyle
open SimulatorTypes

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props



let private createComponent  
        (comp:CommonTypes.Component) 
        (pos:XYPos) 
        : Symbol =
        let h, w = getHeightWidthOf comp.Type
        let hostId = CommonTypes.ComponentId comp.Id
        let inputPortsPosList, outputPortsPosList = getPortPositions comp.Type pos
        let inputPorts, outputPorts = createPorts comp.Type hostId inputPortsPosList outputPortsPosList
        {
            Id = hostId 
            Type = comp.Type
            Label = comp.Label
    
            InputPorts = inputPorts
            OutputPorts = outputPorts
    
            InputOrientation = Left
            OutputOrientation = Right
    
            Pos = pos
            LastDragPos = {X=0. ; Y=0.} // initial value can always be this
            IsDragging = false // initial value can always be this
            IsSelected = false
            HasError = false
            NumberOfConnections = 0
    
            H = h
            W = w
            BBox = calculateBoundingBox h w pos
        }