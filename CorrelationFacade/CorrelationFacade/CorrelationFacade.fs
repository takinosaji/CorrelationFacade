module CorrelationFacade

    open System
    open System.Threading
    open System.Web
    
    let [<Literal>] private AbsentCorrelationId = "[no-correlation-id]"
    let [<Literal>] private CurrentCorrelationIdKey = "CurrentCorrelationId"
    let private CorrelationIdHolder = AsyncLocal<string>()
    
    type CorrelationScope(dispose: unit -> unit) =
        interface IDisposable with
            member x.Dispose() =
                dispose()  
        
    let CreateNewCorrelationId() = Guid.NewGuid().ToString()
    
    let private CorrelationIdIsMissing correlationId =
        correlationId = null || correlationId = AbsentCorrelationId
    
    let SetCurrentCorrelationId (newCorrelationId: string) =
        let validatedCorrelationId =
            if newCorrelationId = null then AbsentCorrelationId else newCorrelationId 
        let currentCorrelationId = match HttpContext.Current with
            | null -> CorrelationIdHolder.Value
            | _ ->
                HttpContext.Current.Items.[CurrentCorrelationIdKey] <- validatedCorrelationId
                HttpContext.Current.Items.[CurrentCorrelationIdKey] :?> string
            
        CorrelationIdHolder.Value <- validatedCorrelationId
        
        if (CorrelationIdIsMissing currentCorrelationId && newCorrelationId <> AbsentCorrelationId) then
            // TODO: log warning correlationId already set
        ()
        
    let GetCurrentCorrelationId() =
        let currentCorrelationId =
            if HttpContext.Current = null || HttpContext.Current.Items.[CurrentCorrelationIdKey] = null
            then CorrelationIdHolder.Value else HttpContext.Current.Items.[CurrentCorrelationIdKey] :?> string
        
        match CorrelationIdIsMissing currentCorrelationId with
            | true ->
                // TODO: log missing correlationId
                AbsentCorrelationId
            | _ -> currentCorrelationId
        
    let SetScopedCorrelationId newCorrelationId =
        SetCurrentCorrelationId newCorrelationId
        new CorrelationScope (fun () -> SetCurrentCorrelationId AbsentCorrelationId)
        
    let CorrelationIdIsValid correlationId =
        Guid.TryParse correlationId |> fst