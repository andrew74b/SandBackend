namespace Sands.Reporting

open System
open Sands.Helpers

type compareReport =
    {
        Site: string;
        DateString: string;
        FileCount: int;
        Message: string;
    }

type getReport (site, source, destination, days:float, extension) =
    let sourceReport source = 
        Sands.Helpers.GetWorklist.getSiteStatus site source extension days 

    let destinationReport source = 
        Sands.Helpers.GetWorklist.getSiteStatus site destination extension days
    
    let determineStatus status = 
        if status.Status = false then
            {
                Site = status.Site;
                DateString = status.Date;
                FileCount = 0;
                Message = "Source directory does not exist";
            }
        else
            {
                Site = status.Site;
                DateString = status.Date;
                FileCount = 0;
                Message = "Source directory does not exist";           
            }

    member this.getReport() = 
        sourceReport source
        |> List.map (fun (status) -> "d")

