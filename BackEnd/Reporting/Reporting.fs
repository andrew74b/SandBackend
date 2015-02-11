namespace Sands.Reporting
open System
open Sands.Helpers

type compareReport =
    {
        Site: string;
        DateString: string;
        Files: string[];
        Message: string;
    }

type mergedReport =
    {
        Site: string
        DateString: string;
        sStatus: bool;
        sFiles: string[];
        dStatus: bool;
        dFiles: string[];
    }

type getReport (site, source, destination, days:float, extension) =
    let sourceReport source =
        Sands.Helpers.GetWorklist.getSiteStatus site source extension days
    
    let destinationReport destination =
        Sands.Helpers.GetWorklist.getSiteStatus site destination extension days
    
    let addIndex i data source =
        data source
        |> Seq.map (fun dat -> i, dat)

    let folderChecker mergedEntry =
        if mergedEntry.dStatus = false then
            if mergedEntry.sStatus = true then
                {
                    Site = mergedEntry.Site;
                    DateString = mergedEntry.DateString;
                    Files = Array.empty;
                    Message = "Destination directory does not exist";
                }

            else
            {
                Site = mergedEntry.Site;
                DateString = mergedEntry.DateString;
                Files = Array.empty;
                Message = "Source/Destination directory does not exist";
            }
        else
        {
            Site = mergedEntry.Site;
            DateString = mergedEntry.DateString;
            Files = Array.empty;
            Message = "pass";
        }

    let fileChecker mergedEntry =
        let uniqueD = set mergedEntry.dFiles - set mergedEntry.sFiles
        let uniqueS = set mergedEntry.sFiles - set mergedEntry.dFiles
        if Set.count uniqueS > 0 then
        {
            Site = mergedEntry.Site;
            DateString = mergedEntry.DateString;
            Files = Set.toArray uniqueS;
            Message = "Source directory contains " + (Set.count uniqueS).ToString() + " more files than destination";
        }
        else
        {
            Site = mergedEntry.Site;
            DateString = mergedEntry.DateString;
            Files = Array.empty;
            Message = "pass";
        }

    member this.data =
        List.zip (sourceReport source) (destinationReport destination)
            |> List.map (fun entry ->
                let s, d = entry
                {
                    Site = s.Site
                    DateString = s.Date;
                    sStatus = s.Status;
                    sFiles = s.Files;
                    dStatus = d.Status;
                    dFiles = d.Files;
                })

    member this.parseData() =
        this.data
        |> List.map (fun status ->
            let checkedFolders = folderChecker status
            if checkedFolders.Message = "pass" then
                fileChecker status
            else
                checkedFolders)