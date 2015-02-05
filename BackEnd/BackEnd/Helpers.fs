namespace Sands.Helpers

open System

type Span = Span of TimeSpan with
    static member (+) (d:DateTime, Span wrapper) = d + wrapper
    static member Zero = Span(new TimeSpan(0L))
type AuditStatus =
    {
        Site: string;
        Date: string;
        Files: string[];
        Status: string;
    }

module GetWorklist =
    (*
        Compares 2 directory paths and returns after the following logic:
            Source contains more files than the destination
            If the source has 0 files for a date, a warning is sent
    *)
    let dateArray days= 
        let now = DateTime.Now
        let startDate = now.AddDays(-(days))
        let ts = TimeSpan.FromDays(1.0)
        let dateList = [startDate .. Span(ts) .. now]
        let toDateString (date:DateTime) = date.ToString("yyyyMMdd")
        List.map toDateString dateList

    let getSiteStatus site location filter days =
        dateArray days
        |> List.map (fun (date) ->
            let statusOutput = 
                if System.IO.Directory.Exists(location + site + @"\" + date) then
                    {
                        Site = site;
                        Date = date;
                        Files = 
                            System.IO.Directory.GetFiles(location + site + @"\" + date, "*." + filter) 
                            |> Seq.filter (fun x -> x.EndsWith(filter) )
                            |> Seq.map (fun filename -> (System.IO.FileInfo filename).Name)
                            |> Seq.toArray
                        Status = "Good"
                    }
                else
                    {
                        Site = site;
                        Date = date;
                        Files = Array.empty;
                        Status = "Dir does not exist"
                    }
            statusOutput)

    let goodCondition siteStatus = (siteStatus.Status = "Good") && (siteStatus.Files.Length > 0)
    let badCondition siteStatus = (siteStatus.Status = "Dir does not exist") || (siteStatus.Files.Length = 0)

    let getFilesStatus site source filter days condition = 
        getSiteStatus site source filter days
        |> Seq.filter (fun siteStatus -> condition siteStatus) 
        |> Seq.map (fun siteStatus -> 
            siteStatus.Files
            |> Seq.map (fun filename -> siteStatus.Date + @"\" + filename ))
        |> Seq.concat
        |> List.ofSeq


    let getWorkList site source destination filter days = 
        let SourceFiles = Set (getFilesStatus site source filter days goodCondition)
        let destinationFiles = Set (getFilesStatus site destination filter days goodCondition)
        
        let uniqueFiles check checkAgainst = 
            check - checkAgainst
            |> Set.toList

        uniqueFiles SourceFiles destinationFiles
