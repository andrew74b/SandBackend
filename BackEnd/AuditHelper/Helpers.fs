namespace Sands.Helpers

open System
open System.Xml

type Span = Span of TimeSpan with
    static member (+) (d:DateTime, Span wrapper) = d + wrapper
    static member Zero = Span(new TimeSpan(0L))

type AuditStatus =
    {
        Site: string;
        Date: string;
        Files: string[];
        Status: bool;
    }

type FilerInfo = 
    {
        Host: string;
        IP: string
    }

type SiteInfo = 
    {
        Site: string;
        Filers: FilerInfo[]
    }

type Config =
    {
        SiteInfo: SiteInfo[];
        Days: float;
        Source: string;
        Destination: string;
    }

type SandsConfig (file) =

    let xml = System.IO.File.ReadAllText(file)
    let doc = new XmlDocument()
    

    let SiteInfo = 
        doc.LoadXml xml
        doc.SelectNodes ("/Config/Sites/Site")
        |> Seq.cast<XmlNode>
        |> Seq.map (fun x -> 
            let siteName = x.Attributes.["name"].Value
            
            let fInfo = 
                x.ChildNodes
                |> Seq.cast<XmlNode>
                |> Seq.map (fun x -> 
                    let host = x.Attributes.["host"].Value
                    let ip = x.Attributes.["IP"].Value
                    {
                        Host = host;
                        IP = ip
                    }
                    )
                |> Seq.toArray
                
            {
                Site = siteName
                Filers = fInfo
            })
        |> Seq.toArray

    let getTag tagname = 
        doc.LoadXml xml
        (doc.SelectNodes ("/Config/" + tagname + "/text()")).[0].Value

    member this.Config = 
        {
            SiteInfo = SiteInfo;
            Days = (float) (getTag "Days");
            Source = getTag "Source";
            Destination = getTag "Destination"        
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
                        Status = true
                    }
                else
                    {
                        Site = site;
                        Date = date;
                        Files = Array.empty;
                        Status = false
                    }
            statusOutput)

    let goodCondition siteStatus = (siteStatus.Status = true) && (siteStatus.Files.Length > 0)
    let badCondition siteStatus = (siteStatus.Status = false) || (siteStatus.Files.Length = 0)

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


