namespace Audit.Copy

open System
open System.Linq
open System.IO.Compression
open System.Xml

type AuditStatus =
    {
        Site: string;
        Date: string;
        Files: string[];
        Status: string;
    }
type Config =
    {
        Sites: string[];
        Days: float;
        Source: string;
        Destination: string;
    }
type Span = Span of TimeSpan with
    static member (+) (d:DateTime, Span wrapper) = d + wrapper
    static member Zero = Span(new TimeSpan(0L))
type copyLedger = 
    {
        Source: string;
        Destination: string;
    }
type zipLedger = 
    {
        zipFile: string;
        evtxs: string[];
    }

type Settings (location) =

    let xml = System.IO.File.ReadAllText(location)
    let doc = new XmlDocument()
    member this.getSettings = 
        doc.LoadXml xml;
        let readnode node = 
            doc.SelectNodes (node + "/text()")
            |> Seq.cast<XmlNode>
            |> Seq.map (fun node -> node.Value)
            |> Seq.toList
        
        {
            Sites = (readnode "/Config/Sites/Site") |> Seq.toArray;
            Days = (float)(readnode "/Config/Days").[0];
            Source = (readnode "/Config/Source").[0];
            Destination = (readnode "/Config/Destination").[0]
        }

module GetWorklist =
    
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
                            System.IO.Directory.GetFiles(location + site + @"\" + date, filter) 
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

    let getFilesStatus site source filter days condition= 
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

type Copy (configLocation) = 

    let configFile = new Settings(configLocation)
    let config = configFile.getSettings
    let source = config.Source
    let destination = config.Destination
    let sites = config.Sites
    let days = config.Days
    let filter = "*.zip"

    member this.WorkList = 
        sites
        |> Seq.map (fun site -> 
            let workListQuery = GetWorklist.getWorkList site source destination filter days
            
            if workListQuery.Length > 0 then
                workListQuery
                |> Seq.filter (fun file-> not (String.IsNullOrEmpty(file)))
                |> Seq.map (fun file -> 
                    {
                        Source = source + site + @"\" + file
                        Destination = destination + site + @"\" + file
                    })
            else
                Seq.empty
            )
        |> Seq.concat

    member this.smartCopy() =
        let copy (workEntry:copyLedger) =
            async {
                System.IO.File.Copy(workEntry.Source, workEntry.Destination)
                return workEntry.Source + " copied to " + workEntry.Destination
            }

        this.WorkList
        |> Seq.map (copy)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.toList

(* used for performance testing 
    let dumCopy filesToCopy site =
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        filesToCopy |> List.map (fun file -> 
            let fullSourceName = source + site + @"\" + file
            let fullDestinationName = destination + site + @"\" + file
            System.IO.File.Copy(fullSourceName, fullDestinationName)
            fullSourceName + " copied to " + fullDestinationName)
*)

type convertAndZip (configLocation)=

    let configFile = new Settings(configLocation)
    let config = configFile.getSettings
    let source = config.Source
    let destination = config.Destination
    let sites = config.Sites
    let days = config.Days
    let filter = "*.zip"
    
    let dateList = GetWorklist.dateArray days
    member this.zipEvtx =
        sites
        |> Seq.map (fun site -> 
            dateList
            |> Seq.filter (fun date -> 
                (System.IO.Directory.Exists(source + site + @"\" + date) && ((System.IO.Directory.GetFiles(source + site + @"\" + date, "*.evtx")).Length > 0) && (System.IO.Directory.GetFiles(source + site + @"\" + date, "*.zip")).Length = 0))

            |> Seq.map (fun date -> 
                {
                    zipFile = source + site + @"\" + date + @"\" + date + ".zip";
                    evtxs = System.IO.Directory.GetFiles(source + site + @"\" + date, "*.evtx");
                }))
                (*
                
                evtxs = source + site + @"\" + date + 
            let folderName = System.IO.Directory.GetParent(zipFolder + @"\" + "filler").Name
            )
        |> Seq.concat


        let folderName = System.IO.Directory.GetParent(zipFolder + @"\" + "filler").Name
        let zipFile = ZipFile.Open(zipFolder + @"\" + folderName + ".zip", ZipArchiveMode.Create)
        let createAndZipFile fileToZip = 
            async {
                ignore (zipFile.CreateEntryFromFile(fileToZip, System.IO.Path.GetFileName(fileToZip)), CompressionLevel.Optimal)
            }

        let results =
            ignore (System.IO.Directory.GetFiles(zipFolder, "*.evtx")
            |> Seq.map (createAndZipFile)
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Seq.toList)
            ignore (zipFile.Dispose())
            

        results *)

    member this.evtFilesToDo = 
        sites
        |> Seq.map (fun site -> 
            GetWorklist.getFilesStatus site source "*.evt" days GetWorklist.goodCondition 
            |> List.filter (fun filename -> filename.EndsWith("evt"))
            |> List.map (fun partialName -> source + site + @"\" + partialName)
            |> List.filter (fun filename -> not (System.IO.File.Exists(filename + "x")))
            )
        |> Seq.concat

    member this.convertEVT() = 
        let convertEvtToEvtx evt = 
            async {
                let evtx = evt + "x"
                let wevt = new System.Diagnostics.Process();
                wevt.StartInfo.FileName <- "wevtutil.exe";
                wevt.StartInfo.Arguments <- ("epl " + evt + " " + evtx + " /lf:true")
                wevt.StartInfo.RedirectStandardOutput <- true
                wevt.StartInfo.UseShellExecute <- false
                let junk = wevt.Start()
                wevt.WaitForExit()
                return evt + " converted!"
            }

        let results =
            this.evtFilesToDo
            |> Seq.map (convertEvtToEvtx)
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Seq.toList

        results



//Cleanup how filenames are used

(*
add-type -Path D:\Audit\Copy\Library1\Library1\bin\Debug\Library1.dll
$files = [Audit.Copy.Copy]::filesToCopy("ARN","*")
measure-command{[Audit.Copy.Copy]::smartCopy($files, "ARN")}

add-type -Path D:\Audit\Copy\Library1\Library1\bin\Debug\Library1.dll
*)





