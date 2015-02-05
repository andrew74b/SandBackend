namespace Sands.Backup

open System
open System.Linq
open System.IO.Compression
open System.Xml
open Sands.Helpers

type Config =
    {
        Sites: string[];
        Days: float;
        Source: string;
        Destination: string;
    }
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

type ConvertToEvtx (configLocation) =

    let configFile = new Settings(configLocation)
    let config = configFile.getSettings
    let source = config.Source
    let sites = config.Sites
    let days = config.Days
   
    let dateList = GetWorklist.dateArray days
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

type ZipEvtx (configLocation) =

    let configFile = new Settings(configLocation)
    let config = configFile.getSettings
    let source = config.Source
    let sites = config.Sites
    let days = config.Days
    let filter = "*.zip"
   
    let dateList = GetWorklist.dateArray days
    member this.zipLedgers =
        sites
        |> Seq.map (fun site -> 
            dateList
            |> Seq.filter (fun date -> 
                System.IO.Directory.Exists(source + site + @"\" + date) 
                && (System.IO.Directory.GetFiles(source + site + @"\" + date, "*.evtx")).Length > 0
                && (System.IO.Directory.GetFiles(source + site + @"\" + date, "*.zip")).Length = 0)

            |> Seq.map (fun date -> 
                {
                    zipFile = source + site + @"\" + date + @"\" + date + ".zip";
                    evtxs = System.IO.Directory.GetFiles(source + site + @"\" + date, "*.evtx");
                }))
        |> Seq.concat
        |> Seq.toList

    member this.zipEvtx() =
        let createAndZipFile zipLedger = 
            async {
                let zipFile = ZipFile.Open(zipLedger.zipFile, ZipArchiveMode.Create)

                //Why do i have to use a for loop ???
                for file in zipLedger.evtxs do
                    zipFile.CreateEntryFromFile(file, System.IO.Path.GetFileName(file), CompressionLevel.Optimal) |> ignore
                zipFile.Dispose()
            }

        let results =
            this.zipLedgers
            |> Seq.map (createAndZipFile)
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Seq.toList

        results 

//type comparePathContents (source, destination)



//Cleanup how filenames are used

(*
Add-Type -path D:\Audit\BackEnd\BackEnd\BackEnd\bin\Debug\BackEnd.dll
$a = (New-Object Audit.Copy.convertAndZip "D:\Audit\config.xml")
$a.convertEVT()

*)