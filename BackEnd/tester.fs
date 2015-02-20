open System.Xml
let file = @"D:\Audit\config.xml"

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

let xml = System.IO.File.ReadAllText(file)
let doc = new XmlDocument()
doc.LoadXml xml

let x =
    let poo = doc.SelectSingleNode ("/Config/Sites")
    poo.ChildNodes
        |> Seq.cast<XmlNode>
        |> Seq.map (fun x -> 
            let siteName = x.Attributes.["name"].Value
            x.ChildNodes
            |> Seq.cast<XmlNode>
            |> Seq.map (fun x -> 
                let host = x.Attributes.["host"].Value
                let ip = x.Attributes.["IP"].Value
                ip
                )
            |> Seq.toArray
                
            )
        |> Seq.toList


let xml = System.IO.File.ReadAllText(@"D:\Audit\config.xml")
let doc = new XmlDocument()
doc.LoadXml xml
let poo = doc.SelectNodes ("/Config/Sites/Site")

poo
    |> Seq.cast<XmlNode>
    |> Seq.map (fun x -> x.Attributes.["name"].Value)
    |> Seq.toList


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





    open System.Xml
let file = @"D:\Audit\config.xml"

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

let xml = System.IO.File.ReadAllText(file)
let doc = new XmlDocument()
doc.LoadXml xml

let x =
    let poo = doc.SelectSingleNode ("/Config/Sites")
    poo.ChildNodes
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
        |> Seq.toList


let xml = System.IO.File.ReadAllText(@"D:\Audit\config.xml")
let doc = new XmlDocument()
doc.LoadXml xml
let l = doc.SelectNodes ("/Config/Days/text()")
l.[0].Value