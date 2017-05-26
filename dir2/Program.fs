// подключаем FSharp.Data
#r "../packages/FSharp.Data.2.3.3/lib/net40/FSharp.Data.dll"
open FSharp.Data
open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "nadysha1997@yandex.ru"

let lab3 () =
    let checkURL (url: string) = async {
        if ((Http.Request(url, silentHttpErrors = true)).StatusCode = 200)
        then return 1  // есть такой url
        else return 0  // нет такого
    }
    let lab = HtmlDocument.Load("https://nuget.codeplex.com/team/view") 
    lab.Descendants ["div"]
        |> Seq.filter (fun (x:HtmlNode) -> x.HasId "DevelopersContainer")  // взятие разработчиков
        |> Seq.collect (fun (x:HtmlNode) -> x.Descendants ["a"])
        |> Seq.map (fun x -> x.InnerText())  // взятие их логинов
        |> Seq.map (fun x -> "https://github.com/" + x)
        |> Seq.map (fun x -> x |> checkURL |> Async.RunSynchronously)  // проверка, что есть url гитхаба с таким логином
        |> Seq.sum


let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("result", lab3().ToString())
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab3"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString
