﻿namespace Firm

open System.IO
open Files
open Firm.Models
open FSharp.Data
open FSharp.Literate

module Transformation =
    type ConfigReader = JsonProvider<"""{ "baseUrl": "http://localhost:8080/", "disqusShortname": "a-name" }""">
    type MetaReader = JsonProvider<"""{ "title": "Hello", "date": "2013-07-27 21:22:35", "tags": ["blog", "hello"] }""">

    type Config =
        { BaseUrl: string
          DisqusShortname: string }

    let dirEnumerator id =
        Directory.EnumerateFiles(id, "*", SearchOption.AllDirectories)

    let fileExists file =
        File.Exists(file)

    let private getTagCloud (allPosts: PostModel list) =
        allPosts
        |> Seq.collect (fun pm -> pm.Tags)
        |> Seq.groupBy (fun t -> t)
        |> Seq.map (fun (t, ts) -> TagModel(t, Seq.length ts))

    let private processPosts config index archive (posts: PostFile list) =
        let postModels =
            posts
            |> List.map (fun pf ->
                let meta = MetaReader.Load(pf.Meta)
                let doc = Literate.WriteHtml(Literate.ParseMarkdownFile(pf.File.Input) |> Urls.withAbsUrls config.BaseUrl) 
                pf, PostModel(pf.Name, meta.Title, meta.Date, meta.Tags, doc))
            |> List.sortBy (fun (_, pm) -> pm.Date)
            |> List.rev
        let allPosts = postModels |> List.map snd
        let tagCloud = getTagCloud allPosts
        postModels
        |> List.map (fun (pf, p) -> (pf, SinglePostModel(config.BaseUrl, config.DisqusShortname, p, allPosts, tagCloud)))
        |> List.iter Output.Razor.writePost
        let allPostsModel = AllPostsModel(config.BaseUrl, config.DisqusShortname, allPosts, tagCloud)
        Output.Razor.writeArchive allPostsModel archive 
        index |> List.iter (Output.Razor.writeIndex allPostsModel)
        (allPosts, tagCloud)

    let private processPages config (pages: PageFile list) (allPosts, tagCloud) =
        pages
        |> List.map (fun p ->
            (p, PageModel(
                    config.BaseUrl,
                    config.DisqusShortname,
                    Literate.WriteHtml(Literate.ParseMarkdownFile(p.File.Input) |> Urls.withAbsUrls config.BaseUrl),
                    allPosts,
                    tagCloud)))
        |> List.iter Output.Razor.writePage

    let private processResources (resources: ResourceFile list) =
        resources |> List.iter Output.copyResource

    let private processInputs config index archive (posts, pages, resources) =
        let allPostsData = processPosts config index archive posts
        processPages config pages allPostsData
        processResources resources

    let generate root =
        let config = ConfigReader.Load(root @+ "config.json")
        let id = root @+ "input"
        let od = root @+ "output"
        Output.Razor.compileTemplates root
        Files.inputFiles dirEnumerator fileExists (id, od)
        |> processInputs
            {BaseUrl = config.BaseUrl; DisqusShortname = config.DisqusShortname}
            (Files.index od)
            (Files.archive od)
