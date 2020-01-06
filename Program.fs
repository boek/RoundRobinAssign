type 'a ``[]`` with
    member x.Shuffle() =
        let swap indexA indexB =
            let temp = x.[indexA]
            x.[indexA] <- x.[indexB]
            x.[indexB] <- temp

        let rand = System.Random()
        let length = x.Length
        for i in 0 .. (length - 1) do
            let swapWith = rand.Next(0, length - 1)
            swap i swapWith
        x

module Environment =
    open System

    type Key = AuthToken | GroupToAssign | LabelToAssign | Repo

    [<Literal>]
    let private AUTH_TOKEN_KEY = "INPUT_REPOTOKEN"
    [<Literal>]
    let private GROUP_TO_ASSIGN = "INPUT_GROUPTOASSIGN"
    [<Literal>]
    let private LABEL_TO_ASSIGN = "INPUT_LABELTOASSIGN"
    [<Literal>]
    let private REPO = "INPUT_REPO"

    let Get key =
        let result = match key with
                     | AuthToken -> AUTH_TOKEN_KEY
                     | GroupToAssign -> GROUP_TO_ASSIGN
                     | LabelToAssign -> LABEL_TO_ASSIGN
                     | Repo -> REPO

        let variable = Environment.GetEnvironmentVariable result

        match variable with
        | null -> None
        | _ -> Some variable

module GitHub =
    open Octokit

    type Client = private Client of GitHubClient
    type ValidString = private ValidString of string

    let Create token =
        let client = GitHubClient(ProductHeaderValue("ActionApi"))
        client.Credentials <- Credentials(token)
        Client client

    let issuesToAssign (Client client) org repo label = async {
        let parameters = RepositoryIssueRequest()
        parameters.Labels.Add(label)
        
        return! client.Issue.GetAllForRepository(org, repo, parameters) |> Async.AwaitTask
    }

    let getMemberstoAssign (Client client) org teamName = async {
        let! allTeams = client.Organization.Team.GetAll(org) |> Async.AwaitTask
        let team = allTeams |> Seq.filter (fun t -> t.Name = teamName) |> Seq.head
        return! client.Organization.Team.GetAllMembers(team.Id) |> Async.AwaitTask
    }

    let updateIssueAsignee (Client client) org repo (issue : Issue) (user : User) = async {
        let issueUpdater = issue.ToUpdate()
        issueUpdater.AddAssignee(user.Login)
        return! client.Issue.Update(org, repo, issue.Number, issueUpdater) |> Async.AwaitTask
    }


type ResultBuilder () =
    member x.Bind(v, f) = Result.bind f v
    member x.Return v = Ok v
    member x.ReturnFrom v = v

let result = ResultBuilder()

type ActionError =
    | MissingTokenEnvironmentVariable
    | MissingRepoEnvironmentVariable
    | MissingGroupEnvironmentVariable
    | MissingLabelEnvironmentVariable
    | InvalidRepoFormat
    | CastingError of string
    | BadCredentials
    | Other of string

let printResult = function
| Ok message -> printfn "%s" message
| Error error -> printfn "%A" error

[<EntryPoint>]
let main argv =
    let res = result {
        let! token =
            match Environment.Get Environment.Key.AuthToken with
            | Some token -> Ok token
            | None -> Error MissingTokenEnvironmentVariable

        let! teamName =
            match Environment.Get Environment.Key.GroupToAssign with
            | Some name -> Ok name
            | None -> Error MissingGroupEnvironmentVariable

        let! repo =
            match Environment.Get Environment.Key.Repo with
            | Some value -> Ok value
            | None -> Error MissingRepoEnvironmentVariable

        let! (org, repo) =
            match repo.Split('/') with
            | [|org; repo|] -> Ok (org, repo)
            | _ -> Error InvalidRepoFormat

        let! labelToAssign =
            match Environment.Get Environment.Key.LabelToAssign with
            | Some token -> Ok token
            | None -> Error MissingGroupEnvironmentVariable

        return
            async {
                let client = GitHub.Create token
                let! issues = GitHub.issuesToAssign client org repo labelToAssign
                let! usersToAssign = GitHub.getMemberstoAssign client org teamName
                let shuffledUsers = (usersToAssign |> Seq.toArray).Shuffle()
                let userSequence = Seq.initInfinite (fun i -> shuffledUsers.[i % shuffledUsers.Length])

                let! issuesUpdated =
                    issues
                    |> Seq.zip userSequence
                    |> Seq.map (fun (u, i) -> GitHub.updateIssueAsignee client org repo i u)
                    |> Async.Parallel

                return (sprintf "Updated %d issues" issuesUpdated.Length)
            } |> Async.RunSynchronously
    }

    printResult res

    0 // return an integer exit code
