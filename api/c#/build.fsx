// include Fake lib
#r "packages/FAKE/tools/FakeLib.dll"
open System

open Fake
open Fake.AssemblyInfoFile
open Fake.EnvironmentHelper
open Fake.Git

// Properties
let buildDir = "./build/"
let testDir  = "./test/"
let packagingDir = "./packaging"
let sharp9pSourceDir = "./Sharp9P"
let datakitSourceDir = "./Datakit"
let testDlls = !! (testDir + "/*Test.dll")

// Packaging info
let buildNumber = environVarOrDefault "APPVEYOR_BUILD_NUMBER" "9999"
let commitHash = Information.getCurrentHash()
let company = "Docker"
let copyright = "Copyright © Docker, Inc. 2016"
let accessKey = environVar "NUGET_API_KEY"
let push = environVarOrDefault "APPVEYOR_REPO_TAG" "false" |> Boolean.Parse


let sharp9pAuthors = ["The Sharp9P Contributors"]
let sharp9pTitle = "Sharp9P"
let sharp9pDescription = "A 9P Client/Server Library written in C#"
let sharp9pSummary = sharp9pDescription
let sharp9pVersionBase = "0.1.6"
let sharp9pVersion = sprintf "%s.%s" sharp9pVersionBase buildNumber
let datakitAuthors = ["The Datakit Contributors"]
let datakitTitle = "Datakit"
let datakitDescription = "C# Datakit Bindings"
let datakitSummary = datakitDescription
let datkaitCompany = "Docker"
let datakitCopyright = "Copyright © Docker, Inc. 2016"
let datakitVersionBase = "0.1.0"
let datakitVersion = sprintf "%s.%s" datakitVersionBase buildNumber

// Targets
Target "Clean" (fun _ ->
  CleanDirs [buildDir; testDir; packagingDir]
)

Target "SetVersions" (fun _ ->
  CreateCSharpAssemblyInfo "./Sharp9P/Properties/AssemblyInfo.cs"
    [ 
      Attribute.Title sharp9pTitle
      Attribute.Description sharp9pDescription
      Attribute.Guid "12dd4614-0f72-4deb-a9d1-37d825b9a07b"
      Attribute.Company company
      Attribute.Copyright copyright
      Attribute.Version sharp9pVersion
      Attribute.FileVersion sharp9pVersion
      Attribute.Metadata("githash", commitHash)
    ]
  CreateCSharpAssemblyInfo "./Datakit/Properties/AssemblyInfo.cs"
    [ 
      Attribute.Title datakitTitle
      Attribute.Description datakitDescription
      Attribute.Guid "58F47959-A6E8-4433-A94A-61DBCC01A1E8"
      Attribute.Company company
      Attribute.Copyright copyright
      Attribute.Version datakitVersion
      Attribute.FileVersion datakitVersion
      Attribute.Metadata("githash", commitHash)
    ]
)

Target "CompileApp" (fun _ ->
  !! @"**/*.csproj"
  -- "*Test/*.csproj"
  |> MSBuildRelease buildDir "Build"
  |> Log "AppBuild-Output: "
)

Target "CompileTest" (fun _ ->
  !! @"*Test/*.csproj"
  |> MSBuildDebug testDir "Build"
  |> Log "TestBuild-Output: "
)

Target "TestApp" (fun _ ->
  testDlls
    |> Fake.Testing.NUnit3.NUnit3 (fun p -> 
      {p with
        ToolPath = "./packages/NUnit.ConsoleRunner/tools/nunit3-console.exe"
        ShadowCopy = false; 
        ResultSpecs = [testDir @@ "TestResults.xml"]})
)

Target "PackageApp" (fun _ ->
  let sharp9pNet45Dir = packagingDir @@ "Sharp9P/lib/net45/"
  let datakitNet45Dir = packagingDir @@ "Datakit/lib/net45/"
  CopyFiles sharp9pNet45Dir [ (buildDir @@ "Sharp9P.dll")
                              (buildDir @@ "Sharp9P.pdb")
  ]
  CopyFiles datakitNet45Dir [ (buildDir @@ "Datakit.dll")
                              (buildDir @@ "Datakit.pdb")
  ]
  let sharp9pSrcDir = packagingDir @@ "Sharp9P/src/"
  let datakitSrcDir = packagingDir @@ "Datatkit/src"
  let srcFiles (path : string) = path.Contains ".cs" && not <| path.Contains "obj"
  CopyDir ( sharp9pSrcDir @@ "Sharp9P" ) sharp9pSourceDir srcFiles
  CopyDir ( datakitSrcDir @@ "Datakit" ) datakitSourceDir srcFiles
  CopyFile ( packagingDir @@ "Sharp9P" ) "LICENSE"
  CopyFile ( packagingDir @@ "Datakit" ) "LICENSE"
  NuGet (fun p -> 
    {p with
      Files = [
              (buildDir + "Sharp9P*.dll", None, None)
              (buildDir + "Sharp9P*.pdb", None, None)
              (sharp9pSrcDir + "*.cs", None, None)
      ]
      Authors = sharp9pAuthors
      Project = sharp9pTitle
      Description = sharp9pDescription               
      OutputPath = ( packagingDir @@ "Sharp9P" )
      Summary = sharp9pSummary
      WorkingDir = ( packagingDir @@ "Sharp9P" )
      Version = sharp9pVersion
      AccessKey = accessKey
      SymbolPackage = NugetSymbolPackage.Nuspec
      Publish = push
      PublishUrl = "https://api.nuget.org/v3/index.json"
      })
      "Sharp9P.nuspec"

  NuGet (fun p -> 
    {p with
      Files = [
              (buildDir + "Datakit*.dll", None, None)
              (buildDir + "Datakit*.pdb", None, None)
              (datakitSrcDir + "*.cs", None, None)
      ]
      Authors = datakitAuthors
      Project = datakitTitle
      Description = datakitDescription                             
      OutputPath = ( packagingDir @@ "Datakit" )
      Summary = datakitSummary
      WorkingDir = ( packagingDir @@ "Datakit" )
      Version = datakitVersion
      AccessKey = accessKey
      SymbolPackage = NugetSymbolPackage.Nuspec
      Publish = push
      PublishUrl = "https://api.nuget.org/v3/index.json"
      })
      "Datakit.nuspec"
)

// Dependencies
"Clean"
  ==> "SetVersions"
  ==> "CompileApp"
  ==> "CompileTest"
  ==> "TestApp"
  ==> "PackageApp"

// start build
RunTargetOrDefault "PackageApp"