open System

open FsAlgo.PerfTest

open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Reports
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Exporters.Csv
open BenchmarkDotNet.Jobs
open System.Globalization
open Perfolizer.Horology
open BenchmarkDotNet.Environments

let summaryStyle =
    SummaryStyle(CultureInfo.CurrentCulture, true, SizeUnit.MB, TimeUnit.Millisecond, false)

let conf =
    ManualConfig
        .Create(DefaultConfig.Instance)
        .AddExporter(CsvExporter(CsvSeparator.CurrentCulture, summaryStyle))
        .AddJob(
            Job
                .Default
                .WithRuntime(CoreRuntime.Core60)
                .WithPlatform(Platform.X64)
        )

BenchmarkRunner.Run<Sort>(conf) |> ignore
