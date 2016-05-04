// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System.IO
open System
open System.Diagnostics
open System.Text

// data types

type Machine =
    {
        hlt : bool;
        ip : int32;
        r : uint32 array;
        zeromem : uint32 array;
        memory : Map<uint32, uint32 array>;
        freeid : List<uint32>;
        }

// aux functions

let NextID mach =
    match mach.freeid.IsEmpty with
        | true -> ( (uint32)mach.memory.Count+1u, mach.freeid )
        | false -> ( mach.freeid.Head, mach.freeid.Tail )

let Create (memMap:Map<uint32, uint32 array>) idx len =
    memMap.Add(idx, Array.create ((int)len) 0u )

let CopyMem memList idxFrom idxTo =
    let temp = (Map.find idxFrom memList)
    memList.Remove(idxTo).Add(idxTo, Array.copy temp )
     
let NextM m =
    {m with ip=(m.ip+1)}

let btou32 (arr:byte[]) idx =
    let v1 = (((uint32)(arr.[idx*4]))<<<24)
    let v2 = (((uint32)(arr.[1 + (idx*4)]))<<<16)
    let v3 = (((uint32)(arr.[2+(idx*4)]))<<<8)
    let v4 = ((uint32)(arr.[3+(idx*4)]))
    v1+v2+v3+v4

let MakeMemory arrByte =
    Array.init ((Array.length arrByte) / 4) (fun x -> btou32 arrByte x)

let FindMem m I =
    match I with
        | 0u->m.zeromem
        | _->Map.find I m.memory

// CPU instructions
let cMove m A B C =
    if C <> 0u then m.r.[(int)A]<-B
    NextM m

let ldArr m A B C =
    let vArr = (FindMem m B).[(int)C]
    m.r.[A]<-vArr
    NextM m

let stArr m A B C =
    (FindMem m A).[(int)B] <- C
    NextM m

let ldVal m A V =
    m.r.[(int)A] <- V
    NextM m

let Add m A B C = ldVal m A (B+C)

let Mul m A B C = ldVal m A (B*C)

let Div m A B C = ldVal m A (B/C)

let NAnd m A B C = ldVal m A (~~~(B &&& C))

let mRead m C = ldVal m C ((uint32)(Console.Read() &&& 0xff))

let NewMem m B C =
    let (nxtId, nxtfree) = NextID m
    m.r.[(int)B] <- nxtId
    {m with memory=(Create m.memory nxtId C); freeid=nxtfree; ip=(m.ip+1)}

let FreeMem m C =
    {m with memory=(Map.remove C m.memory); freeid=( C::m.freeid ) ; ip=(m.ip+1)}

let mPrint m C =
    Console.OpenStandardOutput().WriteByte((byte)((int)(C &&& 0xffu)))
    NextM m

let Program m B C =
    match B with
    | 0u -> {m with ip=((int)C)}
    | _ -> {m with zeromem=(Array.copy (Map.find B m.memory) ); ip=((int)C)}

let rec Step m idx =
    let code = m.zeromem.[m.ip]
    let rC = (int)(code &&& 0x7u)
    let rB = (int)((code >>> 3) &&& 0x7u)
    let rA = (int)((code >>> 6) &&& 0x7u)
    let dC = m.r.[rC]
    let dB = m.r.[rB]
    let dA = m.r.[rA]
    let instr = (code >>> 28) &&& 0xfu
    let next = 
        match instr with
        | 0u -> cMove m rA dB dC
        | 1u -> ldArr m rA dB dC
        | 2u -> stArr m dA dB dC
        | 3u -> Add m rA dB dC
        | 4u -> Mul m rA dB dC
        | 5u -> Div m rA dB dC
        | 6u -> NAnd m rA dB dC
        | 8u -> NewMem m rB dC
        | 9u -> FreeMem m dC
        | 10u -> mPrint m dC
        | 11u -> mRead m rC
        | 12u -> Program m dB dC
        | 13u -> ldVal m ((((int)code >>> 25) &&& 0x7)) (code &&& 0x01ffffffu)
        | _ -> { m with hlt = true }
    if (next.hlt) then next else Step next (idx+1L)

// main stuff

[<EntryPoint>]
let main argv = 
    if System.IO.File.Exists(argv.[0]) = true then (
        let data = System.IO.File.ReadAllBytes(argv.[0])
        //printfn "%A" data.Length
        let pgm = (MakeMemory data)
        let m = {hlt = false; ip=0; r=Array.create 8 0u; freeid=List.empty; memory=Map.empty; zeromem=pgm;}
        let lastM = Step m 0L
        0 // return an integer exit code
    ) else 1
