(*           Wanderers - open world adventure game.
            Copyright (C) 2013-2014  Alexey Nikolaev.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

open Base
(* open Common
 *)

(* Global map object *)

type region_id = int
type region_loc = (int*loc)

module Area = struct
  type 'a t = {data: 'a array array}
  let make w h v =
    let data = Array.make_matrix w h v in
    {data}
  let init w h f =
    let data = Array.init w (fun i -> Array.init h (fun j -> f i j)) in
    {data}

  let get a (i,j) = a.data.(i).(j)
  let set a (i,j) v = a.data.(i).(j) <- v

  let w a = Array.length a.data
  let h a = Array.length (a.data.(0))
  let is_within a (i,j) =
    let width = w a in
    let height = h a in
    i >= 0 && i < width && j >= 0 && j < height
  let put_inside a (i,j) =
    let width = w a in let height = h a in
    ((i+width) mod width, (j+height) mod height)
end

module Tile = struct
  type t = Grass | Tree1 | Tree2 | Rock1 | Rock2 | BigRock of int

  type tile_class = CFloor | CObstacle

  let can_walk tile = if (tile = CFloor) then true else false

  let classify = function
    | Grass -> CFloor
    | Tree1 | Tree2 | Rock1 | Rock2 | BigRock _ -> CObstacle
end

let is_walkable area loc =
  Area.is_within area loc && Tile.can_walk (Tile.classify (Area.get area loc))

type faction = int

type edge_type = East | North | West | South | Up | Down | Other

type path = loc list

and op_obj_type = OpObjOpen | OpObjClose

and action =
  | Walk of (path*float)
  | OperateObj of (loc * op_obj_type)

(* faction specialization *)
type fac_spec = Humanoid | Domestic | Pokeml

module Species = struct
  type genus =
    | Hum
    | Cow | Horse
    | Pokeml

  type t = genus * int

  let add v dv x = v +. dv *. float x

  let xmass = function
    | Cow, _ -> 3.0
    | Horse, _ -> 2.5
    | Pokeml, _ -> 3.0
    | _ -> 1.0

  let xathletic = function
    | Cow, _ -> 2.0
    | Horse, _ -> 2.0
    | Pokeml, _ -> 3.0
    | _ -> 1.0

(*   let def_inv = function
    | Hum, _ -> Inv.default
    | Cow, _ | Horse, _ -> Inv.animal
    | Pokeml, _ -> Inv.pokeml *)
end

(* Region meta info *)
module RM = struct
  type biome = Sea | Beach | Plains
  type t = {biome:biome; altitude:int;}

end

(* Region module *)
module R = struct
  (* active objects *)
  module Zone = struct
    type label = string
    module S = Set.Make (struct type t = label let compare = compare end)
    type t = S.t Area.t
    let mark z ij lbl = Area.set z ij (Area.get z ij |> S.add lbl)
    let unmark z ij lbl = Area.set z ij (Area.get z ij |> S.remove lbl)
    let get z ij = Area.get z ij
    let check z ij v = Area.get z ij |> S.mem v
  end

  type t = {
    rid: region_id;
    a: Tile.t Area.t;
    loc0: loc;
(*     e: E.t;
 *)    explored: (Tile.t option) Area.t;
(*     optinv: (Inv.t option) Area.t;
 *)    zones: Zone.t;
    obj: Obj.t;}

  let zone_mark r ij zlbl = Zone.mark r.zones ij zlbl
  let zone_unmark r ij zlbl = Zone.unmark r.zones ij zlbl
  let zone_check r ij zlbl = Zone.check r.zones ij zlbl

  let get_rid reg = reg.rid
end


(* Kinda priority list of R.t *)
module Prio = struct
  module Ml = Map.Make(struct type t = region_id let compare = compare end)
  type 'a t = {ml: 'a Ml.t; rank: region_id list}
  let num = 10

  let make () =
    {ml=Ml.empty; rank=[]}

  let upd r pl =
    if Ml.mem r.R.rid pl.ml then {pl with ml = Ml.add r.R.rid r pl.ml} else pl

  let get rid pl =
    if Ml.mem rid pl.ml then Some (Ml.find rid pl.ml) else None
end


(* Global map graph *)
module G = struct
  (* maps edge type to smthing *)
  module Me = Map.Make(struct type t = edge_type let compare = compare end)
  type geo = {currid : region_id; loc: region_loc array; rm: RM.t array;
   nb: (region_id Me.t) array; prio: R.t Prio.t}

  let length g = Array.length g.rm

  (* see module Genmap for generating geo objects *)

  (* update region *)
  let upd r g = {g with prio = Prio.upd r g.prio}

  (* region option by rid *)
  let getro rid g = Prio.get rid g.prio
  (* current region *)
  let curr g = match Prio.get g.currid g.prio with
      |Some r -> r
      | _ -> failwith "current region is invalid"

  (* current + neighbour regions list *)
  let curnb_ls g =
    Me.fold ( fun _ rid ls ->
      match Prio.get rid g.prio with
        Some reg -> reg :: ls
      | _ -> ls )
      (g.nb.(g.currid))
      [curr g]

  (* only neighbour regions list *)
  let get_nb_ls givenrid g =
    Me.fold ( fun _ rid ls ->
      match Prio.get rid g.prio with
        Some reg -> reg :: ls
      | _ -> ls )
      (g.nb.(givenrid))
      []

  (* only neighbour regions list *)
  let only_nb_ls g = get_nb_ls g.currid g

  (* get rid of a neighbor *)
  let get_nb g rid dir =
    let nb = g.nb.(rid) in
    if Me.mem dir nb then
      Some (Me.find dir nb)
    else
      None

  let get_only_nb_rid_ls givenrid g =
    Me.fold (fun _ rid ls -> rid :: ls) (g.nb.(givenrid)) []


end

(* Player's map memory *)
module Atlas = struct
  module RidKey = struct type t = region_id let compare = compare end
  module Mrid = Map.Make(RidKey)

  module Srloc = Set.Make(struct type t = region_loc let compare = compare end)

  type rmpoint = {rid: region_id; rloc: region_loc; biome:RM.biome;}
  type t = {
    rmp: (rmpoint option) array;
    visible : int Mrid.t;
    currid : region_id;
    curloc : region_loc;
    mountains : Srloc.t
  }

  let visible_rid_of_rloc atlas rloc =
    Array.fold_left (fun opt opt_rmp ->
      ( match opt_rmp with
          | Some rmp when rmp.rloc = rloc -> Some rmp.rid
          | _ -> opt )
    ) None atlas.rmp

  let iter_visible f atlas =
    Mrid.iter (fun rid _ ->
        match atlas.rmp.(rid) with
          Some rmp -> f rmp
        | _ -> ()
      ) atlas.visible

  (* auxiliary function computing Mrid for the neighboring tiles *)
  let rec explore geo rid radius mrid =
    let do_update = (* true if the region is not ~ explored fully ~ *)
      not (Mrid.mem rid mrid) ||
      ( let prev_radius = Mrid.find rid mrid in
        radius > prev_radius )
    in
    if do_update then
    ( let mrid' = Mrid.add rid radius mrid in
      G.Me.fold
        ( fun edge nbrid mridacc ->
          match edge with
          | East | West | North | South ->
            let incr_radius =
              (geo.G.rm.(rid).RM.altitude - geo.G.rm.(nbrid).RM.altitude) / 150
            in
            let biome_radius_reduction = 2 in
            let radius' = radius - biome_radius_reduction + incr_radius in

            if radius > 0 then
              explore geo nbrid radius' mridacc
            else
              mridacc

          | _ -> mridacc
         )
        geo.G.nb.(rid) mrid'
    )
    else mrid

  (* compute map of all visible rids *)
  let comp_visible rid geo =
    let z,loc = geo.G.loc.(rid) in
    (* no vision in dungeons *)
    if z < 0 then
      explore geo rid 0 Mrid.empty
    else
      let biome_radius = 2 in
      explore geo rid (biome_radius) Mrid.empty

  (* map of all rids *)
  let comp_all rid geo =
    fold_lim (fun acc rid ->
      Mrid.add rid 1 acc
    ) Mrid.empty 0 (Array.length geo.G.rm - 1)

  (* update the atlas from the map of rids *)
  let update_generic comp_func pol geo atlas =
    let currid = geo.G.currid in
    let visible = comp_func currid geo in
    (* update teh rmpoint array *)
    Mrid.iter (fun rid _ ->
      atlas.rmp.(rid) <-
        Some {rid=rid; rloc = geo.G.loc.(rid); biome = geo.G.rm.(rid).RM.biome;}
    ) visible;
    (* update the mountains set *)
    let mountains =
      Mrid.fold (fun rid _ acc ->
        let (z, (x,y)) as rloc = geo.G.loc.(rid) in
        if z = 0 then
          List.fold_left (fun acc (edge,(dx,dy)) ->
            if G.Me.mem edge geo.G.nb.(rid) then
              acc
            else
              Srloc.add (z,(x+dx, y+dy)) acc
          )
          acc [East,(1,0); North,(0,1); West,(-1,0); South,(0,-1)]
        else
          acc
      ) visible atlas.mountains
    in
    {atlas with visible; currid; curloc = geo.G.loc.(currid); mountains}

  let update = update_generic comp_visible

  let update_all = update_generic comp_all

  let make pol geo =
    let rmnum = Array.length geo.G.rm in
    let rmp = Array.make rmnum None in
    update pol geo {rmp; visible = Mrid.empty; currid = 0; curloc = (0,(0,0)); mountains = Srloc.empty}

end
