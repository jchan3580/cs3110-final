open PAttributes
open pActions
open Battle
open Items
open PokeML

open Sdl
open Video
open Window
open Timer
open Event

open SDLGL
open Draw
open Glcaml

open View
open Base

open Printf

let save s = State.save_to_file s "game.save"

let other_inputs ctrl key g s =
  State.
  ( if (key.unicode land 0xFF80) = 0 then
    ( let x = key.unicode land 0x7F in
      let v = if ctrl then x + 0x60 else x in
      let ch = Char.chr v in
      match ch with
      | 'e' -> g Msg.Eat
      | 'i' -> g Msg.OpenInventory
      | 'm' -> g Msg.Atlas
      | 'r' -> g Msg.Rest
      | 'q' -> g Msg.Cancel
      | 'u' -> g Msg.Use
      | _ -> State.Play s
    )
    else State.Play s
  )

let typing_input ctrl k g s =
  State.
  ( if (k.unicode land 0xFF80) = 0 then
    ( let x = k.unicode land 0x7F in
      let v = if ctrl then x + 0x60 else x in
      let ch = Char.chr v in
      match ch with
      | ' ' | '0'..'9' | 'a'..'z' | 'A'..'Z' -> g (Msg.Char ch)
      | _ -> State.Play s
    )
    else
      State.Play s
  )

let process_key key = function
    State.Play s ->
      let g m = State.Play (State.respond s m) in
      let ctrl = List.mem KMOD_LCTRL k.modifiers in
      State.
      ( match key.sym, key.keystate with
        | K_Q, PRESSED when ctrl -> finalize s; State.Exit
        | K_LEFT, PRESSED -> if ctrl then g (Msg.Attack 2) else g Msg.Left
        | K_RIGHT, PRESSED -> if ctrl then g (Msg.Attack 0) else g Msg.Right
        | K_UP, PRESSED -> if ctrl then g (Msg.Attack 1) else g Msg.Up
        | K_DOWN, PRESSED -> if ctrl then g (Msg.Attack 3) else g Msg.Down
        | K_ESCAPE, PRESSED -> g Msg.Cancel
        | K_RETURN, PRESSED -> g Msg.Confirm
        | K_BACKSPACE, PRESSED -> g Msg.Backspace
        | K_DELETE, PRESSED -> g Msg.Delete
        | _, PRESSED ->
            ( match s.State.cm with
              | State.CtrlM.Console _ -> typing_input ctrl k g s
              | _ -> other_inputs ctrl k g s
            )
        | _ -> State.Play s
      )
  | x -> x

(*Calls all the necessary components to start the game*)
let rec main_helper mode_state prev_ticks died = failwith "TODO"
  let ticks = Timer.get_ticks () in

  let dead_now =
  ( match mode_state with
    |  State.Play s ->
        ( match s.State.cm with State.CtrlM.Died _ -> true | _ -> false )
    | _ -> false )
  in

  let prev_ticks = if died && not dead_now then ticks else prev_ticks in

  draw_gl_scene
    ( fun () ->
        ( match mode_state with
          |  State.Play s ->
              draw_state ticks s;
              (* FPS *)
              if s.State.debug then
              ( let fps = 1000.0 /. float (ticks - prev_ticks) in
                glColor4f 1.0 1.0 1.0 1.0;
                Grafx.Draw.put_string (sprintf "FPS: %.0f" fps)
                Grafx.Draw.gr_ui (0,0);)
          | _ -> () );
    );
  let mode_state' = match mode_state with
  | State.Play s ->
      let speed = s.State.opts.State.Options.game_speed in
      let speedup = 1.07 ** float speed in
      State.Play (Sim.run ( 0.011 *. float (ticks - prev_ticks) *. speedup) s)
  | ms -> ms
  in

  delay(5);

  if mode_state' <> State.Exit then
  ( match poll_event () with
    | Key k ->
        main_loop (process_key_pressed k mode_state') ticks dead_now
    | Quit ->
        (* on exit *)
        ( match mode_state' with
          | State.Play s -> finalize s
          | _ -> ()
        );
        main_loop State.Exit ticks dead_now
    | _ -> main_loop mode_state' ticks dead_now
  )

let main () =
  Random.self_init();

  init [VIDEO];
  let w = 854 / 2 * Grafx.Draw.zi and h = 480 / 2 * Grafx.Draw.zi and bpp = 32 in
  let _ = set_video_mode w h bpp [OPENGL; DOUBLEBUF] in
  (* enable_key_repeat default_repeat_delay default_repeat_interval; *)
  (* enable_key_repeat 10 10; *)
  enable_key_repeat 100 27;
  ignore (enable_unicode ENABLE);

  set_caption "PokeML" "Welcome to PokeML";
  Grafx.init_gl w h;
      let strng =
      if Array.length Sys.argv > 1 then
      ( let first = Sys.argv.(1) in
        if first = load
        ( if Sys.file_exists "game.save" then
            State.load_from_file "game.save"
          else
            State.init_full None false)
    else State.init_full (Some "a") false
    in
    State.Play strng
  in

  main_loop state0 (Timer.get_ticks()) false;

  quit ()


main ()