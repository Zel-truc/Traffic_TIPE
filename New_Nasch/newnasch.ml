type info = (int * int) list 
type voiture = {mutable vitesse : int; mutable itineraire : (int * int) list}
type state = Pleine of voiture | Empty  
type case = Route of (state * (int*int)) |Choice of state |Block 



let is_next_empty grille (x,y) = 
	match grille.(y).(x) with
	|Block -> false
	|_ -> true


let rec move_aux (grille: case array array) (v: voiture) ((x,y): int*int) (inc:int) : unit=
	let choice_handler v =
		match v.itineraire with 
		|t::q -> v.itineraire <- q; t
		|_ -> failwith "erreur"
	in
		if inc = 0 
			then match grille.(y).(x) with
			|Route (_,next) -> grille.(y).(x) <- Route (Pleine v, next)
			|Choice _ -> grille.(y).(x) <- Choice (Pleine v)
			|_ -> ()
		else
			match grille.(y).(x) with
			|Route (_,next) when next <> (-1,-1) -> move_aux grille v next (inc-1)
			|Choice _ -> (*possible bug car v n'est pas mis à jour, normalement le mutable est une référence*)
						move_aux grille v (choice_handler v) (inc-1)	
			|_ -> ()		
	

let move (grille: case array array)  s (x,y) : unit=
		match s with 
		|Pleine v ->  move_aux grille v (x,y) v.vitesse  
		|_ -> ()


let mvt grille =
	let xmax = Array.length grille.(0) in
	let ymax = Array.length grille in 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
			match grille.(y).(x) with
			|Route (s,next) when s <> Empty -> 
								begin 
								grille.(y).(x) <- Route (Empty,next);
								move grille s (x,y)  
								end
			|Choice s -> begin 
							grille.(y).(x) <- Choice Empty; 
							move grille s (x,y)
						 end 
			|Block -> ()
			|_ -> ()
		done
	done

let accelerate l vmax =
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
			match l.(y).(x) with
			|Route (Pleine v,_) -> if v.vitesse < vmax then v.vitesse <- v.vitesse + 1
			|Choice Pleine v -> if v.vitesse < vmax then v.vitesse <- v.vitesse + 1
			|_ -> ()
		done
	done	
	
let rec newv grille v (x,y) x =
	if x = v then v else 
	match grille.(y).(x) with
	|Route (Pleine _,_) -> x-1
	|Route (_, next) -> newv grille v next (x+1)
	|_ -> 0

let deccelerate l vmax =
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
			match l.(y).(x) with
			|Route (Pleine v, next) -> v.vitesse <- newv l v.vitesse next 1
			|Choice Pleine v -> match v.itineraire with
								|t::q -> v.vitesse <- newv l v.vitesse t 1
			|_ -> ()
		done
	done
(*let randomize grille p =
let tout grille =
let fill grille =
let heatmap_inc =
let heatmap_gen = 



let setup () =
	Raylib.init_window 1200 800 "Nasch model";
	Raylib.set_target_fps 144

let rec loop grille =
	match Raylib.window_should_close () with
	|true -> Raylib.close_window()
	|false ->
	tout grille;
	begin_drawing();
	draw_tableau();
	end_drawing();
	loop grille;
*)		
