type info = (int * int) list 
type voiture = {mutable vitesse : int; mutable itineraire : (int * int) list}
type state = Pleine of voiture | Empty  
type case = Route of (state * (int*int)) |Choice of state |Block 


let finale_grille_gen ()  = let l =Array.make_matrix 200 200 Block in
			for y = 1 to 198 do
				l.(y).(102) <- Route (Empty, (102,y-1))
			done;
			l.(131).(102) <- Choice Empty;
			l.(119).(102) <- Choice Empty;
			l.(89).(102) <- Choice Empty;
			l.(41).(102) <- Choice Empty;
			for x = 1 to 101 do 
				l.(89).(x) <- Route(Empty, (x-1, 89))
			done;
			for x = 103 to 198 do
				l.(89).(x) <- Route(Empty, (x+1,89))
			done;
			for x = 103 to 151 do 
				l.(41).(x) <- Route(Empty ,(x+1,41))
			done;
			l.(41).(151) <- Route(Empty ,(151,42));
			for y = 42 to 88 do 
				l.(y).(151) <- Route(Empty, (151,y+1))
			done;
			for y = 90 to 131 do 
				l.(y).(44) <- Route(Empty, (44,y-1))
			done;
			for x = 45 to 101 do
				l.(131).(x) <- Route(Empty, (x-1,131))
			done;
			for x = 43 downto 1 do
				l.(120).(x) <- Route(Empty, (x-1, 131))
			done;
			for y = 90 to 118 do
				l.(y).(158) <- Route(Empty, (158, y+1))
			done;
			for x = 159 to 199 do 
				l.(118).(x) <- Route(Empty, (x+1, 118))
			done;
			for x = 103 to 129 do 
				l.(119).(x) <- Route(Empty, (x+1,119))
			done;
			for y = 119 to 140 do
				l.(y).(130) <- Route(Empty, (130, y+1))
			done;
			for x = 130 to 198 do
				l.(140).(x) <- Route(Empty, (x+1, 140))
			done;		
			l.(120).(44) <- Choice Empty;
			l.(89).(158) <- Choice Empty;
			l

let grille_gen() = 
	let l =Array.make_matrix 200 200 Block in
	for x = 0 to 198 do 
		l.(x).(100) <- Route(Empty, (100,x+1))
	done;
	for x = 0 to 198 do 
		l.(150).(x) <- Route(Empty, (x+1,150))
	done;
	l
let is_next_empty grille (x,y) = 
	match grille.(y).(x) with
	|Block -> false
	|_ -> true


let rec move_aux (grille: case array array) (v: voiture) ((x,y): int*int) (inc:int) : unit=
	let choice_handler v =
		match v.itineraire with 
		|t::q -> v.itineraire <- q; t
		|_ -> (0,0) (* A CORRIGER*)
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
	
let rec newv grille v (x,y) i =
	if x = v then v else 
	match grille.(y).(x) with
	|Route (Pleine _,_) -> i-1
	|Route (_, next) -> newv grille v next (i+1)
	|_ -> 5

let deccelerate l vmax =
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
			match l.(y).(x) with
			|Route (Pleine v, next) -> v.vitesse <- if (newv l v .vitesse next 1) > vmax then vmax else  newv l v .vitesse next 1
			|Choice (Pleine v) -> begin match v.itineraire with
								|t::_ -> v.vitesse <- if (newv l v .vitesse t 1) > vmax then vmax else  newv l v .vitesse t 1
								|[] -> failwith "itineraire faux" end
			|_ -> ()
		done
	done
let randomize l p =
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in 
	for y = ymax-1 downto 0 do
		for x = xmax-1 downto 0 do
			match l.(y).(x) with
			|Route (Pleine v, _) -> if v.vitesse <> 0 && Random.int 99 < p
								then v.vitesse <- v.vitesse-1
			|Choice Pleine v -> if v.vitesse <> 0 && Random.int 99 < p
								then v.vitesse <- v.vitesse-1
			|_ -> ()
	done
done
let tout grille = 
	accelerate grille 5;
	deccelerate grille 5;
	randomize grille 30;
	mvt grille
	
	
(*let fill grille =
let heatmap_inc =
let heatmap_gen = 

*)

let draw_case l (x,y) =
	let xmax = Array.length l.(0) in
    let ymax = Array.length l in	
    let xstep = 800 / xmax in 
    let ystep = 800 / ymax in
    let open Raylib in

    let emptycolor = Color.create 1 25 54 255 in
    let redcolor = Color.create 237 37 78 255 in
    let bluecolor = Color.create 194 234 189 255 in 
	let blackcolor = Color.create 194 54 0 255	in
	let greencolor = Color.create 0 255 0 255 in
    draw_rectangle (x*xstep) (y*ystep) (xstep) (ystep) 
	(match l.(y).(x) with
	|Route (Pleine v, _) -> if v.vitesse >3 then redcolor else bluecolor
	|Choice Pleine v -> if v.vitesse >3 then redcolor else bluecolor
	|Block -> blackcolor
	|Choice Empty -> greencolor
	|_ -> emptycolor)

let draw_tableau l =
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in

	for y = 0 to ymax-1 do
		for x = 0 to xmax-1  do
		    draw_case l (x,y)
        done 
    done


let car_add g (x,y) = 
match g.(y).(x) with
|Route (_,next) -> g.(y).(x) <- Route(Pleine {vitesse = 1; itineraire= []},next)
|_ -> ()

let setup () =
	Raylib.init_window 1200 800 "Nasch model";
	Raylib.set_target_fps 10

let rec loop grille =
	match Raylib.window_should_close () with
	|true -> Raylib.close_window()
	|false ->
	car_add grille (102,180);
	tout grille;
	let open Raylib in
	begin_drawing();
	draw_tableau grille;
	end_drawing();
	loop grille
let _ =
	setup ();
	loop (finale_grille_gen())
