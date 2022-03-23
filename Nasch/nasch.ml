open Graphics
type case = Empty | Voiture of int


let accelerate l vmax =
	for i = 0 to Array.length l -1 do
		match l.(i) with 
		|Empty -> ()
		|Voiture a -> if a < vmax then l.(i) <- Voiture (a+1)
	done

let deccelerate l = 
	let collision l n v =
	let newv = ref v in
		for i = (n+1) to (n+v) do
			match l.(i) with
			|Empty -> ()
			|Voiture _ -> if (i-n) < !newv then newv:= (i-n-1)
	done;
	!newv
in
for i = 0 to Array.length l - 1 do 
	match l.(i) with
	|Empty -> ()
	|Voiture a -> l.(i) <- Voiture (collision l i a)
done


let randomizer l p =
	for i = 0 to Array.length l-1 do 
	match l.(i) with
	|Empty -> ()
	|Voiture a -> if a > 0 && Random.int 100 < p then l.(i) <- Voiture (a-1)
done

let mvt l = 
	for i = Array.length l - 1 downto 0 do
		match l.(i) with
		|Empty -> ()
		|Voiture a -> if (i+a) < Array.length l then l.(i+a) <- Voiture a; l.(i) <- Empty
done

let tout grille =
	(* Definitions*)
	let vmax = 5 in
	let proba = 50 in 
	(*fin des defs*)
	accelerate grille vmax;
	deccelerate grille;
	randomizer grille proba;
	mvt grille
 
let fill grille = 
	grille.(0) <- Voiture 3;
	grille.(2) <- Voiture 3;
	grille.(5) <- Voiture 3;
	grille.(6) <- Voiture 3;
	grille.(9) <- Voiture 3;
	grille.(730) <- Voiture 3


let draw_grille () = 
	set_color black;
	for y = 0 to 20 do
	for x = 0 to  40 do
		set_color black;
	draw_rect (x*32) (y*36) 32 36;
set_color red;
	fill_rect (x*32+1) (y*36+1) 31 35
done
done

let draw_tableau l =
	let i = ref 0 in 
	set_color black;
	for y = 0 to 19 do
		for x = 0 to  39 do
			set_color black;
			draw_rect (x*32) (y*36) 32 36;
			match l.(!i) with
				|Empty -> i:=!i+1 ; set_color magenta; fill_rect (x*32+1) (y*36+1) 31 35
				|Voiture a -> if a = 0 then begin i:=!i+1 ; set_color red; i:=!i+1 ; fill_rect (x*32+1) (y*36+1) 31 35 end
			else begin i:=!i+1 ; set_color cyan; fill_rect (x*32+1) (y*36+1) 31 35 end;
				done
done
 (*
let draw_tableau l = 
	let level = ref 0 in
	for i = 0 to Array.length l - 1 do
	match l.(i) with
	|Empty -> putpixel gris_fonce (i mod 50) !level
	|Voiture a -> if a = 0 then putpixel rouge (i mod 50) !level else putpixel orange (i mod 50) !level
done

let _ =
	let grille = Array.make 200 Empty in
	couleur rouge noir;
	attroff(A.color);
	fill grille;

   couleur rouge noir;
		
        draw_grille grille;


*)

let _ = 
	let grille = Array.make 800 Empty in
	open_graph " 1280x720";
	remember_mode true;
	display_mode false;
	synchronize ();
fill grille;
	let check = ref false in
	while !check = false do  draw_tableau grille; Unix.sleepf 0.005; check:= Graphics.key_pressed() done
