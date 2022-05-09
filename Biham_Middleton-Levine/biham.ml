type case = Empty | Blue | Red



let fill_grille p l =
let test = ref 0 in
let separateur = ref 0 in
	for y = 0 to Array.length l - 1 do
	for x = 0 to Array.length l.(0) - 1 do
		test:= Random.int 100;
		separateur:=Random.int 2;
		if !test < p then if !separateur = 1 then l.(y).(x) <- Blue else l.(y).(x) <- Red
				else ()

done
done


let draw_case x y l =
let xmax = Array.length l.(0) in
let ymax = Array.length l in	
let xstep = 1280 / xmax in 
let ystep = 1280 / ymax in
let open Raylib in
let emptycolor = Color.create 255 255 255 255 in
let redcolor = Color.create 219 48 105 255 in
let bluecolor = Color.create 20 70 160 255 in 	
match l.(y).(x) with
|Empty ->begin begin_drawing ();
	draw_rectangle (x*xstep) (y*ystep) (xstep) (ystep) emptycolor;
	end_drawing();
	end
|Red -> begin begin_drawing ();
	draw_rectangle (x*xstep) (y*ystep) (xstep) (ystep) redcolor;
	end_drawing();
	end                                        
|Blue -> begin begin_drawing ();
	draw_rectangle (x*xstep) (y*ystep) (xstep) (ystep) bluecolor;
	end_drawing();
	end
     
let draw_tableau l =

	let xmax = Array.length l.(0) in
	let ymax = Array.length l in
	for y = 0 to ymax-1 do
		for x = 0 to xmax-1  do
		draw_case x y l
done done



let mvt l = 
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in
	for y = ymax-1 downto 0 do	 
  		for x = xmax - 1 downto 0 do
			match l.(y).(x) with 
			|Empty -> ()
			|Red -> if x+1 = xmax then
				if l.(y).(0) != Empty then ()
						    else begin l.(y).(0) <- Red; draw_case 0 y l;
							l.(y).(x) <- Empty ; draw_case x y l end
				
				else
				if l.(y).(x+1) <> Empty then ()
				else begin l.(y).(x+1) <- Red;l.(y).(x) <- Empty end
			|Blue -> if y+1 = ymax then
				if l.(0).(x) != Empty then ()
						    else begin l.(0).(x) <- Blue; draw_case x 0 l
							; l.(y).(x) <- Empty; draw_case x y l end
				
				else
				if l.(y+1).(x) <> Empty then ()
				else begin l.(y+1).(x) <- Blue;l.(y).(x) <- Empty end
		
done
done


let setup () = 
	Raylib.init_window 1280 720 "biham model";
	Raylib.set_target_fps 144


let rec loop grille =
	match Raylib.window_should_close () with
		|true -> Raylib.close_window ()
		|false ->
	let open Raylib in 
	begin_drawing ();
	mvt grille;
	draw_tableau grille;
	end_drawing();
	loop grille

let _ =
let grille = Array.make_matrix (1280/2) (720/2) Empty in 
fill_grille 10 grille;
setup (); 
loop grille;




