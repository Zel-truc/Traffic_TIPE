open Graphics
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
let draw_tableau l =

	let xmax = Array.length l.(0) in
	let ymax = Array.length l in
	let xstep = 1280 / xmax in 
	let ystep = 1280 / ymax in
	set_color (rgb 31 19 0);
        fill_rect 0 0 1280 720;
	for y = 0 to ymax-1 do
		for x = 0 to xmax-1  do
			set_color black;
			match l.(y).(x) with
				|Empty -> set_color (rgb 255 255 255); 
                                                   fill_rect (x*xstep) (y*ystep) (xstep) (ystep)
				|Red -> set_color (rgb 219 48 105); 
                                                   fill_rect (x*xstep) (y*ystep) (xstep) (ystep)
				|Blue ->  set_color (rgb 20 70 160); 
                                                   fill_rect (x*xstep) (y*ystep) (xstep) (ystep)
				done
done

let draw_case x y l =
let xmax = Array.length l.(0) in
let ymax = Array.length l in	
let xstep = 1280 / xmax in 
let ystep = 1280 / ymax in	
match l.(y).(x) with
|Empty -> set_color (rgb 255 255 255); 
	fill_rect (x*xstep) (y*ystep) (xstep) (ystep)

|Red -> set_color (rgb 219 48 105); 
	fill_rect (x*xstep) (y*ystep) (xstep) (ystep)

|Blue ->  set_color (rgb 20 70 160); 
	fill_rect (x*xstep) (y*ystep) (xstep) (ystep)


let mvt l = 
	let xmax = Array.length l.(0) in
	let ymax = Array.length l in
	for y = 0 to ymax - 1 do	 
  		for x = 0 to xmax - 1 do
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
  let _ = 
	let grille = Array.make_matrix (1280/2) (720/2)  Empty in
	open_graph " 1280x720";
	remember_mode true;
	display_mode false;
        fill_grille 10 grille;
        draw_tableau grille;	
        let sleep = ref 0.00005 in
        for _ = 1 to 90000  do   
                                synchronize();
				mvt grille;
                                if key_pressed () then let c = read_key () in
                                match c with
				|'r' -> draw_tableau grille 
                                |'c' -> sleep:= !sleep +. 0.05
                                |'b' -> sleep:= !sleep -. 0.05
                                |_ -> () 
                                else
                                Unix.sleepf !sleep;
        done
