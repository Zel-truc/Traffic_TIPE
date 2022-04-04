open Graphics
type case = Empty | Voiture of int | Block


let accelerate l vmax =
	for i = 0 to Array.length l -1 do
		match l.(i) with 
		|Empty -> ()
                |Block -> ()
		|Voiture a -> if a < vmax then l.(i) <- Voiture (a+1)
	done


	let collision l n v =
                let newv = ref v in
                let check = ref true in 
                for i = n+1 to n+v do
                        if i < Array.length l then
                        match l.(i) with
                        |Empty -> ()
                        |Voiture _ -> if !check then begin newv:= i-n-1;
                                                          check:= false end
                        |Block ->     if !check then begin newv:= i-n-1;
                                                          check:= false end
                done;
                !newv





let deccelerate l = 
for i = 0 to Array.length l - 1 do 
	match l.(i) with
	|Empty -> ()
        |Block -> ()
	|Voiture a -> l.(i) <- Voiture (collision l i a)
done


let randomizer l p =
	for i = 0 to Array.length l-1 do 
	match l.(i) with
	|Empty -> ()
        |Block -> ()
	|Voiture a -> if a > 0 && Random.int 100 < p then l.(i) <- Voiture (a-1)
done

let mvt l = 
	for i = Array.length l - 1 downto 0 do
		match l.(i) with
		|Empty -> ()
                |Block -> ()
		|Voiture a -> if (i+a) < Array.length l then l.(i+a) <- Voiture a; if a != 0 then  l.(i) <- Empty
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
         grille.(2) <- Voiture 2;
         grille.(5) <- Voiture 1




let draw_tableau l =
	let i = ref 0 in 
	set_color (rgb 31 19 0);
        fill_rect 0 0 1280 720;
	for y = 0 to 39 do
		for x = 0 to  79 do
			set_color black;
			draw_rect (x*16) (y*18) 16 13;
			match l.(!i) with
                               |Block -> i:=!i+1 ; set_color (rgb 247 147 76 ); 
                                         fill_rect (x*16+1) (y*18+1) 16 13


				|Empty -> i:=!i+1 ; set_color (rgb 31 19 0); 
                                                   fill_rect (x*16+1) (y*18+1) 16 13
				|Voiture a -> 
                        if a < 3 then begin i:=!i+1 ; set_color (rgb 204 88 3); 
                                  fill_rect (x*16+1) (y*18+1) 16 13 end
			else begin i:=!i+1 ; set_color (rgb 255 193 94); 
                                  fill_rect (x*16+1) (y*18+1) 16 13 end;
				done
done

let block l t case startmod  =
        if t mod 100 = 0 then
                if l.(case) = Block 
                then l.(case) <- Empty
                else
                     if t mod startmod = 0 then 
                             l.(case) <- Block
  
  let _ = 
	let grille = Array.make 16000 Empty in
	open_graph " 1280x720";
	remember_mode true;
	display_mode false;
        fill grille;
        let sleep = ref 0.5 in
        for t = 1 to 90000  do   
                                block grille t 1000 600;  
                                grille.(0) <- Voiture 0; 
                                clear_graph();
                                draw_tableau grille; 
                                synchronize();
                                tout grille;
                                if key_pressed () then let c = read_key () in
                                match c with 
                                |'c' -> sleep:= !sleep +. 0.05
                                |'b' -> sleep:= !sleep -. 0.05
                                |_ -> () 
                                else
                                Unix.sleepf !sleep;
        done
