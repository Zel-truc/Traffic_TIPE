type case = Empty | Voiture of int

let grille = Array.make 100 Empty

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
			|Voiture a -> if (i-n) < !newv then newv:= (i-n-1)
	done;
	!newv
in
for i = 0 to Array.length l - 1 do 
	match l.(i) with
	|Empty -> ()
	|Voiture a -> l.(i) <- Voiture (collision l i a)
done

