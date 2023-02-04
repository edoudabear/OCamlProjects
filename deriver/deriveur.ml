type operation = Fonction of (float->float*float->float) | Mult | Sum | Diff;;
type monome = (float*int);;
type pol = monome list;;
type expression = | Node of (operation*expression*expression) | Leaf of pol;;


let expr=Node (Fonction (fun h->sin h,fun h->cos h),Leaf [(2.5,0);(1.,1)],Leaf [(0.,0)]);;