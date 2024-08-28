module CsvHashTable = struct

  type t = (string * string, string) Hashtbl.t

  (* Load CSV file into a hash table *)
  let load_csv filename =
    let csv = Csv.load filename in
    let table = Hashtbl.create 100 in
    
    match csv with
    | [] -> failwith "Empty CSV file"
    | header_row :: data_rows ->
        List.iter (fun row ->
          let row_key = List.hd row in
          List.iteri (fun j value ->
            if j > 0 then
              let col_key = List.nth header_row j in
              Hashtbl.add table (row_key, col_key) value
          ) row
        ) data_rows;
        table

  (* Get value from the hash table *)
  let get_value table row_header col_header =
    Hashtbl.find_opt table (row_header, col_header)

  (* Get all row headers *)
  let get_row_headers table =
    Hashtbl.fold (fun (row, _) _ acc -> 
      if not (List.mem row acc) then row :: acc else acc
    ) table [] |> List.rev

  (* Get all column headers *)
  let get_column_headers table =
    Hashtbl.fold (fun (_, col) _ acc -> 
      if not (List.mem col acc) then col :: acc else acc
    ) table [] |> List.rev

  (* Print the entire table (for debugging) *)
  let print_table table =
    let rows = get_row_headers table in
    let cols = get_column_headers table in
    
    (* Define fixed width for columns *)
    let column_width = 10 in  (* Adjust this width as necessary *)
    
    (* Print column headers *)
    Printf.printf "%-*s" column_width "";  (* Print empty space for row header *)
    List.iter (fun col -> Printf.printf "%-*s" column_width col) cols;
    Printf.printf "\n";
    
    (* Print each row with the corresponding values *)
    List.iter (fun row ->
      Printf.printf "%-*s" column_width row;  (* Print row header *)
      List.iter (fun col ->
        match get_value table row col with
        | Some v -> Printf.printf "%-*s" column_width v  (* Print value *)
        | None -> Printf.printf "%-*s" column_width "-"  (* Print placeholder for missing values *)
      ) cols;
      Printf.printf "\n"
    ) rows
  
end
