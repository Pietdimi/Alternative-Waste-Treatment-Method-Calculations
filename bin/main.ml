open Constants
module Bd = Bigdecimal

module StringMap = Map.Make(String)
let _get_default_waste_percentage_for_waste_mix_type waste_type scenario =

  match StringMap.find_opt waste_type Constants.default_waste_percentage_for_waste_mix_type with
  | Some scenario_map -> 
      (match StringMap.find_opt scenario scenario_map with
      | Some value -> value
      | None -> failwith "Scenario not found")
  | None -> failwith "Waste type not found"

(* Equation 6 *)

(* PW is the quantity of putrescible eligible waste processed in the reporting period
EW is the total quantity of eligible waste received in tonnes worked out using equation 7
QRP is the quantity of residual waste disposed of during the reporting period in tonnes.
QNBP is the quantity of non-biobased products manufactured during the reporting period, in tonnes.
HW is the historic quantity of putrescible eligible waste for the reporting period found using section 27.  *)

let _putrescible_waste (ew : Bd.t) (qrp : Bd.t) (qnbp : Bd.t) (hw : Bd.t) : Bd.t =
  Bd.(ew - (qrp + qnbp + hw))


  (* Equation 7 *)
  
  (* QMSW is the quantity of the eligible waste which is municipal solid waste in tonnes.
  QC&I is the quantity of the eligible waste which is commercial and industrial waste in tonnes.
QC&D is the quantity of the eligible waste which is construction and demolition waste in tonnes. *)

let _eligible_waste (qmsw : Bd.t) (qci : Bd.t) (qcd : Bd.t) : Bd.t = 
  Bd.(qmsw + qci + qcd)


(* Equation 8 *)

(* HQEW is the quantity of eligible waste received by the facility in the relevant 24 month period, in tonnes, *worked using appropriate evidence.*
HQRW is the quantity of residual waste disposed of by the facility during the relevant 24 month period in tonnes, *worked using appropriate evidence.*
HQNBP is the quantity of non-biobased products manufactured by the facility during the relevant 24 month period in tonnes, *worked using appropriate evidence*. *)

let _historic_putrescible_waste (hqew : Bd.t) (hqrw : Bd.t) (hqnbp : Bd.t) : Bd.t =
  Bd.(hqew - (hqrw + hqnbp))
  
(* Equation 13 *)

(* QTW is the total quantity of putrecible waste received during the reporting period, in tonnes.
QRW is the quantity of residual waste disposed of during the reporting period, in tonnes, worked out in accordance with the monitoring requirements.
QNBP is the quantity of non-biobased products manufactured during the reporting period, in tonnes, worked out in accordance with the monitoring requirements. *)
let _total_putrescible_waste (qtw : Bd.t) (qrw : Bd.t) (qnhp : Bd.t) : Bd.t =
  Bd.(qtw - qrw - qnhp)

let () =
  (* print_endline "Hello, World!" *)
  let waste_percentage = Bd.to_string_no_sn (_get_default_waste_percentage_for_waste_mix_type "food" "msw_class_2") in
    Printf.printf "Default waste percentage for waste mix type: %s\n" waste_percentage


  (* let number = Bd.to_string_no_sn Constants.cubic_meters_methane_to_tonnes_co2_eq in
  Printf.printf "Default waste percentage for waste mix type: %s\n" number *)