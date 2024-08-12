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

(* Equation 1 *)

let _net_abatement (a_0 : Bd.t) (a_acc : Bd.t) = 
  Bd.(a_0+a_acc)

(* Equation 2 *)

let _activity_abatement (emissions_baseline : Bd.t) (emissions_project : Bd.t) =
  Bd.(div (emissions_baseline-emissions_project) (Bd.of_string "7"))

(* Equation 3 *)
(* wlfg is the average capture rate set out in the table from subsection 2 for methane emissions from landfill for the relevant state/territory in which the project is located.
mb is the methane generation potential of the *degradable* organic portion of the waste in tonnes $CH_4$, found using equation 4.
OFlf is the oxidation factor for near surface methane in landfill mentioned in subsection 5.4(1) of NGER (Measurement) Determination
GWPCH4 is the global warming potential of methane as specified by regulation 2.02 of NGER Regulations *)

let _emissions_baseline (wlfg : Bd.t) (mb : Bd.t) =
  Bd.((Bd.one - wlfg)*mb*(Bd.one - Constants.oxidation_factor_near_surface_methane)*Constants.global_warming_potential_methane)

(* Equation 4 for a single waste type w *)

(* WMw is the mass (tonnes) of waste mix type w present in the eligible waste worked out using equation 5.
DOCw is the fraction of waste mix type w which is degradable organic carbon mentioned in 5.12 NGER (Measurement) Determination
DOCF,w is the fraction of the degradable organic carbon that decomposes under landfill conditions mentioned in 5.14A NGER (Measurement) Determination
MCF is the methane correction factor for an aerobic decomposition process mentioned in section 5.14B of NGER (Measurement) Determination
WLFG,CH4 is the fraction by volume of methane in any generated landfill gas, mentioned in section 5.14C of NGER (Measurement) Determination
FC->CH4 = 1.336, being the factor to convert a mass of carbon to a mass of methane *)

let _methane_generation_potential_of_degradable_organic_waste (wmw : Bd.t) (docw : Bd.t) (docf : Bd.t) =
  Bd.((wmw*docw*docf)*Constants.methane_correction_factor_aerobic_decomposition*Constants.methane_volume_fraction_in_land_fill_gas*Constants.mass_of_carbon_to_mass_of_methane)

(* Equation 5 *)

let _quantity_of_waste_mix_type_w_present_in_eligible_waste (qmsw : Bd.t) (ew : Bd.t) (pw : Bd.t) (wmsww : Bd.t) (qcd : Bd.t) (wcdw : Bd.t) (qci : Bd.t) (wciw : Bd.t) =
    Bd.(((div qmsw ew)*pw*wmsww)+((div qcd ew)*pw*wcdw)+((div qci ew)*pw*wciw))

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

(* Equation 9 *)
(* PW is the eligible putrescible waste in tonnes found using equation 6.
EW is the total quantity of eligible waste received during the reporting period in tonnes, worked out using equation 7.
EF is the emissions from fuel used during the reporting period in tonnes CO2 e found using equation 10.
EEP is the emissions of purchased electricity used during reporting period in tonnes CO2e found using equation 11.
EPW is the emissions from processing putrescible waste during the reporting period, in tonnes CO2 e, found using equation 12. *)

let _project_emissions (pw : Bd.t) (ew : Bd.t) (ef : Bd.t) (eep : Bd.t) (epw : Bd.t) =
  Bd.((div pw ew)*(ef+eep)+epw) 

(* Equation 10 - for single fuel and greenhouse gas only*)

let _emissions_from_fuel (qfi : Bd.t) (eci : Bd.t) (efij : Bd.t) =
  Bd.(div (qfi * eci * efij) (Bd.of_string "1000"))

(* Equation 11 *)
let _emissions_from_purchased_electricity (qep: Bd.t) (efep : Bd.t) =
  Bd.(div (qep*efep) (Bd.of_string "1000"))

(* Equation 12 *)

let _emissions_from_processing_putrescible_waste (pw : Bd.t) (tpw : Bd.t) (e_compost : Bd.t) (e_ad : Bd.t) (e_combustion : Bd.t) =
  Bd.((div pw tpw)*(e_compost+e_ad+e_combustion))
  
(* Equation 13 *)

(* QTW is the total quantity of putrecible waste received during the reporting period, in tonnes.
QRW is the quantity of residual waste disposed of during the reporting period, in tonnes, worked out in accordance with the monitoring requirements.
QNBP is the quantity of non-biobased products manufactured during the reporting period, in tonnes, worked out in accordance with the monitoring requirements. *)
let _total_putrescible_waste (qtw : Bd.t) (qrw : Bd.t) (qnhp : Bd.t) : Bd.t =
  Bd.(qtw - qrw - qnhp)

(* Equation 14 *)

let _emssions_from_composting_unmonitored (qc : Bd.t) (biofilter_used : bool) =
  let re_compost = if biofilter_used then Bd.of_string "0.1" else Bd.zero
  in
  Bd.((Constants.methane_from_composting_emission_factor + Constants.nitrous_oxide_from_composting_emission_factor ) * qc * Bd.one - re_compost)
 
(* Equation 15 *)
let _emssions_from_composting_monitored (dm_methane : Bd.t) (dm_nitrous_oxide : Bd.t) (biofilter_used : bool) =
  let re_compost = if biofilter_used then Bd.of_string "0.1" else Bd.zero
  in
  Bd.((dm_methane + dm_nitrous_oxide)*(Bd.one - re_compost))

(* Equation 16 *)
let _emssions_from_anaerobic_digestion (m_sent : Bd.t) (m_vent : Bd.t) =
  Bd.(Constants.cubic_meters_methane_to_tonnes_co2_eq*((div (Bd.of_string "1") (Bd.of_string "0.98")- Bd.one)*m_sent + m_vent))

(* Equation 17 *)
let _volume_of_methane_sent_to_combustion_device (qbg : Bd.t) (wbg_ch4 : Bd.t) =
  Bd.(qbg*wbg_ch4)

(* Equation 18 *)
let _volume_of_methane_sent_to__internal_combustion_device ?(eeh : Bd.t option = None) (qeg : Bd.t) (ecbg : Bd.t) =
  Bd.(div (qeg * Constants.megawatt_hours_to_giga_joules_conversion_factor) ((if Option.is_some eeh then Option.get eeh else Bd.of_string "0.36")*ecbg))
  
(* Equation 19 *)
let _volume_of_methane_vented ?(wbg_ch4 : Bd.t option = None) (msbsc : Bd.t) (frq : Bd.t) (tq : Bd.t) =
  Bd.((msbsc+(frq*tq))*(if Option.is_some wbg_ch4 then Option.get wbg_ch4 else Bd.of_string "0.7"))

(* Equation 20 *)
let _emissions_from_combusion_device (m_sent : Bd.t) (ecbg : Bd.t) (efj : Bd.t) =
  Bd.(div (m_sent*ecbg*efj) (Bd.of_string "1000"))


let () =
  print_endline "Hello, World!";
  let waste_percentage = Bd.to_string_no_sn (_get_default_waste_percentage_for_waste_mix_type "food" "msw_class_2") in
    Printf.printf "Default waste percentage for waste mix type: %s\n" waste_percentage;
  let number = Bd.to_string_no_sn Constants.cubic_meters_methane_to_tonnes_co2_eq in
  Printf.printf "Cubic meters methane to tonnes CO2 equivalent: %s\n" number