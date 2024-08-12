module Constants = struct
  open Stdlib
  module Bd = Bigdecimal
  let cubic_meters_methane_to_tonnes_co2_eq = let open Bd in of_string "6.784" * (of_string "10e-4") * of_string "28"
  let default_methane_fraction_in_biogas = Bd.of_string "0.7"
  let oxidation_factor_near_surface_methane = Bd.of_string "0.1"
  let global_warming_potential_methane = Bd.of_string "28.0"
  let carbon_to_methane_conversion_factor = Bd.of_string "1.336"
  let methane_correction_factor_aerobic_decomposition = Bd.of_string "1.0"
  let methane_volume_fraction_in_land_fill_gas = Bd.of_string "0.5"
  let methane_from_composting_emission_factor = Bd.of_string "0.021"
  let nitrous_oxide_from_composting_emission_factor = Bd.of_string "0.025"
  let methane_from_anaerobic_digestion_emission_factor = Bd.of_string "0.028"
  let nitrous_oxide_from_anaerobic_digestion_emission_factor = Bd.zero
  let megawatt_hours_to_giga_joules_conversion_factor = Bd.of_string "3.6"
  let mass_of_carbon_to_mass_of_methane = Bd.of_string "1.336"

  module StringMap = Map.Make(String)

  type table_content = Bd.t StringMap.t StringMap.t

  let default_waste_percentage_for_waste_mix_type : table_content =
    let open StringMap in
    empty
    |> add "food" (empty
      |> add "msw_class_1" (Bd.of_string "0.35")
      |> add "msw_class_2" (Bd.of_string "0.403")
      |> add "ci" (Bd.of_string "0.215")
      |> add "cd" Bd.zero
    )
    |> add "paper_and_cardboard" (empty
      |> add "msw_class_1" (Bd.of_string "0.13")
      |> add "msw_class_2" (Bd.of_string "0.15")
      |> add "ci" (Bd.of_string "0.155")
      |> add "cd" (Bd.of_string "0.03")
    )
    |> add "garden_and_park" (empty
      |> add "msw_class_1" (Bd.of_string "0.165")
      |> add "msw_class_2" (Bd.of_string "0.039")
      |> add "ci" (Bd.of_string "0.04")
      |> add "cd" (Bd.of_string "0.02")
    )
    |> add "wood_and_wood_waste" (empty
      |> add "msw_class_1" (Bd.of_string "0.01")
      |> add "msw_class_2" (Bd.of_string "0.012")
      |> add "ci" (Bd.of_string "0.125")
      |> add "cd" (Bd.of_string "0.06")
    )
    |> add "textiles" (empty
      |> add "msw_class_1" (Bd.of_string "0.015")
      |> add "msw_class_2" (Bd.of_string "0.017")
      |> add "ci" (Bd.of_string "0.04")
      |> add "cd" Bd.zero
    )
    |> add "sludge" (empty
      |> add "msw_class_1" Bd.zero
      |> add "msw_class_2" Bd.zero
      |> add "ci" (Bd.of_string "0.015")
      |> add "cd" Bd.zero
    )
    |> add "nappies" (empty
      |> add "msw_class_1" (Bd.of_string "0.04")
      |> add "msw_class_2" (Bd.of_string "0.046")
      |> add "ci" Bd.zero
      |> add "cd" Bd.zero
    )
    |> add "rubber_and_leather" (empty
      |> add "msw_class_1" (Bd.of_string "0.01")
      |> add "msw_class_2" (Bd.of_string "0.012")
      |> add "ci" (Bd.of_string "0.035")
      |> add "cd" Bd.zero
    )
    |> add "inert_waste" (empty
      |> add "msw_class_1" (Bd.of_string "0.28")
      |> add "msw_class_2" (Bd.of_string "0.321")
      |> add "ci" (Bd.of_string "0.375")
      |> add "cd" (Bd.of_string "0.89")
    )

  let average_capture_rate_for_methane_emissions_from_landfill : table_content =
    let open StringMap in
    empty
    |> add "nsw" (empty
      |> add "transitioning" (Bd.of_string "0.24")
      |> add "other" (Bd.of_string "0.37")
    )
    |> add "vic" (empty
      |> add "transitioning" (Bd.of_string "0.32")
      |> add "other" (Bd.of_string "0.45")
    )
    |> add "qld" (empty
      |> add "transitioning" (Bd.of_string "0.16")
      |> add "other" (Bd.of_string "0.30")
    )
    |> add "wa" (empty
      |> add "transitioning" (Bd.of_string "0.27")
      |> add "other" (Bd.of_string "0.30")
    )
    |> add "sa" (empty
      |> add "transitioning" (Bd.of_string "0.29")
      |> add "other" (Bd.of_string "0.29")
    )
    |> add "tas" (empty
      |> add "transitioning" (Bd.of_string "0.33")
      |> add "other" (Bd.of_string "0.39")
    )
    |> add "act" (empty
      |> add "transitioning" (Bd.of_string "0.47")
      |> add "other" (Bd.of_string "0.66")
    )
    |> add "nt" (empty
      |> add "transitioning" (Bd.of_string "0.25")
      |> add "other" (Bd.of_string "0.18")
    )

  let degradable_organic_carbon_content =
    let open StringMap in
    empty
    |> add "food" (Bd.of_string "0.15")
    |> add "paper_and_cardboard" (Bd.of_string "0.4")
    |> add "garden_and_green" (Bd.of_string "0.2")
    |> add "wood" (Bd.of_string "0.43")
    |> add "textiles" (Bd.of_string "0.24")
    |> add "sludge" (Bd.of_string "0.05")
    |> add "nappies" (Bd.of_string "0.24")
    |> add "rubber_and_leather" (Bd.of_string "0.39")
    |> add "inert_waste" Bd.zero
    |> add "alternative_waste_treatment_residues" (Bd.of_string "0.08")

  let fraction_of_degradable_organic_carbon_dissimilated =
    let open StringMap in
    empty
    |> add "food" (Bd.of_string "0.84")
    |> add "paper_and_cardboard" (Bd.of_string "0.49")
    |> add "garden_and_green" (Bd.of_string "0.47")
    |> add "wood" (Bd.of_string "0.23")
    |> add "textiles" (Bd.of_string "0.50")
    |> add "sludge" (Bd.of_string "0.50")
    |> add "nappies" (Bd.of_string "0.50")
    |> add "rubber_and_leather" (Bd.of_string "0.50")
    |> add "inert_waste" Bd.zero
    |> add "alternative_waste_treatment_residues" (Bd.of_string "0.50")


end
  