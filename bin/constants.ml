module Constants = struct
  let cubic_meters_methane_to_tonnes_co2_eq = 6.784*.(10.**(-4.))*.28.0
  let default_methane_fraction_in_biogas = 0.7
  let oxidation_factor_near_surface_methane = 0.1
  let global_warming_potential_methane = 28.0
  let carbon_to_methane_conversion_factor = 1.336
  let methane_correction_factor_aerobic_decomposition = 1.0
  let methane_volume_fraction_in_land_fill_gas = 0.5

  module StringMap = Map.Make(String)

  type table_content = float StringMap.t StringMap.t

  let default_waste_percentage_for_waste_mix_type : table_content =
    let open StringMap in
    empty
    |> add "food" (empty
      |> add "msw_class_1" 0.35
      |> add "msw_class_2" 0.403
      |> add "ci" 0.215
      |> add "cd" 0.
    )
    |> add "paper_and_cardboard" (empty
      |> add "msw_class_1" 0.13
      |> add "msw_class_2" 0.15
      |> add "ci" 0.155
      |> add "cd" 0.03
    )
    |> add "garden_and_park" (empty
      |> add "msw_class_1" 0.165
      |> add "msw_class_2" 0.039
      |> add "ci" 0.04
      |> add "cd" 0.02
    )
    |> add "wood_and_wood_waste" (empty
      |> add "msw_class_1" 0.01
      |> add "msw_class_2" 0.012
      |> add "ci" 0.125
      |> add "cd" 0.06
    )
    |> add "textiles" (empty
      |> add "msw_class_1" 0.015
      |> add "msw_class_2" 0.017
      |> add "ci" 0.04
      |> add "cd" 0.
    )
    |> add "sludge" (empty
      |> add "msw_class_1" 0.
      |> add "msw_class_2" 0.
      |> add "ci" 0.015
      |> add "cd" 0.
    )
    |> add "nappies" (empty
      |> add "msw_class_1" 0.04
      |> add "msw_class_2" 0.046
      |> add "ci" 0.
      |> add "cd" 0.
    )
    |> add "rubber_and_leather" (empty
      |> add "msw_class_1" 0.01
      |> add "msw_class_2" 0.012
      |> add "ci" 0.035
      |> add "cd" 0.
    )
    |> add "inert_waste" (empty
      |> add "msw_class_1" 0.28
      |> add "msw_class_2" 0.321
      |> add "ci" 0.375
      |> add "cd" 0.89
    )

  let average_capture_rate_for_methane_emissions_from_landfill : table_content =
    let open StringMap in
    empty
    |> add "nsw" (empty
      |> add "transitioning" 0.24
      |> add "other" 0.37
    )
    |> add "vic" (empty
      |> add "transitioning" 0.32
      |> add "other" 0.45
    )
    |> add "qld" (empty
      |> add "transitioning" 0.16
      |> add "other" 0.30
    )
    |> add "wa" (empty
      |> add "transitioning" 0.27
      |> add "other" 0.30
    )
    |> add "sa" (empty
      |> add "transitioning" 0.29
      |> add "other" 0.29
    )
    |> add "tas" (empty
      |> add "transitioning" 0.33
      |> add "other" 0.39
    )
    |> add "act" (empty
      |> add "transitioning" 0.47
      |> add "other" 0.66
    )
    |> add "nt" (empty
      |> add "transitioning" 0.25
      |> add "other" 0.18
    )
  
  let degradable_organic_carbon_content =
    let open StringMap in
    empty
    |> add "food" 0.15
    |> add "paper" 0.4
    |> add "garden_and_green" 0.2
    |> add "wood" 0.43
    |> add "textiles" 0.24
    |> add "sludge" 0.05
    |> add "nappies" 0.24
    |> add "rubber_and_leather" 0.39
    |> add "inert_waste" 0.
    |> add "alternative_waste_treatment_residues" 0.08
    
end