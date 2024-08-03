

(* Equation 6 *)

(* PW is the quantity of putrescible eligible waste processed in the reporting period
EW is the total quantity of eligible waste received in tonnes worked out using equation 7
QRP is the quantity of residual waste disposed of during the reporting period in tonnes.
QNBP is the quantity of non-biobased products manufactured during the reporting period, in tonnes.
HW is the historic quantity of putrescible eligible waste for the reporting period found using section 27.  *)

let putrescible_waste ew qrp qnbp hw : float =
  ew -. (qrp +. qnbp +. hw)


  (* Equation 7 *)
  
  (* QMSW is the quantity of the eligible waste which is municipal solid waste in tonnes.
  QC&I is the quantity of the eligible waste which is commercial and industrial waste in tonnes.
QC&D is the quantity of the eligible waste which is construction and demolition waste in tonnes. *)

let eligible_waste qmsw qci qcd : float = 
  qmsw +. qci +. qcd


(* Equation 8 *)

(* HQEW is the quantity of eligible waste received by the facility in the relevant 24 month period, in tonnes, *worked using appropriate evidence.*
HQRW is the quantity of residual waste disposed of by the facility during the relevant 24 month period in tonnes, *worked using appropriate evidence.*
HQNBP is the quantity of non-biobased products manufactured by the facility during the relevant 24 month period in tonnes, *worked using appropriate evidence*. *)

let historic_putrescible_waste hqew hqrw hqnbp : float =
  hqew -. (hqrw +. hqnbp)
  
(* Equation 13 *)

(* QTW is the total quantity of putrecible waste received during the reporting period, in tonnes.
QRW is the quantity of residual waste disposed of during the reporting period, in tonnes, worked out in accordance with the monitoring requirements.
QNBP is the quantity of non-biobased products manufactured during the reporting period, in tonnes, worked out in accordance with the monitoring requirements. *)
let total_putrescible_waste qtw qrw qnhp : float =
  qtw -. qrw -. qnhp

