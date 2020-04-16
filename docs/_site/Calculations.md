Below are the detailed explanations for each of the calculations made within the script:

## Outputs in Files

---

### [cell name].csv

---

#### Q.c / Q.d
the charge/discharge capacity *per unit mass* (if the masses are not provided, this operation is not performed):

![f2][Q mass equation]

---

#### CC
the continuous capacity (units depend on if the masses are present):

![f3][CC equation]

---

**`CE`** : the coulombic efficiency:

![f4][CE equation]

---

### [cell name] dQdV Data.csv

---

#### c_d
indicates whether or not the particular data applies to a charge or discharge step, This value will be `1` if the following equation returns `TRUE`, else the value will be `0`:

![f8][c_d equation]

Though not reflected in this value, the direction of the step (charge or discharge) is later determined by the sign of `Current(A)` from the **input file**.

---

#### dQdV
the differential capacity:

![f5][dQdV equation]

a `0` is appended at the beginning of the data frame to ensure the column lengths remain equal.

---

#### F_L
indicates whether or not the particular data is the first of a given rate series (applied *per cycle*). This value will be `1` if the following equation returns `TRUE`, else the value will be `0`:

![f7][F_L equation]

---

### [cell name] Cycle Facts.csv

---

#### chV / dchV
the average charge/discharge voltage of each cycle:

![f1][chV equation]

where `a` and `b` are the voltage limits in which the cell was cycled and `x` is the discharge capacity.

---

#### avgV
the average voltage of a given cycle:

![f6][avgV equation]

---

#### dV
the delta voltage of a given cycle:

![f7][dV equation]

---

#### DCap / CCap
the discharge/charge capacity of a given cycle.

This is the last measurement of `Discharge_Capacity(Ah)` or `Charge_Capacity(Ah)` within the cycle from the **input file**.

If masses are specified, the values of `Q.d` and `Q.c` will be used instead.

---

#### CE
the columbic efficiency of a given cycle.

The last value of `CE` in modified **input file**.

[chV equation]: http://mathurl.com/render.cgi?chV%20%3D%20%5Cfrac%7B1%7D%7Ba%20+%20b%7D%20%5Cint_a%5Eb%20f%28x%29%20%20%5Cmathrm%7Bd%7Dx%5Cnocache

[Q mass equation]: http://mathurl.com/render.cgi?Q%20%3D%20%5Cfrac%7BRaw%20%5C%20Capacity%20%5C%20%28Ah%29%7D%7Bmass%20%5C%20%28g%29%7D%5Cnocache

[CC equation]: http://mathurl.com/render.cgi?CC%20%3D%20Charge%20%5C%20Capacity%20%5C%20%28Ah%20%5C%20or%20%5C%20%5Cfrac%7BmAh%7D%7Bg%7D%29%20-%20Discharge%20%5C%20Capacity%20%5C%20%28Ah%20%5C%20or%20%5C%20%5Cfrac%7BmAh%7D%7Bg%7D%29%5Cnocache

[CE equation]: http://mathurl.com/render.cgi?CE%20%3D%20%5Cfrac%7BDischarge%20%5C%20Capacity%20%5C%20%28Ah%29%7D%7BCharge%20%5C%20Capacity%20%5C%20%28Ah%29%7D%5Cnocache

[dQdV equation]: http://mathurl.com/render.cgi?dQdV%20%3D%20%5Cfrac%7BPresent%20%5C%20Discharge%20%5C%20Capacity%20%5C%20%28Ah%29%20%5C%20-%20%5C%20Previous%20%5C%20Discharge%20%5C%20Capacity%20%5C%20Measurement%20%5C%20%28Ah%29%7D%7BPresent%20%5C%20Voltage%20%5C%20%28V%29%20%5C%20-%20%5C%20Previous%20%5C%20Voltage%20%5C%20%28V%29%7D%5Cnocache

[avgV equation]: http://mathurl.com/render.cgi?avgV%20%3D%20%5Cfrac%7BchV%20+%20dchV%7D%7B2%7D%5Cnocache

[dV equation]: http://mathurl.com/render.cgi?dV%20%3D%20chV%20-%20dchV%5Cnocache

[F_L equation]: http://mathurl.com/render.cgi?Present%20%5C%20Current%20%5C%20%28A%29%20%5C%20-%20%5C%20Previous%20%5C%20Current%20%5C%20%28A%29%20%3E%200.0005%0A%5Cnocache

[c_d equation]: http://mathurl.com/render.cgi?%5Cmid%20Last%20%5C%20Voltage%20%5C%20Measurement%20%5C%20%28V%29%20%5C%20-%20%5C%20First%20%5C%20Voltage%20%5C%20Measurement%20%5C%20%28V%29%20%5Cmid%20%5C%20%3E%200.5%5Cnocache