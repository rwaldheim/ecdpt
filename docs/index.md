# Welcome to the Arbin Battery Analysis Tool!

This program, written in [R](https://www.r-project.org/) using the [Shiny](https://shiny.rstudio.com/) package, aims to greatly simplify the battery analysis process and accelerating diagnostic data return on energy storage cells cycling on an Arbin battery cycler. It works by taking the raw export files from the [Arbin MITS Pro](https://www.arbin.com/software/) software and automating many standard analysis techniques while also allowing for rapid generation of common visual aids. 

If you are new to the program and would like to get started, see our [Installation](Installation.md) page!

If you already have all the required software installed and configured, see our [Begin Analyzing](begin-analyzing.md)!

## High-Level Operation

Let's start with the basics: what are the inputs?

### Inputs

At a minimum, all the application needs is the '''data file''' as exported from the Arbin Excel macro. The data file from Arbin is characterized as a large Excel file that contains the following information:

<table>
  <tr>
    <th colspan="3">ArbinExportFile.xlsx</th>
  </tr>
  <tr>
    <td><code>Date_Time</code></td>
    <td><code>Test_Time(s)</code>*</td>
    <td><code>Step_Time(s)</code></td>
  </tr>
  <tr>
    <td><code>Step_Index</code>*</td>
    <td><code>Cycle_Index</code>*</td>
    <td><code>Voltage(V)</code>*</td>
  </tr>
  <tr>
    <td><code>Current(A)</code>*</td>
    <td><code>Discharge_Capacity(Ah)</code>*</td>
    <td><code>Charge_Capacity(Ah)</code>*</td>
  </tr>
  <tr>
    <td><code>Charge_Time(s)</code></td>
    <td><code>DisCharge_Time(s)</code></td>
    <td><code>Charge_Energy(Wh)</code></td>
  </tr>
  <tr>
    <td><code>Discharge_Energy</code></td>
    <td><code>Internal Resistance(Ohm)</code></td>
    <td><code>dV/dt(V/s)</code></td>
  </tr>
</table>

The exact format and which values are present are configurable within the Arbin software. In order for the program to execute corrrectly, the values marked with a (*) should be present.

Some other inputs that are optional, depending on the desired outputs, are the **area of the limiting electrode**, **weight percent of active material in the limiting electrode**, and the **capacity of the limiting active material**. 

### Outputs

Now that we've established what the required inputs are, what does the program generate? 

Regardless of inputs selected, the program will generate a directory (folder) in which all data will be placed, then a subdirectory for each cell analyzed. If select graphs are chosen to be generated, these will create directries of their own within each respective cell's folder. 

In its default state, the program will generate numerous data files placed throughout this file system. At the highest level (in the main folder), there will be two files generated: a **[directory name] Summary.csv** and **[directory name] Total.csv**. The **Summary** file will contain all the values in the input file, as well as some additional metrics, averaged on a *per cycle* basis for each cell individually. The **Total** file will contain all the raw data imported for each cell, concatenated into one large file. Two additional files that could be present, based on the user selection, are the **Total Discharge Capacity** and **Total Discharge Areal Capacity** graphs.

Within each cell's directory, there will be three data files generated: **[cell name].csv**, **[cell name] dQdV Data.csv**, and **[cell name] Cycle Facts.csv**. The contents of each file are as follows:

<table>
  <tr>
    <th>File</th>
    <th>Attributes</th>
  </tr>
  <tr>
    <td>[cell name].csv</td>
    <td><ul><li>the raw data from the input file plus some additional columns:</li><ul><li><code>Q.d</code> : Discharge Capacity (mAh/g)*</li><li><code>Q.c</code> : Charge Capacity (mAh/g)*</li><li><code>CC</code> : Continuous Capacity (if masses are specifed, units are mAh/g, and Ah if not)</li><li><code>CE</code> : Coulombic Efficiency (%)</li><ul><ul>
  <tr>
    <td>[cell name] dQdV Data.csv</td>
    <td><ul><li>an index<br></li><li><code>cycle</code> : the cycle number</li><li><code>cell</code> : the cell number (expressed as the order in which they were processed)</li><li><code>c_d</code> : indicated whether the given values correlate to a charge or discharge cycle</li><li><code>voltage</code> : the voltage (V)</li><li><code>dQdV</code> : the differential capacity (Ah/V)</li><li><code>F_L</code> : indictes whether the given values correlate to the first of a new rate</li><ul>
  <tr>
    <td>[cell name] Cycle Facts.csv</td>
    <td><ul><li>an index</li><li><code>cycle</code> : the cycle number</li><li><code>cell</code> : the cell number (expressed as the order in which they were processed)</li><li><code>chV</code> : the charge voltage (V)</li><li><code>dchV</code> : the discharge voltage (V)</li><li><code>avgV</code> : the average voltage (V)</li><li><code>dV</code> : the delta voltage (V)</li><li><code>DCap</code> : the dsicharge capacity (either mAh/g or Ah)</li><li><code>CCap</code> : the charge capacity (either mAh/g or Ah)</li><li><code>CE</code> : the coulombic efficiency (%)</li><ul></td>
  </tr>
</table>
      
Values with (*) are only present if the cell masses are specified

If you would like to know how each of these values are calculated, see our [[Calculations]] page.

In addition to all the data files, there are multiple graphs than can be generated alongside for quick and easy diagnostics. The following graphs will be generated on a *per cycle* basis, placed in their own respective subdirectories due to the large number of filed generated:

- dQdV Graphs
- Voltage Profiles
- Voltage vs. Time

The following are plotted on a *per cell* basis, and will live in the root directory of said cell:

- Discharge Capacity (plus Coulombic Efficiency)
- Discharge Areal Capacity (plus Coulombic Efficiency)*
- Average Voltage
- Delta Voltage

The Discharge Areal Capacity (*) can only be plotted if the area of the limiting electrode is specified.

Lastly, the **Total Discharge Capacity** graph is a summation of all cells within the set and plots the mean capacity with respective error bars. Columbic efficiency is also averaged and plotted within the same graph. This graph will be placed in the root directory created by the program.
