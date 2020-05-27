# Welcome to the Electrochemical Data Processing Tool (EcDPT)!

This program, written in [R](https://www.r-project.org/) using the [Shiny](https://shiny.rstudio.com/) package, aims to simplify the battery analysis process and acelerate diagnostic data return on energy storage cells cycling on an Arbin battery cycler. After uploading the raw Excel files from the [Arbin MITS Pro](https://www.arbin.com/software/) software to the program, it automates many standard analysis techniques while also allowing for rapid generation of common battery analysis graphs. The Arbin Battery Analysis Tool was developed by and is currently maintained by the Materials Innovation group at [Birla Carbon](https://www.birlacarbon.com). 

If you are new to the program and would like to get started, see our [Installation](Installation.md) page.

If you already have all the required software installed and configured, see our [Begin Analyzing](begin-analyzing.md) page.

We encourage all who desire to contribute to use the [issues](https://github.com/rwaldheim/ecdpt/issues) to highlight bugs or feature requests and submit pull requests as needed!

## High-Level Operation

Let's start with the basics: what are the inputs?

### Inputs

At a minimum, all the application needs is the **data file** as exported from the Arbin Excel macro. The data file from Arbin is characterized as a large Excel file that contains the following information:

<table class="center">
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

The exact format and which values are present are configurable within the Arbin software. In order for the program to execute corrrectly, the values marked with a (\*) should be present.

All other inputs are optional, depending on the desired outputs.

### Outputs

Now that we've established what the required inputs are, what does the program generate? 

Regardless of inputs selected, the program will generate a directory (folder) in which all data will be placed, then a subdirectory for each cell analyzed. 

In its default state, the program will generate numerous data files placed throughout this file system. At the highest level (in the main folder), there will be four files generated: **[directory name] Total.csv**, **[directory name] dQdV Data.csv**, **[directory name] Cycle Facts.csv**, and **[directory name] Summary.csv**. The **Total** file will contain all the raw data imported for each cell, concatenated into one large file for easy import into other analyzers. The contents of the remaining files are as follows:

<table>
  <tr>
    <th>File</th>
    <th>Attributes</th>
  </tr>
  <tr>
    <td>[directory name] dQdV Data.csv</td>
    <td style="text-align: left;">
      <ul>
        <li>an index<br></li>
        <li><code>cycle</code> : the cycle number</li>
        <li><code>cell</code> : the cell number (expressed as the order in which they were processed)</li>
        <li><code>c_d</code> : indicated whether the given values correlate to a charge or discharge cycle</li>
        <li><code>voltage</code> : the voltage (V)</li>
        <li><code>dQdV</code> : the differential capacity (Ah/V)</li>
        <li><code>F_L</code> : indicates whether the given values correlate to the first of a new rate</li>
      </ul>
    </td>
  </tr>
  <tr>
    <td>[directory name] Cycle Facts.csv</td>
    <td style="text-align: left;">
      <ul>
        <li>an index</li>
        <li><code>cycle</code> : the cycle number</li>
        <li><code>cell</code> : the cell number (expressed as the order in which they were processed)</li>
        <li><code>chV</code> : the charge voltage (V)</li>
        <li><code>dchV</code> : the discharge voltage (V)</li>
        <li><code>avgV</code> : the average voltage (V)</li>
        <li><code>dV</code> : the delta voltage (V)</li>
        <li><code>DCap</code> : the discharge capacity (mAh/g or Ah)</li>
        <li><code>CCap</code> : the charge capacity (mAh/g or Ah)</li>
        <li><code>CE</code> : the coulombic efficiency (%)</li>
        <li><code>lostCap</code> : the capacity lost between the current and previous cycle (mAh/g or Ah)</li>
      </ul>
    </td>
  </tr>
  <tr>
  <td>[directory name] Summmary.csv</td>
  <td style="text-align: left;">
    <ul>
      <li>an index<br></li>
      <li><code>cycle</code> : the cycle number</li>
      <li><code>DCap</code> : the average discharge capacity for the cells within the analysis (mAh/g or Ah)</li>
      <li><code>CE</code> : the average coulombic efficiency for the cells within the analysis (%)</li>
      <li><code>capSE</code> : the discharge capacity standard error (mAh/g or Ah)</li>
      <li><code>ceSE</code> : the coulombic efficiency standard error (%)</li>
    </ul>
  </td>
</tr>
</table>
      
Values with (\*) are only present if the cell masses are specified

If you would like to know how each of these values are calculated, see our [Calculations](Calculations.md) page.

In addition to all the data files, there are multiple graphs than can be generated alongside for quick and easy diagnostics. 

<table>
  <tr>
    <th>Graph</th>
    <th>X Axis</th>
    <th>Y Axis</th>
    <th><span style="font-weight:bold">Plot Frequency</span><br></th>
  </tr>
  <tr>
    <td>Discharge Capacity</td>
    <td>Cycle</td>
    <td>Discharge Capacity (mAh/g or Ah)</td>
    <td>per cell</td>
  </tr>
  <tr>
    <td>Discharge Areal Capacity</td>
    <td>Cycle</td>
    <td>Discharge Capacity (Ah/cm^2)</td>
    <td>per cell</td>
  </tr>
  <tr>
    <td>Total Discharge Capacity</td>
    <td>Cycle</td>
    <td>Discharge Capacity (mAh/g or Ah)</td>
    <td>per analysis</td>
  </tr>
  <tr>
    <td>Average Voltage</td>
    <td>Cycle</td>
    <td>Voltage (V)</td>
    <td>per cell</td>
  </tr>
  <tr>
    <td>Delta Voltage</td>
    <td>Cycle</td>
    <td>Voltage (V)</td>
    <td>per cell</td>
  </tr>
  <tr>
    <td>Capacity Loss</td>
    <td>Cycle</td>
    <td>Capacity (mAh/g or Ah)</td>
    <td>per cell</td>
  </tr>
</table>

The Discharge Areal Capacity (\*) can only be plotted if the area of the limiting electrode is specified.

Lastly, the **Total Discharge Capacity** graph is a summation of all cells within the set and plots the mean capacity with respective error bars. Columbic efficiency is also averaged and plotted within the same graph.
