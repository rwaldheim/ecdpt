
graphInfoTable <- HTML('
              <style type="text/css">
              .tg  {border-collapse: collapse;border-spacing: 0;}
              .tg td{font-family: Arial, sans-serif;font-size: 14px;padding: 10px 5px;border-style: solid;border-width: 1px;overflow: hidden;word-break: normal;border-color: black;}
              .tg th{font-family: Arial, sans-serif;font-size: 14px;font-weight: normal;padding: 10px 5px;border-style: solid;border-width: 1px;overflow: hidden;word-break: normal;border-color: black;}
              .tg .tg-gfnm{background-color: #efefef;border-color: #000000;text-align: center;vertical-align: middle}
              .tg .tg-i0p4{font-weight: bold;background-color: #ecf4ff;border-color: #000000;text-align: center;vertical-align: middle}
              .tg .tg-3fas{background-color: #efefef;border-color: #000000;text-align: left;vertical-align: middle}
              .tg .tg-o3hj{background-color: #ecf4ff;border-color: #000000;text-align: center;vertical-align: middle}
              .tg .tg-xwyw{border-color: #000000;text-align: center;vertical-align: middle}
              .tg .tg-0a7q{border-color: #000000;text-align: left;vertical-align: middle}
              </style>
              <table class="tg">
                <tr>
                  <th class="tg-i0p4">Graph</th>
                  <th class="tg-i0p4">X Axis</th>
                  <th class="tg-i0p4">Y Axis</th>
                  <th class="tg-o3hj"><span style="font-weight: bold">Plot Frequency</span><br></th>
                  <th class="tg-i0p4">Description</th>
                </tr>
                <tr>
                  <td class="tg-xwyw">dQdV Graph</td>
                  <td class="tg-xwyw">Voltage (V)</td>
                  <td class="tg-xwyw">dQdV (Ah/V)</td>
                  <td class="tg-xwyw">per cycle</td>
                  <td class="tg-0a7q">The differential capacity plot for each cycle<br></td>
                </tr>
                <tr>
                  <td class="tg-gfnm">Voltage Profile</td>
                  <td class="tg-gfnm">Continuous Capacity (mAh/g or Ah)</td>
                  <td class="tg-gfnm">Voltage (V)</td>
                  <td class="tg-gfnm">per cycle</td>
                  <td class="tg-3fas">Voltage vs. capacity plot for each cycle. Units depend if the masses are specified.</td>
                </tr>
                <tr>
                  <td class="tg-xwyw">Voltage vs. Time</td>
                  <td class="tg-xwyw">Time (min)</td>
                  <td class="tg-xwyw">Voltage (V)</td>
                  <td class="tg-xwyw">per cycle</td>
                  <td class="tg-0a7q">The voltage as a function of time, including all steps</td>
                </tr>
                <tr>
                  <td class="tg-gfnm">Discharge Capacity</td>
                  <td class="tg-gfnm">Cycle</td>
                  <td class="tg-gfnm">Discharge Capacity (mAh/g or Ah)</td>
                  <td class="tg-gfnm">per cell</td>
                  <td class="tg-3fas">Discharge capacity for each <span style="font-weight: bold">individual cell </span>per cycle. Coulombic efficiency is also plotted on a secondary axis. Units depend if the masses are specified.</td>
                </tr>
                <tr>
                  <td class="tg-xwyw">Discharge Areal Capacity</td>
                  <td class="tg-xwyw">Cycle</td>
                  <td class="tg-xwyw">Discharge Capacity (Ah/cm<sup>2</sup>)</td>
                  <td class="tg-xwyw">per cell</td>
                  <td class="tg-0a7q">Discharge areal capacity for each <span style="font-weight: bold">individual cell </span>per cycle. Coulombic efficiency is also plotted on a secondary axis.</td>
                </tr>
                <tr>
                  <td class="tg-gfnm">Total Discharge Capacity</td>
                  <td class="tg-gfnm">Cycle</td>
                  <td class="tg-gfnm">Discharge Capacity (mAh/g or Ah)</td>
                  <td class="tg-gfnm">per analysis</td>
                  <td class="tg-3fas">Discharge capacity summarized for <span style="font-weight: bold">all cells</span> in the analysis. Coulombic efficiency is also plotted on a secondary axis. Mean is plotted as a point with error bars presenting the standard error between the cells. Units depend if the masses are specified.</td>
                </tr>
                <tr>
                  <td class="tg-xwyw">Average Voltage</td>
                  <td class="tg-xwyw">Cycle</td>
                  <td class="tg-xwyw">Voltage (V)</td>
                  <td class="tg-xwyw">per cell</td>
                  <td class="tg-0a7q">The average voltage vs capacity for each cycle. The charge voltage (V<sub>charge</sub>) and discharge voltage (V<sub>discharge</sub>) were calculated using the average value theorem. The average voltage is then (V<sub>charge</sub> + V<sub>discharge</sub>)/2. Charge and discharge voltages are plotted alongside the average.</td>
                </tr>
                <tr>
                  <td class="tg-gfnm">Delta Voltage</td>
                  <td class="tg-gfnm">Cycle</td>
                  <td class="tg-gfnm">Voltage (V)</td>
                  <td class="tg-gfnm">per cell</td>
                  <td class="tg-3fas">The delta voltage vs capacity for each cycle. The charge voltage (V<sub>charge</sub>) and discharge voltage (V<sub>discharge</sub>) were calculated using the average value theorem. The delta voltage is then V<sub>charge</sub> - V<sub>discharge</sub>). Charge and discharge voltages are plotted alongside the average.</td>
                </tr>
                <tr>
                  <td class="tg-xwyw">Capacity Loss</td>
                  <td class="tg-xwyw">Cycle</td>
                  <td class="tg-xwyw">Capacity (mAh/g or Ah)</td>
                  <td class="tg-xwyw">per cell</td>
                  <td class="tg-0a7q">The discharge capacity minus the charge capacity for each cycle. Units depend if the masses are specified.</td>
                </tr>
              </table>
            ')
