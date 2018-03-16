##### Why should I balance my test set?
Balancing your data ensures that your test set has data from every label. **Your test set is balanced by default.**

[TODO : Learn More ]()

<div class="row well m15 p15">
    <div class="col-sm-6">
        <h6 class="center">Not balanced</h6>
        <table class="table table-bordered confusion-matrix" style="background-color: #fff;">
            <tbody>
                <tr>
                    <th class="left">Negative</th>
                    <td class="good">710</td>
                    <td class="medium">63</td>
                    <td class="medium">27</td>
                </tr>
                <tr>
                    <th class="left">Neutral</th>
                    <td class="really-bad">128</td>
                    <td class="good">151</td>
                    <td class="medium">29</td>
                </tr>
                <tr>
                    <th class="left">Positive</th>
                    <td class="medium">46</td>
                    <td class="medium">31</td>
                    <td class="good">166</td>
                </tr>
            </tbody>
        </table>
    </div>
    <div class="col-sm-6">
        <h6 class="center">Balanced</h6>
        <table class="table table-bordered confusion-matrix" style="background-color: #fff;">
            <tbody>
                <tr>
                    <th class="left">Negative</th>
                    <td class="good">695</td>
                    <td class="medium">72</td>
                    <td class="medium">32</td>
                </tr>
                <tr>
                    <th class="left">Neutral</th>
                    <td class="really-bad">107</td>
                    <td class="good">169</td>
                    <td class="medium">30</td>
                </tr>
                <tr>
                    <th class="left">Positive</th>
                    <td class="medium">39</td>
                    <td class="medium">29</td>
                    <td class="good">176</td>
                </tr>
            </tbody>
        </table>
    </div>
</div>