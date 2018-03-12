##### Why should I balance my test set?
Balancing your data ensures that your test set has data from every label. **Your test set is balanced by default.**

[TODO : Learn More ]()

<div class="row well m15 p15">
    <div class="col-sm-6">
        <h6 class="center">Not balanced</h6>
        <table class="table table-bordered" style="background-color: #fff;">
            <tbody>
                <tr>
                    <th class="left">Negative</th>
                    <td class="success">710</td>
                    <td class="warning">63</td>
                    <td class="warning">27</td>
                </tr>
                <tr>
                    <th class="left">Neutral</th>
                    <td class="danger">128</td>
                    <td class="success">151</td>
                    <td class="warning">29</td>
                </tr>
                <tr>
                    <th class="left">Positive</th>
                    <td class="warning">46</td>
                    <td class="warning">31</td>
                    <td class="success">166</td>
                </tr>
            </tbody>
        </table>
    </div>
    <div class="col-sm-6">
        <h6 class="center">Balanced</h6>
        <table class="table table-bordered" style="background-color: #fff;">
            <tbody>
                <tr>
                    <th class="left">Negative</th>
                    <td class="success">695</td>
                    <td class="warning">72</td>
                    <td class="warning">32</td>
                </tr>
                <tr>
                    <th class="left">Neutral</th>
                    <td class="danger">107</td>
                    <td class="success">169</td>
                    <td class="warning">30</td>
                </tr>
                <tr>
                    <th class="left">Positive</th>
                    <td class="warning">39</td>
                    <td class="warning">29</td>
                    <td class="success">176</td>
                </tr>
            </tbody>
        </table>
    </div>
</div>