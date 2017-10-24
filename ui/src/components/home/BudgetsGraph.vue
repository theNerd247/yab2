<template>
<home-card title="Budget Status">
	<div slot="content">
		<balances-chart ref="bchart" :chart-data="balancesData"></balances-chart>
	</div>
</home-card>

</template>

<script>
import Vue from 'vue'
import BalancesChart from '@/shared/BalancesChart.vue'
import HomeCard from '@/shared/HomeCard.vue'
import _ from 'lodash'
import { HTTP } from '@/shared/http-common'
import moment from 'moment'

export default {
  components: {
    HomeCard,
    BalancesChart
	},
	data  () {
		return {
			balancesData: null,
      budgetStatus: []
		}
	},
  mounted () {
    this.httpGetStatus("tst");
    let bs = _.map(this.budgetStatus, x => x[0]);
    let es = _.map(this.budgetStatus, x => x[1]);
    let ds = _.map(this.budgetStatus, x => x[2]);

		this.balancesData = {
			labels: ds,
			datasets: [
				{
					label: 'Budget',
					backgroundColor: 'rgba(32, 160,255, 0.5)',
					data: bs
				},
				{
					label: 'Expenses',
					backgroundColor: 'rgba(19, 206, 102, 0.5)',
					data: es 
				}
			]
		};
	},
  methods: {
    httpGetStatus(name){
			let sdate = moment().subtract(30, 'days').format("YYYY-MM-DD");
			let edate = moment().format("YYYY-MM-DD");
			let query = "budget/budget-status/" + name + "/" + sdate + "/" + edate;

			HTTP.get(query)
				.then(response => {
          console.log(response.data.length);
					this.budgetStatus = response.data;
				})
				.catch(e => {
					this.$notify.error({
						title: 'Error',
						message: 'Could not get budget data at: ' + query,
            duration: 0
					})
				});
		}
  }
}
</script>
