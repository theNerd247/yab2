<template>
<home-card title="Budget Status">
	<div slot="content">
		<balances-graph ref="bgraph" :chart-data="balancesData"></balances-graph>
	</div>
</home-card>

</template>

<script>
import Vue from 'vue'
import BalancesGraph from './BalancesGraph.vue'
import HomeCard from './HomeCard.vue'
import _ from 'lodash'
import { HTTP } from '@/shared/http-common'
import moment from 'moment'

export default {
  components: {
    HomeCard,
    BalancesGraph
	},
	props: ['budgetName'],
	data  () {
		return {
			balancesData: null,
      budgetStatus: [],
		}
	},
  mounted () {
    this.httpGetStatus();
	},
  methods: {
    httpGetStatus(){
			let sdate = moment().subtract(30, 'days').format("YYYY-MM-DD");
			let edate = moment().format("YYYY-MM-DD");
			let query = "budget/budget-status/" + this.budgetName + "/" + sdate + "/" + edate;

			HTTP.get(query)
				.then(response => {
					this.budgetStatus = response.data;
					this.makeBudgetData();
				})
				.catch(e => {
					this.$notify.error({
						title: 'Error',
						message: 'Could not get budget data at: ' + query,
            duration: 0
					})
				});
		},
		makeBudgetData () {
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
		}
  }
}
</script>
