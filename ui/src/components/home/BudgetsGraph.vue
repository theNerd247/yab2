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

export default {
  components: {
    HomeCard,
    BalancesChart
	},
	data  () {
		return {
			balancesData: null,
		}
	},
  mounted () {
		let ys = [];
		let xs = [];
		let is = [];
		for (var i = 0; i < 30; i++){
			is[i] = i;
			xs[i] = 0.2*Math.cos(6.28/30*i);
			ys[i] = Math.sin(6.28/30*i);
		};

		this.balancesData = {
			labels: is,
			datasets: [
				{
					label: 'Budget',
					backgroundColor: 'rgba(32, 160,255, 0.5)',
					data: ys
				},
				{
					label: 'Expenses',
					backgroundColor: 'rgba(19, 206, 102, 0.5)',
					data: xs 
				}
			]
		};    
	},
}
</script>
