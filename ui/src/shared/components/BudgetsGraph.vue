<template>
<home-card title="Budget Status">
		<div slot="action">
			<el-button @click=httpGetStatus()>Update</el-button>
		</div>
	  <div slot="content">
    <el-row>
    <el-form :inline="true">
      <el-form-item label="Start Date">
        <el-date-picker
          v-model="sdate"
          type="date"
          format="yyyy-MM-dd"
          placeholder="Date">
        </el-date-picker>
      </el-form-item>
      <el-form-item label="End Date">
        <el-date-picker
          v-model="edate"
          type="date"
          format="yyyy-MM-dd"
          placeholder="Date">
        </el-date-picker>
      </el-form-item>
      <el-form-item>
      </el-form-item>
      </el-form>
  </el-row>
  <el-row>
    <balances-graph ref="bgraph" width="844" :chart-data="balancesData" :options="options"></balances-graph>
  </el-row>
	</div>
</home-card>

</template>

<script>
import Vue from 'vue'
import BalancesGraph from './BalancesGraph.vue'
import HomeCard from './HomeCard.vue'
import _ from 'lodash'
import { HTTP, baseURL, httpWithNotify } from '@/shared/http-common'
import moment from 'moment'

export default {
  components: {
    HomeCard,
    BalancesGraph,
	},
	props: ['budgetName'],
	data  () {
		return {
			sdate: moment().subtract(30, 'days').format("YYYY-MM-DD"),
			edate: moment().format("YYYY-MM-DD"),
			balancesData: null,
      budgetStatus: [],
			options: {
        responsive: false,
				elements: {
					line: {
						tension: 0
					}
				}
			}
		}
	},
  mounted () {
    this.httpGetStatus();
	},
  methods: {
    httpGetStatus(){
			let query = "/budget-list/name/"+this.budgetName+"/status/between";

      httpWithNotify(
        '',
        "Couldn't get budget data",
        HTTP.get(query, { params: { 
            sdate: moment(this.sdate).format("YYYY-MM-DD"),
            edate: moment(this.edate).format("YYYY-MM-DD")
        }}),
        true
      )
      .then(d => {
        this.budgetStatus = d.items;
        this.makeBudgetData();
      })
		},
		makeBudgetData () {
			let ds = _.map(this.budgetStatus, x => moment(x[0]).format("YYYY-MM-DD"));
			let bs = _.map(this.budgetStatus, x => x[1]);
			let es = _.map(this.budgetStatus, x => x[2]);

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
