<template>
<home-card title="Budget Status">
		<div slot="action">
			<el-button @click=httpGetStatus()>Update</el-button>
		</div>>
	  <div slot="content">
    <el-row>
    <el-form :inline="true">
      <el-form-item label="Start Date">
        <el-input placeholder="Start Date" v-model="sdate"></el-input>
      </el-form-item>
      <el-form-item label="End Date">
        <el-input placeholder="End Date" v-model="edate"></el-input>
      </el-form-item>
      <el-form-item>
      </el-form-item>
      </el-form>
  </el-row>
  <el-row>
    <balances-graph ref="bgraph" :chart-data="balancesData"></balances-graph>
  </el-row>
	</div>
</home-card>

</template>

<script>
import Vue from 'vue'
import BalancesGraph from './BalancesGraph.vue'
import HomeCard from './HomeCard.vue'
import _ from 'lodash'
import { HTTP, baseURL } from '@/shared/http-common'
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
		}
	},
  mounted () {
    this.httpGetStatus();
	},
  methods: {
    httpGetStatus(){
			let query = "/budget-list/name/"+this.budgetName+"/status/between";

			HTTP.get(query, {
        params: { 
          sdate: this.sdate,
          edate: this.edate
        }
      })
      .then(response => {
        this.budgetStatus = response.data.items;
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
