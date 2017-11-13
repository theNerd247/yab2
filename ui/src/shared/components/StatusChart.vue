<template>
<home-card title="Current Balances">
		<el-table slot="content" :data="statuses" :row-class-name="budgetStatus">
			<el-table-column prop="name" label="Budget" ></el-table-column>
			<el-table-column prop="amount" label="Amount" > </el-table-column>
			<el-table-column prop="limit" label="Limit"> </el-table-column>
	</el-table>
</home-card>
</template>

<script>
import Vue from 'vue'
import HomeCard from './HomeCard.vue'
import { HTTP } from '@/shared/http-common.js'
import _ from 'lodash'

export default {
  components: {
    HomeCard,
	},
	data  () {
		return {
			statuses: []
		}
	},
	created () {
		HTTP.get("budget-list/status")
			.then(resp => {
				this.statuses = _.map(resp.data.items, x => { 
					return { 
						name: x[0],
						limit: x[1],
						amount: x[2],
					}
				});
			});
	},
  methods: {
    budgetStatus(budget) {
      if(budget.amount > budget.limit)
        return 'alert-row';
      else
        return '';
    }
  }
}
</script>
